(ns dynamic-recursion-cards 
  (:require
   [cljs.core.async :as async]
   [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
   [com.fulcrologic.fulcro.data-fetch :as df]
   [com.fulcrologic.fulcro.dom :as dom]
   [com.fulcrologic.fulcro.mutations :as m]
   [com.fulcrologic.fulcro.networking.mock-server-remote :refer [mock-http-server]]
   [com.fulcrologic.fulcro.react.hooks :as hooks]
   [com.wsscode.pathom3.connect.indexes :as pci]
   [com.wsscode.pathom3.connect.operation :as pco]
   [com.wsscode.pathom3.interface.async.eql :as p.a.eql]
   [nubank.workspaces.card-types.fulcro3 :as ct.fulcro]
   [nubank.workspaces.core :as ws]))

(defonce pretend-server-database
  (atom
    {:recipe/id           {1 {:recipe/id         1
                              :recipe/name       "Pancake Batter"
                              :recipe/line-items [{:recipe-line-item/id 10}
                                                  {:recipe-line-item/id 11}
                                                  {:recipe-line-item/id 12}]}
                           2 {:recipe/id         2
                              :recipe/name       "Yogurt"
                              :recipe/line-items [{:recipe-line-item/id 13}
                                                  {:recipe-line-item/id 14}]}}
     :ingredient/id       {101 {:ingredient/id   101
                                :ingredient/name "Eggs"}
                           102 {:ingredient/id   102
                                :ingredient/name "Flour"}
                           103 {:ingredient/id   103
                                :ingredient/name "Milk"}
                           104 {:ingredient/id   104
                                :ingredient/name "Yogurt Starter"}}
     :recipe-line-item/id {10 {:recipe-line-item/id     10
                               :recipe-line-item/qty    4
                               :recipe-line-item/uom    :piece
                               :recipe-line-item/ingredient {:ingredient/id 101}}
                           11 {:recipe-line-item/id     11
                               :recipe-line-item/qty    0.25
                               :recipe-line-item/uom    :kilo
                               :recipe-line-item/ingredient {:ingredient/id 102}}
                           12 {:recipe-line-item/id     12
                               :recipe-line-item/qty    0.5
                               :recipe-line-item/uom    :litre
                               :recipe-line-item/sub-recipe {:recipe/id 2}}
                           13 {:recipe-line-item/id     13
                               :recipe-line-item/qty    1
                               :recipe-line-item/uom    :litre
                               :recipe-line-item/ingredient {:ingredient/id 103}}
                           14 {:recipe-line-item/id     14
                               :recipe-line-item/qty    10
                               :recipe-line-item/uom    :gram
                               :recipe-line-item/ingredient {:ingredient/id 104}}}}))

(pco/defresolver recipe-resolver [_ {:recipe/keys [id]}]
  {::pco/input  [:recipe/id]
   ::pco/output [:recipe/name {:recipe/line-items [:recipe-line-item/id]}]}
  (get-in @pretend-server-database [:recipe/id id]))

(pco/defresolver recipe-line-item-resolver [_ {:recipe-line-item/keys [id]}]
  {::pco/input  [:recipe-line-item/id]
   ::pco/output [:recipe-line-item/qty
                 :recipe-line-item/uom
                 {:recipe-line-item/ingredient [:ingredient/id]}
                 {:recipe-line-item/sub-recipe [:recipe/id]}]}
  (when-let [it (get-in @pretend-server-database [:recipe-line-item/id id])]
    (merge
     ;; Provide default values so that Pathom won't complain about missing-attribute:
     {:recipe-line-item/ingredient nil
      :recipe-line-item/sub-recipe nil}
     it)))

(pco/defresolver ingredient-resolver [_ {:ingredient/keys [id]}]
  {::pco/input  [:ingredient/id]
   ::pco/output [:ingredient/name]}
  (get-in @pretend-server-database [:ingredient/id id]))

(pco/defresolver all-recipe-resolver [_ _]
  {::pco/output [{:recipe/all [:recipe/id]}]}
  (let [ids (keys (get @pretend-server-database :recipe/id))]
    {:recipe/all (mapv (fn [id] {:recipe/id id}) ids)}))

(def resolvers [recipe-resolver recipe-line-item-resolver ingredient-resolver all-recipe-resolver])

(def default-env
  (-> {:com.wsscode.pathom3.error/lenient-mode? true}
      #_(p.plugin/register pbip/mutation-resolve-params) ; needed or not?
      (pci/register resolvers)))

(defn process-eql [eql]
  (let [ch (async/promise-chan)]
    (-> (p.a.eql/process default-env eql)
        (.then #(async/go (async/>! ch %))))
    ch))


(declare Recipe ui-recipe)

(defsc Ingredient [_this {:ingredient/keys [name]}]
  {:ident :ingredient/id
   :query [:ingredient/id
           :ingredient/name]}
  (dom/span " " (str name)))

(def ui-ingredient (comp/factory Ingredient {:keyfn :ingredient/id}))

(defsc DynamicRecipe [this {:recipe/keys [id]}]
  {:use-hooks? true}
  (let [recipe (hooks/use-component (comp/any->app this) Recipe {:keep-existing? true}
                                    #_ ; this below would have been necessary if DB - :recipe/id - <id> didn't exist already
                                    {:initialize?    true
                                     :initial-params {:recipe/id id}
                                     :keep-existing? true})]
    ;; Load could be hooked into "expand" mutation to remove side-effect from UI logic
    (hooks/use-lifecycle (fn [] (df/load! this [:recipe/id id] Recipe)))
    (when recipe
      (ui-recipe recipe))))

(def ui-dynamic-recipe (comp/factory DynamicRecipe))

(defsc RecipeReference [this {:ui/keys     [expand?]
                              :recipe/keys [id name]}]
  {:ident :recipe/id
   :query [:ui/expand?
           :recipe/id
           :recipe/name]}
  (if expand?
    (ui-dynamic-recipe {:recipe/id id})
    (dom/span {:style {:cursor "pointer"}
               :onClick (fn [] (m/toggle! this :ui/expand?))}
              " ▶ " (dom/span {:style {:textDecoration "underline"}} name))))

(def ui-recipe-reference (comp/factory RecipeReference {:keyfn :recipe/id}))

(defsc RecipeLineItem [_this {:recipe-line-item/keys [qty uom ingredient sub-recipe]}]
  {:ident :recipe-line-item/id
   :query [:recipe-line-item/id
           :recipe-line-item/qty
           :recipe-line-item/uom
           {:recipe-line-item/ingredient (comp/get-query Ingredient)}
           {:recipe-line-item/sub-recipe (comp/get-query RecipeReference)}]} 
  (dom/ul (str qty " " uom)
    (if (not-empty ingredient)
      (ui-ingredient ingredient)
      (ui-recipe-reference sub-recipe))))

(def ui-line-item (comp/factory RecipeLineItem {:keyfn :recipe-line-item/id}))

(defsc Recipe [_this {:recipe/keys [name line-items]}]
  {:initial-state (fn [{:recipe/keys [id]}] {:recipe/id id}) ; needed for dynamic use
   :ident         :recipe/id
   :query         [:recipe/id
                   :recipe/name
                   {:recipe/line-items (comp/get-query RecipeLineItem)}]}
  (dom/div
    (dom/span {:style {:textDecoration "underline"}}  (str name))
    (dom/ul
      (mapv ui-line-item line-items))))

(def ui-recipe (comp/factory Recipe {:keyfn :recipe/id}))

(defsc RecipeList [this {:recipe-list/keys [recipes]}]
  {:query         [{:recipe-list/recipes (comp/get-query Recipe)}]
   :ident         (fn [] [:component/id ::RecipeList])
   :initial-state {:recipe-list/recipes []}}
  (dom/div {}
    (when-not (seq recipes)
      (dom/button {:onClick (fn []
                              (df/load this :recipe/all Recipe {:target [:component/id ::RecipeList :recipe-list/recipes]}))}
                  "Load all recipes"))
    (dom/ul
      (mapv ui-recipe recipes))))

(ws/defcard dynamic-recursive-entity-card
  (ct.fulcro/fulcro-card
    {::ct.fulcro/wrap-root? true
     ::ct.fulcro/root       RecipeList
     ::ct.fulcro/app        (let [remote      (mock-http-server {:parser process-eql})]
                              {:remotes {:remote remote}})}))

