(ns factorio.compute
  (:require [factorio.items :as items :refer [item-list item-lookup-map]]))

(def intermediate-products [:coal
                             :stone
                             :iron-ore
                             :copper-ore
                             :uranium-ore
                             :lubricant
                             :sulfuric-acid
                             :iron-plate
                             :copper-plate
                             :steel-plate
                             :solid-fuel
                             :plastic-bar
                             :sulfur
                             :battery
                             :explosives
                             :copper-wire
                             :iron-stick
                             :iron-gear-wheel
                             :empty-barrel
                             :electronic-circuit
                             :advanced-circuit
                             :processing-unit
                             :engine-unit
                             :electric-engine-unit
                             :flying-robot-frame
                             :satellite
                             :rocket-part
                             :rocket-control-unit
                             :low-density-structure
                             :rocket-fuel
                             :nuclear-fuel
                             :automation-science-pack
                             :logistic-science-pack
                             :military-science-pack
                             :chemical-science-pack
                             :production-science-pack
                             :utility-science-pack
                             :science])

;; We care about the actual production processes for productivity modules, not the items themselves


(def all-productions
  (concat
   (map (fn [product-name] (first (:production (product-name item-lookup-map)))) intermediate-products)
   [items/basic-oil-processing
    items/advanced-oil-processing
    items/coal-liquefaction
    items/light-oil-cracking
    items/heavy-oil-cracking
    items/solid-fuel-from-heavy-oil
    items/solid-fuel-from-light-oil
    items/solid-fuel-from-petroleum-gas]))

(defn item-to-production [item]
  (let [production (first (:production item))]))

(defn add-resources [a b]
  (merge-with (fnil + 0 0) a b))

(defn mul-resources [number resources]
  ; Multiply the quantity for each resource by number
  (into {} (map
            (fn [[key quantity]]
              [key (* quantity number)])
            resources)))

(defn div-resources [number resources]
  ; Multiply the quantity for each resource by number
  (into {} (map
            (fn [[key quantity]]
              [key (/ quantity number)])
            resources)))

(def raw-types #{:iron-ore :copper-ore :coal :crude-oil :water :stone})

(defn get-requires [item]
  (:requires (first (:production item))))

; Given a list of 'values' for resources, returns a function
; that can take a resource-map and give it a numerical overall 'value'
(defn make-value-fn [weights]
  (fn [resources]
    (reduce + 0
            (map
             (fn [[key quantity]]
               (* quantity (get weights key 0)))
             resources))))

(declare compute-raw-values-for-item)

(defn get-total-resources [resources]
    ; for each item in the resources list get raw values and multiply by the quantity
  (reduce add-resources (map
                         (fn [[item-key quantity]]
                           (mul-resources quantity
                                          (compute-raw-values-for-item (item-key item-lookup-map))))
                         resources)))

(defn compute-raw-values-for-production [{:keys [requires produces name] :as production}]
  "Computes the raw resources required for a single run of a production cycle"
  (if (contains? raw-types name)
    (hash-map name 1)
    (get-total-resources requires)))

(defn compute-raw-values-for-item [{name-key :name [{produces :produces :as first-prod} & _] :production :as item}]
  "Computes the raw cost for a single item using the first available production in its
  production list. Ideally the production list should be sorted best-to-worst"
  (let [raw-production-value (compute-raw-values-for-production first-prod)
        production-quantity (name-key produces)]
    (div-resources production-quantity raw-production-value)))

(defn resources-to-decimal [resources]
  (into {} (map
            (fn [[key quantity]]
              [key (double quantity)])
            resources)))

(defn resources-per-second [time resources]
  (div-resources time resources))

(defn get-production-per-second [production weights]
  "Get the raw value of stuff produced by this production per second"
  (let [raw-resources (get-total-resources (:produces production))]
    (resources-per-second (:time production) raw-resources)))

(defn product-costs [productions weights]
  (let [score-fn (make-value-fn weights)]
  (sort-by second (into [] (map (fn [production] [(:name production) (score-fn (get-production-per-second production weights))]) productions)))))

(defn metals-product-costs []
  (product-costs all-productions {:iron-ore 1 :copper-ore 1}))

(defn with-coal-product-costs []
  (product-costs all-productions {:iron-ore 1 :copper-ore 1 :coal 1}))

(defn with-coal-and-oil-product-costs []
  (product-costs all-productions {:iron-ore 1 :copper-ore 1 :coal 1 :crude-oil 0.1}))

(defn compute-raw-values [item-key]
  (compute-raw-values-for-item (item-key item-lookup-map)))
;
; Prints values in decimal form, since fractions are not helpful
;
(defn raw-decimal-values [item-key]
  (resources-to-decimal (compute-raw-values item-key)))
