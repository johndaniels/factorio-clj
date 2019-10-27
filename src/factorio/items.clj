(ns factorio.items)

; Special production methods that get their own names
(def basic-oil-processing {:name :basic-oil-processing
                           :produces {:petroleum-gas 45}
                           :requires {:crude-oil 100}
                           :time 5.0
                           :type :oil-refinery})

(def advanced-oil-processing
  {:name :advanced-oil-processing
   :produces {:heavy-oil 25 :light-oil 45 :petroleum-gas 55}
   :requires {:crude-oil 100 :water 50}
   :time 5.0
   :type :oil-refinery})

(def coal-liquefaction
  {:name :coal-liquefaction,
   :produces {:heavy-oil 90, :light-oil 20, :petroleum-gas 10},
   :requires {:coal 10, :heavy-oil 25, :steam 50},
   :time 5.0,
   :type :oil-refinery})

(def light-oil-cracking
  {:name :light-oil-cracking,
   :produces {:petroleum-gas 20},
   :requires {:light-oil 30, :water 30},
   :time 2.0,
   :type :chemical-plant})

(def heavy-oil-cracking
  {:name :heavy-oil-cracking,
   :produces {:light-oil 30},
   :requires {:heavy-oil 40, :water 30},
   :time 2.0,
   :type :chemical-plant})

(def solid-fuel-from-heavy-oil
  {:name :solid-fuel-from-heavy-oil,
   :produces {:solid-fuel 1},
   :requires {:heavy-oil 20},
   :time 2.0,
   :type :chemical-plant})

(def solid-fuel-from-light-oil
  {:name :solid-fuel-from-light-oil,
   :produces {:solid-fuel 1},
   :requires {:light-oil 10},
   :time 2.0,
   :type :chemical-plant})

(def solid-fuel-from-petroleum-gas
  {:name :solid-fuel-from-petroleum-gas,
   :produces {:solid-fuel 1},
   :requires {:petroleum-gas 20},
   :time 2.0,
   :type :chemical-plant})

(def uranium-processing
  {:name :uranium-processing,
   :produces {:uranium-235 7/1000, :uranium-238 993/1000},
   :requires {:uranium-ore 10},
   :time 12.0,
   :type :centrifuge})

(def kovarex-enrichment-process
  {:name :kovarex-enrichment-process,
   :produces {:uranium-235 41, :uranium-238 2},
   :requires {:uranium-235 40, :uranium-238 5},
   :time 60.0,
   :type :centrifuge})

(def nuclear-fuel-reprocessing
  {:name :nuclear-fuel-reprocessing,
   :produces {:uranium-238 3},
   :requires {:used-up-uranium-fuel-cell 5},
   :time 60.0,
   :type :centrifuge})

(def create-empty-barrel
  {:name :create-empty-barrel,
   :produces {:empty-barrel 1},
   :requires {:steel-plate 1},
   :time 1.0,
   :type :assembling-machine})

;
; Makes a production object representing emptying the specified content
;
(defn make-empty-barrel-production [name barrel-name]
  {:type :assembling-machine
   :produces {:empty-barrel 1 name 50}
   :requires {barrel-name 1}
   :time 0.2})

;
; Makes a production object representing emptying the specified content
;


(defn make-fill-barrel-production [name barrel-name]
  {:type :assembling-machine
   :produces {barrel-name 1}
   :requires {name 50 :empty-barrel 1}
   :time 0.2})

(defn make-barrel-item [name barrel-name]
  {:name barrel-name
   :production [(make-fill-barrel-production name barrel-name)]})

(def item-list
  [{:name :coal,
    :production
    [{:name :coal,
      :type :mine,
      :produces {:coal 1},
      :requires {},
      :time 0.5}]}
   {:name :iron-ore,
    :production
    [{:name :iron-ore,
      :type :mine,
      :produces {:iron-ore 1},
      :requires {},
      :time 0.5}]}
   {:name :copper-ore,
    :production
    [{:name :copper-ore,
      :type :mine,
      :produces {:copper-ore 1},
      :requires {},
      :time 0.5}]}
   {:name :uranium-ore,
    :production
    [{:name :uranium-ore,
      :type :mine,
      :produces {:uranium-ore 1},
      :requires {:sulfuric-acid 1},
      :time 1.0}]}

   {:name :uranium-238
    :production [uranium-processing kovarex-enrichment-process nuclear-fuel-reprocessing]}
   {:name :uranium-235
    :production [uranium-processing kovarex-enrichment-process]}
   {:name :stone,
    :production
    [{:name :stone,
      :type :mine,
      :produces {:stone 1},
      :requires {},
      :time 0.5}]}
   {:name :wood, :production []}
   {:name :iron-plate,
    :production
    [{:name :iron-plate,
      :produces {:iron-plate 1},
      :type :furnace,
      :requires {:iron-ore 1},
      :time 1.6}]}
   {:name :copper-plate,
    :production
    [{:name :copper-plate,
      :produces {:copper-plate 1},
      :type :furnace,
      :requires {:copper-ore 1},
      :time 1.6}]}
   {:name :steel-plate,
    :production
    [{:name :steel-plate,
      :produces {:steel-plate 1},
      :type :furnace,
      :requires {:iron-plate 5},
      :time 8}]}
   {:name :copper-wire,
    :production
    [{:name :copper-wire,
      :type :assembling-machine,
      :produces {:copper-wire 2},
      :requires {:copper-plate 1},
      :time 0.5}]}
   {:name :iron-gear-wheel,
    :production
    [{:name :iron-gear-wheel,
      :type :assembling-machine,
      :produces {:iron-gear-wheel 1},
      :requires {:iron-plate 2},
      :time 0.5}]}
   {:name :iron-stick,
    :production
    [{:name :iron-stick,
      :type :assembling-machine,
      :produces {:iron-stick 2},
      :requires {:iron-plate 1},
      :time 0.5}]}
   {:name :plastic-bar,
    :production
    [{:name :plastic-bar,
      :type :chemical-plant,
      :produces {:plastic-bar 2},
      :requires {:coal 1, :petroleum-gas 20},
      :time 1.0}]}
   {:name :sulfur,
    :production
    [{:name :sulfur,
      :type :chemical-plant,
      :produces {:sulfur 2},
      :requires {:petroleum-gas 30, :water 30},
      :time 1.0}]}
   {:name :electric-engine-unit,
    :production
    [{:name :electric-engine-unit,
      :type :assembling-machine,
      :produces {:electric-engine-unit 1},
      :requires {:electronic-circuit 2, :engine-unit 1, :lubricant 15},
      :time 10.0}]}
   {:name :engine-unit,
    :production
    [{:name :engine-unit,
      :type :assembling-machine,
      :produces {:engine-unit 1},
      :requires {:iron-gear-wheel 1, :pipe 2, :steel-plate 1},
      :time 10.0}]}
   {:name :battery,
    :production
    [{:name :battery,
      :type :chemical-plant,
      :produces {:battery 1},
      :requires {:copper-plate 1, :iron-plate 1, :sulfuric-acid 20},
      :time 4.0}]}
   {:name :lamp,
    :production
    [{:name :lamp,
      :type :assembling-machine,
      :produces {:assembling-machine 1},
      :requires {:copper-wire 3, :electronic-circuit 1, :iron-plate 1},
      :time 0.5}]}
   {:name :empty-barrel
    :production [create-empty-barrel
                 (make-empty-barrel-production :crude-oil :crude-oil-barrel)
                 (make-empty-barrel-production :heavy-oil :heavy-oil-barrel)
                 (make-empty-barrel-production :light-oil :light-oil-barrel)
                 (make-empty-barrel-production :petroleum-gas :petroleum-gas-barrel)
                 (make-empty-barrel-production :lubricant :lubricant-barrel)
                 (make-empty-barrel-production :sulfuric-acid :sulfuric-acid-barrel)
                 (make-empty-barrel-production :water :water-barrel)]}

   {:name :repair-pack,
    :production
    [{:name :repair-pack,
      :type :assembling-machine,
      :produces {:repair-pack 1},
      :requires {:electronic-circuit 2, :iron-gear-wheel 2},
      :time 0.5}]}

   {:name :solid-fuel
    :production [solid-fuel-from-light-oil
                 solid-fuel-from-heavy-oil
                 solid-fuel-from-petroleum-gas]}

   {:name :nuclear-fuel,
    :production
    [{:name :nuclear-fuel,
      :type :centrifuge,
      :produces {:nuclear-fuel 1},
      :requires {:rocket-fuel 1, :uranium-235 1},
      :time 90.0}]}
   {:name :uranium-fuel-cell,
    :production
    [{:name :uranium-fuel-cell,
      :type :centrifuge,
      :produces {:uranium-fuel-cell 10},
      :requires {:iron-plate 10, :uranium-235 1, :uranium-238 19},
      :time 90.0}]}
   {:name :used-up-uranium-fuel-cell,
    :production
    [{:name :used-up-uranium-fuel-cell,
      :type :nuclear-reactor,
      :produces {:used-up-uranium-fuel-cell 1},
      :requires {:uranium-fuel-cell 1},
      :time 200.0}]}

    ;
    ; Mining
    ;


   {:name :burner-mining-drill,
    :production
    [{:name :burner-mining-drill,
      :type :assembling-machine,
      :produces {:burner-mining-drill 1},
      :requires {:iron-gear-wheel 3, :iron-plate 3, :stone-furnace 1},
      :time 2.0}]}
   {:name :electric-mining-drill,
    :production
    [{:name :electric-mining-drill,
      :type :assembling-machine,
      :produces {:electric-mining-drill 1},
      :requires
      {:electronic-circuit 3, :iron-gear-wheel 5, :iron-plate 10},
      :time 2.0}]}


    ;
    ; Furnaces
    ;


   {:name :stone-furnace,
    :production
    [{:name :stone-furnace,
      :type :assembling-machine,
      :produces {:stone-furnace 1},
      :requires {:stone 5},
      :time 0.5}]}
   {:name :steel-furnace,
    :production
    [{:name :steel-furnace,
      :type :assembling-machine,
      :produces {:steel-furnace 1},
      :requires {:steel-plate 6, :stone-brick 10},
      :time 3.0}]}
   {:name :electric-furnace,
    :production
    [{:name :electric-furnace,
      :type :assembling-machine,
      :produces {:electric-furnace 1},
      :requires {:advanced-circuit 5, :steel-plate 10, :stone-brick 10},
      :time 5.0}]}

    ;
    ; Circuits
    ;


   {:name :electronic-circuit,
    :production
    [{:name :electronic-circuit,
      :type :assembling-machine,
      :produces {:electronic-circuit 1},
      :requires {:iron-plate 1, :copper-wire 3},
      :time 0.5}]}
   {:name :advanced-circuit,
    :production
    [{:name :advanced-circuit,
      :type :assembling-machine,
      :produces {:advanced-circuit 1},
      :requires {:electronic-circuit 2, :copper-wire 4, :plastic-bar 2},
      :time 6.0}]}
   {:name :processing-unit,
    :production
    [{:name :processing-unit,
      :type :assembling-machine,
      :produces {:processing-unit 1},
      :requires
      {:advanced-circuit 2, :electronic-circuit 20, :sulfuric-acid 5},
      :time 10.0}]}

    ;
    ; belts and friends
    ;


   {:name :transport-belt,
    :production
    [{:name :transport-belt,
      :type :assembling-machine,
      :produces {:transport-belt 2},
      :requires {:iron-plate 1, :iron-gear-wheel 1},
      :time 0.5}]}
   {:name :fast-transport-belt,
    :production
    [{:name :fast-transport-belt,
      :type :assembling-machine,
      :produces {:fast-transport-belt 1},
      :requires {:transport-belt 1, :iron-gear-wheel 5},
      :time 0.5}]}
   {:name :express-transport-belt,
    :production
    [{:name :express-transport-belt,
      :type :assembling-machine,
      :produces {:express-transport-belt 1},
      :requires
      {:fast-transport-belt 1, :iron-gear-wheel 10, :lubricant 20},
      :time 0.5}]}
   {:name :underground-belt,
    :production
    [{:name :underground-belt,
      :type :assembling-machine,
      :produces {:underground-belt 2},
      :requires {:transport-belt 5, :iron-plate 10},
      :time 0.5}]}
   {:name :fast-underground-belt,
    :production
    [{:name :fast-underground-belt,
      :type :assembling-machine,
      :produces {:fast-underground-belt 2},
      :requires {:underground-belt 2, :iron-gear-wheel 40},
      :time 0.5}]}
   {:name :express-underground-belt,
    :production
    [{:name :express-underground-belt,
      :type :assembling-machine,
      :produces {:express-underground-belt 2},
      :requires
      {:fast-underground-belt 2, :iron-gear-wheel 80, :lubricant 40},
      :time 0.5}]}
   {:name :splitter,
    :production
    [{:name :splitter,
      :type :assembling-machine,
      :produces {:splitter 1},
      :requires
      {:electronic-circuit 5, :iron-plate 5, :transport-belt 4},
      :time 1.0}]}
   {:name :fast-splitter,
    :production
    [{:name :fast-splitter,
      :type :assembling-machine,
      :produces {:fast-splitter 1},
      :requires
      {:electronic-circuit 10, :iron-gear-wheel 10, :splitter 1},
      :time 2.0}]}
   {:name :express-splitter,
    :production
    [{:name :express-splitter,
      :type :assembling-machine,
      :produces {:fast-splitter 1},
      :requires
      {:advanced-circuit 10,
       :iron-gear-wheel 10,
       :fast-splitter 1,
       :lubricant 80},
      :time 1.0}]}

    ;
    ; Inserters
    ;


   {:name :burner-inserter,
    :production
    [{:name :burner-inserter,
      :type :assembling-machine,
      :produces {:burner-inserter 1},
      :requires {:iron-gear-wheel 1, :iron-plate 1},
      :time 0.5}]}
   {:name :inserter,
    :production
    [{:name :inserter,
      :type :assembling-machine,
      :produces {:inserter 1},
      :requires
      {:electronic-circuit 1, :iron-gear-wheel 1, :iron-plate 1},
      :time 0.5}]}
   {:name :long-handed-inserter,
    :production
    [{:name :long-handed-inserter,
      :type :assembling-machine,
      :produces {:long-handed-inserter 1},
      :requires {:inserter 1, :iron-gear-wheel 1, :iron-plate 1},
      :time 0.5}]}
   {:name :fast-inserter,
    :production
    [{:name :fast-inserter,
      :type :assembling-machine,
      :produces {:fast-inserter 1},
      :requires {:inserter 1, :electronic-circuit 2, :iron-plate 2},
      :time 0.5}]}
   {:name :filter-inserter,
    :production
    [{:name :filter-inserter,
      :type :assembling-machine,
      :produces {:filter-inserter 1},
      :requires {:fast-inserter 1, :electronic-circuit 4},
      :time 0.5}]}
   {:name :stack-inserter,
    :production
    [{:name :stack-inserter,
      :type :assembling-machine,
      :produces {:stack-inserter 1},
      :requires
      {:fast-inserter 1,
       :electronic-circuit 15,
       :advanced-circuit 1,
       :iron-gear-wheel 15},
      :time 0.5}]}
   {:name :stack-filter-inserter,
    :production
    [{:name :stack-filter-inserter,
      :type :assembling-machine,
      :produces {:stack-filter-inserter 1},
      :requires {:stack-inserter 1, :electronic-circuit 5},
      :time 0.5}]}

    ;
    ; Power
    ;


   {:name :small-electric-pole,
    :production
    [{:name :small-electric-pole,
      :type :assembling-machine,
      :produces {:small-electric-pole 2},
      :requires {:copper-wire 2, :wood 1},
      :time 0.5}]}
   {:name :medium-electric-pole,
    :production
    [{:name :medium-electric-pole,
      :type :assembling-machine,
      :produces {:medium-electric-pole 1},
      :requires {:copper-plate 2, :iron-stick 4, :steel-plate 2},
      :time 0.5}]}
   {:name :big-electric-pole,
    :production
    [{:name :big-electric-pole,
      :type :assembling-machine,
      :produces {:medium-electric-pole 1},
      :requires {:copper-plate 5, :iron-plate 4, :steel-plate 5},
      :time 0.5}]}
   {:name :substation,
    :production
    [{:name :substation,
      :type :assembling-machine,
      :produces {:medium-electric-pole 1},
      :requires {:advanced-circuit 5, :copper-plate 5, :steel-plate 10},
      :time 0.5}]}
   {:name :boiler,
    :production
    [{:name :boiler,
      :type :assembling-machine,
      :produces {:boiler 1},
      :requires {:pipe 4, :stone-furnace 1},
      :time 0.5}]}
   {:name :steam-engine,
    :production
    [{:name :steam-engine,
      :type :assembling-machine,
      :produces {:steam-engine 1},
      :requires {:iron-gear-wheel 8, :iron-plate 10, :pipe 5},
      :time 0.5}]}
   {:name :steam-turbine,
    :production
    [{:name :steam-turbine,
      :type :assembling-machine,
      :produces {:steam-turbine 1},
      :requires {:copper-plate 50, :iron-gear-wheel 50, :pipe 20},
      :time 3.0}]}
   {:name :solar-panel,
    :production
    [{:name :solar-panel,
      :type :assembling-machine,
      :produces {:solar-panel 1},
      :requires
      {:copper-plate 5, :electronic-circuit 10, :steel-plate 5},
      :time 10.0}]}
   {:name :accumulator,
    :production
    [{:name :accumulator,
      :type :assembling-machine,
      :produces {:accumulator 1},
      :requires {:battery 5, :iron-plate 2},
      :time 10.0}]}
   {:name :nuclear-reactor,
    :production
    [{:name :nuclear-reactor,
      :type :assembling-machine,
      :produces {:nuclear-reactor 1},
      :requires
      {:advanced-circuit 500,
       :concrete 500,
       :copper-plate 500,
       :steel-plate 500},
      :time 8.0}]}
   {:name :heat-exchanger,
    :production
    [{:name :heat-exchanger,
      :type :assembling-machine,
      :produces {:heat-exchanger 1},
      :requires {:copper-plate 100, :pipe 10, :steel-plate 10},
      :time 3.0}]}
   {:name :heat-pipe,
    :production
    [{:name :heat-pipe,
      :type :assembling-machine,
      :produces {:heat-pipe 1},
      :requires {:copper-plate 20, :steel-plate 10},
      :time 1.0}]}


    ;
    ; Pipes/Fluid Handling
    ;


   {:name :pipe,
    :production
    [{:name :pipe,
      :type :assembling-machine,
      :produces {:pipe 1},
      :requires {:iron-plate 1},
      :time 0.5}]}
   {:name :pipe-to-ground,
    :production
    [{:name :pipe-to-ground,
      :type :assembling-machine,
      :produces {:pipe-to-ground 2},
      :requires {:iron-plate 5, :pipe 10},
      :time 0.5}]}
   {:name :pump,
    :production
    [{:name :pump,
      :type :assembling-machine,
      :produces {:pump 1},
      :requires {:engine-unit 1, :pipe 1, :steel-plate 1},
      :time 2.0}]}
   {:name :storage-tank,
    :production
    [{:name :storage-tank,
      :type :assembling-machine,
      :produces {:storage-tank 1},
      :requires {:iron-plate 20, :steel-plate 5},
      :time 3.0}]}
   {:name :offshore-pump,
    :production
    [{:name :offshore-pump,
      :type :assembling-machine,
      :produces {:offshore-pump 1},
      :requires {:electronic-circuit 2, :iron-gear-wheel 1, :pipe 1},
      :time 0.5}]}
   {:name :pumpjack,
    :production
    [{:name :pumpjack,
      :type :assembling-machine,
      :produces {:pumpjack 1},
      :requires
      {:electronic-circuit 5,
       :iron-gear-wheel 10,
       :pipe 10,
       :steel-plate 5},
      :time 5.0}]}

    ;
    ; Manufacturing Machines
    ;


   {:name :assembling-machine-1,
    :production
    [{:name :assembling-machine-1,
      :type :assembling-machine,
      :produces {:assembling-machine-1 1},
      :requires
      {:electronic-circuit 3, :iron-gear-wheel 5, :iron-plate 9},
      :time 0.5}]}
   {:name :assembling-machine-2,
    :production
    [{:name :assembling-machine-2,
      :type :assembling-machine,
      :produces {:assembling-machine-2 1},
      :requires
      {:assembling-machine-1 1,
       :electronic-circuit 3,
       :iron-gear-wheel 5,
       :steel-plate 2},
      :time 0.5}]}
   {:name :assembling-machine-3,
    :production
    [{:name :assembling-machine-3,
      :type :assembling-machine,
      :produces {:assembling-machine-3 1},
      :requires
      {:assembling-machine-2 2, :speed-module 4, :iron-plate 9},
      :time 0.5}]}
   {:name :oil-refinery,
    :production
    [{:name :oil-refinery,
      :type :assembling-machine,
      :produces {:oil-refinery 1},
      :requires
      {:electronic-circuit 10,
       :iron-gear-wheel 10,
       :pipe 10,
       :steel-plate 15},
      :time 8}]}
   {:name :chemical-plant,
    :production
    [{:name :chemical-plant,
      :type :assembling-machine,
      :produces {:chemical-plant 1},
      :requires
      {:electronic-circuit 5,
       :iron-gear-wheel 5,
       :pipe 5,
       :steel-plate 5},
      :time 0.5}]}
   {:name :centrifuge,
    :production
    [{:name :centrifuge,
      :type :assembling-machine,
      :produces {:centrifuge 1},
      :requires
      {:advanced-circuit 100,
       :concrete 100,
       :iron-gear-wheel 100,
       :steel-plate 50},
      :time 4.0}]}

    ;
    ; Transport/Vehicle
    ;


   {:name :rail,
    :production
    [{:name :rail,
      :type :assembling-machine,
      :produces {:rail 2},
      :requires {:iron-stick 1, :steel-plate 1, :stone 1},
      :time 0.5}]}
   {:name :train-stop,
    :production
    [{:name :train-stop,
      :type :assembling-machine,
      :produces {:train-stop 1},
      :requires
      {:electronic-circuit 5,
       :iron-plate 6,
       :iron-stick 6,
       :steel-plate 3},
      :time 0.5}]}
   {:name :rail-signal,
    :production
    [{:name :rail-signal,
      :type :assembling-machine,
      :produces {:rail-signal 1},
      :requires {:electronic-circuit 1, :iron-plate 5},
      :time 0.5}]}
   {:name :rail-chain-signal,
    :production
    [{:name :rail-chain-signal,
      :type :assembling-machine,
      :produces {:rail-chain-signal 1},
      :requires {:electronic-circuit 1, :iron-plate 5},
      :time 0.5}]}
   {:name :locomotive,
    :production
    [{:name :locomotive,
      :type :assembling-machine,
      :produces {:locomotive 1},
      :requires
      {:electronic-circuit 10, :engine-unit 20, :steel-plate 30},
      :time 4.0}]}
   {:name :fluid-wagon,
    :production
    [{:name :fluid-wagon,
      :type :assembling-machine,
      :produces {:fluid-wagon 1},
      :requires
      {:iron-gear-wheel 10, :pipe 8, :steel-plate 16, :storage-tank 1},
      :time 1.5}]}
   {:name :cargo-wagon,
    :production
    [{:name :cargo-wagon,
      :type :assembling-machine,
      :produces {:cargo-wagon 1},
      :requires {:iron-gear-wheel 10, :iron-plate 20, :steel-plate 20},
      :time 1.0}]}
   {:name :artillery-wagon,
    :production
    [{:name :artillery-wagon,
      :type :assembling-machine,
      :produces {:artillery-wagon 1},
      :requires
      {:advanced-circuit 20,
       :engine-unit 64,
       :iron-gear-wheel 10,
       :pipe 16,
       :steel-plate 40},
      :time 4.0}]}
   {:name :car,
    :production
    [{:name :car,
      :type :assembling-machine,
      :produces {:car 1},
      :requires {:engine-unit 8, :iron-plate 20, :steel-plate 5},
      :time 2.0}]}
   {:name :tank,
    :production
    [{:name :tank,
      :type :assembling-machine,
      :produces {:tank 1},
      :requires
      {:advanced-circuit 10,
       :engine-unit 32,
       :iron-gear-wheel :15,
       :steel-plate 50},
      :time 5.0}]}

    ;
    ; Chests
    ;


   {:name :wooden-chest,
    :production
    [{:name :wooden-chest,
      :type :assembling-machine,
      :produces {:wooden-chest 1},
      :requires {:wood 2},
      :time 0.5}]}
   {:name :iron-chest,
    :production
    [{:name :iron-chest,
      :type :assembling-machine,
      :produces {:iron-chest 1},
      :requires {:iron-plate 8},
      :time 0.5}]}
   {:name :steel-chest,
    :production
    [{:name :steel-chest,
      :type :assembling-machine,
      :produces {:steel-chest 1},
      :requires {:steel-plate 8},
      :time 0.5}]}

    ;
    ; Logistics/Robots
    ;


   {:name :flying-robot-frame,
    :production
    [{:name :flying-robot-frame,
      :type :assembling-machine,
      :produces {:flying-robot-frame 1},
      :requires
      {:battery 2,
       :electric-engine-unit 1,
       :electronic-circuit 3,
       :steel-plate 1},
      :time 20.0}]}
   {:name :logistic-robot,
    :production
    [{:name :logistic-robot,
      :type :assembling-machine,
      :produces {:logistic-robot 1},
      :requires {:advanced-circuit 2, :flying-robot-frame 1},
      :time 0.5}]}
   {:name :construction-robot,
    :production
    [{:name :construction-robot,
      :type :assembling-machine,
      :produces {:construction-robot 1},
      :requires {:electronic-circuit 2, :flying-robot-frame 1},
      :time 0.5}]}
   {:name :logistic-chest-active-provider,
    :production
    [{:name :logistic-chest-active-provider,
      :type :assembling-machine,
      :produces {:logistic-chest-active-provider 1},
      :requires
      {:advanced-circuit 1, :electronic-circuit 3, :steel-chest 1},
      :time 0.5}]}
   {:name :logistic-chest-passive-provider,
    :production
    [{:name :logistic-chest-passive-provider,
      :type :assembling-machine,
      :produces {:logistic-chest-passive-provider 1},
      :requires
      {:advanced-circuit 1, :electronic-circuit 3, :steel-chest 1},
      :time 0.5}]}
   {:name :logistic-chest-storage,
    :production
    [{:name :logistic-chest-storage,
      :type :assembling-machine,
      :produces {:logistic-chest-storage 1},
      :requires
      {:advanced-circuit 1, :electronic-circuit 3, :steel-chest 1},
      :time 0.5}]}
   {:name :logistic-chest-buffer,
    :production
    [{:name :logistic-chest-buffer,
      :type :assembling-machine,
      :produces {:logistic-chest-buffer 1},
      :requires
      {:advanced-circuit 1, :electronic-circuit 3, :steel-chest 1},
      :time 0.5}]}
   {:name :logistic-chest-requester,
    :production
    [{:name :logistic-chest-requester,
      :type :assembling-machine,
      :produces {:logistic-chest-requester 1},
      :requires
      {:advanced-circuit 1, :electronic-circuit 3, :steel-chest 1},
      :time 0.5}]}
   {:name :roboport,
    :production
    [{:name :roboport,
      :type :assembling-machine,
      :produces {:roboport 1},
      :requires
      {:advanced-circuit 45, :iron-gear-wheel 45, :steel-plate 45},
      :time 5.0}]}
    ;
    ; Circuit Network
    ;


   {:name :red-wire,
    :production
    [{:name :red-wire,
      :type :assembling-machine,
      :produces {:red-wire 1},
      :requires {:copper-wire 1, :electronic-circuit 1},
      :time 0.5}]}
   {:name :green-wire,
    :production
    [{:name :green-wire,
      :type :assembling-machine,
      :produces {:green-wire 1},
      :requires {:copper-wire 1, :electronic-circuit 1},
      :time 0.5}]}
   {:name :arithmetic-combinator,
    :production
    [{:name :arithmetic-combinator,
      :type :assembling-machine,
      :produces {:arithmetic-combinator 1},
      :requires {:copper-wire 5, :electronic-circuit 5},
      :time 0.5}]}
   {:name :decider-combinator,
    :production
    [{:name :decider-combinator,
      :type :assembling-machine,
      :produces {:decider-combinator 1},
      :requires {:copper-wire 5, :electronic-circuit 5},
      :time 0.5}]}
   {:name :constant-combinator,
    :production
    [{:name :constant-combinator,
      :type :assembling-machine,
      :produces {:constant-combinator 1},
      :requires {:copper-wire 5, :electronic-circuit 2},
      :time 0.5}]}
   {:name :power-switch,
    :production
    [{:name :power-switch,
      :type :assembling-machine,
      :produces {:power-switch 1},
      :requires {:copper-wire 5, :electronic-circuit 2, :iron-plate 5},
      :time 0.5}]}
   {:name :programmable-speaker,
    :production
    [{:name :programmable-speaker,
      :type :assembling-machine,
      :produces {:programmable-speaker 1},
      :requires
      {:copper-wire 5,
       :electronic-circuit 4,
       :iron-plate 3,
       :iron-stick 4},
      :time 2.0}]}

    ;
    ; Stone/Concrete
    ;


   {:name :stone-brick,
    :production
    [{:name :stone-brick,
      :type :furnace,
      :produces {:stone-brick 1},
      :requires {:stone 2},
      :time 1.6}]}
   {:name :concrete,
    :production
    [{:name :concrete,
      :type :assembling-machine,
      :produces {:concrete 10},
      :requires {:iron-ore 1, :stone 5, :water 100},
      :time 10.0}]}
   {:name :hazard-concrete,
    :production
    [{:name :hazard-concrete,
      :type :assembling-machine,
      :produces {:hazard-concrete 10},
      :requires {:concrete 10},
      :time 0.25}]}
   {:name :refined-concrete,
    :production
    [{:name :refined-concrete,
      :type :assembling-machine,
      :produces {:refined-concrete 10},
      :requires
      {:concrete 20, :iron-stick 8, :steel-plate 1, :water 100},
      :time 15}]}
   {:name :refined-hazard-concrete,
    :production
    [{:name :refined-hazard-concrete,
      :type :assembling-machine,
      :produces {:refined-hazard-concrete 10},
      :requires {:refined-concrete 10},
      :time 0.25}]}
   {:name :landfill,
    :production
    [{:name :landfill,
      :type :assembling-machine,
      :produces {:landfill 1},
      :requires {:stone 20},
      :time 0.5}]}

    ;
    ; Explosives
    ;


   {:name :explosives,
    :production
    [{:name :explosives,
      :type :chemical-plant,
      :produces {:explosives 2},
      :requires {:coal 1, :sulfur 1, :water 10},
      :time 4.0}]}
   {:name :cliff-explosives,
    :production
    [{:name :cliff-explosives,
      :type :assembling-machine,
      :produces {:cliff-explosives 1},
      :requires {:empty-barrel 1, :explosives 10, :grenade 1},
      :time 8.0}]}

    ;
    ; Modules (and Beacon)
    ;


   {:name :beacon,
    :production
    [{:name :beacon,
      :type :assembling-machine,
      :produces {:beacon 1},
      :requires
      {:advanced-circuit 20,
       :copper-wire 10,
       :electronic-circuit 20,
       :steel-plate 10},
      :time 15.0}]}
   {:name :speed-module,
    :production
    [{:name :speed-module,
      :type :assembling-machine,
      :produces {:speed-module 1},
      :requires {:advanced-circuit 5, :electronic-circuit 5},
      :time 15.0}]}
   {:name :speed-module-2,
    :production
    [{:name :speed-module-2,
      :type :assembling-machine,
      :produces {:speed-module-2 1},
      :requires
      {:advanced-circuit 5, :processing-unit 5, :speed-module 4},
      :time 30.0}]}
   {:name :speed-module-3,
    :production
    [{:name :speed-module-3,
      :type :assembling-machine,
      :produces {:speed-module-3 1},
      :requires
      {:advanced-circuit 5, :processing-unit 5, :speed-module-2 5},
      :time 60.0}]}
   {:name :effectivity-module,
    :production
    [{:name :effectivity-module,
      :type :assembling-machine,
      :produces {:effectivity-module 1},
      :requires {:advanced-circuit 5, :electronic-circuit 5},
      :time 15.0}]}
   {:name :effectivity-module-2,
    :production
    [{:name :effectivity-module-2,
      :type :assembling-machine,
      :produces {:effectivity-module-2 1},
      :requires
      {:advanced-circuit 5, :processing-unit 5, :effectivity-module 4},
      :time 30.0}]}
   {:name :effectivity-module-3,
    :production
    [{:name :effectivity-module-3,
      :type :assembling-machine,
      :produces {:effectivity-module-3 1},
      :requires
      {:advanced-circuit 5, :processing-unit 5, :effectivity-module-2 5},
      :time 60.0}]}
   {:name :productivity-module,
    :production
    [{:name :productivity-module,
      :type :assembling-machine,
      :produces {:productivity-module 1},
      :requires {:advanced-circuit 5, :electronic-circuit 5},
      :time 15.0}]}
   {:name :productivity-module-2,
    :production
    [{:name :productivity-module-2,
      :type :assembling-machine,
      :produces {:productivity-module-2 1},
      :requires
      {:advanced-circuit 5, :processing-unit 5, :productivity-module 4},
      :time 30.0}]}
   {:name :productivity-module-3,
    :production
    [{:name :productivity-module-3,
      :type :assembling-machine,
      :produces {:productivity-module-3 1},
      :requires
      {:advanced-circuit 5,
       :processing-unit 5,
       :productivity-module-2 5},
      :time 60.0}]}

    ; 
    ; Fluids
    ;


   {:name :crude-oil
    :production [{:name :crude-oil,
                  :type :pumpjack,
                  :produces {:crude-oil 1},
                  :requires {},
                  :time 1.0} (make-empty-barrel-production :crude-oil :crude-oil-barrel)]}
   (make-barrel-item :crude-oil :crude-oil-barrel)
   {:name :water
    :production [{:name :water,
                  :type :offshore-pump,
                  :produces {:water 1200},
                  :requires {},
                  :time 1.0} (make-empty-barrel-production :water :water-barrel)]}
   (make-barrel-item :water :water-barrel)
   {:name :steam
    :production [{:type :boiler
                  :produces {:steam 60}
                  :requires {}
                  :time 1.0}
                 {:type :heat-exchanger
                  :produces {:steam 103}
                  :requires {}
                  :time 1.0}]}

   {:name :heavy-oil
    :production [advanced-oil-processing
                 coal-liquefaction
                 (make-empty-barrel-production :heavy-oil :heavy-oil-barrel)]}
   (make-barrel-item :heavy-oil :heavy-oil-barrel)
   {:name :light-oil
    :production [advanced-oil-processing
                 coal-liquefaction
                 heavy-oil-cracking
                 (make-empty-barrel-production :light-oil :light-oil-barrel)]}
   (make-barrel-item :light-oil :light-oil-barrel)
   {:name :petroleum-gas
    :production [advanced-oil-processing
                 basic-oil-processing
                 coal-liquefaction
                 light-oil-cracking
                 (make-empty-barrel-production :petroleum-gas :petroleum-gas-barrel)]}
   (make-barrel-item :petroleum-gas :petroleum-gas-barrel)
   {:name :lubricant
    :production [{:name :lubricant
                  :type :chemical-plant
                  :produces {:lubricant 10}
                  :requires {:heavy-oil 10}
                  :time 1.0} (make-empty-barrel-production :lubricant :lubricant-barrel)]}
   (make-barrel-item :lubricant :lubricant-barrel)
   {:name :sulfuric-acid
    :production [{:type :chemical-plant
                  :name :sulfuric-acid
                  :produces {:sulfuric-acid 50}
                  :requires {:iron-plate 1 :sulfur 5 :water 100}
                  :time 1.0} (make-empty-barrel-production :sulfuric-acid :sulfuric-acid-barrel)]}
   (make-barrel-item :sulfuric-acid :sulfuric-acid-barrel)

    ;
    ; Millitary (Weapons)
    ;
   {:name :pistol,
    :production
    [{:name :pistol,
      :type :assembling-machine,
      :produces {:pistol 1},
      :requires {:copper-plate 5, :iron-plate 5},
      :time 5.0}]}
   {:name :submachine-gun,
    :production
    [{:name :submachine-gun,
      :type :assembling-machine,
      :produces {:submachine-gun 1},
      :requires {:copper-plate 5, :iron-gear-wheel 10, :iron-plate 10},
      :time 10.0}]}
   {:name :shotgun,
    :production
    [{:name :shotgun,
      :type :assembling-machine,
      :produces {:shotgun 1},
      :requires
      {:copper-plate 10, :iron-gear-wheel 5, :iron-plate 15, :wood 5},
      :time 10.0}]}
   {:name :combat-shotgun,
    :production
    [{:name :combat-shotgun,
      :type :assembling-machine,
      :produces {:combat-shotgun 1},
      :requires
      {:copper-plate 10, :iron-gear-wheel 5, :steel-plate 15, :wood 10},
      :time 10.0}]}
   {:name :rocket-launcher,
    :production
    [{:name :rocket-launcher,
      :type :assembling-machine,
      :produces {:rocket-launcher 1},
      :requires
      {:electronic-circuit 5, :iron-gear-wheel 5, :iron-plate 5},
      :time 10.0}]}
   {:name :flamethrower,
    :production
    [{:name :flamethrower,
      :type :assembling-machine,
      :produces {:flamethrower 1},
      :requires {:iron-gear-wheel 10, :steel-plate 5},
      :time 10.0}]}
   {:name :land-mine,
    :production
    [{:name :land-mine,
      :type :assembling-machine,
      :produces {:land-mine 4},
      :requires {:explosives 2, :steel-plate 1},
      :time 5.0}]}

    ;
    ; Millitary (Ammo)
    ;


   {:name :firearm-magazine,
    :production
    [{:name :firearm-magazine,
      :type :assembling-machine,
      :produces {:firearm-magazine 1},
      :requires {:iron-plate 4},
      :time 1.0}]}
   {:name :piercing-rounds-magazine,
    :production
    [{:name :piercing-rounds-magazine,
      :type :assembling-machine,
      :produces {:piercing-rounds-magazine 1},
      :requires {:copper-plate 5, :firearm-magazine 1, :steel-plate 1},
      :time 5.0}]}
   {:name :uranium-rounds-magazine,
    :production
    [{:name :uranium-rounds-magazine,
      :type :assembling-machine,
      :produces {:uranium-rounds-magazine 1},
      :requires {:piercing-rounds-magazine 1, :uranium-238 1},
      :time 10.0}]}
   {:name :shotgun-shell,
    :production
    [{:name :shotgun-shell,
      :type :assembling-machine,
      :produces {:shotgun-shell 1},
      :requires {:iron-plate 2, :copper-plate 2},
      :time 3.0}]}
   {:name :piercing-shotgun-shell,
    :production
    [{:name :piercing-shotgun-shell,
      :type :assembling-machine,
      :produces {:piercing-shotgun-shell 1},
      :requires {:copper-plate 5, :shotgun-shell 2, :steel-plate 2},
      :time 8.0}]}
   {:name :cannon-shell,
    :production
    [{:name :cannon-shell,
      :type :assembling-machine,
      :produces {:cannon-shell 1},
      :requires {:explosives 1, :plastic-bar 2, :steel-plate 2},
      :time 8.0}]}
   {:name :explosive-cannon-shell,
    :production
    [{:name :explosive-cannon-shell,
      :type :assembling-machine,
      :produces {:explosive-cannon-shell 1},
      :requires {:explosives 2, :plastic-bar 2, :steel-plate 2},
      :time 8.0}]}
   {:name :uranium-cannon-shell,
    :production
    [{:name :uranium-cannon-shell,
      :type :assembling-machine,
      :produces {:uranium-cannon-shell 1},
      :requires {:cannon-shell 1, :uranium-238 1},
      :time 12.0}]}
   {:name :explosive-uranium-cannon-shell,
    :production
    [{:name :explosive-uranium-cannon-shell,
      :type :assembling-machine,
      :produces {:explosive-uranium-cannon-shell 1},
      :requires {:explosive-cannon-shell 1, :uranium-238 1},
      :time 12.0}]}
   {:name :artillery-shell,
    :production
    [{:name :artillery-shell,
      :type :assembling-machine,
      :produces {:artillery-shell 1},
      :requires {:explosive-cannon-shell 4, :explosives 8, :radar 1},
      :time 15.0}]}
   {:name :rocket,
    :production
    [{:name :rocket,
      :type :assembling-machine,
      :produces {:rocket 1},
      :requires {:electronic-circuit 1, :explosives 1, :iron-plate 2},
      :time 8.0}]}
   {:name :explosive-rocket,
    :production
    [{:name :explosive-rocket,
      :type :assembling-machine,
      :produces {:explosive-rocket 1},
      :requires {:explosives 2, :rocket 1},
      :time 8.0}]}
   {:name :atomic-bomb,
    :production
    [{:name :atomic-bomb,
      :type :assembling-machine,
      :produces {:atomic-bomb 1},
      :requires
      {:explosives 10, :rocket-control-unit 10, :uranium-235 30},
      :time 50.0}]}
   {:name :flamethrower-ammo,
    :production
    [{:name :flamethrower-ammo,
      :type :assembling-machine,
      :produces {:flamethrower-ammo 1},
      :requires {:crude-oil 100, :steel-plate 5},
      :time 6.0}]}

    ;
    ; Millitary (Armor/Equipment)
    ;


   {:name :light-armor,
    :production
    [{:name :light-armor,
      :type :assembling-machine,
      :produces {:light-armor 1},
      :requires {:iron-plate 100},
      :time 3.0}]}
   {:name :hevay-armor,
    :production
    [{:name :hevay-armor,
      :type :assembling-machine,
      :produces {:hevay-armor 1},
      :requires {:copper-plate 100, :steel-plate 50},
      :time 8.0}]}
   {:name :modular-armor,
    :production
    [{:name :modular-armor,
      :type :assembling-machine,
      :produces {:modular-armor 1},
      :requires {:advanced-circuit 30, :steel-plate 50},
      :time 15.0}]}
   {:name :power-armor,
    :production
    [{:name :power-armor,
      :type :assembling-machine,
      :produces {:power-armor 1},
      :requires
      {:electric-engine-unit 20, :processing-unit 40, :steel-plate 40},
      :time 20.0}]}
   {:name :power-armor-mk2,
    :production
    [{:name :power-armor-mk2,
      :type :assembling-machine,
      :produces {:power-armor-mk2 1},
      :requires
      {:effectivity-module-2 25,
       :speed-module-2 25,
       :electric-engine-unit 40,
       :low-density-structure 30,
       :processing-unit 60},
      :time 25.0}]} {:name :solar-panel-equipment,
                     :production
                     [{:name :solar-panel-equipment,
                       :type :assembling-machine,
                       :produces {:solar-panel-equipment 1},
                       :requires {:advanced-circuit 2, :solar-panel 1, :steel-plate 5},
                       :time 10.0}]}
   {:name :fusion-reactor-equipment,
    :production
    [{:name :fusion-reactor-equipment,
      :type :assembling-machine,
      :produces {:fusion-reactor-equipment 1},
      :requires {:low-density-structure 50, :processing-unit 200},
      :time 10.0}]}
   {:name :energy-shield-equipment,
    :production
    [{:name :energy-shield-equipment,
      :type :assembling-machine,
      :produces {:energy-shield-equipment 1},
      :requires {:advanced-circuit 5, :steel-plate 10},
      :time 10.0}]}
   {:name :energy-shield-mk2-equipment,
    :production
    [{:name :energy-shield-mk2-equipment,
      :type :assembling-machine,
      :produces {:energy-shield-mk2-equipment 1},
      :requires
      {:energy-shield-equipment 10,
       :low-density-structure 5,
       :processing-unit 5},
      :time 10.0}]}
   {:name :battery-equipment,
    :production
    [{:name :battery-equipment,
      :type :assembling-machine,
      :produces {:battery-equipment 1},
      :requires {:battery 5, :steel-plate 10},
      :time 10.0}]}
   {:name :battery-mk2-equipment,
    :production
    [{:name :battery-mk2-equipment,
      :type :assembling-machine,
      :produces {:battery-mk2-equipment 1},
      :requires
      {:battery-equipment 10,
       :low-density-structure 5,
       :processing-unit 15},
      :time 10.0}]}
   {:name :personal-laser-defense-equipment,
    :production
    [{:name :personal-laser-defense-equipment,
      :type :assembling-machine,
      :produces {:personal-laser-defense-equipment 1},
      :requires
      {:laser-turret 5, :low-density-structure 5, :processing-unit 20},
      :time 10.0}]}
   {:name :discharge-defense-equipment,
    :production
    [{:name :discharge-defense-equipment,
      :type :assembling-machine,
      :produces {:discharge-defense-equipment 1},
      :requires {:laser-turret 10, :processing-unit 5, :steel-plate 20},
      :time 10.0}]}
   {:name :belt-immunity-equipment,
    :production
    [{:name :belt-immunity-equipment,
      :type :assembling-machine,
      :produces {:belt-immunity-equipment 1},
      :requires {:advanced-circuit 5, :steel-plate 10},
      :time 10.0}]}
   {:name :exoskeleton-equipment,
    :production
    [{:name :exoskeleton-equipment,
      :type :assembling-machine,
      :produces {:exoskeleton-equipment 1},
      :requires
      {:electric-engine-unit 30, :processing-unit 10, :steel-plate 20},
      :time 10.0}]}
   {:name :personal-roboport-equipment,
    :production
    [{:name :personal-roboport-equipment,
      :type :assembling-machine,
      :produces {:personal-roboport-equipment 1},
      :requires
      {:advanced-circuit 10,
       :battery 45,
       :iron-gear-wheel 40,
       :steel-plate 20},
      :time 10.0}]}
   {:name :personal-roboport-mk2-equipment,
    :production
    [{:name :personal-roboport-mk2-equipment,
      :type :assembling-machine,
      :produces {:personal-roboport-mk2-equipment 1},
      :requires
      {:low-density-structure 20,
       :personal-roboport-equipment 5,
       :processing-unit 100},
      :time 20.0}]}
   {:name :night-vision-equipment,
    :production
    [{:name :night-vision-equipment,
      :type :assembling-machine,
      :produces {:night-vision-equipment 1},
      :requires {:advanced-circuit 5, :steel-plate 10},
      :time 10.0}]}

    ;
    ; Millitary (Stationary)
    ;


   {:name :stone-wall,
    :production
    [{:name :stone-wall,
      :type :assembling-machine,
      :produces {:stone-wall 1},
      :requires {:stone-brick 5},
      :time 0.5}]}
   {:name :gate,
    :production
    [{:name :gate,
      :type :assembling-machine,
      :produces {:gate 1},
      :requires {:electronic-circuit 2, :steel-plate 2, :stone-wall 1},
      :time 0.5}]}
   {:name :gun-turret,
    :production
    [{:name :gun-turret,
      :type :assembling-machine,
      :produces {:gun-turret 1},
      :requires {:copper-plate 10, :iron-gear-wheel 10, :iron-plate 20},
      :time 8.0}]}
   {:name :laser-turret,
    :production
    [{:name :laser-turret,
      :type :assembling-machine,
      :produces {:laser-turret 1},
      :requires {:battery 12, :electronic-circuit 20, :steel-plate 20},
      :time 20.0}]}
   {:name :flamethrower-turret,
    :production
    [{:name :flamethrower-turret,
      :type :assembling-machine,
      :produces {:flamethrower-turret 1},
      :requires
      {:engine-unit 5, :iron-gear-wheel 15, :pipe 10, :steel-plate 30},
      :time 20.0}]}
   {:name :artillery-turret,
    :production
    [{:name :artillery-turret,
      :type :assembling-machine,
      :produces {:artillery-turret 1},
      :requires
      {:advanced-circuit 20,
       :concrete 60,
       :iron-gear-wheel 40,
       :steel-plate 60},
      :time 40.0}]}
   {:name :rocket-silo,
    :production
    [{:name :rocket-silo,
      :type :assembling-machine,
      :produces {:rocket-silo 1},
      :requires
      {:concrete 1000,
       :electric-engine-unit 200,
       :pipe 100,
       :processing-unit 200,
       :steel-plate 1000},
      :time 30.0}]}
   {:name :radar,
    :production
    [{:name :radar,
      :type :assembling-machine,
      :produces {:radar 1},
      :requires
      {:electronic-circuit 5, :iron-gear-wheel 5, :iron-plate 10},
      :time 0.5}]} {:name :grenade,
                    :production
                    [{:name :grenade,
                      :type :assembling-machine,
                      :produces {:grenade 1},
                      :requires {:coal 10, :iron-plate 5},
                      :time 8.0}]}
   {:name :cluster-grenade,
    :production
    [{:name :cluster-grenade,
      :type :assembling-machine,
      :produces {:cluster-grenade 1},
      :requires {:explosives 5, :grenade 7, :steel-plate 1},
      :time 8.0}]}
   {:name :poison-capsule,
    :production
    [{:name :poison-capsule,
      :type :assembling-machine,
      :produces {:poison-capsule 1},
      :requires {:coal 10, :electronic-circuit 3, :steel-plate 3},
      :time 8.0}]}
   {:name :slowdown-capsule,
    :production
    [{:name :slowdown-capsule,
      :type :assembling-machine,
      :produces {:slowdown-capsule 1},
      :requires {:coal 5, :electronic-circuit 2, :steel-plate 2},
      :time 8.0}]}
   {:name :defender-capsule,
    :production
    [{:name :defender-capsule,
      :type :assembling-machine,
      :produces {:defender-capsule 1},
      :requires
      {:electronic-circuit 3,
       :iron-gear-wheel 3,
       :piercing-rounds-magazine 3},
      :time 8.0}]}
   {:name :distractor-capsule,
    :production
    [{:name :distractor-capsule,
      :type :assembling-machine,
      :produces {:distractor-capsule 1},
      :requires {:advanced-circuit 3, :defender-capsule 4},
      :time 15.0}]}
   {:name :destroyer-capsule,
    :production
    [{:name :destroyer-capsule,
      :type :assembling-machine,
      :produces {:destroyer-capsule 1},
      :requires {:distractor-capsule 4, :speed-module 1},
      :time 15.0}]}
    ;
    ; Millitary (Other)
    ;


   {:name :discharge-defense-remote,
    :production
    [{:name :discharge-defense-remote,
      :type :assembling-machine,
      :produces {:discharge-defense-remote 1},
      :requires {:electronic-circuit 1},
      :time 0.5}]}
   {:name :artillery-targeting-remote,
    :production
    [{:name :artillery-targeting-remote,
      :type :assembling-machine,
      :produces {:artillery-targeting-remote 1},
      :requires {:processing-unit 1, :radar 1},
      :time 0.5}]}

    ;
    ; Research
    ;


   {:name :automation-science-pack,
    :production
    [{:name :automation-science-pack,
      :type :assembling-machine,
      :produces {:automation-science-pack 1},
      :requires {:iron-gear-wheel 1, :copper-plate 1},
      :time 5.0}]}
   {:name :logistic-science-pack,
    :production
    [{:name :logistic-science-pack,
      :type :assembling-machine,
      :produces {:logistic-science-pack 1},
      :requires {:inserter 1, :transport-belt 1},
      :time 6.0}]}
   {:name :military-science-pack,
    :production
    [{:name :military-science-pack,
      :type :assembling-machine,
      :produces {:military-science-pack 2},
      :requires {:grenade 1, :piercing-rounds-magazine 1, :stone-wall 2},
      :time 10.0}]}
   {:name :chemical-science-pack,
    :production
    [{:name :chemical-science-pack,
      :type :assembling-machine,
      :produces {:chemical-science-pack 2},
      :requires {:advanced-circuit 3, :engine-unit 2, :sulfur 1},
      :time 24.0}]}
   {:name :production-science-pack,
    :production
    [{:name :production-science-pack,
      :type :assembling-machine,
      :produces {:production-science-pack 3},
      :requires {:electric-furnace 1, :productivity-module 1, :rail 30},
      :time 21.0}]}
   {:name :utility-science-pack,
    :production
    [{:name :utility-science-pack,
      :type :assembling-machine,
      :produces {:utility-science-pack 3},
      :requires
      {:flying-robot-frame 1,
       :low-density-structure 3,
       :processing-unit 2},
      :time 21.0}]}
   {:name :lab
    :production [{:name :lab
                  :type :assembling-machine
                  :time 2.0
                  :produces {:lab 1}
                  :requires
                  {:electronic-circuit 10
                   :iron-gear-wheel 10
                   :transport-belt 4}}]}
   {:name :science
    :production [{:name :science
                  :type :assembling-machine
                  :time 30.0 ; rouch estimate of late game science usage research takes 60 secs but we probably have 100% bonus
                  :produces {:science 1}
                  :requires
                  {:automation-science-pack 1
                   :logistic-science-pack 1
                   :production-science-pack 1
                   :chemical-science-pack 1
                   :military-science-pack 1
                   :utility-science-pack 1}}]}
    ;
    ; Satellite stuff
    ;


   {:name :rocket-control-unit,
    :production
    [{:name :rocket-control-unit,
      :type :assembling-machine,
      :produces {:rocket-control-unit 1},
      :requires {:processing-unit 1, :speed-module 1},
      :time 30.0}]}
   {:name :low-density-structure,
    :production
    [{:name :low-density-structure,
      :type :assembling-machine,
      :produces {:low-density-structure 1},
      :requires {:copper-plate 20, :plastic-bar 5, :steel-plate 2},
      :time 20.0}]}
   {:name :rocket-fuel,
    :production
    [{:name :rocket-fuel,
      :type :assembling-machine,
      :produces {:rocket-fuel 1},
      :requires {:light-oil 10, :solid-fuel 10},
      :time 30.0}]}
   {:name :rocket-part,
    :production
    [{:name :rocket-part,
      :type :rocket-silo,
      :produces {:rocket-part 1},
      :requires
      {:low-density-structure 10,
       :rocket-control-unit 10,
       :rocket-fuel 10},
      :time 3.0}]}
   {:name :satellite,
    :production
    [{:name :satellite,
      :type :satellite,
      :produces {:satellite 1},
      :requires
      {:accumulator 100,
       :low-density-structure 100,
       :processing-unit 100,
       :radar 5,
       :rocket-fuel 50,
       :solar-panel 100},
      :time 5.0}]}])

(defn name-value-pair [item]
  [(:name item) item])

; Map for looking up items by name
(def item-lookup-map (into {} (map name-value-pair item-list)))