# Usage

[Install clj](https://clojure.org/guides/getting_started).

Start Clj: 
```sh
clj
```

Run some commands
```clojure
user=> (require '[factorio.compute :as compute])
nil
user=> (compute/raw-decimal-values :utility-science-pack)
{:copper-ore 49.83333333333333, :iron-ore 33.33333333333333, :crude-oil 204.8484848484848, :water 160.7575757575758, :coal 3.833333333333333}

```