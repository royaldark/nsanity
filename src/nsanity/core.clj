(ns nsanity.core
  "nsanity - bringing sanity to your namespaces"
  (:require [clojure.string :as str]
            [medley.core :as medley]
            [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]))

(defn ^:private ->import-seq
  [x]
  (->> x
       (iterate z/right)
       (drop 1)
       (take-while some?)))

(defn ^:private ->ns-name
  [zloc]
  (if (= :vector (z/tag zloc))
    (-> zloc z/down z/node n/string keyword)
    (-> zloc z/node n/string keyword)))

(defn ^:private sort-import-zlocs
  [zlocs]
  (->> zlocs
       ->import-seq
       (group-by ->ns-name)
       (medley/map-kv (fn [ns-name zlocs]
                        (assert (= 1 (count zlocs))
                                (format "Multiple entries for NS %s: [%s]"
                                        (name ns-name)
                                        (str/join ", " (map z/string zlocs))))
                        [ns-name (first zlocs)]))
       (into (sorted-map))
       vals))

(defn ^:private reorg-import-zlocs
  [orig sorted]
  (reduce
   (fn [zloc-old zloc-new]
     (z/replace (z/right zloc-old)
                (z/node zloc-new)))
   orig
   sorted))

(defn nsanify
  ([path]
   (nsanify path nil))
  ([path opts]
   (let [data       (z/of-file path)
         ns-form    (z/find-value data z/next 'ns)
         ns-require (z/find-value ns-form z/next :require)]
     (when ns-require
       (let [sorted  (sort-import-zlocs ns-require)
             reorg'd (reorg-import-zlocs ns-require sorted)]
         (spit path
               (n/string (z/root reorg'd))))))))
