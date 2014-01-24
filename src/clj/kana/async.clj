(ns kana.async)
(require '[clojure.core.async :as async :refer [<! >! timeout chan alt! go put!]])
(require '[clj-http.client :as client])


(defn real-search [query]
  (let [c (chan)]
   (put! c [query (client/get query)])
   c))

(def sites ["https://www.google.se/#q=clojure" 
            "https://search.yahoo.com/search;_ylt=AiE.KKtdmaD147Vw__BJveSbvZx4?p=clojure&toggle=1&cop=mss&ei=UTF-8&fr=yfp-t-901"
            "http://se.ask.com/web?qsrc=1&o=6581&l=sem&q=clojure"
            "http://www.bing.com/search?q=clojure&go=&qs=n&form=QBLH&filt=all&pq=clojure&sc=2-7&sp=-1&sk="
            "http://blekko.com/#?q=clojure"
            ])

(defn search-competition []
  (let [t (timeout 500)
        chans (conj (map #(real-search %) sites) t)]  
    (go
      (let [[v c] (alts! chans)]
        (condp = c
          t (println "timeout!")
          (println (format "Winner is: %s" (first v))))))
    (println "done"))) 
        
      

