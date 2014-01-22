(ns kana.core
  (:use [clojure.pprint])
  )



(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))





(def kana-vocabulary
   ["a"     "i"    "u"    "e"    "o"    "n"  
         "ka"    "ki"   "ku"   "ke"   "ko"  
         "sa"    "shi"   "su"   "se"   "so"  
         "za"    "zi"   "zu"   "ze"   "zo"  
         "ta"    "chi"   "tu"   "te"   "to"  
         "da"    "di"   "du"   "de"   "do"  
         "na"    "ni"   "nu"   "ne"   "no"  
         "ha"    "hi"   "hu"   "he"   "ho"  
         "ba"    "bi"   "bu"   "be"   "bo"  
         "pa"    "pi"   "pu"   "pe"   "po"  
         "ma"    "mi"   "mu"   "me"   "mo"  
         "ya"    "yu"   "yo"  
         "ra"    "ri"   "ru"   "re"   "ro"  
         "wa"    "wu"   "we"   "wo"  
         "sha"   "shu"   "sho"])

(def kana-vocabulary-as-set
  (into #{} kana-vocabulary))

(def max-length (->> (map #(.length %) kana-vocabulary) (apply max)))


(defn length-first [s]
  (loop [wl (min (.length s) max-length)]
    (if (> wl 0) 
      (let [ka (.substring s 0 wl)]
        (if (contains? kana-vocabulary-as-set ka)
          ka
          (recur (dec wl))))
      (throw (IllegalStateException.)))
    ))

(defn kana-grammar [alphabet]
  (loop [s alphabet
         res []]
    (if (= (.length s) 0)
      res
      (let [k (length-first s)]
        (recur (.substring s (.length k)) (conj res k))))))

(def alphabet-hiragana
  [
   "a" "\u3042"
   "i" "\u3044"
   "u" "\u3046"
   "e" "\u3048"
   "o" "\u304a"

   "ka" "\u304b"
   "ki" "\u304d"
   "ku" "\u304f"
   "ke" "\u3051"
   "ko" "\u3053"

   "ga" "\u304c"
   "gi" "\u304e"
   "gu" "\u3050"
   "ge" "\u3052"
   "go" "\u3054"


   "sa" "\u3055"
   "shi" "\u3057"
   "su" "\u3059"
   "se" "\u305b"
   "so" "\u305d"

   "za" "\u3056"
   "zi" "\u3058"
   "zu" "\u305a"
   "ze" "\u305c"
   "zo" "\u305e"

   "ta" "\u305f"
   "chi" "\u3061"
   "tu" "\u3064"
   "te" "\u3066"
   "to" "\u3068"

   "da" "\u3060"
   "di" "\u3062"
   "du" "\u3065"
   "de" "\u3067"
   "do" "\u3069"

   "na" "\u306a"
   "ni" "\u306b"
   "nu" "\u306c"
   "ne" "\u306d"
   "no" "\u306e"

   "ha" "\u306f"
   "hi" "\u3072"
   "hu" "\u3075"
   "he" "\u3078"
   "ho" "\u307b"

   "ba" "\u3070"
   "bi" "\u3073"
   "bu" "\u3076"
   "be" "\u3079"
   "bo" "\u307c"

   "pa" "\u3071"
   "pi" "\u3074"
   "pu" "\u3077"
   "pe" "\u307a"
   "po" "\u307d"


   "ma" "\u307e"
   "mi" "\u307f"
   "mu" "\u3080"
   "me" "\u3081"
   "mo" "\u3082"

   "ya" "\u3084"
   "yi" nil
   "yu" "\u3086"
   "ye" nil
   "yo" "\u3088"

   "ra" "\u3089"
   "ri" "\u308a"
   "ru" "\u308b"
   "re" "\u308c"
   "ro" "\u308d"

   "wa" "\u308f"
   "wi" nil
   "wu" "\u3090"
   "we" "\u3091"
   "wo" "\u3092"

   "n" "\u3093"
   "nni" nil
   "nnu" nil
   "nne" nil
   "nno" nil

   "sha" "\u3057\u3083"
   "_shi" nil
   "shu"  "\u3057\u3085"
   "_she" nil
   "sho"  "\u3057\u3087"
   ])


(def alphabet-to-hiragana-map
  (apply hash-map alphabet-hiragana)
  )
(def hiragana-to-alphabet-map
 (apply hash-map (mapcat reverse (filter (fn [[_ v]] v) (partition 2 alphabet-hiragana)))))

(def vowels #{"a" "i" "u" "e" "o"})

(def consonants #{"k" "s" "z" "t" "d" "h" "b" "p" "m" "y" "r" "w" "sh" "ch" "n"})



(defn hiragana-of [alphabet]
  (->> (kana-grammar alphabet) (map #(alphabet-to-hiragana-map %)) (reduce str))
  )

;;------------------------------------------------------------------------------------

