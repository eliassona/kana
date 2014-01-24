(ns kana.crossover.core)


(comment 
  (cemerick.austin.repls/exec :exec-cmds ["open" "-ga" "/Applications/Google Chrome.app"])
  (ns hej (:require [kana.crossover.core :refer [hiragana-of katakana-of alphabeth-of kana-vocabulary]])) 
)


(comment
(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))
)
(defn max-length [l] 
  (->> (map #(.-length %) l) (apply max)))

(def max-length-memo 
  (memoize max-length))

(defn reverse-map [m]
 (apply hash-map (mapcat reverse (filter (fn [[_ v]] v) (partition 2 m)))))

  
(defn kana-grammar [alphabet vocabulary]
  (let [l-fn 
        (fn [s]
          (loop [wl (min (.-length s) (max-length-memo vocabulary))]
				    (if (> wl 0) 
				      (let [ka (.substring s 0 wl)]
				        (if (contains? vocabulary ka)
				          ka
				          (recur (dec wl))))
				      (assert false (str s " is not valid")))
				    ))]
	  (loop [s alphabet
	         res []]
	    (if (= (.-length s) 0)
	      res
	      (let [k (l-fn s)]
	        (recur (.substring s (.-length k)) (conj res k)))))))


(def vowels #{"a" "i" "u" "e" "o"})

(def consonants #{"k" "s" "z" "t" "d" "h" "b" "p" "m" "y" "r" "w" "sh" "ch" "n"})


(def kana-vocabulary
   ["a"     "i"    "u"    "e"    "o"    "n"  
     "ka"    "ki"   "ku"   "ke"   "ko"  
     "ga"    "gi"   "gu"   "ge"   "go"  
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



(defn hiragana-char? [ch]
  (or 
    (and (>= ch 0x3040) (<= ch 0x309F)) 
    (and (>= ch 0x1B000) (<= ch 0x1B0FF))))

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



(defn katakana-char? [ch]
  (or 
    (and (>= ch 0x30A0) (<= ch 0x30FF))
    (and (>= ch 0x31F0) (<= ch 0x31FF))
    (and (>= ch 0x3200) (<= ch 0x32FF))
    (and (>= ch 0xFF00) (<= ch 0xFFEF))
    (and (>= ch 0x1B000) (<= ch 0x1B0FF))))
    
(def alphabet-katakana
  [
   "a" "\u30a2"
   "i" "\u30a4"
   "u" "\u30a6"
   "e" "\u30a8"
   "o" "\u30aa"

   "ka" "\u30ab"
   "ki" "\u30ad"
   "ku" "\u30af"
   "ke" "\u30a1"
   "ko" "\u30a3"

   "ga" "\u30ac"
   "gi" "\u30ae"
   "gu" "\u30b0"
   "ge" "\u30b2"
   "go" "\u30b4"


   "sa" "\u30b5"
   "shi" "\u30b7"
   "su" "\u30b9"
   "se" "\u30bb"
   "so" "\u30bd"

   "za" "\u30b6"
   "zi" "\u30b8"
   "zu" "\u30ba"
   "ze" "\u30bc"
   "zo" "\u30be"

   "ta" "\u30bf"
   "chi" "\u30b1"
   "tu" "\u30b4"
   "te" "\u30b6"
   "to" "\u30b8"

   "da" "\u30c0"
   "di" "\u30c2"
   "du" "\u30c5"
   "de" "\u30c7"
   "do" "\u30c9"

   "na" "\u30ca"
   "ni" "\u30cb"
   "nu" "\u30cc"
   "ne" "\u30cd"
   "no" "\u30ce"

   "ha" "\u30cf"
   "hi" "\u30c2"
   "hu" "\u30c5"
   "he" "\u30c8"
   "ho" "\u30cb"

   "ba" "\u30d0"
   "bi" "\u30d3"
   "bu" "\u30d6"
   "be" "\u30d9"
   "bo" "\u30dc"

   "pa" "\u30d1"
   "pi" "\u30d4"
   "pu" "\u30d7"
   "pe" "\u30da"
   "po" "\u30dd"


   "ma" "\u30de"
   "mi" "\u30df"
   "mu" "\u30e0"
   "me" "\u30e1"
   "mo" "\u30e2"

   "ya" "\u30e4"
   "yi" nil
   "yu" "\u30e6"
   "ye" nil
   "yo" "\u30e8"

   "ra" "\u30e9"
   "ri" "\u30ea"
   "ru" "\u30eb"
   "re" "\u30ec"
   "ro" "\u30ed"

   "wa" "\u30ef"
   "wi" nil
   "wu" "\u3090"
   "we" "\u3091"
   "wo" "\u3092"

   "n" "\u30f3"
   "nni" nil
   "nnu" nil
   "nne" nil
   "nno" nil

   "sha" "\u30b7\u30e3"
   "_shi" nil
   "shu"  "\u30b7\u30e5"
   "_she" nil
   "sho"  "\u30b7\u30e7"
   ])


(def alphabet-to-hiragana-map
  (apply hash-map alphabet-hiragana)
  )

(def hiragana-to-alphabet-map
  (reverse-map alphabet-hiragana))

(def hiragana-vocabulary-as-set (into #{} (keys hiragana-to-alphabet-map)))

(def alphabet-to-katakana-map
  (apply hash-map alphabet-katakana)
  )

(def katakana-to-alphabet-map
  (reverse-map alphabet-katakana))

(def katakana-vocabulary-as-set (into #{} (keys katakana-to-alphabet-map)))


(defn translate [word vocabulary voc-map] (->> (kana-grammar word vocabulary) (map #(voc-map %)) (reduce str)))

(defn partition-word [word]
  (partition-by 
    #(let [uc (int %)]
       (cond (hiragana-char? uc) 1
             (katakana-char? uc) 2
             :else 0)) 
    word))

(defn alphabet-of [word]
  (apply str (map #(let [s (apply str %)
                         ch (.substring s 0 1)]
                     (cond 
                       (contains? hiragana-vocabulary-as-set ch)
                       (translate s hiragana-vocabulary-as-set hiragana-to-alphabet-map)
                       (contains? katakana-vocabulary-as-set ch)
                       (translate s katakana-vocabulary-as-set katakana-to-alphabet-map)
                       :else 
                       s))
                       (partition-word word))))

(defn hiragana-of [word]
  (translate (alphabet-of word) kana-vocabulary-as-set alphabet-to-hiragana-map)
  )

(defn katakana-of [word]
  (translate (alphabet-of word) kana-vocabulary-as-set alphabet-to-katakana-map)
  )

(def english-japanese
  ["fish" "sakana"]
  )
(def japanese-to-english-map
  (reverse-map english-japanese))

(def english-to-japanese-map
  (apply hash-map english-japanese)
  )

(defn japanese-of [word]
  (english-to-japanese-map word)
  )
(defn english-of [word]
  (japanese-to-english-map (alphabet-of word))
  )

;;------------------------------------------------------------------------------------


