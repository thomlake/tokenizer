(ns tokenizer.core)

(def emoticons 
  [":-)" ":)"
   ":-(" ":("
   "(-:" "(:"
   ")-:" "):"]
)

(def emoticons-reg 
  [":-\\)" ":\\)"
   ":-\\(" ":\\("
   "\\(-:" "\\(:"
   "\\)-:" "\\):"]
)

(defn str-contains?
  [s token]
  (if (< (.indexOf s token) 0) false true)
)

(defn str-emoticon?
  [s]
  ((fn [s i] 
    (if (< i (count emoticons)) 
      (if (= (nth emoticons i) s)
        true
        (recur s (inc i)))
      false)) s 0)
)

(defn url?
  [s]
  (or (str-contains? s ".com")
      (str-contains? s ".org")
      (str-contains? s ".net")
      (str-contains? s "http://")
      (str-contains? s "https://")
      (str-contains? s "www."))
)

(defn supress-extra-letters-inner
  [s snew len dup? last-c index]
  (if (< index len)
    (let [c (nth s index)]
      (if (= last-c c)
        (if dup?
          (recur s snew len true c (inc index))
          (recur s (conj snew c) len true c (inc index)))
        (recur s (conj snew c) len false c (inc index))))
    snew)
)

(defn supress-extra-letters
  [s]
  (reduce str (supress-extra-letters-inner (vec s) [] (count s) false nil 0))
)

(defn char-letter? 
  [c] 
  (and (<= (int c) (int \z)) (>= (int c) (int \a)))
)

(defn char-number? 
  [c] 
  (and (<= (int c) (int \9)) (>= (int c) (int \0)))
)

(defn non-alphanumeric?
  [c] 
  (not (or (char-number? c) (char-letter? c)))
)

(defn split-on-punc-inner
  [vs vs-new len vs-index vs-new-index]
  (if (< vs-index len)
    (let [c (nth vs vs-index)]
      (if (non-alphanumeric? c)
        (recur vs (conj (conj vs-new [(str c)]) []) len (inc vs-index) (+ vs-new-index 2))
        (recur vs (update-in vs-new [vs-new-index] #(conj % c)) len (inc vs-index) vs-new-index)))
    vs-new)
)

(defn split-on-punc
  [s]
  (remove empty? (map #(reduce str %) (split-on-punc-inner (vec s) [[]] (count s) 0 0)))
)

(defn cleanse
  [s]
  (cond
    (str-emoticon? s) [s]
    (url? s) ["<url>"]
    (= (first s) \@) ["<mention>"]
    (= (first s) \#) ["<hashtag>"]
    ;(.endsWith s "n't") (reduce str "<neg>" (subvec (vec s) 0 (- (count s) 3)))
    :else (map #(-> % supress-extra-letters str) (split-on-punc s)))
)

(defn split-emoticons-inner
  [s new-vec emoticon-index]
  (if (< emoticon-index (count emoticons))
    (let [i (.indexOf s (nth emoticons emoticon-index))]
      (if (> i -1)
        (let [leftovers (.split s (nth emoticons-reg emoticon-index))]
          (if (empty? leftovers)
            (conj new-vec (nth emoticons emoticon-index))
            (recur (reduce #(str %1 " " %2) leftovers) 
                   (conj new-vec (nth emoticons emoticon-index))
                   (inc emoticon-index))))
        (recur s new-vec (inc emoticon-index))))
    (flatten [new-vec (remove empty? (vec (.split s " ")))]))
)

(defn split-emoticons
  [s]
  (vec (split-emoticons-inner s [] 0))
)

(defn tokenize
  [s]
  (let [v (flatten (map split-emoticons (-> s (.toLowerCase) (.split " "))))]
   (reduce #(reduce conj %1 %2) [] (map cleanse v)))
)

