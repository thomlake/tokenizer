(ns tokenizer.test.core
  (:use [tokenizer.core] :reload)
  (:use [clojure.test]))

(defn debug? [] false)

(deftest str-contains-test
  (is (str-contains? "hello" "ll"))
  (is (not (str-contains? "hello" "lll")))
)

(deftest is-test
  ;char-letter?
  (is (char-letter? \c))
  (is (char-letter? \z))
  (is (not (char-letter? \!)))
  (is (not (char-letter? \2)))
  ;char-number?
  (is (char-number? \0))
  (is (char-number? \9))
  (is (not (char-number? \#)))
  (is (not (char-number? \a)))
  ;non-alphanumeric?
  (is (non-alphanumeric? \!))
  (is (non-alphanumeric? \$))
  (is (not (non-alphanumeric? \a)))
  (is (not (non-alphanumeric? \3)))
  ;str-emoticon?
  (is (str-emoticon? ":-)"))
  (is (str-emoticon? ")-:"))
  (is (str-emoticon? ":-)"))
  (is (str-emoticon? ")-:"))
  (is (str-emoticon? ":)"))
  (is (str-emoticon? "):"))
)

(deftest split-on-punc-test
  (let [result (split-on-punc "hello!world") 
        expect ["hello" "!" "world"]]
    (if (debug?)
      (println result "[count =" (count result)"]"))
    (is (== (.compareTo (nth result 0) (nth expect 0)) 0))
    (is (== (.compareTo (nth result 1) (nth expect 1)) 0))
    (is (== (.compareTo (nth result 2) (nth expect 2)) 0)))

  (let [result (split-on-punc "!helloworld") 
        expect ["!" "helloworld"]]
    (if (debug?)
      (println result "[count =" (count result) "]"))
    (is (== (.compareTo (nth result 0) (nth expect 0)) 0))
    (is (== (.compareTo (nth result 1) (nth expect 1)) 0)))

  (let [result (split-on-punc "helloworld!") 
        expect ["helloworld" "!"]]
    (if (debug?)
      (println result "[count =" (count result) "]"))
    (is (== (.compareTo (nth result 0) (nth expect 0)) 0))
    (is (== (.compareTo (nth result 1) (nth expect 1)) 0)))
 
  (let [result (split-on-punc "hello[%!")
        expect ["hello" "[" "%" "!"]]
    (if (debug?)
      (println result "[count =" (count result) "]"))
    (is (== (.compareTo (nth result 0) (nth expect 0)) 0))
    (is (== (.compareTo (nth result 1) (nth expect 1)) 0))
    (is (== (.compareTo (nth result 2) (nth expect 2)) 0))
    (is (== (.compareTo (nth result 3) (nth expect 3)) 0)))
)

(deftest split-emoticons-test
  (let [result (split-emoticons ":-)hello")
        expect [":-)" "hello"]]
    (if (debug?)
      (println result))
    (is (= (sort result) (sort expect))))

  (let [result (split-emoticons ":-)hello:-)")
        expect [":-)" "hello"]]
    (if (debug?)
      (println result))
    (is (= (sort result) (sort expect))))

  (let [result (split-emoticons ":-)hel:)lo:-(")
        expect [":-)" "hel" ":)" "lo" ":-("]]
    (if (debug?)
      (println result))
    (is (= (sort result) (sort expect))))

   (let [result (split-emoticons ":-)")
         expect [":-)"]]
     (if (debug?)
      (println result))
     (is (= (sort result) (sort expect))))

   (let [result (split-emoticons ":-)(-:")
         expect [":-)" "(-:"]]
     (if (debug?)
      (println result))
     (is (= (sort result) (sort expect))))

    (let [result (split-emoticons ":-)!")
         expect [":-)" "!"]]
     (if (debug?)
      (println result))
     (is (= (sort result) (sort expect))))
        
 (let [result (split-emoticons "hel)-:lo")
       expect ["hel" ")-:" "lo"]]
    (if (debug?)
      (println result))
   (is (= (sort result) (sort expect))))
 (let [result (split-emoticons "hello")
       expect ["hello"]]
    (if (debug?)
      (println result))
   (is (= (sort result) (sort expect))))

 (let [result (split-emoticons "some_words-in!:-a,string")
       expect ["some_words-in!:-a,string"]]
    (if (debug?)
      (println result))
   (is (= (sort result) (sort expect))))
)

(deftest cleanse-test
  (let [result (cleanse "www.google.com")
        expect ["<url>"]]
    (if (debug?)
      (println result))
    (is (= expect result)))

  (let [result (cleanse "omgzzz!!!")
        expect ["omgzz" "!" "!" "!"]]
    (if (debug?)
      (println result expect))
    (is (= result expect)))

  (let [result (cleanse "@somedude")
        expect ["<mention>"]]
    (if (debug?)
      (println result))
    (is (= result expect)))
  
  (let [result (cleanse "#boredatwork")
        expect ["<hashtag>"]]
    (if (debug?)
      (println result))
    (is (= result expect)))
   
  (let [result (cleanse ":-)")
        expect [":-)"]]
    (if (debug?)
      (println result))
    (is (= result expect)))

  (let [result (cleanse ")-:")
        expect [")-:"]]
    (if (debug?)
      (println result))
    (is (= result expect)))

  (let [result (cleanse "#boredatwork")
        expect ["<hashtag>"]]
    (if (debug?)
      (println result))
    (is (= result expect)))     
)

(deftest tokenize-test
  (let [result (tokenize "hello world")
        expect ["hello" "world"]]
    (if (debug?)
      (println result))
    (is (= (sort result) (sort expect))))

  (let [result (tokenize "hello:-) world")
        expect ["hello" ":-)" "world"]]
    (if (debug?)
      (println result))
    (is (= (sort result) (sort expect))))

 (let [result (tokenize "hello world")
        expect ["hello" "world"]]
    (if (debug?)
      (println result))
    (is (= (sort result) (sort expect))))

 (let [result (tokenize "MY cat ^--^ can dooooo baCKFlip www.catbackflips.com")
        expect ["my" "cat" "^" "-" "-" "^" "can" "doo" "backflip" "<url>"]]
    (if (debug?)
      (println result expect))
    (is (= (sort result) (sort expect))))
)
