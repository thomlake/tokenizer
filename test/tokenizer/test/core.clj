(ns tokenizer.test.core
  (:use [tokenizer.core] :reload)
  (:use [clojure.test]))

(deftest tokenize-with-str-test
	(let [split-keep "?.,$#()" split-ditch " "]
	(is (= (count (tokenize "hello world" split-keep split-ditch)) 2))
	(is (= (count (tokenize "hello$world" split-keep split-ditch)) 3))
	(is (= (count (tokenize "#hello world" split-keep split-ditch)) 3))
	(is (= (count (tokenize "he(lo world" split-keep split-ditch)) 4))
	(is (= (count (tokenize "hello world." split-keep split-ditch)) 3)))
)

(deftest tokenize-with-fn-test
	(let [lfunc (fn [c] (and (<= (int c) (int \z)) (>= (int c) (int \a)))) 
		nfunc (fn [c] (and (<= (int c) (int \9)) (>= (int c) (int \0))))
		sfunc (fn [c] (== (int c) (int \ )))
		notalphanum (fn [c] (not (or (lfunc c) (nfunc c))))]
	(is (= (count (tokenize "hello world" notalphanum sfunc)) 2))
	(is (= (count (tokenize "hello$world" notalphanum sfunc)) 3))
	(is (= (count (tokenize "#hello world" notalphanum sfunc)) 3))
	(is (= (count (tokenize "he(lo world" notalphanum sfunc)) 4)))
)
