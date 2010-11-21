(ns tokenizer.core)

(defn split
	{:doc "if the char c is in string split-str, return true, else return false"}
	[c split-str]
	(let [f (fn [c s i len]
	(if (< i len)
		(if (= c (.charAt s i)) 
			true 
			(recur c s (inc i) len)) 
		false))]
	(f c split-str 0 (count split-str)))
)

(defn tok-with-fn
	{:doc "takes a string, line, and returns a sequence of string token split on the arguments func-keep (tokens to split on, but keep) and func-ditch (tokens to split on, and ignore). func-keep and func-ditch must be functions which take a single char argument and return true (split) or false (don't split)."}
	[line func-keep func-ditch]
	(let [f (fn
		[line len start current result]
		(if (< current len) (let [c (.charAt line current)]
			(cond
			(func-ditch c)
				(if (= current start)
					(recur line len (inc current) (inc current) result) ;skip space
					(recur line len (inc current) (inc current) (assoc result (count result) (.substring line start current)))) ;skip space, get word
			(func-keep c)
				(if (= current start)
					(recur line len (inc current) (inc current) (assoc result (count result) (.substring line start (inc current)))) ;get punc
					(recur line len (inc current) (inc current) (assoc (assoc result (count result) (.substring line start current)) (+ (count result) 1) (.substring line current (inc current))))) ;get punc, get word
			:else 
				(recur line len start (inc current) result)))
			(if (= start current)
				result
				(assoc result (count result) (.substring line start current)))))]
		(f line (count line) 0 0 []))
)

(defn tok-with-str
	{:doc "takes a string, line, and returns a sequence of string tokens split on the arguments split-keep (string of all tokens to split on, but keep) and split-ditch (string of all tokens to split on, and ignore)."}
	[line split-keep split-ditch]
	(let [f (fn
		[line len start current result]
		(if (< current len) (let [c (.charAt line current)]
			(cond
			(split c split-ditch)
				(if (= current start)
					(recur line len (inc current) (inc current) result) ;skip space
					(recur line len (inc current) (inc current) (assoc result (count result) (.substring line start current)))) ;skip space, get word
			(split c split-keep)
				(if (= current start)
					(recur line len (inc current) (inc current) (assoc result (count result) (.substring line start (inc current)))) ;get punc
					(recur line len (inc current) (inc current) (assoc (assoc result (count result) (.substring line start current)) (+ (count result) 1) (.substring line current (inc current))))) ;get punc, get word
			:else 
				(recur line len start (inc current) result)))
			(if (= start current)
				result
				(assoc result (count result) (.substring line start current)))))]
		(f line (count line) 0 0 []))
)

(defn tokenize
	[s split-keep split-ditch]
	(cond 
		(and (fn? split-keep) (fn? split-ditch))
			(tok-with-fn s split-keep split-ditch)
		(and (string? split-keep) (string? split-ditch))
			(tok-with-str s split-keep split-ditch)
		:else nil)
)
		
