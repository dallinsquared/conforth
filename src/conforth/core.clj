(ns conforth.core)

;;; language
(defn pcomp [& fs]
  (fn [e] (some #(= true %) (map #(% e) fs))))
(defn popn [x s]
  (reduce #(%2 %1) s (repeat x pop)))
(defn push [x s]
  (conj s x))
(defn eval* [s env] (letfn [(eval1 [e z env]
                               (cond
                                ((pcomp number? char?) e) s
                                (vector? e) (evalvec e z)
                                ))
                        (evalvec [v z env]
                                 (reduce #(eval1 (%2 %1 env)) z v))])) ;;implement mutable env
(defn true* [s]
  (let [;f (peek s)
        t (peek (pop s))
        z (popn 2 s)]
    ))
(defn false* [x y]
  y)
(defn slift [n f]
  #(conj (popn n %) (apply f (take-last n %))))
(defn rslift [n f]
  #(conj (popn n %) (apply f (reverse (take-last n %)))))
(def p {:plus (slift 2 +)
        :minus (slift 2 -)
        :times (slift 2 *)
        :div (slift 2 /)})
;;; test bed
(def stack [1 2 3 4 5 6 [2 2 + -]])  ;;eval each element of vector to eval vector
(def plus (slift 2 +))
(def minus (slift 2 -))
(def times (slift 2 *))
(def div (slift 2 /))
;; (div stack)
;; (times stack)
;; (minus stack)
;; (plus stack)
(subvec [:a :b :c :d] 1)
(concat [:a :b] [:c :d])
(let [x stack]
  (take-last 2 x))
(pop [1 2 3 4])
(reduce #(%2 %1) [1 2 3 4 5 6] (repeat 3 pop))
((first (repeat 3 pop)) [1 2 3])
(reverse (take-last 3 [1 2 3 4 5]))
(contains? [:a :b :c :d] :a)
(some #(= % :s) [:a :b :c :d])
(pcomp number? char?)
(conj [1 2 3 4] 5)
