(ns conforth.core)

;;; language
(defn pcomp [& fs]
  (fn [e] (some #(= true %) (map #(% e) fs))))
(defn popn [x s]
  (reduce #(%2 %1) s (repeat x pop)))
(defn push [x s]
  (conj s x))
(defn rcomp [& coll]
  (apply comp (reverse coll)))
(defn eval. [s env] (letfn [])) ;;implement mutable env
(defn true. [s]
  (let [;f (peek s)
        t (peek (pop s))
        z (popn 2 s)]
    ))
(defn compile. [v]
  (apply rcomp v))
(defn false. [x y]
  y)
(defn slift [n f]
  #(conj (popn n %) (apply f (take-last n %))))
(defn slift-const [x]
  (slift 0 (fn [] x)))
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
((slift-const "abc") stack)
((apply rcomp [(partial * 2) inc (partial / 3)]) 5)
(def x 5)
((fn [] (def x (inc x))))
((fn [] (def x (inc x))))

x
((fn [] (def x (inc x))))
x
(/ 5 3)
