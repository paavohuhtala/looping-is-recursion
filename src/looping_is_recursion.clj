(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? exp) 1
                   (if (== 1 n)
                     acc
                     (recur (* acc base) (dec n)))))]
    (helper base exp)))

(defn last-element [a-seq]
  (let [helper (fn [rest-seq]
                 (if (empty? (rest rest-seq))
                   (first rest-seq)
                   (recur (rest rest-seq))))]
    (helper a-seq)))

(defn seq= [a-seq b-seq]
  (let [helper (fn [a-sub b-sub]
                 (cond
                   (and (empty? a-sub) (empty? b-sub))
                     true
                   (= (first a-sub) (first b-sub))
                     (let [a-rest (rest a-sub)
                           b-rest (rest b-sub)]
                       (if (=
                             (not (empty? a-rest))
                             (not (empty? b-rest)))
                         (recur a-rest b-rest)
                         false))
                   :else
                     false))]
    (helper a-seq b-seq)))

(defn find-first-index [pred a-seq]
  (loop [seq-iterated a-seq
         n 0]
    (let [seq-first (first seq-iterated)]
      (cond
        (empty? seq-iterated)
          nil
        (pred seq-first)
          n
        :else
          (recur (rest seq-iterated) (inc n))))))

(defn avg [a-seq]
  (loop [elements a-seq
         sum 0
         element-count 0]
    (if (empty? elements)
      (if (zero? element-count)
        0
        (/ sum element-count))
      (recur (rest elements) (+ sum (first elements)) (inc element-count)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [elements a-seq
         appeared-once #{}]
    (if (empty? elements)
      appeared-once
      (recur (rest elements) (toggle appeared-once (first elements))))))

(defn fast-fibo [n]
  (cond
    (zero? n)
      0
    (< n 3)
      1
    :else
      (loop [iter-remain (- n 3)
             n-1 1
             n-2 1]
        (if (<= iter-remain 0)
          (+ n-1 n-2)
          (recur (dec iter-remain) (+ n-1 n-2) n-1)))))

(defn cut-at-repetition [a-seq]
  (loop [elements a-seq
         so-far []
         appeared #{}]
    (let [first-element (first elements)]
      (if (or (empty? elements) (contains? appeared first-element))
        so-far
        (recur (rest elements) (conj so-far first-element) (conj appeared first-element))))))

