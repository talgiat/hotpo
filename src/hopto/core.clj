(ns hopto.core)

(defn hopto-cached [cache n]
  (loop [n n
         steps '()]
    (if-let [cached (get @cache n)]
      (do
        (doseq [[i num] (map-indexed vector steps)]
          (swap! cache assoc num (+ i 1 cached)))
        (+ (count steps) cached))
      (if (= (mod n 2) 0)
        (recur (/ n 2) (conj steps n))
        (recur (inc (* 3 n)) (conj steps n))))))

(defn hopto [n]
  (loop [n n
         i 0]
    (cond
      (= n 1) i
      (= (mod n 2) 0) (recur (/ n 2) (inc i))
      :else (recur (inc (* 3 n)) (inc i)))))

(defn test-hopto-helper [n f msg]
  (println msg)
  (println (time (apply max (map f (range 1 (inc n)))))))

(defn test-hopto [n]
  (test-hopto-helper n hopto "not cached")
  (test-hopto-helper n (partial hopto-cached (atom {1 0})) "cached"))

(defn -main [& args]
  (test-hopto (-> args first Integer/parseInt)))