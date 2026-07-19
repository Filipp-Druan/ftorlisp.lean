(dec fact [Number] Number)
(def fact [n]
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(let x 5)

(println_num (fact x))