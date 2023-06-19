(def my-map {:a {:b {:c 5}
                 :d 6}
             :e {:f 7}
             :g {:h {:i 77}}})

(:d (:a my-map))
((comp :d :a) my-map)
(get-in my-map [:a :d])

; Think of the [k v] pair as the current key-value pair to check against our search-key.
; to-check works like a stack (or queue?) that contains all other key-value pairs.
; If a key isn't the correct one, we still have to check for sub-keys in the key's value --
; but of course only if the key's value actually is a map as well.
; Pay particular attention to how first and rest work on maps:
; (first {:a 'a, :b 'b, :c 'c}) => [:a a]
; (rest {:a 'a, :b 'b, :c 'c}) => ([:b b] [:c c])
; Notice how they return two-element vectors like: [key value].
(defn attr
  [search-key]
  (fn [m]
    (loop [[k v] (first m)
           to-check (rest m)]
      (if (nil? k)
        nil
        (if (= search-key k)
          v
          (recur (first to-check)
                 (if (map? v)
                   (concat (rest to-check) v)
                   (rest to-check))))))))

(defn attr
  [search-key]
  (fn [m]
    (loop [[k v] (first m)
           to-check (rest m)]
      (when (not (nil? k))
        (if (= search-key k)
          v
          (recur (first to-check)
                 (if (map? v)
                   (concat (rest to-check) v)
                   (rest to-check))))))))

; Few couple lines of code, many thousand lines of thought!

((attr :a) my-map) ; => {:b {:c 5}, :d 6}
((attr :e) my-map) ; => {:f 7}
((attr :d) my-map) ; => 6
((attr :f) my-map) ; => 7
((attr :i) my-map) ; => 77
((attr :z) my-map) ; => nil

;; -----------------------------------------------------------------------------

(defn my-comp
  [& fns]
  (fn [& args]
    (reduce (fn [arg fun]
              (fun arg))
            (apply (last fns) args)
            (butlast fns))))

((my-comp inc +) 1 2 3) ; => 7
((my-comp #(/ % 2) +) 1 2 3) ; => 3

;; -----------------------------------------------------------------------------

(defn my-assoc-in
  [m [k & ks] v]
  (reduce (fn [map key]
            (assoc {} key map))
          (assoc m k v)
          ks))

; Uh.. oups! This is the wrong way around.
(my-assoc-in {} [:a :b :c] 5) ; => {:c {:b {:a 5}}}
(my-assoc-in {:v 7} [:a :b :c] 5) ; => {:c {:b {:v 7, :a 5}}} <-- That's wrong as well!

(defn my-assoc-in
  [m [k & ks] v]
  (if (empty? ks)
    (assoc m k v)
    (assoc m k (my-assoc-in {} ks v))))

(assoc-in {} [:a :b :c] 5) ; => {:a {:b {:c 5}}}
(assoc-in {:v 7} [:a :b :c] 5) ; => {:v 7, :a {:b {:c 5}}}
(my-assoc-in {} [:a :b :c] 5) ; => {:a {:b {:c 5}}}
(my-assoc-in {:v 7} [:a :b :c] 5) ; => {:v 7, :a {:b {:c 5}}}

(assoc-in {:a {:b 5, :c 8}} [:a :b] 7) ; => {:a {:b 7, :c 8}}
(my-assoc-in {:a {:b 5, :c 8}} [:a :b] 7) ; => {:a {:b 7}} <-- Wrong!!!

(defn my-assoc-in
  [m [k & ks] v]
  (if (empty? ks)
    (assoc m k v)
    (assoc m k (my-assoc-in (get m k) ks v))))

(my-assoc-in {:a {:b 5, :c 8}} [:a :b] 7) ; => {:a {:b 7, :c 8}}

(assoc {} :a (assoc {} :b (assoc {} :c 5)))

;; -----------------------------------------------------------------------------

(update-in {:a {:c 5} :b 7} [:a :c] inc) ; => {:a {:c 6}, :b 7}
(update-in {:a {:c 5} :b 7} [:a :c] #(/ %1 %2) 2) ; => {:a {:c 5/2}, :b 7}

(defn my-update-in
  [m ks f & args]
  (assoc-in m ks (apply f (get-in m ks) args)))

(my-update-in {:a {:c 5} :b 7} [:a :c] #(/ %1 %2) 2) ; => {:a {:c 5/2}, :b 7}

;; -----------------------------------------------------------------------------

; 1 + 2 * 3 = 7
; (+ 1 (* 2 3) = 7

;(def precedences
;  {nil 0
;   :+ 1
;   :- 1
;   :/ 2
;   :* 2})
;
;(def get-precedence (partial get precedences))
;
;(defn lower-precedence?
;  [op1 op2]
;  (let [op1 (if (symbol? op1) (keyword op1) op1)
;        op2 (if (symbol? op2) (keyword op2) op2)]
;    (< (get-precedence op1) (get-precedence op2))))
;
;(defn push-op
;  [stack op]
;  (update-in stack [:ops] #(conj % op)))
;
;(defn top-op
;  [stack]
;  (first (:ops stack)))
;
;(defn push-num
;  [stack num]
;  (update-in stack [:nums] #(conj % num)))
;
;(defn create-pair
;  [{nums :nums ops :ops}]
;  (let [op (first ops)
;        [num1 num2] (take 2 nums)
;        pair (list op num1 num2)]
;    {:nums (conj (drop 2 nums) pair) :ops (rest ops)}))
;
;;(eval (first (:nums (create-pair {:nums '(3 2 1) :ops (list * +)}))))
;
;(defn pre-infix
;  [infixed]
;  (reduce (fn [a e]
;            (if (number? e)
;              (push-num a e)
;              (push-op (if (lower-precedence? e (top-op a))
;                         (create-pair a)
;                         a)
;                       e)))
;          {:nums '() :ops '()}
;          infixed))
;
;(defmacro infix
;  [infixed]
;  (loop [a (pre-infix infixed)]
;    (if (empty? (:ops a))
;      (first (:nums a))
;      (recur (create-pair a)))))
;
;(macroexpand '(infix (1 + 2 * 3)))

; iterate through input list
;   el is a number
;     push to number stack
;   el is an operator
;     el has higher or equal precedence to top of operator stack
;       push to operator stack
;     el has lower precedence to top of operator stack
;       put top of op stack and top two nums into list and store list back onto number stack
;       put operator onto op stack
; while op stack not empty
;   take top two numbers from number stack
;   take top operator from operator stack
;   add together in a list, place back on number stack

(macroexpand '(infix (1 + 2 * 3))) ; => (+ (* 3 2) 1)
(infix (1 + 2 * 3)) ; => 7

(macroexpand '(infix (1 * 2 + 3))) ; => (+ 3 (* 2 1))
(infix (1 * 2 + 3)) ; => 5

;; -----------------------------------------------------------------------------

(def order-details-invalid {:name ""
                            :email "mitchard.blimmonsgmail.com"})

(def order-details-valid {:name "Mitchard Blimmons"
                          :email "mitchard.blimmons@gmail.com"})

(def order-details-validations
  {:name ["Please enter a name" not-empty]
   :email ["Please enter an email address" not-empty
           "Your email address doesn't look like an email address" #(or (empty? %) (re-seq #"@" %))]})

(defn error-messages-for
  [to-validate message-validator-pairs]
  (map first (filter #(not ((second %) to-validate))
                     (partition 2 message-validator-pairs))))

(defn validate
  [to-validate validations]
  (reduce (fn [errors validation]
            (let [[fieldname validation-check-groups] validation
                  value (get to-validate fieldname)
                  error-messages (error-messages-for value validation-check-groups)]
              (if (empty? error-messages)
                errors
                (assoc errors fieldname error-messages))))
          {}
          validations))

; (validate order-details-invalid order-details-validations)

(defmacro when-valid
  [to-validate validations & body]
  `(when (empty? (validate ~to-validate ~validations))
     ~@body))

(when-valid order-details-valid order-details-validations
            (println "It's a success!")
            :success)

;; -----------------------------------------------------------------------------

(defmacro my-or
  ([] nil)
  ([x] x)
  ([x & next]
   `(let [or# ~x]
      (if or# or# (my-or ~@next)))))

(macroexpand '(my-or false true false))
(my-or false false true)

;; -----------------------------------------------------------------------------

(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})

;(def c-int (comp :intelligence :attributes))

(defmacro defattrs
  [& attrs]
  `(do
     ~@(map #(let [name (first %)
                   attr (second %)]
               `(def ~name (comp ~attr :attributes)))
            (partition 2 attrs))))

(macroexpand '(defattrs
                c-int :intelligence
                c-str :strength
                c-dex :dexterity))

(defattrs
  c-int :intelligence
  c-str :strength
  c-dex :dexterity)

(c-int character)
(c-str character)
(c-dex character)

;; -----------------------------------------------------------------------------

(def precedences
  {nil 0
   '+ 1
   '- 1
   '/ 2
   '* 2})

(defn lower-precedence?
  [left right]
  (< (precedences left) (precedences right)))

(defn update-stack
  [key stack val]
  (update-in stack [key] #(conj % val)))

(def push-num (partial update-stack :nums))
(def push-op (partial update-stack :ops))

(defn create-pair
  [{[n1 n2 & nums] :nums [op & ops] :ops}]
  (let [pair `(~op ~n2 ~n1)]
    {:nums (conj nums pair) :ops ops}))

(defmacro infix
  [& infixed]
  (loop [[el & rst] infixed
         {nums :nums ops :ops :as stack} {:nums '() :ops '()}]
    (if (and (nil? el) (empty? ops))
      (first nums)
      (recur rst (if (nil? el)
                   (create-pair stack)
                   (if (number? el)
                     (push-num stack el)
                     (push-op (if (lower-precedence? el (first ops))
                                (create-pair stack)
                                stack)
                              el)))))))

(defmacro infix
  [& infixed]
  (loop [[el & rst] infixed
         {nums :nums ops :ops :as stack} {:nums '() :ops '()}]
    (if (and (nil? el) (empty? ops))
      (first nums)
      (recur rst
             (cond
               (number? el) (push-num stack el)
               (symbol? el) (push-op (if (lower-precedence? el (first ops))
                                       (create-pair stack)
                                       stack)
                                     el)
               :else (create-pair stack))))))

;(= (symbol '+) '+)
;(precedences +)
(macroexpand '(infix 1 + 2 * 3))
(infix 1 + 2 * 3)

(macroexpand '(infix 1 * 2 - 3))
(infix 1 * 2 - 3)

(macroexpand '(infix -1 + 4))
(infix -1 + 4)

(macroexpand '(infix 4 + -1))
(infix 4 + -1)

;; -----------------------------------------------------------------------------

(defmacro wait
  [timeout & body]
  `(do (Thread/sleep ~timeout) ~@body))

(let [saying3 (promise)]
  (future (deliver saying3 (wait 100 "Cheerio!")))
  @(let [saying2 (promise)]
     (future (deliver saying2 (wait 400 "Pip pip!")))
     @(let [saying1 (promise)]
        (future (deliver saying1 (wait 200 "'Ello, gov'na!")))
        (println @saying1)
        saying1)
     (println @saying2)
     saying2)
  (println @saying3)
  saying3)

(defmacro enqueue
  ([q concurrent-promise-name concurrent serialized]
   `(let [~concurrent-promise-name (promise)]
      (future (deliver ~concurrent-promise-name ~concurrent))
      (deref ~q)
      ~serialized
      ~concurrent-promise-name))
  ([concurrent-promise-name concurrent serialized]
   `(enqueue (future) ~concurrent-promise-name ~concurrent ~serialized)))

(time @(-> (enqueue saying (wait 200 "'Ello, gov'na!") (println @saying))
           (enqueue saying (wait 400 "Pip pip!") (println @saying))
           (enqueue saying (wait 100 "Cheerio!") (println @saying))))

(defn some-network-call
  [n]
  (Thread/sleep n)
  n)

(def ops [1000 4000 3000 2000])

(time (second (map deref (for [op ops]
                           (future (some-network-call op))))))

;; -----------------------------------------------------------------------------

(def fred (atom {:cuddle-hunger-level 0
                 :percent-deteriorated 0}))
@fred
(swap! fred #(merge-with + % {:cuddle-hunger-level 1}))
@fred
(swap! fred #(merge-with + % {:cuddle-hunger-level 1
                              :percent-deteriorated 1}))

(defn increase-cuddle-hunger-level
  [zombie-state increase-by]
  (merge-with + zombie-state {:cuddle-hunger-level increase-by}))

(increase-cuddle-hunger-level @fred 10) ; => {:cuddle-hunger-level 12, :percent-deteriorated 1}
(swap! fred increase-cuddle-hunger-level 10)
@fred
(swap! fred update-in [:cuddle-hunger-level] + 10)

(reset! fred {:cuddle-hunger-level 0
              :percent-deteriorated 0})

(defn shuffle-speed
  [zombie]
  (* (:cuddle-hunger-level zombie)
     (- 100 (:percent-deteriorated zombie))))

(shuffle-speed @fred)

(defn shuffle-alert
  [key watched old-state new-state]
  (let [sph (shuffle-speed new-state)]
    (if (> sph 5000)
      (do
        (println "Run, you fool!")
        (println "The zombie's SPH is now" sph)
        (println "This message brought to your courtesy of" key))
      (do
        (println "All's well with" key)
        (println "Cuddle hunger:" (:cuddle-hunger-level new-state))
        (println "Percent deteriorated:" (:percent-deteriorated new-state))
        (println "SPH:" sph)))))

(reset! fred {:cuddle-hunger-level 22
              :percent-deteriorated 2})
(add-watch fred :fred-shuffle-alert shuffle-alert)
(swap! fred update-in [:percent-deteriorated] + 1)
(swap! fred update-in [:cuddle-hunger-level] + 30)

(defn percent-deteriorated-validator
  [{:keys [percent-deteriorated]}]
  (or (and (>= percent-deteriorated 0)
           (<= percent-deteriorated 100))
      (throw (IllegalStateException. "That's not mathy!"))))

(def bobby
  (atom
   {:cuddle-hunger-level 0 :percent-deteriorated 0}
   :validator percent-deteriorated-validator))

(swap! bobby update-in [:percent-deteriorated] + 200) ; => IllegalStateException

(take 5 (repeatedly (partial rand-int 10)))
(def letters (mapv (comp str char (partial + 65)) (range 26)))
(defn random-string
  [length]
  (apply str (take length (repeatedly #(rand-nth letters)))))
(defn random-string-list
  [list-length string-length]
  (doall (take list-length (repeatedly (partial random-string string-length)))))

(def orc-names (random-string-list 3000 7000))
(time (dorun (map clojure.string/lower-case orc-names))) ; => 34ms
(time (dorun (pmap clojure.string/lower-case orc-names))) ; => 18ms

(def orc-names (random-string-list 20000 300))
(time (dorun (map clojure.string/lower-case orc-names))) ; => 9ms
(time (dorun (pmap clojure.string/lower-case orc-names))) ; => 43ms
;; Parallelization comes with its own overheads!

(time
 (dorun
  (apply concat
         (pmap (fn [name] (doall (map clojure.string/lower-case name)))
               (partition-all 1000 orc-names)))))
;; increase grain-size: 6ms

;; ----------------------------------------------------------------------------

(defn divisors
  [n]
  (conj (vec (filter #(zero? (mod n %))
                     (range 1 (inc (quot n 2)))))
        n))

(def m-divisors (memoize divisors))

(defn sigma
  [n]
  (reduce + (map #(reduce + (m-divisors %))
                 (range 1 (inc n)))))

(time (sigma 12))
(time (sigma (Math/pow 10 5))) ; => 40209 msecs

;; ----------------------------------------------------------------------------

(defn sigma
  ([n] (sigma n (quot n 2) n))
  ([n c a]
   (if (zero? c)
     a
     (recur n (dec c) (if (zero? (mod n c))
                        (+ a c)
                        a)))))

(time (sigma (Math/pow 10 5))) ; => 5.89 msecs
(time (reduce + (map #(sigma %) (range 1 (Math/pow 10 5))))) ; => 43876 msecs

(def m-sigma (memoize sigma))

(time (m-sigma 12))
(time (m-sigma (Math/pow 10 5))) ; => 15 msec

(time (reduce + (map #(m-sigma %) (range 1 (Math/pow 10 5))))) ; => (first time computing result) 33535 msecs / 44069 msecs
(time (reduce + (map #(m-sigma %) (range 1 (Math/pow 10 5))))) ; => (reuse last result) 30.99 msecs

;; ----------------------------------------------------------------------------

(defn make-sigma
  [n]
  (let [sigma-impl (fn [sigma-m c a]
                     (let [sigma (fn [c a] (sigma-m sigma-m c a))]
                       (if (zero? c)
                         a
                         (sigma (dec c) (if (zero? (mod n c))
                                          (+ a c)
                                          a)))))
        sigma-m (memoize sigma-impl)]
    (partial sigma-m sigma-m)))

((make-sigma 12) 12 0)

;; ----------------------------------------------------------------------------

(defn make-sigma
  [n]
  (let [sigma-impl (fn [sigma-m c a]
                     (let [sigma (fn [c a] (sigma-m sigma-m c a))]
                       (if (zero? c)
                         a
                         (sigma (dec c) (if (zero? (mod n c))
                                          (+ a c)
                                          a)))))
        sigma-m (memoize sigma-impl)]
    (partial sigma-m sigma-m (quot n 2) n)))

((make-sigma 12))

(time (reduce + (map #(sigma %) (range 1 (Math/pow 10 3))))) ; => 21.78 msecs
(time (reduce + (map #((make-sigma %)) (range 1 (Math/pow 10 3))))) ; => 140.1 msecs

(time ((make-sigma (Math/pow 10 5)))) ; => Execution error (StackOverflowError)

;; ----------------------------------------------------------------------------

(defn make-sigma
  []
  (let [sigma-impl (fn [sigma-m n c a]
                     (let [sigma (fn [c a] (sigma-m sigma-m n c a))]
                       (if (zero? c)
                         a
                         (sigma (dec c) (if (zero? (mod n c))
                                          (+ a c)
                                          a)))))
        sigma-m (memoize sigma-impl)]
    (partial sigma-m sigma-m)))

(def m-sigma (make-sigma))
(m-sigma 12 12 0) ; => 28

(time (reduce + (map #(sigma %) (range 1 (Math/pow 10 3))))) ; => 9 msecs
(time (reduce + (map #(sigma %) (range 1 (Math/pow 10 4))))) ; => 482.4 msecs
(time (reduce + (map #(sigma %) (range 1 (Math/pow 10 5))))) ; => 8224494757 in 43962 msecs
(time (reduce + (map #(m-sigma % % 0) (range 1 (Math/pow 10 3))))) ; => 449.7 msecs
; so this memo thingy actually makes things even slower ... ???

;; ----------------------------------------------------------------------------

; multithreaded solution in Go
; // multithreading: res: 8224494757, took: 597.418562ms
; func sigma3(n int, answer chan<- int) {
;     a := n
;     top := n/2 + 1
;     for i := 1; i < top; i++ {
;         if n%i == 0 {
;             a += i
;         }
;     }
;     answer <- a
; }
; 
; func main() {
;     start := time.Now()
; 
;     n := int(math.Pow10(5))
;     a := 0
;     ch := make(chan int)
; 
;     for i := 1; i < n; i++ {
;         go sigma3(i, ch)
;     }
;     for i := 1; i < n; i++ {
;         a += <-ch
;     }
; 
;     end := time.Now()
;     took := end.Sub(start)
; 
;     fmt.Printf("res: %d, took: %s", a, took)
; }

; multithreaded solution in Julia
; function sigma(n)
;     a = n
;     top = n / 2
;     for i in 1:n-1
;         if n % i == 0
;             a += i
;         end
;     end
;     a
; end
; 
; function main()
;     a = zeros(10^5-1)
;     Threads.@threads for i in 1:10^5-1
;         a[i] += sigma(i)
;     end
;     convert(Int, reduce(+, a))
; end
; 
; # julia -t 16 sigma.jl
; # 8224494757
; # 1.365092 seconds (21.87 k allocations: 2.318 MiB, 87.69% compilation time)
; # --------------------------------------------------------------------------
; # julia -t (nproc) sigma.jl
; #   ..where nproc = 32
; # 8224494757
; # 1.451240 seconds (21.87 k allocations: 2.319 MiB, 56.54% compilation time)
; @time println(main())

(defn sigma
  ([n] (sigma n (quot n 2) n))
  ([n c a]
   (if (zero? c)
     a
     (recur n (dec c) (if (zero? (mod n c))
                        (+ a c)
                        a)))))

; using parallel map
(time (reduce + (pmap #(sigma %) (range 1 (Math/pow 10 5))))) ; => 8224494757 in 9594 msecs

(time (reduce + (flatten (pmap (fn [s] (map #(sigma %) s)) (partition-all 100 (range 1 (Math/pow 10 5))))))) ; 12382 msecs
(time (reduce + (flatten (pmap (fn [s] (map #(sigma %) s)) (partition-all 1000 (range 1 (Math/pow 10 5))))))) ; 12453 msecs


(time (reduce + (pmap (fn [s] (reduce + (map #(sigma %) s)))
                      (partition-all 100 (range 1 (Math/pow 10 5)))))) ; 9414 msecs
(time (reduce + (pmap (fn [s] (reduce + (map #(sigma %) s)))
                      (partition-all 1000 (range 1 (Math/pow 10 5)))))) ; 9446 msecs

(defn sum-sigma
  [n]
  (let [r (atom 0)
        fs (for [i (range 1 n)]
             (future (swap! r + (sigma i))))]
    (doseq [f fs] @f)
    @r))

(time (sum-sigma (Math/pow 10 5))) ; => 8224494757 in 9731 msecs

;; ----------------------------------------------------------------------------

(def v (atom 0))
(reset! v 0)
(dotimes [_ 5] (swap! v + 1))
@v

(defn get-quote
  "Parsing JSON the worst way imaginable..."
  []
  (second (re-find #"\"content\":\"([A-Za-z .,-_:]+)\"" (slurp "https://api.quotable.io/quotes/random"))))

(defn quotes
  [n]
  (let [word-count (atom {})
        futures (repeatedly n (fn [] (future (let [q (get-quote)]
                                               (swap! word-count #(assoc % q (count q)))))))]
    (doseq [f futures] @f)
    @word-count))

(quotes 5)

;; ----------------------------------------------------------------------------

; The Go coroutine/channel solution in Clojure:
; Note that we have to create a (highly) buffered channel or we'll get an
; exception!
; 9522 msecs
(time (let [ch (chan (Math/pow 10 5))
            n (Math/pow 10 5)]
       (doseq [i (range 1 n)] (go (>! ch (sigma i))))
       (reduce (fn [a _] (+ a (<!! ch))) 0 (range 1 n))))

;; ----------------------------------------------------------------------------

(.toUpperCase "Hello, World!")
(macroexpand '(.toUpperCase "Hello, World!")) ; => (. "Hello, World!" toUpperCase)
; . dot operator is a special form!

java.lang.Math/PI

(new String)
(String.)
(String. "To Davey Jones's Locker with ye hardies")

(let [stack (java.util.Stack.)]
  (.push stack "Latest episode of Game of Thrones, ho!")
  (first stack))

(doto (java.util.Stack.)
  (.push "Latest episode of Game of Thrones, ho!")
  (.push "Whoops, I meant 'Land, ho!'"))

(System/getenv)

; date literal
#inst "2016-09-19"
