(ns playsync.core
  (:require
   [clojure.core.async
    :as a
    :refer [<! <!! >! >!! alts!! chan close! go thread timeout]]))

; parking put / take: >! / <!
; blocking put / take: >!! / <!!
; parking only possible from within a coroutine (go ...)

(def echo-chan (chan))
(go (println (<! echo-chan)))
(>!! echo-chan "ketchup")

(def echo-buffer (chan (a/sliding-buffer 2)))
(>!! echo-buffer "ketchup 1")
(>!! echo-buffer "ketchup 2")
(>!! echo-buffer "ketchup 3")
(<!! echo-buffer) ; => "ketchup 2"
(<!! echo-buffer) ; => "ketchup 3"

(def echo-buffer (chan (a/dropping-buffer 2)))
(>!! echo-buffer "ketchup 1")
(>!! echo-buffer "ketchup 2")
(>!! echo-buffer "ketchup 3")
(<!! echo-buffer) ; => "ketchup 1"
(<!! echo-buffer) ; => "ketchup 2"

; similar to future, but returns a channel, return value of thread's process is put on the channel
; thread creates a new thread, so use it for long running operations instead of clogging the coroutine thread pool by using (go ...)
(let [t (thread "chili")]
  (<!! t))

(defn hot-dog-machine
  []
  (let [in (chan)
        out (chan)]
    (go (<! in)
        (>! out "hot dog"))
    [in out]))

(let [[in out] (hot-dog-machine)]
  (>!! in "pocket lint")
  (<!! out))

(defn hot-dog-machine-v2
  [hot-dog-count]
  (let [in (chan)
        out (chan)]
    (go (loop [hc hot-dog-count]
          (if (> hc 0)
            (let [input (<! in)]
              (if (= 3 input)
                (do (>! out "hot dog")
                    (recur (dec hc)))
                (do (>! out "wilted lettuce")
                    (recur hc))))
            (do (close! in)
                (close! out)))))
    [in out]))

(let [[in out] (hot-dog-machine-v2 2)]
  (>!! in 3)
  (println (<!! out))
  (>!! in "pocket lint")
  (println (<!! out))
  (>!! in 3)
  (println (<!! out))
  (>!! in 3)
  (println (<!! out)))

(let [c1 (chan)
      c2 (chan)
      c3 (chan)]
  (go (>! c2 (clojure.string/upper-case (<! c1))))
  (go (>! c3 (clojure.string/reverse (<! c2))))
  (go (println (<! c3)))
  (>!! c1 "redrum"))

; alts!! like Go's select

(defn upload
  [headshot c]
  (go (<! (timeout (rand 100)))
      (>! c headshot)))

(let [c1 (chan)
      c2 (chan)
      c3 (chan)]
  (upload "serious.jpg" c1)
  (upload "fun.jpg" c2)
  (upload "sassy.jpg" c3)
  (let [[headshot channel] (alts!! [c1 c2 c3])]
    (println "Sending headshot notification for" headshot)))

(let [c1 (chan)]
  (upload "serious.jpg" c1)
  (let [[headshot channel] (alts!! [c1 (timeout 20)])]
    (if headshot
      (println "Sending headshot notification for" headshot)
      (println "Timed out!"))))

; try to do a take on c1 and try to do a put with the value "put!" on c2
(let [c1 (chan)
      c2 (chan)]
  (go (<! c2))
  (let [[value channel] (alts!! [c1 [c2 "puts!"]])]
    (println value)
    (= channel c2)))

; -----------------------------------------------------------------------------

(defn sigma
  ([ch n] (sigma ch n (quot n 2) n))
  ([ch n c a]
   (if (zero? c)
     (>!! ch a)
     (recur ch n (dec c) (if (zero? (mod n c))
                           (+ a c)
                           a)))))

(time (reduce + (pmap #(sigma %) (range 1 (Math/pow 10 5))))) ; => 8224494757 in 9594 msecs
