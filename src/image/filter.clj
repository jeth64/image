(ns image.filter
  (:require [clojure.math.numeric-tower :as math]))

;; edge detection filter

(def sobel-y '((1 2 1) (0 0 0) (-1 -2 -1)))
(def sobel-x '((1 0 -1) (2 0 -2) (1 0 -1)))

(def prewitt-y '((-1 -1 -1) (0 0 0) (1 1 1)))
(def prewitt-x '((-1 0 1) (-1 0 1) (-1 0 1)))

(def roberts-y '((0 -1) (1 0)))
(def roberts-x '((-1 0) (0 1)))

(def scharr-y '((3 10 3) (0 0 0) (-3 -10 -3)))
(def scharr-x '((3 0 -3) (10 0 -10) (3 0 -3)))

; separated

(def sobel1 '(1 0 -1))
(def sobel2 '(1 2 1))

(def prewitt1 '(-1 0 1))
(def prewitt2 '(1 1 1))

(def scharr1 '(1 0 -1))
(def scharr2 '(3 10 3))

;; low-pass filter (blurring)

(defn gauss
  "Computes the normal distribution"
  [sigma x]
  (/ 1 (* (math/sqrt (* 2 Math/PI))
          sigma
          (math/expt Math/E (/ (math/expt (/ x sigma) 2) 2)))))

(defn gaussian
  "Returns 1D gaussian filter for separated convolution"
  ([n] (gaussian 1 n))
  ([sigma n] (let [k (quot n 2)
                   x (range (inc k))
                   g (map (partial gauss sigma) x)
                   h (map #(math/round (/ % (last g))) g)]
               (concat (reverse (rest h)) h))))

(defn binomialflt [n]
  (gaussian n))


(defn blockflt [n]
  (take n (repeat (take n (repeat 1)))))


; separated low-pass filter

(defn blockflt-sep [n]
  (take n (repeat 1)))

(def binomialflt-sep [n]
  (gaussian-sep n))


; necessary follow-up

(defn normfac
  "Returns the factor the pixel has to be multiplied with
   after applying a blurring-filter"
  [filter] (/ 1 (apply + (flatten filter))))


;; rank order function

(defn median [& x]
  (nth (sort x) (quot (count x) 2)))



;; convolution

; simple

(defn conv-loop [filter v-window]
  (apply map + (map #(map (comp (partial apply +) (partial map * (nth filter %)))
                          (nth v-window %))
                    (range 3))))

(defn conv [im filter]
  (let [m (count filter)
        n (count (first filter))
        parted (partition m 1 (map (partial partition n 1) im))]
    (map (partial conv-loop filter) parted)))

; separated

(defn h-conv [im hfilter]
  (let [hsize (count hfilter)]
    (map #(map (comp (partial apply +) (partial map * hfilter))
               (partition hsize 1 %)) im)))

(defn v-conv [im vfilter]
  (map (comp (partial apply map +)
             (partial map #(map (partial * %1) %2) vfilter))
       (partition (count vfilter) 1 im)))

(defn conv-sep [im hfilter vfilter] ; slower than conv
  (h-conv (v-conv im hfilter) vfilter))

; order

(defn h-conv-fnc [im f size]
    (map #(map f (partition size 1 %)) im))

(defn v-conv-fnc [im f size]
  (map (partial apply map f)
       (partition size 1 im)))


(defn conv-fnc
  "Convolution with a function for rank order filtering
   (conv-fnc I median 3) -> median filtering with size 3
      (conv-fnc I max 3) -> maximum filtering"
  [im f size]
  (h-conv-fnc (v-conv-fnc f size) f size))

;; temporal test funtions

(defn rand-vec [n range]
  (take n (repeatedly #(rand-int range))))

(def mat (take 500 (repeatedly #(rand-vec 500 10))))
