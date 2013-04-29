(ns image.core
  (:require clojure.java.io)
  (:import (javax.imageio ImageIO)
           (java.awt.image BufferedImage)) )



;(with-open [input (new java.io.FileInputStream "../../photo.png")
;;            output (new java.io.ByteArrayOutputStream)]
;;(io/copy input output)
;;(.toByteArray output))

;(defn getPixels [^BufferedImage image] (-> image .getRaster .getDataBuffer .getData))


(BufferedImage/toString (ImageIO/read (java.io.File. "../../photo.png")))


;; matrix functions
(defn transpose [M colnum]
  (partition colnum (apply interleave M )))


(defn conv-loop [filter v-window]
  (apply map + (map #(map (comp (partial apply +) (partial map * (nth filter %)))
                          (nth v-window %))
                    (range 3))))

(defn conv [im filter]; filter: ((1 2 1) (0 0 0) (-1 -2 -1))
  (let [m (count filter)
        n (count (first filter))
        parted (partition m 1 (map (partial partition n 1) im))]
    (map (partial conv-loop filter) parted)))


(defn h-conv [im hfilter]; filter: (1 2 1)
  (let [hsize (count hfilter)]
    (map #(map (comp (partial apply +) (partial map * hfilter))
               (partition hsize 1 %)) im)))

(defn v-conv [im vfilter]; filter: (1 0 -1)
  (transpose (h-conv (transpose im (count im)) vfilter) (count (first im))))

(defn sep-conv [im hfilter vfilter] ; slower than conv
  (v-conv (h-conv im hfilter) vfilter))





(defn rand-vec [n range]
  (take n (repeatedly #(rand-int range))))

(def im (take 4 (repeatedly #(rand-vec 5 256))))
(def parts (partition 3 1 im))

(conv im '((1 2 1) (0 0 0) (-1 -2 -1)))

(-> im)
(v-conv im [1 2 1])




















(defn rand-mat [range dims]
  (loop [i 0 f #(rand-int range)]
    (if (< i (count dims))
      (recur (inc i) #(take (nth dims i) (repeatedly f)))
      '(f)
      )));; not working
