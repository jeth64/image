(ns image.core
  (:use image.filter)
  (:require clojure.java.io))

(defn im2mat
  "Takes a Java BufferedImage and returns HxWx3 matrix of RGB values"
  [image]
  (let [w (.getWidth image)
        h (.getHeight image)]
    (partition w (for [x (range w)
                       y (range h)
                       :let [color (java.awt.Color. (.getRGB image x y))]]
                   [(.getRed color) (.getGreen color) (.getBlue color)]))))

(defn imread [filename]
  (im2mat (javax.imageio.ImageIO/read
           (clojure.java.io/input-stream filename))))

(defn view [image]
  (doto (javax.swing.JFrame. "Image")
    (.add (proxy [javax.swing.JPanel] []
            (paintComponent [g]
                            (proxy-super paintComponent g)
                            (.drawImage g image 0 0 this))))
    (.setSize (.getWidth image) (.getHeight image))
    (.setResizable false)
    (.setVisible true)))

;; colorspace transformations

(defn rgb2grey [image]
  (map (partial map #(quot (apply + %) (count %))) image))

(defn normalize [image maximum]
  (map (partial map (partial map #(/ %  maximum))) image))

(defn rgb2hsv-pixel [pixel]
  (let [max (apply max pixel)
        min (apply min pixel)
        range (- max min)
        argmax (.indexOf pixel max)
        [r g b] pixel
        h (if (= r g b) 0
            (case argmax
              0 (* 60 (/ (- g b) range))
              1 (* 60 (+ 2 (/ (- b r) range)))
              2 (* 60 (+ 4 (/ (- r g) range)))))
        s (if (= max 0) 0 (/ range max))]
    [h s max]))

(defn rgb2hsv [image]
  (map (partial map rgb2hsv-pixel) image))

(defn hsv2grey [image]
  (map (partial map last) image))


;; test image
(def im1 (imread "/home/jasem/image/img/lena.jpg"))
(def im2 (imread "/home/jasem/image/img/photo.jpg"))
