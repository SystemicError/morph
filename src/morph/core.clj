(ns morph.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [uncomplicate.neanderthal.linalg :refer :all]
            [uncomplicate.neanderthal.native :refer :all]
            [uncomplicate.neanderthal.core :refer :all])
  (:gen-class))

; We find an affine transform for each triangular region in both before and after(let corners of image be anchors to ensure every point in a region).
; The affine transforms are functions of t.
; Call the transform based on before picture Tb and the one based on after picture Ta.
; For every point p in image at time t, use color(invTb(t)(p)) + color(invTa(t)(p)) for that point's color.

(defn interpolate-anchors [before after t]
  "Interpolates a set of anchors parameterized with t=0 as before and t=1 as after."
  (for [i (range (count before))]
    (let [p0 (nth before i)
          p1 (nth after i)]
      (map #(+ (* (- 1.0 t) %1) (* t %2)) p0 p1))))

(defn affine-transform [before after]
  "Find the affine transform mapping three points to their images."
  (let [b0 (first before)
        b1 (nth before 1)
        b2 (nth before 2)
        b0x (first b0)
        b0y (nth b0 1)
        b1x (first b1)
        b1y (nth b1 1)
        b2x (first b2)
        b2y (nth b2 1)
        M (dge 6 6 [b0x 0 b1x 0 b2x 0
                    b0y 0 b1y 0 b2y 0
                    0 b0x 0 b1x 0 b2x
                    0 b0y 0 b1y 0 b2y
                    1 0 1 0 1 0
                    0 1 0 1 0 1])
        a0 (first after)
        a1 (nth after 1)
        a2 (nth after 2)
        a0x (first a0)
        a0y (nth a0 1)
        a1x (first a1)
        a1y (nth a1 1)
        a2x (first a2)
        a2y (nth a2 1)
        coeffs (sv M (dge 6 1 [a0x a0y a1x a1y a2x a2y]))
        ;dummy (println (str "before" before "\nb0" b0 "\nb1" b1 "\nb2" b2 "\nb0x" b0x "\nb0y" b0y "\nb1x" b1x "\nb1y" b1y "\nb2x" b2x "\nb2y" b2y
        ;                    "\nafter" after "\na0" a0 "\na1" a1 "\na2" a2 "\na0x" a0x "\na0y" a0y "\na1x" a1x "\na1y" a1y "\na2x" a2x "\na2y" a2y
        ;                    "\nM" (into [] (for [r (range 6)] (for [c (range 6)] (entry M r c))))))
        ]
    (fn [p]
      [(+ (* (entry coeffs 0 0) (first p)) (* (entry coeffs 1 0) (nth p 1)) (entry coeffs 4 0))
       (+ (* (entry coeffs 2 0) (first p)) (* (entry coeffs 3 0) (nth p 1)) (entry coeffs 5 0))])))

(defn point-in-triangle? [point triangle]
  "Returns true if point is inside triangle."
  (let [aff (affine-transform triangle [[0.0 0.0] [1.0 0.0] [0.0 1.0]])
        tr-point (aff point)
        x (first tr-point)
        y (nth tr-point 1)]
    (and (<= 0.0 x) (<= 0.0 y) (<= y (- 1.0 x)))))

(defn distance-from-triangle [point triangle]
  "Returns the sum of the distances between a point and each vertex."
  (let [dist-fn (fn [u v] (reduce + (map #(* % %) (map - u v))))]
    (reduce + (map (partial dist-fn point) triangle))))

; For interpolating the entire set of triangles partitioning the image
(defn interpolate-partition [before after t]
  "Interpolates all triangles in a partition of the image."
  (for [i (range (count before))]
    (interpolate-anchors (nth before i)
                         (nth after i)
                         t)))

(defn bounding-triangle-index [point triangles]
  (let[filtered (filter #(point-in-triangle? point (nth triangles %)) (range (count triangles)))
       distances (map (partial distance-from-triangle point) triangles)
       min-dist (reduce min distances)
       mindex (first (filter #(= min-dist (nth distances %)) (range (count distances))))
       dummy (if (empty? filtered)
               (println (str "Failed to find bounding triangle, using best guess of nearest triangle:"
                             "\ntriangles = " (into [] triangles)
                             "\npoint = " point
                             ;"\nfiltered = " (into [] filtered)
                             ;"\ndistances = " (into [] distances)
                             "\nmin-dist = " min-dist
                             "\nmindex = " mindex)))
       ]
    (if (not (empty? filtered))
      (first filtered)
      ; pick the one "nearest" to us
      mindex)))


(defn find-interpolation-points [before-partition after-partition t point]
  "Returns a key of the before point and the after point corresponding to this point."
  (let [triangles (interpolate-partition before-partition after-partition t)
        index (bounding-triangle-index point triangles)
        before-aff (affine-transform (nth triangles index) (nth before-partition index))
        after-aff (affine-transform (nth triangles index) (nth after-partition index))]
    {:before (before-aff point)
     :after (after-aff point)}))

(defn interpolate-images [before-image after-image before-partition after-partition t]
  "Returns an image interpolated from the given images using the given partitions at the given t."
  (let [inter-image (q/create-image (q/width) (q/height) :rgb)]
    ))

(defn setup []
  (q/frame-rate 30) ; haha
  {:t 0
   :before-image (q/load-image "before.jpg")
   :after-image (q/load-image "after.jpg")
   :before-partition (eval (read-string (slurp "before.partition.edn")))
   :after-partition  (eval (read-string (slurp "after.partition.edn")))})

(defn update-state [state]
  (assoc state
         :t (+ (:t state) (Math/pow 2 -4))))

(defn draw-state [state]
  (let [before-partition (:before-partition state)
        after-partition (:after-partition state)
        before-image (:before-image state)
        after-image (:after-image state)
        t (:t state)
        s (- 1.0 t)
        dummy (println (str "\nt = " t))]
    (dotimes [x (q/width)]
      (dotimes [y (q/height)]
        (let [pts (find-interpolation-points before-partition after-partition t [x y])
              before-pt (:before pts)
              before-x (first before-pt)
              before-y (nth before-pt 1)
              after-pt (:after pts)
              after-x (first after-pt)
              after-y (nth after-pt 1)
              before-pixel (q/get-pixel before-image before-x before-y)
              after-pixel (q/get-pixel after-image after-x after-y)
              before-r (q/red before-pixel)
              before-g (q/green before-pixel)
              before-b (q/blue before-pixel)
              after-r (q/red after-pixel)
              after-g (q/green after-pixel)
              after-b (q/blue after-pixel)
              pixel (q/color (+ (* s before-r) (* t after-r))
                             (+ (* s before-g) (* t after-g))
                             (+ (* s before-b) (* t after-b)))
              ]
          (q/set-pixel x y pixel))))
    (q/save-frame "morph-####.jpg")
  ))

(q/defsketch morph
  :title "Morph"
  :size [300 259]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
