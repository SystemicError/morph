(ns morph.core-test
  (:require [clojure.test :refer :all]
            [morph.core :refer :all]))

(deftest interpolate-anchors-test
  (testing "Interpolate anchors fail."
    (is (= (interpolate-anchors [[0 0]] [[1 1]] 0.5) [[0.5 0.5]]))
    (is (= (interpolate-anchors [[0 0] [1 3]] [[1 1] [2 3]] 0.5) [[0.5 0.5] [1.5 3.0]]))
    ))

(deftest affine-transform-test
  (testing "affine transform fail."
    (is (= ((affine-transform [[0.0 0.0] [1.0 0.0] [0.0 1.0]] [[0.0 0.0] [1.0 0.0] [0.0 1.0]]) [3.0 3.0]) [3.0 3.0]))
    (is (= ((affine-transform [[0.0 0.0] [1.0 0.0] [0.0 1.0]] [[1.0 1.0] [2.0 1.0] [1.0 2.0]]) [3.0 3.0]) [4.0 4.0]))
    (is (= ((affine-transform [[0.0 0.0] [1.0 0.0] [0.0 1.0]] [[0.0 0.0] [2.0 0.0] [0.0 1.0]]) [3.0 3.0]) [6.0 3.0]))
    (is (= ((affine-transform [[0.0 0.0] [1.0 0.0] [0.0 1.0]] [[0.0 0.0] [1.0 0.0] [0.0 2.0]]) [3.0 3.0]) [3.0 6.0]))
    (is (= ((affine-transform [[0.0 0.0] [1.0 0.0] [0.0 1.0]] [[0.0 0.0] [0.0 -1.0] [1.0 0.0]]) [3.0 3.0]) [3.0 -3.0]))
    ))

(deftest point-in-triangle?-test
  (testing "point-in-triangle? fail."
    (is (= false (point-in-triangle? [-1.0 0.0] [[1.0 1.0] [5.0 0.0] [0.0 5.0]])))
    (is (= true (point-in-triangle? [1.1 1.1] [[1.0 1.0] [5.0 0.0] [0.0 5.0]])))
    (is (= true (point-in-triangle? [1.0 1.0] [[1.0 1.0] [5.0 0.0] [0.0 5.0]])))
    ;(is (= true (point-in-triangle? [0.0 2.0]  [[29.0 48.0] [0.0 0.0] [0.0 67.0]])))
    ))

(deftest distance-from-triangle-test
  (testing "distance-from-triangle fail."
    (is (= 30.0 (distance-from-triangle [-1.0 0.0] [[0.0 0.0] [1.0 0.0] [2.0 4.0]])))
    ))

(deftest interpolate-partition-test
  (testing "Interpolate partition fail."
    (is (= (interpolate-partition [[[0.0 0.0] [1.0 0.0] [0.0 1.0]]
                                   [[1.0 0.0] [1.0 1.0] [0.0 1.0]]]
                                  [[[0.0 0.0] [2.0 0.0] [0.0 2.0]]
                                   [[2.0 0.0] [2.0 2.0] [0.0 2.0]]]
                                  0.5)
           [[[0.0 0.0] [1.5 0.0] [0.0 1.5]]
            [[1.5 0.0] [1.5 1.5] [0.0 1.5]]]))
    ))

(deftest bounding-triangle-index-test
  (testing "Bounding triangle index fail."
    (is (= (bounding-triangle-index [0.5 0.5]
                                   [[[0.0 0.0] [1.0 0.0] [0.0 1.0]]
                                   [[1.0 0.0] [1.0 1.0] [0.0 1.0]]])
           0))

    ))

(deftest find-interpolation-points-test
  (testing "Find bounding triangle fail."
    (is (= (find-interpolation-points [[[0.0 0.0] [1.0 0.0] [0.0 1.0]]
                                       [[1.0 0.0] [1.0 1.0] [0.0 1.0]]]
                                      [[[0.0 0.0] [4.0 0.0] [0.0 4.0]]
                                       [[4.0 0.0] [4.0 4.0] [0.0 4.0]]]
                                      0.5
                                      [0.5 0.5])
           {:before [0.2 0.2]
            :after [0.8 0.8]}))
    ))
