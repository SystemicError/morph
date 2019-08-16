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
    ))