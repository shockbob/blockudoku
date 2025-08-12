(ns blockudoku.core-test
  (:require [clojure.test :refer :all]
            [blockudoku.core :refer :all]))


(def test-grid-add-piece [[EMPTY EMPTY EMPTY][EMPTY EMPTY EMPTY][EMPTY EMPTY EMPTY][EMPTY EMPTY EMPTY]])
(def new-grid [[EMPTY FULL FULL][EMPTY EMPTY EMPTY][EMPTY EMPTY EMPTY][EMPTY EMPTY EMPTY]])
(deftest add-piece-test
  (testing "add-piece ok"
    (is (= new-grid (add-piece {:coords [[0 1][0 2]]} test-grid-add-piece [0 0] FULL)))))
(deftest all-match-test
   (testing "all-match-ok"
      (is (= false (all-match [[0 1][0 0][0 2]] new-grid FULL)))
      (is (= true (all-match [[0 1][0 2]] test-grid-add-piece EMPTY)))))
(deftest groups-test
  (testing "all-groups ok"
    (is (= 27 (count all-groups)))))

(deftest full-is-full
  (testing "full works"
     (is (= true (full [[0 0][1 1][2 2]] full-grid)))))

(deftest full-grid-is-full
   (testing "full-grid is full"
       (is (= #{FULL} (set (flatten full-grid))))))

(deftest find-starts-works
   (testing "find-starts works"
      (is (= (map (fn [pc] (count (find-starts empty-grid pc))) pieces)
[81 72 72 63 63 54 54 45 45 64 64 49 49 49 64 56 56 56 56 64 64 64 64 49 49 49 49 56 56 56 56]))
      (is (= (map (fn [pc] (count (find-starts full-grid pc))) pieces) (repeat (count pieces) 0)))
       (is (= [] (find-starts [] sing)))
       (is (= [[0 0]] (find-starts [[EMPTY]] sing)))
       (is (= [[0 0][0 1]] (find-starts [[EMPTY EMPTY]] sing)))
       (is (=  81 (count (find-starts empty-grid sing))))
       (is (= [] (find-starts [[FULL]] sing)))
          ))

(def test-grid-fix-grid 
               [[FULL FULL FULL FULL FULL FULL FULL FULL FULL]         
                [FULL FULL FULL  EMPTY  EMPTY  EMPTY  EMPTY  EMPTY  FULL]  
                [FULL FULL FULL  EMPTY  EMPTY  EMPTY  EMPTY  EMPTY  FULL]  
                [EMPTY EMPTY EMPTY  FULL  FULL  FULL  EMPTY  EMPTY  FULL]  
                [EMPTY EMPTY EMPTY  FULL  FULL  FULL  EMPTY  EMPTY  FULL]  
                [EMPTY EMPTY EMPTY  FULL  FULL  FULL  EMPTY  EMPTY  FULL]  
                [EMPTY EMPTY EMPTY  EMPTY  EMPTY  EMPTY  EMPTY  EMPTY  FULL]  
                [EMPTY EMPTY EMPTY  EMPTY  EMPTY  EMPTY  EMPTY  EMPTY  FULL]  
                [EMPTY EMPTY EMPTY  EMPTY  EMPTY  EMPTY  EMPTY  EMPTY  FULL] ]) 

(def test-grid-fix-grid-2
               [[FULL FULL EMPTY FULL FULL FULL FULL FULL FULL]         
                [FULL EMPTY FULL  EMPTY  EMPTY  EMPTY  EMPTY  EMPTY  FULL]  
                [FULL FULL FULL  EMPTY  EMPTY  EMPTY  EMPTY  EMPTY  FULL]  
                [EMPTY EMPTY EMPTY  FULL  FULL  FULL  EMPTY  EMPTY  FULL]  
                [EMPTY EMPTY EMPTY  FULL  EMPTY  FULL  EMPTY  EMPTY  FULL]  
                [EMPTY EMPTY EMPTY  FULL  FULL  FULL  EMPTY  EMPTY  EMPTY]  
                [EMPTY EMPTY EMPTY  EMPTY  EMPTY  EMPTY  EMPTY  EMPTY  FULL]  
                [EMPTY EMPTY EMPTY  EMPTY  EMPTY  EMPTY  EMPTY  EMPTY  FULL]  
                [EMPTY EMPTY EMPTY  EMPTY  EMPTY  EMPTY  EMPTY  EMPTY  FULL] ]) 

(deftest fix-grid-test
   (testing "fix-grid works"
     (is (= empty-grid (fix-grid test-grid-fix-grid)))
     (is (= empty-grid (fix-grid full-grid)))
     (is (= test-grid-fix-grid-2 (fix-grid test-grid-fix-grid-2)))
     ))

