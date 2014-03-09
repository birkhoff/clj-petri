(ns petri.core-test
  (:require [clojure.test :refer :all]
            [petri.petri_net_state :refer :all])
  (:use clojure.test)
  (:use petri.petri_net_state))



;(use '[clojure.walk :only (prewalk-replace)])
(use '[petri.petri_net_state])
;(use '[petri.simulator])


(deftest add_net_test
  (testing "adding a petri net to the state"
                   (do
                     (add_petri (petri "Petri_A"))
                     (is (not (nil? ((keyword "Petri_A") @state)))))))


(deftest delete_net_test
  (testing "deleting a petri net to the state"
                   (do
                     (add_petri (petri "Petri_A"))
                     (add_petri (petri "Petri_C"))
                     (delete_petri "Petri_C")

                     (is ((keyword "Petri_A") @state))
                     (is (nil? ((keyword "Petri_C") @state))))))


(deftest add_vertices
  (testing "adding vertices to a net in the current state"
       (do
          	(state_add_vertex "Petri_A" "v-a" 1)
            (state_add_vertex "Petri_A" "v-b" 6)
            (state_add_vertex "Petri_A" "v-a" 9)
            (let [v (vals (:vertices (:Petri_A @state)))]
                 (is (and (some #{["v-a" 9]}  v) (some #{["v-b" 6]}  v)))))))



(deftest add_transitions
  (testing "adding transitions to a net in the current state"
      (do
        (state_add_transition "Petri_A" "z")
        (state_add_transition "Petri_A" "y")
        (let [v (vals (:transitions (:Petri_A @state)))]
          (is (and (some #{["z"]}  v) (some #{["y"]}  v)))))))


(deftest testing_hashing
  (testing "testing hashing"
    (do
        (add_petri (petri "Petri_A"))
        (state_add_vertex "Petri_A" "v-b" 6)
        (state_add_vertex "Petri_A" "v-a" 9)
        (is (keyword? (get_vertex_hash ((keyword "Petri_A") @state) "v-a")))
        (is  (not= (get_vertex_hash ((keyword "Petri_A") @state) "v-a")
                   (get_vertex_hash ((keyword "Petri_A") @state) "v-b")) ))))



(deftest testing_edges
  (testing "Testing addition of edges"
    (do

        (add_petri (petri "Petri_A"))

        (state_add_vertex "Petri_A" "v-b" 6)
        (state_add_vertex "Petri_A" "v-a" 9)
        (state_add_transition "Petri_A" "z")
        (state_add_transition "Petri_A" "y")

        (state_add_edges_in "Petri_A" "v-a" "z" 9)
        (state_add_edges_in "Petri_A" "v-a" "z" 8)
        (state_add_edges_in "Petri_A" "v-a" "y" 7)
        (state_add_edges_in "Petri_A" "v-b" "y" 6)

        (state_add_edges_out "Petri_A"  "v-a" "y" 8)
        (state_add_edges_out "Petri_A" "v-a" "z" 5)
        (state_add_edges_out "Petri_A" "v-b" "y" 9)
        
        (let [e_in  (:edges_in (:Petri_A @state))
              e_out (:edges_out (:Petri_A @state))
              v-a (get_vertex_hash ((keyword "Petri_A") @state) "v-a")
              v-b (get_vertex_hash ((keyword "Petri_A") @state) "v-b")
              z   (get_transition_hash ((keyword "Petri_A") @state) "z")
              y   (get_transition_hash ((keyword "Petri_A") @state) "y")]
          (do
            (is (contains? e_in  [z v-a 8]))
            (is (contains? e_in  [y v-a 7]))
            (is (contains? e_in  [y v-b 6]))
            
            (is (contains? e_out [y v-a 8]))
            (is (contains? e_out [z v-a 5]))
            (is (contains? e_out [y v-b 9])))))))




(deftest testing_merging
  (testing "Testing merging of nets"
    (do
        (add_petri (petri "Petri_A"))
        (state_add_vertex "Petri_A" "v-b" 6)
        (state_add_vertex "Petri_A" "v-a" 9)
        (state_add_transition "Petri_A" "z")
        (state_add_transition "Petri_A" "y")
        (state_add_edges_in "Petri_A" "v-a" "z" 8)
        (state_add_edges_in "Petri_A" "v-a" "y" 7)
        (state_add_edges_in "Petri_A" "v-b" "y" 6)
        (state_add_edges_in "Petri_A" "v-b" "z" 1)
        (state_add_edges_out "Petri_A"  "v-a" "y" 8)
        (state_add_edges_out "Petri_A" "v-a" "z" 5)
        (state_add_edges_out "Petri_A" "v-b" "y" 9)

        (add_petri (petri "Petri_B"))
       	(state_add_vertex "Petri_B" "d" 7)
        (state_add_transition "Petri_B" "z")
        (state_add_edges_out "Petri_B" "d" "z" 2)
        (state_add_edges_in "Petri_B" "d" "z" 1)
                                        ;merging
        (add_petri (hash_merge_petri "A_B_1" "Petri_A" "Petri_B" {}  {}))

        (is (= 3 (count (:vertices    (:A_B_1 @state)))))
	      (is (= 3 (count (:transitions (:A_B_1 @state)))))
        (is (= 5 (count (:edges_in (:A_B_1 @state)))))
        (is (= 4 (count (:edges_out (:A_B_1 @state)))))
        
        (add_petri (hash_merge_petri "A_B_2" "Petri_A" "Petri_B"
                               {(get_vertex_hash ((keyword "Petri_A") @state)  "v-b")
                                (get_vertex_hash ((keyword "Petri_B") @state) "d")}

                               {(get_transition_hash ((keyword "Petri_A")@state) "z")
                                (get_transition_hash ((keyword "Petri_B") @state)	"z") })) ))

        (is (= 2 (count (:vertices    (:A_B_2 @state)))))
	      (is (= 2 (count (:transitions (:A_B_2 @state)))))
        (is (= 4 (count (:edges_in (:A_B_2 @state)))))
        (is (= 4 (count (:edges_out (:A_B_2 @state))))) )


