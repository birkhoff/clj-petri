(ns petri.core-test
  (:require [clojure.test :refer :all]
            [petri.petri_net_state :refer :all]
            [petri.simulator :refer :all])
  (:use clojure.test)
  (:use petri.petri_net_state)
  (:use petri.simulator))



;(use '[clojure.walk :only (prewalk-replace)])
(use '[petri.petri_net_state])
(use '[petri.simulator])
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
                                (get_transition_hash ((keyword "Petri_B") @state)	"z") })) 

        (add_petri (hash_merge_petri "A_B_2_2" "Petri_A" "Petri_B"  {"v-b" "d"}
                                     {(get_transition_hash ((keyword "Petri_A")@state) "z")
                                      (get_transition_hash ((keyword "Petri_B") @state)	"z") }))
        
        (is (= 2 (count (:vertices    (:A_B_2 @state)))))
        (is (= 2 (count (:transitions (:A_B_2 @state)))))
        (is (= 4 (count (:edges_in (:A_B_2 @state)))))
        (is (= 4 (count (:edges_out (:A_B_2 @state)))))
        (is (= (set (map second (:vertices  (:A_B_2 @state))))
               (set (map second (:vertices    (:A_B_2_2 @state))))))
        (is (= (set (map second (:transitions    (:A_B_2 @state))))
               (set (map second (:transitions    (:A_B_2_2 @state)))))))))



(deftest testing_copying
  (testing "Testing copy function"
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

        (let [v     (:vertices (copy_petri "cp" "Petri_A"))
              t     (:transitions (copy_petri "cp" "Petri_A"))
              e_in  (:edges_in (copy_petri "cp" "Petri_A"))
              e_out (:edges_out (copy_petri "cp" "Petri_A"))]
          (is (= 2 (count v)))
          (is (= 2 (count t)))
          (is (= 4 (count e_in)))
          (is (= 3 (count e_out)))))))




(deftest testing_merge_properties
  (testing "Testing merging of properties:
 Two vertices and transitions are merged together and should be merged together in the properties as well resulting in only 3 different properties instead of 6 different"
    (let [k
          (unite_properties "A_B"
                            #{{:type :transition_alive, :args '(:-1965068709)} {:type :net_alive, :args nil}
                              {:type :non_empty :args '(:-1965068733)}}

                            #{{:type :transition_alive, :args '( :-1965068678)} {:type :net_alive, :args nil}
                              {:type :non_empty :args '( :-1965068699)}}
             

                                        ; vertices va and vb:
                            {:-1965068733 ["b" 5], :-1965068734 ["a" 12]}
                            {:-1965068699 ["e" 6], :-1965068700 ["d" 1]}
                                        ; transitions ta and tb:

                            {:-1965068710 ["y"], :-1965068709 ["z"]}
                            {:-1965068681 ["w"], :-1965068678 ["x"]}
                                        ; merged v and t

                            {:-1965068733 :-1965068699}
                            {:-1965068709 :-1965068678}
                            "Net_A" "Net_B"  )]
      (is (= 3 (count k)) ))))


;; simulator tests

(deftest testing_elements_in_list?
  (testing "Own function to check if all elements of a list are also present in the first one"
    (is
     (elements_in_list?
      '([:a :b 10] [:c :d 11] [:e :f 12]) '([:a :b 10] [:e :f 12] )))
    (is (not
         (elements_in_list?
          '([:a :b 10] [:c :d 11] [:e :f 12]) '([:a :b 10] [:e :f 10] ))))))


(deftest testing_least_one_element_in_list?
  (testing "Own function to check if at least one element of the latter list is also in the first one"
    (is (least_one_elements_in_list? '([:a :b]) '([:a :b] [:s])))
    (is (least_one_elements_in_list? '([:a :b] [:s]) '([:a :b] [:s])))
    (is (least_one_elements_in_list? '([:a] [:s] [:b] ) '([:a :b] [:s])))
    (is (not (least_one_elements_in_list? '([:a :c]) '([:a :b] [:s]))))))



(deftest testing_fireable_edges
  (testing "list of fireable edges of a net of the state"
    (do
      (add_petri (petri "Petri_A"))
      (state_add_vertex "Petri_A" "v-b" 6)
      (state_add_vertex "Petri_A" "v-a" 9)
      (state_add_transition "Petri_A" "z")
      (state_add_transition "Petri_A" "y")
      (state_add_edges_in "Petri_A" "v-a" "z" 8)
      (state_add_edges_in "Petri_A" "v-a" "y" 7)
      (state_add_edges_in "Petri_A" "v-b" "y" 6)
      (state_add_edges_in "Petri_A" "v-b" "z" 100)
      (state_add_edges_out "Petri_A"  "v-a" "y" 8)
      (state_add_edges_out "Petri_A" "v-a" "z" 5)
      (state_add_edges_out "Petri_A" "v-b" "y" 9)

      (is (= 3 (count (net_fireable_edges "Petri_A"))))

      (state_add_vertex "Petri_A" "v-b" 100)
      (is (= 4 (count (net_fireable_edges "Petri_A")))))))


(deftest testing_not_fireable_edges
  (testing "list of fireable edges of a net of the state"
    (do
      (add_petri (petri "Petri_A"))
      (state_add_vertex "Petri_A" "v-b" 6)
      (state_add_vertex "Petri_A" "v-a" 9)
      (state_add_transition "Petri_A" "z")
      (state_add_transition "Petri_A" "y")
      (state_add_edges_in "Petri_A" "v-a" "z" 8)
      (state_add_edges_in "Petri_A" "v-a" "y" 7)
      (state_add_edges_in "Petri_A" "v-b" "y" 6)
      (state_add_edges_in "Petri_A" "v-b" "z" 100)
      (state_add_edges_out "Petri_A"  "v-a" "y" 8)
      (state_add_edges_out "Petri_A" "v-a" "z" 5)
      (state_add_edges_out "Petri_A" "v-b" "y" 9)

      (is (= 1 (count (net_not_fireable_edges "Petri_A"))))

      (state_add_vertex "Petri_A" "v-b" 100)
      (is (= 0 (count (net_not_fireable_edges "Petri_A")))))))


(deftest testing_fireable_transitions
  (testing "list of fireable transitions of a net of the state"
    (do
      (add_petri (petri "Petri_A"))
      (state_add_vertex "Petri_A" "v-b" 6)
      (state_add_vertex "Petri_A" "v-a" 9)
      (state_add_transition "Petri_A" "z")
      (state_add_transition "Petri_A" "y")
      (state_add_edges_in "Petri_A" "v-a" "z" 8)
      (state_add_edges_in "Petri_A" "v-a" "y" 7)
      (state_add_edges_in "Petri_A" "v-b" "y" 6)
      (state_add_edges_in "Petri_A" "v-b" "z" 100)
      (state_add_edges_out "Petri_A"  "v-a" "y" 8)
      (state_add_edges_out "Petri_A" "v-a" "z" 5)
      (state_add_edges_out "Petri_A" "v-b" "y" 9)

      (is (contains?
           (state_get_fireable_transitions "Petri_A")
           (get_transition_hash ((keyword "Petri_A") @state) "y")))

      (state_add_vertex "Petri_A" "v-b" 100)
      (is (and
           (contains?
             (state_get_fireable_transitions "Petri_A")
             (state_transition_hash "Petri_A" "z"))
           (contains?
             (state_get_fireable_transitions "Petri_A")
             (state_transition_hash "Petri_A" "y") ))))))




(deftest testing_edges_to_transition_hash
  (testing "testing function which returns all edges to a transition"
      (do
        (add_petri (petri "Petri_A"))
        (state_add_vertex "Petri_A" "v-b" 6)
        (state_add_vertex "Petri_A" "v-a" 9)
        (state_add_transition "Petri_A" "z")
        (state_add_transition "Petri_A" "y")
        (state_add_edges_in "Petri_A" "v-a" "z" 8)
        (state_add_edges_in "Petri_A" "v-a" "y" 7)
        (state_add_edges_in "Petri_A" "v-b" "y" 6)
        (state_add_edges_in "Petri_A" "v-b" "z" 100)
        (state_add_edges_out "Petri_A"  "v-a" "y" 8)
        (state_add_edges_out "Petri_A" "v-a" "z" 5)
        (state_add_edges_out "Petri_A" "v-b" "y" 9)

        (is (= #{6 7}
           (set (map last
              (edges_to_transition_hash (:Petri_A @state)
                         (state_transition_hash "Petri_A" "y"))))))
        (is (= #{8 100}
           (set (map last
              (edges_to_transition_hash (:Petri_A @state)
                      (state_transition_hash "Petri_A" "z")))))))))



(deftest testing_simple_fireing
  (testing "Testing functionality of fireing a transition"
    (do
        (add_petri (petri "Petri_A"))
        (state_add_vertex "Petri_A" "v-b" 6)
        (state_add_vertex "Petri_A" "v-a" 9)
        (state_add_transition "Petri_A" "z")
        (state_add_transition "Petri_A" "y")
        (state_add_edges_in "Petri_A" "v-a" "z" 8)
        (state_add_edges_in "Petri_A" "v-a" "y" 7)
        (state_add_edges_in "Petri_A" "v-b" "y" 6)
        (state_add_edges_in "Petri_A" "v-b" "z" 100)
        (state_add_edges_out "Petri_A"  "v-a" "y" 8)
        (state_add_edges_out "Petri_A" "v-a" "z" 5)
        (state_add_edges_out "Petri_A" "v-b" "y" 9)

        (state_fire_transition "Petri_A"
                               (state_transition_hash "Petri_A" "y"))
        
        (is (= #{["v-a" 10] ["v-b" 9]}
               (set (map second (:vertices (:Petri_A @state))))))

        (state_fire_transition "Petri_A" "z"
                               ;(state_transition_hash "Petri_A" "z")
                               )

        (is (= #{["v-a" 10] ["v-b" 9]}
               (set (map second (:vertices (:Petri_A @state))))))

        (state_add_vertex "Petri_A" "v-b" 100)
        
        (state_fire_transition "Petri_A"
                               "z")

        (is (= #{["v-a" 7] ["v-b" 0]}
               (set (map second (:vertices (:Petri_A @state))))))) ))



(deftest testing_open_file
  (testing "Tests if a file is loaded correctly"
    (do
     (open_file "test/petri/state2.txt")
     (is (and
          (contains? @state :Net_A)
          (contains? @state :Net_B))))))


(deftest testing_simple_eval_properties
  (testing "Simple tests for properties"
    (do

      (open_file "test/petri/state2.txt")

      (add_property
       "Net_A"
       (property :not (property :not (property :transition_alive "y"))))

      (add_property "Net_A"
                    (property :transition_alive "y"))

       (add_property "Net_A"
                     (property :net_alive))

       (add_property "Net_A"
                     (property :or
                               (property :non_empty "a")
                               (property :non_empty "b")))
       
       (let [res (map second (eval_properties "Net_A"))]
         (do (is  (= #{true} (set res)))
             (is (= 4 (count res))))))))


