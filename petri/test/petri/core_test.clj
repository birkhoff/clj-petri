(ns petri.core-test
  (:require [clojure.test :refer :all]
            [petri.petri_net :refer :all]))


(use '[clojure.walk :only (prewalk-replace)])
(use '[petri.petri_net])
(use '[petri.simulator])

;(deftest a-test  (testing "FIXME, I fail."   (is (= 0 1))))



(def net_a
  {:name "Net_A", 
   :properties #{}, 
   :vertices {:-1965068733 ["b" 5], :-1965068734 ["a" 12]}, 
   :transitions {:-1965068710 ["y"], :-1965068709 ["z"]}, 
   :edges_in #{[:-1965068710 :-1965068733 0] [:-1965068709 :-1965068733 1] [:-1965068709 :-1965068734 1]}, 
   :edges_out #{[:-1965068710 :-1965068733 1] [:-1965068709 :-1965068734 3]}})


;; setting up demo petri nets

	init

	(add_petri (petri "Petri_A"))
	(add_petri (petri "Petri_B"))
	(add_petri (petri "Petri_C"))
	;(deref state)
	(state_add_vertex "Petri_A" "v-a" 9)
	(state_add_vertex "Petri_A" "v-b" 6) 
	(state_add_vertex "Petri_A" "c" 0)

	(state_add_transition "Petri_A" "z")
	(state_add_transition "Petri_A" "y")


@state

(get_vertex_hash ((keyword "Petri_A") @state) "v-a")

	(state_add_edges_in "Petri_A" "v-a" "z" 9)
	(state_add_edges_in "Petri_A" "v-a" "z" 8)
	(state_add_edges_in "Petri_A" "v-a" "y" 7)
	(state_add_edges_in "Petri_A" "v-b" "y" 6)

	(state_add_edges_out "Petri_A"  "v-a" "y" 8)
	(state_add_edges_out "Petri_A" "v-a" "z" 5)
	(state_add_edges_out "Petri_A" "v-b" "y" 9)




	(state_add_vertex "Petri_B" "d" 7)
	;(state_add_vertix "Petri_B" "e" 11)
	(state_add_transition "Petri_B" "z")
	(state_add_edges_out "Petri_B" "d" "z" 10)
	(state_add_edges_in "Petri_B" "d" "z" 9)

	(deref state)

	;(delete_petri "Petri_C")

	(deref state)
	                                        ; own_petri [name vertices transitions in out]
	;(get_vertix_hash "Petri_A" "v-a")

	;(get_transition_hash "Petri_A" "kk")




(hash_merge_petri
 "A_B" "Petri_A"
 "Petri_B"
 {(get_vertex_hash "Petri_A"  "v-a") (get_vertex_hash "Petri_B" "d")}
 {(get_transition_hash "Petri_A" "z") (get_transition_hash "Petri_B" "z") })

	(add_petri (hash_merge_petri "REAL_A" "Petri_A" "Petri_B" {}  {}))

	                                        
	(add_petri (hash_merge_petri "A_B" "Petri_A" "Petri_B"
          {(get_vertex_hash ((keyword "Petri_A") @state)  "v-b")
           (get_vertex_hash ((keyword "Petri_B") @state) "d")}

          {(get_transition_hash ((keyword "Petri_A")@state) "z")                       (get_transition_hash ((keyword "Petri_B") @state)	"z") }))



	(copy_petri "cp" "Petri_A")

@state

(rename_vertex "A_B" :-1349607183 "peter")
(rename_transition "A_B" :-1349607119 "zorro")

;	(state_fire_transition "Petri_A" :563948993)



	;(rename-keys '{:a 9 :c 2 :d 4} '{:a :b})

	;(:tunac-Petri_A-v-a(:vertices (mergesimple "tunac" "Petri_A"
	;"Petri_B" {} {})))

	;(merge "tunac" "Petri_A" "Petri_B")



(unite_properties "A_B"
                   #{'(net_alive) '(transition_alive :-1965068709)
                     '(or (net_alive "Net_A") (net_alive "Net_B"))}

                   #{'(net_alive) '(transition_alive :-1965068678)
                     '(or (net_alive "Net_A") (net_alive "Net_B"))}

                   {:-1965068733 ["b" 5], :-1965068734 ["a" 12]}
                   {:-1965068699 ["e" 1], :-1965068700 ["d" 1]}

                   {:-1965068710 ["y"], :-1965068709 ["z"]}
                   {:-1965068681 ["w"], :-1965068678 ["x"]}
                   
                   {}
                   {:-1965068709 :-1965068678}
                   "Net_A" "Net_B"
                   )



;; simulator tests

(elements_in_list? '([:a :b 10] [:c :d 11] [:e :f 12]) '([:a :b 10] [:e :f 12] ))

(least_one_elements_in_list? '([:a :b]) '([:a :b] [:s]))

(net_fireable_edges "A_B")

(net_fireable_edges "Petri_A")
(net_not_fireable_edges "Petri_A")

(state_get_fireable_transitions "Petri_A")

(non_empty_vertices "Petri_A")


(edges_to_transition_hash (:A_B @state) :-1349607119)
(state_fire_transition "A_B"
                       (get_transition_hash
                        ((keyword "A_B") @state)  "y"))



(add_property "Petri_A" '(or (net_alive "Petri_A") (net_alive "Petri_B")))
(add_property "Petri_A" '(transition_alive "z"))
;(delete_property "Petri_A")
(deref state)

(eval '(non_empty "Petri_A" :789101586))

(eval_property "Petri_A")


(state_vertex_hash "Net_B" "e")
(state_transition_hash "Net_B" "x")



@state

(state_get_fireable_transitions "A_B")
(edges_to_transition_hash (:A_B state) :-1349628046)
(state_fire_transition "A_B" :-1349628046)
(state_fire_transition "A_B" :-1349607119)
