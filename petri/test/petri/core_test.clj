(ns petri.core-test
  (:require [clojure.test :refer :all]
            [petri.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

;; setting up demo petri nets

	init

	(add_petri (petri "Petri_A"))
	(add_petri (petri "Petri_B"))
	(add_petri (petri "Petri_C"))
	;(deref state)
	(state_add_vertix "Petri_A" "v-a" 9)
	(state_add_vertix "Petri_A" "v-b" 6) 
	(state_add_vertix "Petri_A" "c" 0)

	(state_add_transition "Petri_A" "z")
	(state_add_transition "Petri_A" "y")




	(state_add_edges_in "Petri_A" "v-a" "z" 9)
	(state_add_edges_in "Petri_A" "v-a" "z" 8)
	(state_add_edges_in "Petri_A" "v-a" "y" 7)
	(state_add_edges_in "Petri_A" "v-b" "y" 6)

	(state_add_edges_out "Petri_A"  "v-a" "y" 8)
	(state_add_edges_out "Petri_A" "v-a" "z" 5)
	(state_add_edges_out "Petri_A" "v-b" "y" 9)


	(state_get_edges_to_transition "Petri_A" "z")

	(state_get_edges_from_transition "Petri_A" "y")



	(state_add_vertix "Petri_B" "d" 7)
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




	(hash_merge_petri "A_B" "Petri_A" "Petri_B" {(get_vertix_hash "Petri_A"  "v-a") (get_vertix_hash "Petri_B" "d")}  {(get_transition_hash "Petri_A" "z") (get_transition_hash "Petri_B" "z") })

	(add_petri (hash_merge_petri "REAL_A" "Petri_A" "Petri_A" {}  {}))

	                                        ;
	(add_petri (hash_merge_petri "A_B" "Petri_A" "Petri_B"
	                                        ;
	{(get_vertix_hash "Petri_A"  "v-a") (get_vertix_hash "Petri_B" "d")}
	                                        ;
	{(get_transition_hash "Petri_A" "z") (get_transition_hash "Petri_B"
	                                ;
	"z") }))



	(copy_petri "cp" "Petri_A")

	(state_get_edges_to_transition "Petri_A" "y")

	(state_fire_transition "Petri_A" :563948993)


	;(rename-keys '{:a 9 :c 2 :d 4} '{:a :b})

	;(:tunac-Petri_A-v-a(:vertices (mergesimple "tunac" "Petri_A"
	;"Petri_B" {} {})))

	;(merge "tunac" "Petri_A" "Petri_B")
