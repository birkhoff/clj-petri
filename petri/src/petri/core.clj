(ns petri.core)

(use '[clojure.set])
(use '[clojure.string :only (replace-first)])


(defn keyword_to_string [k]
  (replace-first (str k) ":" ""))

(defn hash_it [net v]
  (hash (str net v)))

(defn keyword_hash_it [net v]
  (keyword (str (hash_it net v))))

(def state (atom nil))

(def init
  (reset! state nil) ) 

                                        ;structure of an empty petri net


(defn petri [name] {:name name , :vertices {}, :transitions {}, :edges_in #{}, :edges_out #{}} )


                                        ;constructor for a custum petri net

(defn own_petri [name vertices transitions in out]
   {:name name , :vertices vertices, :transitions transitions, :edges_in in, :edges_out out})



                                        ;adds a given petri net to the state

(defn add_petri [petri]
  (swap! state assoc (keyword (:name petri)) petri))



                                        ;deletes a specified petri net (name of petri net)

(defn delete_petri [petri]
  (swap! state (dissoc (deref state) (keyword petri))))  


                                        ; adds a vertex to the specified net
                                        ; cats number of units

(defn state_add_vertix [net vertix cats]                  ;cats unit
  (println (hash_it net vertix))
  (let [v  (:vertices ((keyword net) (deref state)))
        n  ((keyword net) (deref state))
        v1 (keyword_hash_it  net  vertix)]
    (swap! state assoc (keyword net) (assoc n :vertices ( assoc v v1 [vertix cats] )))))



                                        ; adds a transition
                                       
(defn state_add_transition [net transition]
  (let [t (:transitions ((keyword net) (deref state)))
        n ((keyword net) (deref state))
        t1 (keyword_hash_it net transition)]
    (swap! state assoc (keyword net) (assoc n :transitions (assoc t t1 [transition])))))


                                        ;nil if edge t1 is not included in t
                                        ; if an edge with the same vertex and transition is already in t1 this
                                        ; edge is returned (in a vector)



(defn contains_edge [t t1]
  (first (filter identity
     (for [x t]
      (if (and (= (first  x)  (first t1))  (= (second x) (second t1)))
         x)))) )

(contains_edge #{[:a ] [:a :b 9] [:a :c] [:b :c]} [:a :b 3])


                                        ;adds an edge t1 in t and replaces an equal edge with the new value
                                        ;(vector)


(defn add_edge [t t1]
     (let [temp (contains_edge t t1)]
       (if (= temp nil)
         (conj t t1)
         (into #{} (replace {temp t1} t ))))
     )


(add_edge #{[:a :b 3] [:a :x 3] [:c :d]} [:a :b 4])
(add_edge #{[:a :b 3] [:a :x 3] [:c :d]} [:a :k 4])

                                        ; gets the first match for hash
                                        ; value of a vertix  (there
                                        ; could possibly be more
                                        ; vertices with the same name
                                        ; because of merging)


(defn get_all_vertix_hashes [net vertix]
  (filter identity (let [vertices (:vertices ((keyword net) (deref state)))]
      (for [x vertices]
        (if (= vertix (first (second x))) (first x)) ))) )

(defn get_vertix_hash [net vertix]
  (first (get_all_vertix_hashes net vertix)))

                                        ; analogue for transitions


(defn get_all_transition_hashes [net transition]
  (filter identity (let [transitions (:transitions ((keyword net) (deref state)))]
      (for [x transitions]
        (if (= transition (first (second x))) (first x)) ))) )

(defn get_transition_hash [net transition]
  (first (get_all_transition_hashes net transition)))





                                        ; adds an edge to a hashset of edges with the keyword
                                        ; "${NameOfVertix}_${NameOfTransition}"
                                        ; for easier replacement of
                                        ; identical edges
                                        ; !!!!!!!!!!!!!!!
                                        ; first element transition
                                        ; second vertix



(defn state_add_edges_in [net vertix transition cost]
   (let [e (:edges_in ((keyword net) (deref state)))
         n ((keyword net) (deref state))
         v1 (get_vertix_hash  net vertix)
         t1 (get_transition_hash net transition)]
     (if (and  (not= v1 nil) (not= t1 nil) )
       (swap! state assoc (keyword net) (assoc n :edges_in (add_edge  e [t1 v1 cost]))))) )


                                        ; like edges in just vise versa
(defn state_add_edges_out [net vertix transition cost]
   (let [e (:edges_out ((keyword net) (deref state)))
         n ((keyword net) (deref state))
         v1 (keyword_hash_it net vertix)
         t1 (keyword_hash_it net transition)]
     (if (and  (not= v1 nil) (not=  t1 nil) )
       (swap! state assoc (keyword net) (assoc n :edges_out (add_edge e  [t1 v1 cost]))))) )


                                        ;returns a set of vectors
                                        ;of edges containing specified
                                        ;transition

(defn state_get_edges_to_transition [net transition]
  (let [n (keyword net)
        t1 (get_transition_hash net transition) ]
    (filter identity
       (for [x (:edges_in (n (deref state)))]
         (if (= (first x) t1)  [(second x) (get x 2)]))) ) )



(defn state_get_edges_from_transition [net transition]
  (let [n (keyword net)
        t1 (get_transition_hash net transition) ]
    (filter identity
       (for [x (:edges_out (n (deref state)))]
         (if (= (second x) t1)  [(second x) (get x 2)]))) ) )





;;; Merging

	                                        ; unites vertices or
	                                        ; transitions (according to parameter) from two
                                          ; petrinets

(defn unite [name va vb vertices]
  (reduce merge (for [v (rename-keys (union va vb) vertices)]
                  { (first v)  (second v)}   )))


(defn unite_edges [ea eb vertices transitions]
  (into #{} (for [x (union ea eb)]
              (replace transitions (replace vertices x))  )  ) )



(defn hash_merge_petri [name net_a net_b same_vertices same_transitions]
  (let [na ((keyword net_a) (deref state))
        nb ((keyword net_b) (deref state))]
    (own_petri
     name
     (unite name (:vertices na) (:vertices nb) same_vertices)
     (unite name (:transitions na) (:transitions nb) same_transitions)
     (unite_edges (:edges_in na) (:edges_in nb) same_vertices same_transitions)
     (unite_edges (:edges_out na) (:edges_out nb) same_vertices same_transitions)
     )))






; (replace-first (str (keyword "String")) ":" "")

init

(add_petri (petri "Petri_A"))
(add_petri (petri "Petri_B"))
(add_petri (petri "Petri_C"))
;(deref state)
(:abba (deref state))
(state_add_vertix "Petri_A" "v-a" 9)
(state_add_vertix "Petri_A" "v-b" 6) 

(state_add_transition "Petri_A" "z")
(state_add_transition "Petri_A" "y")



(state_add_edges_in "Petri_A" "v-a" "z" 9)
(state_add_edges_in "Petri_A" "v-a" "z" 10)
(state_add_edges_in "Petri_A" "v-a" "y" 7)

(state_add_edges_out "Petri_A"  "v-a" "y" 7)
(state_add_edges_out "Petri_A" "v-b" "y" 9)


(state_get_edges_to_transition "Petri_A" "z")

(state_get_edges_from_transition "Petri_A" "y")



(state_add_vertix "Petri_B" "d" 7)
(state_add_transition "Petri_B" "z")
(state_add_edges_out "Petri_B" "d" "z" 10)
(state_add_edges_in "Petri_B" "d" "z" 9)

(deref state)

;(delete_petri "Petri_C")

(deref state)
                                        ; own_petri [name vertices transitions in out]
;(get_vertix_hash "Petri_A" "v-a")

;(get_transition_hash "Petri_A" "kk")


                                        
                                        
(hash_merge_petri "tunac" "Petri_A" "Petri_B" {(get_vertix_hash "Petri_A"  "v-a") (get_vertix_hash "Petri_B" "d")}  {(get_transition_hash "Petri_A" "z") (get_transition_hash "Petri_B" "z") })



;(rename-keys '{:a 9 :c 2 :d 4} '{:a :b})




;(:tunac-Petri_A-v-a(:vertices (mergesimple "tunac" "Petri_A"
;"Petri_B" {} {})))


;(merge "tunac" "Petri_A" "Petri_B")
