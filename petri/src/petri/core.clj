(ns petri.core)

(use '[clojure.set])
(use '[clojure.string :only (replace-first)])

(defn extend_keyword [old new]
  (keyword (replace-first (str old) ":" (str new "-"))))

(def state (atom nil))

(def init
  (reset! state nil) ) 

(defn petri [name] {:name name , :vertices {}, :transitions #{}, :edges_in {}, :edges_out {}} )


                                        ;adds ad petri net

(defn own_petri [name vertices transitions in out]
   {:name name , :vertices vertices, :transitions transitions, :edges_in in, :edges_out out})

(defn add_petri [petri]
  (swap! state assoc (keyword (:name petri)) petri))

                                        ; adds a vertex to the specified net
                                        ; cats number of units

(defn state_add_vertix [net vertix cats]                  ;cats unit
  (let [v (:vertices ((keyword net) (deref state)))
        n ((keyword net) (deref state))
        v1 (str net "-" vertix)]
    (swap! state assoc (keyword net) (assoc n :vertices ( assoc v (keyword v1) cats )))))

                                        ; adds a transition
                                        ; transitions is a set because they do not contain more information
                                        ; than their own name



(defn state_add_transition [net transition]
  (let [t (:transitions ((keyword net) (deref state)))
        n ((keyword net) (deref state))
        t1 (str net "-" transition)]
    (swap! state assoc (keyword net) (assoc n :transitions (conj t t1)))))


                                        ; adds an edge to a hashset of edges with the keyword
                                        ; "${NameOfVertix}_${NameOfTransition}"
                                        ; for easier replacement of
                                        ; identical edges 

(defn state_add_edges_in [net vertix transition cost]
   (let [e (:edges_in ((keyword net) (deref state)))
         n ((keyword net) (deref state))
         v1 (str net "-" vertix)
         t1 (str net "-" transition)]
     (if (and  (contains? (:vertices n) (keyword v1)) (contains? (:transitions n) t1) )
       (swap! state assoc (keyword net) (assoc n :edges_in (assoc e (keyword (str t1 "_" v1)) [t1 (keyword v1) cost]))))) )


                                        ; like edges in just vise versa
(defn state_add_edges_out [net vertix transition cost]
   (let [e (:edges_out ((keyword net) (deref state)))
         n ((keyword net) (deref state))
         v1 (str net "-" vertix)
         t1 (str net "-" transition)]
     (if (and  (contains? (:vertices n) (keyword v1)) (contains? (:transitions n) t1) )
       (swap! state assoc (keyword net) (assoc n :edges_out (assoc e (keyword (str t1 "_" v1)) [(keyword v1) t1 cost]))))) )


                                        ;returns a set of vectors
                                        ;of edges containing specified
                                        ;transition

(defn state_get_edges_to_transition [net transition]
  (let [n1 (keyword net)
        t1 (str net "-" transition) ]
    (filter identity
       (for [x (:edges_in (n1 (deref state)))]
         (if (= (first (second x)) t1)  (second x)))) ) )



(defn state_get_edges_from_transition [net transition]
  (let [n1 (keyword net)
        t1 (str net "-" transition) ]
    (filter identity
       (for [x (:edges_out (n1 (deref state)))]
         (if (= (second (second x)) t1)  (second x)))) ) )




                                        ;unites vertices from two
                                        ;petrinets and prefixes them
                                        ;with the name of the target net 
(defn unite_vertices [name va vb vertices]
  (reduce merge (for [v (rename-keys (union va vb) vertices)]
                     {(extend_keyword (first v) name ) (second v)}
                     )))





                                        ;merging two Petri Nets
                                        

(defn mergesimple [name net_a net_b vertices]
  (let [na ((keyword net_a) (deref state))
        nb ((keyword net_b) (deref state))]
    (own_petri
     name
     (unite_vertices name (:vertices na) (:vertices nb) vertices)
     (union (:transitions na) (:transitions nb))
     (union (:edges_in na) (:edges_in nb))
     (union (:edges_out na) (:edges_out nb))
     )))


(defn merge_nets [name net_a net_b same_vertices same_transitions]
  (let [na ((keyword net_a) (deref state))
        nb ((keyword net_b) (deref state))]
    (own_petri
     name
     (unite_vertices name (:vertices na) (:vertices nb))
     (union (:transitions na) (:transitions nb))
     (union (:edges_in na) (:edges_in nb))
     (union (:edges_out na) (:edges_out nb))
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


(contains? #{:foo 2} (keyword "foo"))


(state_add_edges_in "Petri_A" "v-a" "z" 9)
(state_add_edges_in "Petri_A" "v-a" "z" 10)
(state_add_edges_in "Petri_A" "v-a" "y" 9)

(state_add_edges_out "Petri_A"  "v-a" "y" 7)
(state_add_edges_out "Petri_A" "v-b" "y" 9)


(state_get_edges_to_transition "Petri_A" "z")

(state_get_edges_from_transition "Petri_A" "y")



(state_add_vertix "Petri_B" "d" 7)
(state_add_transition "Petri_B" "zz")
(state_add_edges_out "Petri_B" "d" "zz" 10)
(state_add_edges_in "Petri_B" "d" "zz" 9)

(deref state)

                                        ; own_petri [name vertices transitions in out]


(mergesimple "tunac" "Petri_A" "Petri_B" '{:Petri_B-d :Petri_A-v-a} )

(rename-keys '{:a 9 :c 2 :d 4} '{:a :b})


(:tunac-Petri_A-v-a(:vertices (mergesimple "tunac" "Petri_A" "Petri_B" {})))

;(merge "tunac" "Petri_A" "Petri_B")
