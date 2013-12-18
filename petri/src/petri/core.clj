(ns petri.core)

(def state (atom nil))

(def init
  (reset! state nil) ) 

(defn petri [name] {:name name , :vertices {}, :transitions #{}, :edges_in {}, :edges_out {}} )

                                        ;adds ad petri net

(defn add_petri [petri]
  (swap! state assoc (keyword (:name petri)) petri))

                                        ; adds a vertex to the specified net
                                        ; cats number of units

(defn state_add_vertix [net vertix cats]                  ;cats unit
  (let [v (:vertices ((keyword net) (deref state)))
        n ((keyword net) (deref state))]
    (swap! state assoc (keyword net) (assoc n :vertices ( assoc v (keyword vertix) cats )))))

                                        ; adds a transition
                                        ; transitions is a set because they do not contain more information
                                        ; than their own name


(defn state_add_transition [net transition]
  (let [t (:transitions ((keyword net) (deref state)))
        n ((keyword net) (deref state))]
    (swap! state assoc (keyword net) (assoc n :transitions (conj t transition)))))


                                        ; adds an edge to a hashset of edges with the keyword
                                        ; "${NameOfVertix}_${NameOfTransition}"
                                        ; for easier replacement of
                                        ; identical edges 

(defn state_add_edges_in [net vertix transition cost]
   (let [e (:edges_in ((keyword net) (deref state)))
         n ((keyword net) (deref state))]
     (if (and  (contains? (:vertices n) (keyword vertix)) (contains? (:transitions n) transition) )
       (swap! state assoc (keyword net) (assoc n :edges_in (assoc e (keyword (str transition "_" vertix)) [transition (keyword vertix) cost]))))) )


                                        ; like edges in just vise versa
(defn state_add_edges_out [net vertix transition cost]
   (let [e (:edges_out ((keyword net) (deref state)))
         n ((keyword net) (deref state))]
     (if (and  (contains? (:vertices n) (keyword vertix)) (contains? (:transitions n) transition) )
       (swap! state assoc (keyword net) (assoc n :edges_out (assoc e (keyword (str transition "_" vertix)) [(keyword vertix) transition cost]))))) )




init

(add_petri (petri "Petri_A"))
(add_petri (petri "Petri_B"))
(add_petri (petri "Petri_C"))
;(deref state)
(:abba (deref state))
(state_add_vertix "Petri_A" "v-a" 9)
(state_add_vertix "Petri_A" "v-b" 6) 

(state_add_transition "Petri_A" "trans-z")
(state_add_transition "Petri_A" "trans-y")

(contains? #{:foo 2} (keyword "foo"))


(state_add_edges_in "Petri_A" "v-a" "trans-z" 9)
(state_add_edges_in "Petri_A" "v-a" "trans-z" 10)
(state_add_edges_in "Petri_A" "v-a" "trans-y" 9)

(state_add_edges_out "Petri_A"  "v-a" "trans-y" 7)
(state_add_edges_out "Petri_A" "v-b" "trans-y" 9)
