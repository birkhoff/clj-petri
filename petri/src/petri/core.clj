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


(defn contains_edge_all [t t1]
   (filter identity
     (for [x t]
       (if (and (= (first  x)  (first t1))  (= (second x) (second t1)))
                 x))) )

(defn contains_edge [t t1]
  (first (contains_edge_all t t1)))

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
         v1 (get_vertix_hash net vertix)
         t1 (get_transition_hash net transition)]
     (if (and  (not= v1 nil) (not=  t1 nil) )
       (swap! state assoc (keyword net) (assoc n :edges_out (add_edge e  [t1 v1 cost]))))) )


                                        ;returns a set of vectors
                                        ;of edges containing specified
                                        ;transition
;NEEDS TO BE UPDATED TO USE THE FUNCTION BELOW

(defn state_get_edges_to_transition [net transition]
  (let [n (keyword net)
        t1 (get_transition_hash net transition) ]
    (filter identity
       (for [x (:edges_in (n (deref state)))]
         (if (= (first x) t1)  x))) ) )



(defn state_get_edges_from_transition [net transition]
  (let [n (keyword net)
        t1 (get_transition_hash net transition) ]
    (filter identity
       (for [x (:edges_out (n (deref state)))]
         (if (= (first x) t1)  x))) ) )


                                        ; returns a set of vectors of
                                        ; edges containing the
                                        ; specified HASH value of the transition

(defn edges_to_transition_hash [net hash]
  (let [n (keyword net)]
    (into #{} (filter identity
                     (for [x (:edges_in (n (deref state)))]
                       (if (= (first x) hash)  x)))) ) )



(defn edges_from_transition_hash [net hash]
  (let [n (keyword net)]
    (into #{} (filter identity
                      (for [x (:edges_out (n (deref state)))]
                        (if (= (first x) hash)  x)))) ) )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Merging                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                        ; unites vertices from two
                                        ; petrinets
                                        ; if two vertices are merged their units are summed up


(defn unite [name va vb vertices]
  (reduce merge
    (for [v  (union va vb)]
      (if (contains? vertices (first v))
        { (keyword_hash_it name ((first v) vertices))
          [ (first (second v))
               (+(second (second v))
               (second(((first v) vertices) vb)))] }
        { (keyword_hash_it name (first v))  (second v)})   )))

                                        ;unites transitions from two petrinets

(defn unite_transitions [name va vb transitions]
  (reduce merge (for [t (rename-keys (union va vb) transitions)]
                  { (keyword_hash_it name (first t))  (second t)}   )))




                                        ;double hashes edges so they are still connected


(defn rename_edges [net e]
  (into #{}
    (for [x e]
       [(keyword_hash_it net (first x))
        (keyword_hash_it net (second x))
        (get x 2)]  )) )

(defn sum_up_equals [t]
  (into #{} (for [x t]
             [(first x) (second x) (reduce + (map last (contains_edge_all t x) ))])))


(sum_up_equals  #{[:563948993 :563949003 7] [:563949025 :563949003 9]
   [:563949025 :563949003 10]})

(defn unite_edges [name ea eb vertices transitions]
  (rename_edges name (sum_up_equals (into #{} (for [t (union ea eb)]
                               (replace transitions (replace vertices t)) )))))



(defn hash_merge_petri [name net_a net_b same_vertices same_transitions]
  (let [na ((keyword net_a) (deref state))
        nb ((keyword net_b) (deref state))]
    (own_petri
     name
     (unite name (:vertices na) (:vertices nb) same_vertices)
     (unite_transitions name (:transitions na) (:transitions nb) same_transitions)
     (unite_edges name (:edges_in na) (:edges_in nb) same_vertices same_transitions)
     (unite_edges name (:edges_out na) (:edges_out nb) same_vertices same_transitions)
     )))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; copy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


                                        ;renames vertices and double hashes

(defn rename_vertices [net v]
  (reduce merge (for [x v]
    {(keyword_hash_it net  (first x))
           [(str "v_" (hash (first (second x)))) (second (second x))]} )))



                                        ;renames transitions and double hashes

(defn rename_transitions [net t]
  (reduce merge (for [x t]
       {(keyword_hash_it net (first x)) [(str "t_" (hash (second x))) ] } )))





(defn copy_petri [copy_name original]
  (let [copy ((keyword original) (deref state))]
    ; own_petri [name vertices transitions in out]
    (own_petri
     copy_name
     (rename_vertices copy_name (:vertices copy)) 
     (rename_transitions copy_name (:transitions copy))
     (rename_edges copy_name (:edges_in copy))
     (rename_edges copy_name (:edges_out copy)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;      Simulator     ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(for [x (state_get_edges_to_transition "Petri_A" "y")] x)


                                        ; checks if an Element is in a List

(defn element_in_list? [List X]
  (contains? (set List) X))

                                        ; checks if multuple Elements are in a List

(defn elements_in_list? [List Elements]
  (if (element_in_list?
         (for [X Elements]
           (element_in_list? List X)) false)
    false
    true))

                                        ;checks if one of the Elements
                                        ;is in the List

(defn least_one_elements_in_list? [List Elements]
  (if (element_in_list?
         (for [X Elements]
           (element_in_list? List X)) true)
    true
    false))


(elements_in_list? '([:a :b 10] [:c :d 11] [:e :f 12]) '([:a :b 10] [:e :f 12] ))


                                        ; returns all edges which are able to fire

(defn net_fireable_edges [net]
  (let [n ((keyword net) (deref state))
        v (:vertices n)
        e (:edges_in n)]
    (into #{} (filter identity (for [x e]
                                 (if (>= (second ((second x) v)) (get x 2)) x )))) ) )

                                        ; returns edges which aren't
                                        ; able to fire 

(defn net_not_fireable_edges [net]
  (reduce disj  (:edges_in ((keyword net) (deref state)))
           (net_fireable_edges net) ))

(net_fireable_edges "Petri_A")
(net_not_fireable_edges "Petri_A")

(state_get_edges_to_transition "Petri_A" "y")

                                        ; returns all fireable
                                        ; transition hashes


(defn state_get_fireable_transitions [net]
  (reduce disj
          (into #{} (map first (net_fireable_edges net)))
          (into #{} (map first (net_not_fireable_edges net)))))

(state_get_fireable_transitions "Petri_A")

                                        ;checks if net is alive

(defn net_alive [net]
  (not (empty? (state_get_fireable_transitions net))))

(net_alive "Petri_A")

                                        ; sees if a transition with
                                        ; the name t is fireable


(defn state_transition_fireable? [net t]
  (elements_in_list? (state_get_edges_to_transition net t) (net_fireable_edges net)))

                                        ; sees if a transition hash is firable

(defn state_transition_hash_fireable [net hash]
  (contains? (state_get_fireable_transitions net) hash))


(state_transition_hash_fireable "Petri_A" :563948994)
(state_transition_fireable? "A_B" "y")



(defn fire_to_edge [net e]
  (let [cost  (get e 2)
        n     ((keyword net) (deref state))
        vertices (:vertices n)
        vhash (second e)
        v1    (vhash (:vertices n))]
    {vhash [(first v1) (- (second v1) cost)]}))


(defn fire_to_all_edges [edges net]
  (let [n ((keyword net) (deref state))
        vertices (:vertices n)]
    (conj vertices (reduce merge (for [X edges]
                              (fire_to_edge net X))))))

(defn fire_from_edge [vertices net e]
  (let [cost  (get e 2)
        vhash (second e)
        v1    (vhash vertices)]
    {vhash [(first v1) (+ (second v1) cost)]}))

(defn fire_from_all_edges [vertices edges net]
  (conj vertices (reduce merge (for [X edges]
                                 (fire_from_edge vertices net X)))))



;  (swap! state assoc (keyword net) (assoc n :vertices ( assoc v v1 [vertix cats] ))

(defn state_fire_transition [net t]
  (let [edges (edges_to_transition_hash net t)
        outs  (edges_from_transition_hash net t)
        n     ((keyword net) (deref state))]
    (if (state_transition_hash_fireable net t)
              
      (swap! state assoc (keyword net)
             (assoc n :vertices (fire_from_all_edges (fire_to_all_edges edges net) outs net))))))


(edges_from_transition_hash "Petri_A" :563948993)


(defn transition_alive [net & args]
  (least_one_elements_in_list?
   (for [X (state_get_fireable_transitions net)]
     (first (X (:transitions ((keyword net) (deref state))))))
     args))

(transition_alive "Petri_A" "y" "a")

(defn non_empty_vertices [net]
  (into #{} (filter identity
     (for [X (:vertices ((keyword net) (deref state)))]
        (if (< 0 (second (second X)))
         X)))))

(non_empty_vertices "Petri_A")

(defn non_empty [net & args]
  (least_one_elements_in_list? 
   (for [X (non_empty_vertices net)]
     (first (second X)))
   args))

(non_empty "Petri_A" "a")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
