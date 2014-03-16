(ns petri.petri_net_state
  )

(require '[clojure.set :as set])
(require '[clojure.string :as strng])
(require '[clojure.walk :only (prewalk-replace) :as walker])



(defn hash_it [net v]
  (hash (str net v)))

(defn keyword_hash_it [net v]
  (keyword (str (hash_it net v))))

(def state (atom nil))

(def init
  (reset! state nil) ) 

                                        ;structure of an empty petri net


(defn petri 
  "creates empty petri_net with a specified name"
  [name]
  {:name name,
   :properties #{},
   :vertices {},
   :transitions {},
   :edges_in #{},
   :edges_out #{}} )


                                        ;constructor for a custum petri net

(defn own_petri 
  "creates a custom petri net by specifying a name, vertices, transitions, in and out sets and properties"
  [name vertices transitions in out properties]
  {:name name,
   :properties properties,
   :vertices vertices,
   :transitions transitions,
   :edges_in in,
   :edges_out out })


                                        ;adds a given petri net to the state

(defn add_petri 
  "adds a petri net to the state map indexed by its name"
  [petri]
  (swap! state assoc (keyword (:name petri)) petri))



                                        ;deletes a specified petri net (name of petri net)

(defn delete_petri
"Deletes the petri_net from the current state map at the specified index"
 [petri]
  (swap! state dissoc (deref state) (keyword petri)))



                                        ; adds a vertex to the specified net
                                        ; cats number of units


(defn state_add_vertex
"adds a vertex to the specified net with a specified amount of units"
  [net vertex cats]      ;units referred as cats for convienience
  (let [v  (:vertices ((keyword net) (deref state)))
        n  ((keyword net) (deref state))
        v1 (keyword_hash_it  net  vertex)]
    (swap! state assoc-in [(keyword net) :vertices] (assoc v v1 [vertex cats] ))))


                                        ; adds a transition
                                       
(defn state_add_transition
"adds a transition to the specified net"
[net transition]
  (let [t (:transitions ((keyword net) (deref state)))
        n ((keyword net) (deref state))
        t1 (keyword_hash_it net transition)]
    (swap! state assoc-in  [(keyword net) :transitions] (assoc t t1 [transition]))))


                            ; nil if edge t1 is not included in t
                            ; if an edge with the same vertex and transition is already in t1 this
                            ; edge is returned (in a vector)


(defn contains_edge_all?
 "returns all edges from t  with the same path as t1"
  [t t1]
   (filter  #(and (= (first  %)  (first t1)) (= (second %) (second t1))) t))

(defn contains_edge?
"returns one edge with the same path if there is none returns nil"
 [t t1]
  (first (contains_edge_all? t t1)))

(contains_edge_all?  #{[:a ] [:a :b 9] [:a :c] [:a :b]} [:a :b 3])
(contains_edge? #{[:a ] [:a :b 9] [:a :c] [:b :c]} [:a :b 3])


               ; adds an edge e1 in e and replaces an equal edge with the new value
               ; (vector)

(defn add_edge
"adds a new edge e1 and replaces an edge with the same path"
  [e e1]
  (if-let [temp (contains_edge? e e1)]
    (set (replace {temp e1} e ))
    (conj e e1)))


(add_edge #{[:a :b 3] [:a :x 3] [:c :d]} [:a :b 4])
(add_edge #{[:a :b 3] [:a :x 3] [:c :d]} [:a :k 4])



                                    ; gets the first match for hash
                                        ; value of a vertex  (there
                                        ; could possibly be more
                                        ; vertices with the same name
                                        ; because of merging)


(defn get_all_vertex_hashes
"get hash values of a vertex"
  [net vertex]
  (let [vertices (:vertices net)]
    (filter identity 
        (for [x vertices]
          (if (= vertex (first (second x))) (first x))))))

(defn get_vertex_hash
"returns a hash value for the vertex name"
  [net vertex]
  (first (get_all_vertex_hashes net vertex)))


                                        ; analogue for transitions


(defn get_all_transition_hashes
 "get hash values for a transition"
 [net transition]
  (let [transitions (:transitions net)]
    (filter identity 
            (for [x transitions]
              (if (= transition (first (second x))) (first x)) ))))

(defn get_transition_hash
"returns a hash value for a transition name"
  [net transition]
  (first (get_all_transition_hashes net transition)))



                            ; adds an edge to a hashset of edges with a keyword
                            ; for easier replacement of
                            ; identical edges
                            ; first element transition
                            ; second vertix


(defn state_add_edges_in
"adds an edge from a vertex to a transition"
  [net vertex transition cost]
   (let [e  (:edges_in ((keyword net) (deref state)))
         n  ((keyword net) (deref state))
         v1 (get_vertex_hash  n vertex)
         t1 (get_transition_hash n transition)]
     (if (and  (not= v1 nil) (not= t1 nil) )
        (swap! state assoc-in [(keyword net) :edges_in] (add_edge e [t1 v1 cost])))))


                                        ; like edges in just vise versa
(defn state_add_edges_out
 "adds and edge from a transition to a vertex"
  [net vertex transition cost]
   (let [e (:edges_out ((keyword net) (deref state)))
         n ((keyword net) (deref state))
         v1 (get_vertex_hash n vertex)
         t1 (get_transition_hash n transition)]
     (if (and  (not= v1 nil) (not=  t1 nil))
       (swap! state assoc-in [(keyword net) :edges_out] (add_edge e  [t1 v1 cost])))))



                               ; returns a set of vectors of
                               ; edges containing the
                               ; specified HASH value of the transition

(defn edges_to_transition_hash
 "returns a set of edges to specified transition hash value"
  [net hash]
  (set (filter identity
         (for [x (:edges_in net)]
           (if (= (first x) hash)  x)))))



(defn edges_from_transition_hash [net hash]
  (set (filter identity
              (for [x (:edges_out net)]
                (if (= (first x) hash)  x)))))





; renames a vertex given a hash and replacing the string representation

(defn rename_vertex
 "renames a vertex identified by its hash value CHANGES STATE!"
  [net hash_v new_name]
  (let [n ((keyword net) (deref state))]
    (if (not= hash_v nil)
      (swap! state assoc-in [(keyword net) :vertices hash_v]
             [new_name (last (hash_v (:vertices n)))]))))



;renames a transition for a given hash value

(defn rename_transition
  "renames a transition identified by its hash value CHANGES STATE!"
  [net hash_v new_name]
   (let [n ((keyword net) (deref state))]
     (if (not= nil hash_v)
       (swap! state assoc-in [(keyword net) :transitions hash_v] [new_name]))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Merging                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                        ; unites vertices from two
                                        ; petrinets
                                        ; if two vertices use max

                                        ; merge-with
                                        ;second approach

(defn- max_v [[a x] [b y]]
  (if (> x y)
    [a x]
    [b y]))


(defn unite_vertices
  "unites two sets of vertices va and vb with a map of
  hash values of vertices  which will be merged
  e.g. 'a'  and 'b will be merged':
  (unite_vertices {:1 ['a' 2]} {:2 ['b' 4]} {:1 :2})"
  [va vb vertices]
  (merge-with max_v
              (set/rename-keys va vertices)
              (set/rename-keys vb vertices)))

(defn unite_vertices_hash
"unites and re-hashes vertices"
  [name va vb vertices]
  (let [v_a_b (unite_vertices va vb vertices)]
    (set/rename-keys v_a_b
         (reduce merge
             (map  (fn [k] {k (keyword_hash_it name k)}) (keys v_a_b))))))



                                        ;unites transitions from two petrinets

(defn unite_transitions [name va vb transitions]
  (reduce merge
          (for [t (set/rename-keys (set/union va vb) transitions)]
                  { (keyword_hash_it name (first t))  (second t)}   )))




                          ;double hashes edges so they are still connected


(defn rename_edges [net e]
  (into #{}
    (for [x e]
       [(keyword_hash_it net (first x))
        (keyword_hash_it net (second x))
        (get x 2)]  )) )

(defn- max_of_equals
"max cost of edges with the same path"
  [t]
  (into #{} (for [x t]
             [(first x) (second x) (apply max (map last (contains_edge_all? t x) ))])))




(defn unite_edges
  "unites two sets ea and eb of edges and adjusts the path of
   transitions and vertices which will be merged"
  [name ea eb vertices transitions]
  (rename_edges name (max_of_equals (into #{}
       (for [t (set/union ea eb)]
         (replace transitions (replace vertices t)))))))


;this could be used in other functions very well:

(defn rename_hash_properties [net va vb ta tb]
  (reduce merge (for [X (keys (set/union va vb ta tb))]
      {X (keyword_hash_it net X)})))


(defn unite_properties [net pa pb va vb ta tb vertices transitions old_a old_b]
  ;(println (rename_hash_properties net va vb ta tb))
  (walker/prewalk-replace
   (rename_hash_properties net va vb ta tb)
   (walker/prewalk-replace
    (set/union vertices transitions {old_a net} {old_b net}) (set/union pa pb))))
   



(defn- hash_name_vertices_map
"returns a hashmap which converts vertices names to their hash values"
  [net_a net_b]
  (reduce merge  (map (fn [v] {(first (second v)) (first v)})
                      (concat
                       (:vertices ((keyword net_b) @state))
                       (:vertices ((keyword net_a) @state))))))

(defn- hash_name_transitions_map
"returns a hashmap which converts transitions names to their hash values"
  [net_a net_b]
  (reduce merge  (map (fn [v] {(first (second v)) (first v)})
                      (concat
                       (:transitions ((keyword net_b) @state))
                       (:transitions ((keyword net_a) @state))))))



(defn hash_merge_petri
  "Merging two Petri Nets Net_A and Net_B to a new one with the specified name
   same_vertices and same_transitions are hashmaps which will merge tuples of transitions and vertices by their hashvalues
   e.g.: (hash_merge_petri \"a_b\" \"a\" \"b\" {:101 :201} {})
          a and b will be merged and the vertices :101 and :201 and  will be merged"
  [name net_a net_b merged_vertices merged_transitions]
  (let [na ((keyword net_a) (deref state))
        nb ((keyword net_b) (deref state))
        same_vertices    (walker/prewalk-replace (hash_name_vertices_map net_a net_b) merged_vertices)
        same_transitions (walker/prewalk-replace (hash_name_transitions_map net_a net_b) merged_transitions)]
   (own_petri
     name
     (unite_vertices_hash name (:vertices na) (:vertices nb) same_vertices)
     (unite_transitions name (:transitions na) (:transitions nb) same_transitions)
     (unite_edges name (:edges_in na) (:edges_in nb) same_vertices same_transitions)
     (unite_edges name (:edges_out na) (:edges_out nb) same_vertices same_transitions)
     (unite_properties name
                       (:properties na) (:properties nb)
                       (:vertices na) (:vertices nb)
                       (:transitions na) (:transitions nb)
                       same_vertices same_transitions (:name na) (:name nb)))))




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
     (rename_edges copy_name (:edges_out copy))
     '())))


