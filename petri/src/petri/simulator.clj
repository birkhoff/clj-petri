
(ns petri.simulator)

(require '[petri.petri_net :as net])
(require '[clojure.walk :only (prewalk-replace) :as walker])



                               ; checks if an Element is in a List
(defn element_in_list? [list X]
  (contains? (set list) X))

                             ; checks if multiple Elements are in a List
(defn elements_in_list? [list elements]
  (if (element_in_list?
         (for [X elements]
           (element_in_list? list X)) false)
    false
    true))


                                        ;checks if one of the Elements
                                        ;is in the List

(defn least_one_elements_in_list? [list elements]
  (if (element_in_list?
         (for [X elements]
           (element_in_list? list X)) true)
    true
    false))



(defn state_vertex_hash [net v]
  (net/get_vertex_hash ((keyword net) @net/state) v))

(defn state_transition_hash [net v]
  (net/get_transition_hash ((keyword net) @net/state) v))





                            ; returns all edges which are able to fire

(defn net_fireable_edges [net]
  (let [n ((keyword net) (deref net/state))
        v (:vertices n)
        e (:edges_in n)]
    (into #{} (filter identity
         (for [x e]
             (if (>= (second ((second x) v)) (get x 2)) x )))) ) )



                                        ; returns edges which aren't
                                        ; able to fire 

(defn net_not_fireable_edges [net]
  (reduce disj  (:edges_in ((keyword net) (deref net/state)))
           (net_fireable_edges net) ))


                                        ; returns all fireable
                                        ; transition hashes


(defn state_get_fireable_transitions [net]
  (reduce disj
          (into #{} (map first (net_fireable_edges net)))
          (into #{} (map first (net_not_fireable_edges net)))))



                                        ;checks if net is alive

(defn net_alive [net]
  (not (empty? (state_get_fireable_transitions net))))

(net_alive "Net_A")

                                        ; sees if a transition with
                                        ; the name t is fireable


(defn state_transition_fireable? [net t]
  (contains? (into  #{}
           (for [X (net/get_all_transition_hashes net t)]
             (first (X (:transitions ((keyword net) @net/state)))))) t))

                                        ; sees if a transition hash is firable

(defn state_transition_hash_fireable? [net hash]
  (contains? (state_get_fireable_transitions net) hash))


(state_transition_hash_fireable? "Net_A" :-1965068709)
(state_transition_fireable? "A_B" "z")
(state_get_fireable_transitions "Net_A")
(net_fireable_edges "Net_A")


                            ;fires tokens to a transition
                            ;and returns the vertex minus the used
                            ;tokens


(defn fire_to_edge
"substract tokens of the connected vertex"
  [net e]
  (let [cost  (last e)
        n     ((keyword net) (deref net/state))
        vertices (:vertices n)
        vhash (second e)
        v1    (vhash (:vertices n))]
    {vhash [(first v1) (- (second v1) cost)]}))



                               ;returns a set of vertices
                               ;including the fired vertices

(defn fire_to_all_edges [edges net]
  (let [n ((keyword net) (deref net/state))
        vertices (:vertices n)]
    (conj vertices (reduce merge (for [X edges]
                              (fire_to_edge net X))))))



(defn fire_from_edge
"adds tokens to connected vertices from a transition"
  [vertices net e]
  (let [cost  (last e)
        vhash (second e)
        v1    (vhash vertices)]
    {vhash [(first v1) (+ (second v1) cost)]}))


                                        ;fire every edge from a
                                        ;transition returns a set of
                                        ;vertices from the net

(defn fire_from_all_edges [vertices edges net]
  (conj vertices (reduce merge (for [X edges]
                                 (fire_from_edge vertices net X)))))



                                        ; fires a transition and swaps
                                        ; the vertices 


(defn state_fire_transition [net t]
  (let [n     ((keyword net) (deref net/state))
        edges (net/edges_to_transition_hash n t)
        outs  (net/edges_from_transition_hash n t)]
    (if (state_transition_hash_fireable? net t)       
      (swap! net/state assoc-in [(keyword net) :vertices]
           (fire_from_all_edges (fire_to_all_edges edges net) outs net)))))






; DSL transtion alive: name of net and variable names of transitions

(defn transition_alive [net & args]
  (least_one_elements_in_list?
   (state_get_fireable_transitions net)
   args))



(defn non_empty_vertices [net]
  (into #{} (filter identity
     (for [X (:vertices ((keyword net) (deref net/state)))]
        (if (< 0 (second (second X)))
         X)))))


(defn non_empty [net & args]
  (least_one_elements_in_list?  (map first (non_empty_vertices net)) args))





                                        ; Adding a Property to the
                                        ; specified net which can be
                                        ; evaluated whenever necessary

                                        ; (net_alive) -> (net_alive "Net_A")
                                        ; (transition_alive "Net_A" "y" "z")
                                        ; (non_empty "Net_A" "a" "b")

(defn delete_property [net]
  "deletes all properties of a net in the current state"
  (let [n ((keyword net) (deref net/state))
        p (:properties n)]
    (swap! net/state assoc (keyword net) (assoc n :properties '()))))



(defn hash_name_map
"returns a hashmap which converts names to their hash values"
  [net]
  (reduce merge  (map (fn [v] {(first (second v)) (first v)})
                      (concat
                       (:transitions ((keyword net) @net/state))
                       (:vertices ((keyword net) @net/state))))))


(defn hash_property
  "converts name values to hash values"
  [net p]
  (walker/prewalk-replace (hash_name_map net) p))


(defn add_property
"adds a property to a net in the current state"
  [net property]
  (let [n ((keyword net) (deref net/state))
        p (:properties n)]
    (swap! net/state assoc-in [(keyword net) :properties]
           (conj p (hash_property net property)))))

(nil? (re-find #"petri." "s"))

(defn is_property?
  "checks wether p is property or not"
  [p]
  (do (if (and (not (nil? (re-find #"(or|not|and)" (str p))))
               (nil? (re-find #"petri." (str p))))
      false
      true)))


(is_property? `(transition_alive :-1965068709))


(defn eval_property
  "evaluates a single property part and adds the
   corresponding net as first parameter"
  [net p]
   (eval `(-> ~net ~p)))

(eval_property "Net_A" `(transition_alive :-1965068709))



(defn  prefixer
"dsl to clojure"
  ([a]
     a)
  ([op a]
     `(~op ~a))
  ([a op b]
     `(~op ~a ~b))
  ([a b c & expr]
     (if (or (=  a `not) (= (str a) "not"))
       `(~c (~a ~b) ~(apply prefixer expr))
       `(~b ~a ~(apply prefixer (cons c expr))))))



(apply prefixer '(not true or true))



(defn eval_property_expr [net expr]
  (doall (for [e expr]
      (if (is_property? e)
        (eval_property net e)
         e))))



(defn eval_properties [net]
  (doall (for [p (:properties ((keyword net) @net/state))]
           [p  (apply prefixer (eval_property_expr net p))])))


(->  "Net_A" (net_alive))




@net/state

(eval_properties "Net_A")

(add_property "Net_A" `((net_alive) or (net_alive)))
(add_property "Net_A" `(not (transition_alive :-1965068709)))
(add_property "Net_A" `(not (non_empty :-1965068709)))


(state_get_fireable_transitions "Net_A")

(transition_alive "Net_A" :-1965068709)

(transition_alive "Net_A" :-1965068710)


(eval_properties "A_B_2")

@net/state





(def state_get_all_fireable_transitions
  (reduce concat
      (map (fn [n] (map #(vector n %) (state_get_fireable_transitions n)))
           (map #(:name (second %)) @net/state))))


(defn state_fire_random_transition
"fires a random transition of the nets in the state map"
[]
(let [t state_get_all_fireable_transitions ]
  (if (not (empty? t))
    (apply state_fire_transition (rand-nth t)))))



 (state_fire_random_transition)


(state_get_fireable_transitions "Net_A")


(defn state_fire_random_transitions
  "fires a given number of random transitions"
 [n]
 (doall (repeatedly n state_fire_random_transition)))

(state_fire_random_transitions 5)

@net/state




