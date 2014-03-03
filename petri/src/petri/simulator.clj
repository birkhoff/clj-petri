
(ns petri.simulator)

(require '[petri.petri_net :as net])


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


(elements_in_list? '([:a :b 10] [:c :d 11] [:e :f 12]) '([:a :b 10] [:e :f 12] ))


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


(defn state_fire_transition2 [net t]
  (let [edges (net/edges_to_transition_hash net t)
        outs  (net/edges_from_transition_hash net t)
        n     ((keyword net) (deref net/state))]
    (if (state_transition_hash_fireable? net t)       
      (swap! net/state assoc (keyword net)
             (assoc n :vertices (fire_from_all_edges (fire_to_all_edges edges net) outs net))))))



(defn state_fire_transition [net t]
  (let [n     ((keyword net) (deref net/state))
        edges (net/edges_to_transition_hash n t)
        outs  (net/edges_from_transition_hash n t)]
    (if (state_transition_hash_fireable? net t)       
      (swap! net/state assoc-in [(keyword net) :vertices]
           (fire_from_all_edges (fire_to_all_edges edges net) outs net)))))


@net/state

(net/edges_to_transition_hash (:A_B @net/state) :-1349607119)
(state_fire_transition "A_B" :-1349067119)



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



(eval '(non_empty "Petri_A" "v-a"))

                                        ; Adding a Property to the
                                        ; specified net which can be
                                        ; evaluated whenever necessary

                                        ; (net_alive "Net_A")
                                        ; (transition_alive "Net_A" "y" "z")
                                        ; (non_empty "Net_A" "a" "b")

(defn delete_property [net]
  (let [n ((keyword net) (deref net/state))
        p (:properties n)]
    (swap! net/state assoc (keyword net) (assoc n :properties '()))))

(defn add_property [net property]
  (let [n ((keyword net) (deref net/state))
        p (:properties n)]
    (swap! net/state assoc (keyword net) (assoc n :properties (conj p property)))))

(add_property "Net_A" '(or (net_alive "Net_A") (net_alive "Net_B")))
(add_property "Net_A" '(transition_alive "z"))
(delete_property "Net_A")
(deref net/state)

(defn eval_property [net]
  (for [X (:properties ((keyword net) (deref net/state)))]
    (if (or (= (first X) 'or) (= (first X) 'and) (= (first X) 'not)) 
              [X  (eval X)]
              [X  (apply (eval (first X)) (vec (conj (rest X) net))) ])))

(eval_property "Net_B")
