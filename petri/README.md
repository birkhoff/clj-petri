# petri

A Clojure library designed to simulate Petri Nets.

## Usage

To use this project please download it and start a lein repl in the <i>petri</i> folder<br><br>
 
Here are various <b>example calls</b>: <br> <br>

```
(net/add_petri (net/petri "Petri_A")) 
(net/state_add_vertex "Petri_A" "a" 5) 
(net/state_add_vertex "Petri_A" "b" 0) 
(net/state_add_transition "Petri_A" "t")
(net/state_add_transition "Petri_A" "s") 

(net/state_add_edges_in "Petri_A" "a" "t" 1) 
(net/state_add_edges_out "Petri_A" "b" "t" 1)

(net/state_add_edges_in "Petri_A" "b" "s" 1) 
(net/state_add_edges_out "Petri_A" "a" "s" 1)

(sim/state_fire_transition "Petri_A"  "t") 

(sim/add_property "Petri_A"   (sim/property :or (sim/property :non_empty "a")  (sim/property :non_empty "b")))

(sim/eval_properties "Petri_A")


(net/add_petri (net/petri "Petri_B")) 
(sim/state_add_vertex "Petri_B" "d" 7)
(sim/state_add_transition "Petri_B" "z")
(sim/state_add_edges_out "Petri_B" "d" "z" 2) 
(sim/state_add_edges_in "Petri_B" "d" "z" 1) 


(net/hash_merge_petri "A_B_2" "Petri_A" "Petri_B"     {"a" "d"}  {"z" "t"}) 

(net/add_petri (net/hash_merge_petri "A_B_2" "Petri_A" "Petri_B"     {"a" "d"}  {"z" "t"}))

(net/copy_petri "B_Copy" "Petri_B") 

(sim/open_file "test/petri/state2.txt")
(sim/save_file "test.txt")

(sim/state_fire_random_transition)
(sim/state_fire_random_transitions 3)  
```
<br>
In case of duplicate names you can always refer to the hash values:
```
(net/state_add_edges_in "Net_B" :-1965068680 :-1965068699 1) 
(net/state_add_edges_out "Net_B" :-1965068680 :-1965068700 10)
```

## License

Copyright © 2013 Mike

Distributed under the Eclipse Public License, the same as Clojure.
