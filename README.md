run <b>lein uberjar</b> to produce a JAR File

In a REPL you can use the core <b>(use ‘petri.core)</b> where each clojure file from the petri net project is referenced:
<ul>
(:require [petri.petri_net_state :as net])<br>
(:require [petri.simulator :as sim])<br>
(:require [petri.gui :as gui])<br>
(:require [petri.game :as game])<br>
 </ul> 
 <br>
 
<h4>Installation:</h4>
<br>
To use this project please download it and start a lein repl in the <i>petri</i> folder<br><br>
 
Here are various <b>example calls</b>: <br> <br>

(net/add_petri (net/petri "Petri_A")) <br>
(net/state_add_vertex "Petri_A" "a" 5) <br>
(net/state_add_vertex "Petri_A" "b" 0) <br>
(net/state_add_transition "Petri_A" "t") <br>
(net/state_add_transition "Petri_A" "s") <br> <br>

(net/state_add_edges_in "Petri_A" "a" "t" 1) <br>
(net/state_add_edges_out "Petri_A" "b" "t" 1) <br> <br>

(net/state_add_edges_in "Petri_A" "b" "s" 1) <br>
(net/state_add_edges_out "Petri_A" "a" "s" 1) <br> <br>

(sim/state_fire_transition "Petri_A"  "t") <br> <br>

(sim/add_property "Petri_A"   (sim/property :or (sim/property :non_empty "a")  (sim/property :non_empty "b"))) <br> <br>

(sim/eval_properties "Petri_A") <br> <br>


(net/add_petri (net/petri "Petri_B")) <br>
(sim/state_add_vertex "Petri_B" "d" 7) <br>
(sim/state_add_transition "Petri_B" "z") <br>
(sim/state_add_edges_out "Petri_B" "d" "z" 2) <br>
(sim/state_add_edges_in "Petri_B" "d" "z" 1) <br> <br>


(net/hash_merge_petri "A_B_2" "Petri_A" "Petri_B"     {"a" "d"}  {"z" "t"}) <br> <br>

(net/add_petri (net/hash_merge_petri "A_B_2" "Petri_A" "Petri_B"     {"a" "d"}  {"z" "t"})) <br> <br>

(net/copy_petri "B_Copy" "Petri_B") <br> <br>

(sim/open_file "test/petri/state2.txt") <br>
(sim/save_file "test.txt") <br> <br>

(sim/state_fire_random_transition) <br>
(sim/state_fire_random_transitions 3) <br> 
<br>
In case of duplicate names you can always refer to the hash values:

(net/state_add_edges_in "Net_B" :-1965068680 :-1965068699 1) <br>
(net/state_add_edges_out "Net_B" :-1965068680 :-1965068700 10) <br>
<br> <br> <br><br><br>

 
Various tests have been written which can be executed via <b>lein test</b>



<br><br><br>

<h4>Adding a property to a net:</h4>

A property is saved as data and has the following skelleton:<br>

<b>{:type TYPE, :args LIST-OF-ARGUMENTS}</b>

The Type can be one of the following keywords:
<ul>:or <br>:not<br> :net_alive<br> :non_empty<br> :transition_alive</ul><br>

The List of arguments depends on the type:<br><ul>
:or and :not get other hashmap properties as arguments<br>
:net_alive gets nil as argument<br>
:transition_alive and :non_empty get either names or hashvalues of vertices or transition as arguments</ul><br>
<br><br>
The simulator offers a function called (property type & args) which returns a property skelleton
<br><br>
example:<br>
<b>(property :or (property :non_empty "a") (property :net_alive))</b><br>
<b>{:type :or, :args ({:type :non_empty, :args ("a")} {:type :net_alive, :args nil})}</b>
<br>
<br>
NOTES: <br>
The <i>add_property</i> and <i>property</i> functions belong to the simulator namespace!<br>
The GUI currently needs correct property skelletons as input!
<br><br><br><br>
<h4>Controls during the visualization of the petri nets:</h4>

 <ul>
    q/w to switch back and forth between petri nets<br>
    a/s to switch back and forth between transitions<br>no
    space to fire a transition<br>
    ESC to Pause the animation<br>
    arrow keys to navigate<br>
 </ul>

<br><br><br><br><br>

<h5>Example calls from the petri_net_state:</h5>
 <table width="100%">
 <th>code</th><th>output</th>

 <tr>
  <td  width="50%">(petri "Petri_A")</td> <td>{:name "Petri_A",<br>
 :properties #{},<br>
 :vertices {},<br>
 :transitions {},<br>
 :edges_in #{},<br>
 :edges_out #{}}
 </td>
  </tr>
 
 <tr>
  <td>(add_petri (petri "Petri_A"))</td> <td> adds a petri net to the state:<br> {:Petri_A<br>
 {:name "Petri_A",<br>
  :properties #{},<br>
  :vertices {},<br>
  :transitions {},<br>
  :edges_in #{},<br>
  :edges_out #{}}}<br> The state is a hashmap and the key of each net is the keyword of its name</td>
  </tr>
  <tr>
  <td>(delete_petri "Petri_C")</td><td>deletes a petri net from the state with the specified name (NO KEYWORD!)</td>
  </tr>
  <tr>
  <td>(state_add_vertex "Petri_A" "v-a" 1)</td><td>adds a vertex to a net in the state and hashes its name in a hashmap of vertices in order to display unique identifiers even after a merging procedure with duplicate names<br>{:Petri_A
 {:name "Petri_A",
  :properties #{},
  :vertices {:789101586 ["v-a" 1]},
  :transitions {},
  :edges_in #{},
  :edges_out #{}}}
</td><tr>

<tr>
<td> (state_add_transition "Petri_A" "z")</td><td>{:Petri_A
 {:name "Petri_A",
  :properties #{},
  :vertices {:789101586 ["v-a" 1]},
  :transitions {:563948994 ["z"]},
  :edges_in #{},
  :edges_out #{}}}</td>
</tr>

<tr><td> (state_add_edges_in "Petri_A" "v-a" "z" 9)</td><td>{:Petri_A
 {:name "Petri_A",
  :properties #{},
  :vertices {:789101586 ["v-a" 1]},
  :transitions {:563948994 ["z"]},
  :edges_in #{[:563948994 :789101586 9]},
  :edges_out #{}}}
</td>
</tr>

<tr>
<td>        (state_add_edges_out "Petri_A" "v-a" "z" 5)</td><td>{:Petri_A
 {:name "Petri_A",
  :properties #{},
  :vertices {:789101586 ["v-a" 1]},
  :transitions {:563948994 ["z"]},
  :edges_in #{[:563948994 :789101586 9]},
  :edges_out #{[:563948994 :789101586 5]}}}
</td>
</tr>


<tr>
<td> (hash_merge_petri "A_B_2" "Petri_A" "Petri_B"<br>
                               {:789101587 :563949003 "a-a" "b-a"}<br>
                               {"x" "y"})) ))</td>
<td>Merging two petri nets together. Two hashmaps of vertices and transitions specify which transitions and vertices are merged together. The input of the hashmaps is either their hashvalue or their name. However if two vertices or transitions have the same name you must use their hashvalues for a propper identification</td>
</tr>

<tr>
<td>(copy_petri "cp" "Petri_A")</td><td>returns a copy of a Petri net with changed vertices and transition names</td>
</tr>

  </table>
  
  <br><br>
  <br><br>
With <b>state_transition_hash </b> and <b>state_vertex_hash</b> from the simulator's namespace you can get the hash values of certain transitions and vertices from the state. (Only one hash value will be returned even if there are duplicate names)
<br><br>example:<br>
<b>(state_transition_hash "Petri_A" "y")<br>
(state_vertex_hash "Petri_A" "v-a")</b>
