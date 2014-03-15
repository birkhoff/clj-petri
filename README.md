run <b>lein uberjar</b> to produce a JAR File

In a REPL you can use the core <b>(use ‘petri.core)</b> where each clojure file from the petri net project is referenced:
<ul>
(:require [petri.petri_net_state :as net])<br>
(:require [petri.simulator :as simulator])<br>
(:require [petri.gui :as gui])<br>
(:require [petri.game :as game])<br>
 </ul> 
Various tests have been written which can be executed via <b>lein test</b>



<br><br><br>

<h4>Adding a property to a net:</h4>

A property is saved as data and has the following skelleton:<br>

<b>{:type TYPE, :args LIST-OF-ARGUMENTS}</b>

The Type can be either of these:
:or :not :net_alive :non_empty :transition_alive

The List of arguments can depends on the type:
:or and :not get other hashmap properties as arguments
:net_alive gets nil as argument
:transition_alive and :non_empty get either names or hashvalues of vertices or transition as arguments
<br><br>
The simulator offers a function called (property type & args) which returns a property skelleton
<br><br>
example:<br>
<b>(property :or  (property :non_empty "a") (property :non_empty "b"))</b><br>
<b>{:type :or, :args ({:type :non_empty, :args ("a")} {:type :non_empty, :args ("b")})}</b>
<br>
<br>
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
 <table>
 <th>code</th><th>output</th>
 <tr>
  <td>(petri "Petri_A")</td> <td>{:name "Petri_A",<br>
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
                               {(get_vertex_hash ((keyword "Petri_A") @state)  "v-b")<br>
                                (get_vertex_hash ((keyword "Petri_B") @state) "d")}<br>
                               {(get_transition_hash ((keyword "Petri_A")@state) "z")<br>
                                (get_transition_hash ((keyword "Petri_B") @state)	"z") })) ))</td>
<td>Merging two petri nets together. Two hashmaps of hashvalues of vertices and transitions specify which transitions and vertices are merged together</td>
</tr>

<tr>
<td>(copy_petri "cp" "Petri_A")</td><td>returns a copy of a Petri net with changed vertices and transition names</td>
</tr>

  </table>
