(ns petri.gui
  (:use seesaw.core)
  (:use seesaw.core
        [clojure.java.io :only [file]])
  (:import [javax.swing JFileChooser JEditorPane JScrollPane BorderFactory]
           java.awt.Font))


(require '[petri.petri_net_state :as net])
(require '[petri.simulator :as sim])
(require '[clojure.walk :only (prewalk-replace) :as walker])
(use '[petri.simulator :only (net_alive)])
(use '[petri.simulator :only (transition_alive)])
(use '[petri.simulator :only (non_empty)])




(def f (frame :title "Petri Netz Simulator 2014"))
(config! f :size [1100 :by 800])

(defn display [content]
  (config! f :content content)
  content)

(def lbl (label ))
(config! lbl :background :grey :foreground "#00f")
(display lbl)




(def field_net
  (text :bounds [10 40 300 30] ))

(def field_vertex
  (text :bounds [10 170 230 30] ))

(def field_vertex_tokens
  (text :bounds [260 170 50 30]))

(def field_transition
  (text :bounds [10 290 220 30]))

(def field_state
  (text :bounds [400 40 600 700] :multi-line? true :editable? false :wrap-lines? false) )

(def field_property
  (text :bounds [10 570 300 30]))

(def field_eval_property
  (text :bounds [10 610 300 60] :multi-line? true :editable? false))





(def button_add_net
  (button :text "ADD NET" :bounds [8 70 150 30] ))

(def button_add_vertex
  (button :text "ADD VERTEX" :bounds [8 200 150 30]))

(def button_add_transition
  (button :text "ADD TRANSITION" :bounds [8 320 150 30]))

(def button_add_edges_in
  (button :text "ADD EDGE FROM VERTEX TO TRANSITION" :bounds [8 420 300 30]))

(def button_add_edges_out
  (button :text "ADD EDGE FROM TRANSITION TO VERTEX" :bounds [8 470 300 30]))

(def button_fire
  (button :text "FIRE" :bounds [200 320 50 30]))

(def button_random_fire
  (button :text "RANDOM FIRE" :bounds [10 360 100 40]))

(def button_add_property
  (button :text "ADD PROPERTY" :bounds [8 600 150 30]))

(def button_eval_property
  (button :text "EVALUATE PROPERTIES" :bounds [8 710 190 30]))




                                        ;pretty printer for line feed after categories


(defn pretty_a [input]                                     
  (clojure.string/replace (str input)
                          #"(.properties)|(.vertices)|(.edges_in)|(.edges_out)|(.transitions)" "\n       $1$2$3$4$5") )



                                        ; pretty printer for line feed after new net

(defn pretty_b [input]
  (clojure.string/replace (str input)
                          "}}," "}},\n"))

;pretty printer combined 

(defn pretty [input]
  (pretty_b (pretty_a input)))


; doesnt work

(defn update_state_field [e]
  (text! field_state (pretty (deref net/state))))

                                        ;escaped Text where space is
                                        ;replaced by _
                                     

(defn esc_text [field]
  (clojure.string/replace (text field) " " "_"))

;;;; Listeners

(listen button_add_net :action
        (fn [e] (if (not= (text field_net) "")
                 (do 
                   (net/add_petri (net/petri  (esc_text field_net)))
                   (text! field_state (pretty (deref net/state)))))))

(defn parse_token []
  (if (empty? (text field_vertex_tokens))
    0
    (let [p (read-string (esc_text field_vertex_tokens))]
           (if (number? p) p 0))))



(listen button_add_vertex :action
        (fn [e]
          (if (and (not= (text field_net) "") (not= text field_vertex) "")
            (do
              (net/state_add_vertex  (esc_text field_net)
                                 (esc_text field_vertex)
                                 (parse_token))
              (text! field_state (pretty (deref net/state)))))))

(listen button_add_transition :action
        (fn [e]
          (if (and (not= (text field_net) "") (not= (text field_transition) ""))
            (do
              (net/state_add_transition  (esc_text field_net)
                                     (esc_text field_transition))
              (text! field_state (pretty (deref net/state)))))))

(listen button_add_edges_in :action
        (fn [e] (do
                 (net/state_add_edges_in (esc_text field_net)

                                     (esc_text field_vertex)
                                     (esc_text field_transition)
                                     (parse_token))
                 (text! field_state (pretty (deref net/state))))))

(listen button_add_edges_out :action
        (fn [e] (do
                 (net/state_add_edges_out (esc_text field_net)
                                     (esc_text field_vertex)
                                     (esc_text field_transition)
                                     (parse_token))
                 (text! field_state (pretty (deref net/state))))))



(listen button_fire :action
        (fn [e] (doall
                 (sim/state_fire_transition
                     (esc_text field_net)
                     (sim/state_transition_hash
                       (esc_text field_net) (esc_text field_transition)))
                 (text! field_state (pretty (deref net/state))))))



(listen button_random_fire :action
        (fn [e] (doall
                 (sim/state_fire_random_transition)
                 (text! field_state (pretty (deref net/state))))))



(listen button_add_property :action
  (fn [e]
    (if (and  (not= (esc_text field_net) "")
              (not= (text field_property) ""))
      (doall
        (sim/add_property (esc_text field_net)
                          (read-string  (text field_property)))
        (text! field_state (pretty (deref net/state)))))))


(defn- pretty_prop
 "Pretty Printer for the results of properties"
  [text]
   (clojure.string/replace text "]" "]\n"))

(defn set_eval_field [net]
   (text! field_eval_property
          (doall (pretty_prop (apply str (sim/eval_properties net))))))

(listen button_eval_property :action
   (fn [e]
      (if (not= (esc_text field_net) "")
          (set_eval_field (esc_text field_net)))))






(def panel
  (xyz-panel :items [(label :text "Name of Net:" :bounds [10 10 100 30])
                     field_net
                     (label :text "Name of Vertex:" :bounds [10 140 110 30])
                     (label :text "Tokens:" :bounds [250 140 50 30])
                     field_vertex
                     field_vertex_tokens
                     (label :text "Name of Transition:" :bounds [10 260 200 30])
                     field_transition

                     (scrollable field_state :bounds [400 40 600 700])
                     
                     button_add_net
                     button_add_vertex
                     button_add_transition
                     button_add_edges_in
                     button_add_edges_out
                     button_fire
                     button_random_fire

                     field_property
                     button_add_property
                     (scrollable field_eval_property :bounds [10 645 300 60])
                     button_eval_property
                     
                     ]))




(def button_copy
  (button :text "copy" :bounds [155 65 60 30]))

(def field_copy_original
  (text :bounds [30 30 150 30]))

(def field_copy_copy
  (text :bounds [190 30 150 30]))

(def field_rename
  (text :bounds [30 30 150 30]))

(def field_rename_transition
  (text :bounds [30 30 150 30]))

(def copypanel
  (xyz-panel :items [button_copy
                     (label :text "Original" :bounds [30 10 150 20])
                     (label :text "Copy" :bounds [191 10 150 20])
                     field_copy_original
                     field_copy_copy]))



(def button_merge
  (button :text "merge" :bounds [200 195 65 30]))

(def field_merge_a
  (text :bounds [30 30 150 30]))

(def field_merge_b
  (text :bounds [190 30 150 30]))

(def field_merge_out
  (text :bounds [350 30 150 30]))

(def field_merge_vertices
  (text :bounds [30 90 470 30] :tip "Please enter the hash values of the vertices you want to merge    e.g.  :19281 :2866 :19280 :2867     Each pair of two consecutive hash values will be merged as one vertex" ))

(def field_merge_transitions
  (text :bounds [30 150 470 30]  :tip "Please enter the hash values of the transitions you want to merge    e.g.  :19281 :2866 :19280 :2867    Each pair of two consecutive hash values will be merged as one transition")) 

(def mergepanel
  (xyz-panel :items [button_merge
                     (label :text "First Net" :bounds [30 10 150 20])
                     (label :text "Second Net" :bounds [191 10 150 20])
                     (label :text "Merged Net" :bounds [352 10 150 20])
                     (label :text "Vertices which will be merged:" :bounds [30 70 300 20])
                     (label :text "Transitions which will be merged:" :bounds [30 130 300 20])
                     field_merge_a
                     field_merge_b
                     field_merge_out
                     field_merge_vertices
                     field_merge_transitions]))

(def button_rename
  (button :text "RENAME" :bounds [60 65 80 30]))

(def button_rename_t
  (button :text "RENAME" :bounds [60 65 80 30]))

(def renamepanel
  (xyz-panel :items [button_rename
                     (label :text "New name:" :bounds [30 10 150 20])
                     field_rename]))

(def renamepanel_transition
  (xyz-panel :items [button_rename_t
                     (label :text "New name:" :bounds [30 10 150 20])
                     field_rename_transition]))

(def button_n_steps
  (button :text "FIRE" :bounds [60 65 80 30]))

(def field_n_steps
  (text :bounds [30 30 150 30]))

(def n_steps_panel
  (xyz-panel :items [button_n_steps
                     field_n_steps
                     (label :text "How many random steps?" :bounds [30 10 250 20])]))



(def n_steps_f
  (frame :size [230 :by 120]
         :content n_steps_panel))

(def copy_f
  (frame :size [400 :by 120]
         :content copypanel ))

(def merge_f
  (frame :size [550 :by 250]
         :content mergepanel))

(def rename_f
  (frame :size [230 :by 120]
         :content renamepanel))

(def rename_f_t
  (frame :size [230 :by 120]
         :content renamepanel_transition) )

(defn dispose_copy [e]
  (let [copy     (esc_text field_copy_copy)
        original (esc_text field_copy_original)]
    (if (and (not= copy "") (not= original ""))
      (do
        (net/add_petri (net/copy_petri
                    copy
                    original))
        (dispose! copy_f)
        (text! field_state (pretty (deref net/state)))))))


(defn dispose_merge [e]
  (let [out (text field_merge_out)
             a   (esc_text field_merge_a)
             b   (esc_text field_merge_b)]
    (if (and (not= out "") (not= a "") (not= b ""))
      (do
        (net/add_petri (net/hash_merge_petri
                    out
                    a
                    b                
                    (read-string (str "{" (text field_merge_vertices) "}"))
                    (read-string (str "{"(text field_merge_transitions) "}"))))
        (dispose! merge_f)
        (text! field_state (pretty (deref net/state)))))))

(defn dispose_rename [e]
  (let [original
        (sim/state_vertex_hash  (esc_text field_net) (esc_text field_vertex))
        rename (esc_text field_rename)]
    (if  (not= original "")
      (do
        (net/rename_vertex (esc_text field_net) original rename)
        (dispose! rename_f)
        (text! field_state (pretty (deref net/state)))))))

(defn dispose_rename_transition [e]
  (let [original    (sim/state_transition_hash
                     (esc_text field_net)
                     (esc_text field_transition))
        rename (esc_text field_rename_transition)]
    (if  (not= original "")
      (do
        (net/rename_transition (esc_text field_net) original rename)
        (dispose! rename_f)
        (text! field_state (pretty (deref net/state)))))))


(defn dispose_n_steps [e]
  (if (integer? (read-string (text field_n_steps)))
    (doall  
     (dispose! n_steps_f)
     (sim/state_fire_random_transitions (read-string (text field_n_steps)))
     (text! field_state (pretty (deref net/state))))))

(integer? (read-string "3"))

(listen button_copy :action dispose_copy)
(listen button_merge :action dispose_merge)
(listen button_rename :action dispose_rename)
(listen button_rename_t :action dispose_rename_transition)
(listen button_n_steps :action dispose_n_steps)



;; save and open functions

;(def current-file (atom (file (System/getProperty "user.home") ".dicscratch")))


(defn select-file []
  (let [chooser (JFileChooser.)]
    (.showDialog chooser panel "Select")
    (.getSelectedFile chooser)))



(defn a-save-as [e]
  (when-let [selected (select-file)]
    (spit selected (str (deref net/state)))))


(defn a-open  [e]
  (when-let [selected (select-file)]
   (reset! net/state  (read-string (slurp selected)))
   (text! field_state (pretty (deref net/state)))))


(defn a-n-steps [e]
  (do
    (-> n_steps_f show!)
    (request-focus! field_n_steps)))

(defn a-copy  [e]
  (do
    (-> copy_f show!)
    (request-focus! field_copy_original)))

(defn a-merge [e]
  (do
    (-> merge_f show!)
    (request-focus! field_merge_a)))

(defn a-delete [e]
  (do
    (net/delete_petri (esc_text field_net))
    (text! field_state (pretty (deref net/state)))))

(defn a-rename [e]
  (do
    (-> rename_f show!)))

(defn a-rename-t [e]
  (do
    (-> rename_f_t show!)))

(defn a-delete-properties [e]
  (do
    (sim/delete_property (esc_text field_net))
     (text! field_state (pretty (deref net/state)))))

(def menus
 (let [a-open (action :handler a-open :name "Open" :tip "Open a file")
       a-copy (action :handler a-copy :name "Copy" :tip "Copy existing Petri Net")
       a-merge (action :handler a-merge :name "Merge" :tip "Merge two existing Petri Nets")
       a-delete (action :handler a-delete :name "Delete Net" :tip "Deletes the Petri Net specified in the \"Name of Net\" field" )
       a-rename (action :handler a-rename :name "Rename Vertex" :tip "Renames vertex specified in the Vertex label"
                        )
       a-rename-t (action :handler a-rename-t :name "Rename Transition" :tip "Renames transition specified in the Transition label")
       a-delete-properties (action :handler a-delete-properties :name "Delete Properties"
                                   :tip "Deletes all properties of the current specified net")
       a-save-as (action :handler a-save-as :name "Save As" :tip "Save the current file")
       a-n-steps (action :handler a-n-steps :name "Random Steps" :tip "Executes a number of random steps")
       ]
   (menubar
    :items [(menu :text "File" :items [a-open a-save-as])
            (menu :text "Edit" :items [a-n-steps a-copy a-merge a-rename a-rename-t a-delete-properties a-delete])])))



(display panel)

(config! f :menubar menus)

(-> f show!)




;(sim/eval_property "Net_A")

(sim/hash_name_map "Net_A")

(reset! net/state (read-string (slurp "test/petri/state2.txt")))

(net_alive "A_B")
@net/state
