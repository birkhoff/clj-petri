(ns petri.game
  (:use seesaw.core))

(require '[petri.petri_net_state :as net])
(require '[petri.simulator :as sim])
(require '[clojure.walk :only (prewalk-replace) :as walker])
(use '[petri.simulator :only (net_alive)])
(use '[petri.simulator :only (transition_alive)])
(use '[petri.simulator :only (non_empty)])


(def f (frame :title "Petri Netz Shooter 2014"
              ;:on-close :exit
              ))
(config! f :size [1200 :by 800])

(defn display [content]
  (config! f :content content)
  content)

(def background_state (ref {:x 300 :y 0 :image "petri/galaxySmall.png"}))
(def player_state (ref {:x 300 :y 400 :u false :d false :r false :l false :shoot false
                        :e "" :net nil :net_i nil :trans nil :trans_i nil :sprite 0}))
(def player_laser_state (ref {:ready [[-20 -20 1] [-20 -20 2] [-20 -20 3] [-20 -20 4]] :shot []}))
(def enemy_state (ref {:1 [200 -600 100] :2 [400 -200 100] :3 [300 -1000 100]}))
(def end? (ref false))

; game objects:

(def bg1
  (label
   :icon (clojure.java.io/resource  (:image @background_state))
   :bounds [0 0 650 800]))


(def bg2
  (label   
   :icon (clojure.java.io/resource  (:image @background_state))
   :bounds [0 800 650 800]))

(def player
  (label
   :icon (clojure.java.io/resource "petri/nyancat.png")
   :bounds [300 400 100 143]))

(def bow
  (label
   :icon (clojure.java.io/resource "petri/rainbow.png")
   :bounds [310 450 67 450]))

;labels for "enemies" 

(def e1
  (label
   :icon (clojure.java.io/resource "petri/tac.png")
   :bounds [200 -600 80 134]))

(def e2
  (label
   :icon (clojure.java.io/resource "petri/tac.png")
   :bounds [400 -200 80 134]))

(def e3
  (label
   :icon (clojure.java.io/resource "petri/tac.png")
   :bounds [300 -1000 80 134]))
;laser labels:

(def l1
  (label
   :icon (clojure.java.io/resource "petri/laserBlueGlow2.png")
   :bounds [-20 200 15 40]))

(def l2
  (label
   :icon (clojure.java.io/resource "petri/laserBlueGlow2.png")
   :bounds [-20 200 15 40]))

(def l3
  (label
   :icon (clojure.java.io/resource "petri/laserBlueGlow2.png")
   :bounds [-20 200 15 40]))

(def l4
  (label
   :icon (clojure.java.io/resource "petri/laserBlueGlow2.png")
   :bounds [-20 200 15 40]))


; HUD objects:

(def current_net
  (label :text "None Selected" :bounds [680 40 200 30]))

(def vertices
  (text :text "nil"
         :multi-line? true :editable? false :wrap-lines? false
         :bounds [680 110 200 150]))

(def transitions
  (text :text "nil"
         :multi-line? true :editable? false :wrap-lines? false
         :bounds [680 355 200 150]))

(def current_trans_name
  (label :text "None Selected" :bounds [680 520 200 30]))

(def current_trans
   (text :text "q/w     to switch nets\n\na/s      to switch transitions\n\nspace  to fire transition\n\nESC     to Start/Pause"
         :multi-line? true :editable? false :wrap-lines? false
         :bounds [680 520 200 200]))

(def properties
   (text :text "Shoot Enemies to evaluate Properties"
         :multi-line? true :editable? false :wrap-lines? false
         :bounds [910 110 250 345]))


(def game_panel
  (xyz-panel :items [(label :text "CURRENT NET:" :bounds [660 15 100 30])
                     (label :text "VERTICES:" :bounds [660 75 100 30])
                     (label :text "TRANSITIONS:" :bounds [660 270 100 30])
                     (label :text "CURRENT TRANSITION:" :bounds [660 490 200 30])
                     (label :text "PROPERTIES:" :bounds [900 75 200 30])
                     current_net
                     current_trans_name
                     (scrollable properties :bounds  [910 110 250 345])
                     (scrollable  vertices :bounds [680 110 200 150])
                     (scrollable  transitions :bounds [680 305 200 150])
                     (scrollable  current_trans :bounds [680 570 200 200])
                     (label :background :lightgrey :bounds [650 0 550 800])
                     player bow  e1 e2 e3 l1 l2 l3 l4 bg1 bg2
                     (label :background "#004478" :bounds [0 0 600 800])]))

(display game_panel)




(import '(java.applet Applet)
        '(java.io File)
    '(java.net URL))
(defn play-url [url-string]
  (.play (Applet/newAudioClip  url-string)))


(defn pretty_v [input]
  (clojure.string/replace
   (clojure.string/replace
     (clojure.string/replace (str input)
                             #"(\],|\])" "]\n")  #"\{|\}" " ") #"\#" ""))

(defn bg_loop []
  (if @end?
    nil
    (do
      (dosync
       (if (< 800 (:y @background_state))
         (alter background_state assoc :y 0)
         (alter background_state assoc :y (+ 2 (:y @background_state)))))
      (move! bg1 :to [:*, (:y @background_state)])
      (move! bg2 :to [:*, (- (:y @background_state) 800)])
      (Thread/sleep 15)
      (recur))))

(defn update_hud_vertices [n]
   (text! vertices (pretty_v (:vertices (n @net/state)))))

(defn player_fire_shot []
  (let [s @player_laser_state p @player_state]
    (if (not (nil? (:trans p)))
        (if (sim/transition_alive (:net p) (first (:trans p)))
        (if (not (empty? (:ready s)))
          (do
            
            (dosync
             (sim/state_fire_transition (:net p) (first (:trans p)))
             (update_hud_vertices (:net p))
             (alter player_state assoc :shoot false)
             (alter player_laser_state assoc :ready (vec (rest (:ready s))))
             (alter player_laser_state assoc :shot (conj (:shot s) [(+ 40(:x p)) (:y p) (last (first (:ready s)))])))
            (future (play-url (clojure.java.io/resource "petri/laser.wav")))))
        (dosync (alter player_state assoc :shoot false))))))

(conj (vec '(1 2)) 2)

(defn player_loop []
  (if @end?
    nil
    (do
      (let [p @player_state]
        (do
          (if (:l p) (dosync (alter player_state assoc :x (- (:x p) 5))))
          (if (:r p) (dosync (alter player_state assoc :x (+ (:x p) 5))))
          (if (:d p) (dosync (alter player_state assoc :y (+ (:y p) 5))))
          (if (:u p) (dosync (alter player_state assoc :y (- (:y p) 5)))))
          (if (:shoot p) (player_fire_shot)))
      (move! player :to [(:x @player_state), (:y @player_state)])
      (move! bow :to [(+ 10(:x @player_state)), (+ 100 (:y @player_state))])
      (Thread/sleep 15)
      (recur))))


(defn enemy_anim [k]
  (do
    (config! e1 :valign k)
    (config! e2 :valign k)
    (config! e3 :valign k)))

(defn player_anim []
  (if @end?
    nil
    (do
      (dosync
       (let [sp (:sprite @player_state)]
         (alter player_state assoc :sprite (mod (inc sp) 3))
         (cond
          (= 0 sp) (do
                     (config! player :valign :bottom)
                     (config! bow :valign :bottom)
                     (enemy_anim :bottom))
          (= 1 sp) (do
                     (config! player :valign :center)
                     (config! bow :valign :center)
                     (enemy_anim :center))
          (= 2 sp) (do
                     (config! player :valign :top)
                     (config! bow :valign :top)
                     (enemy_anim :top)))))
      (Thread/sleep 100)
      (recur))))


(defn move_laser [[x y n]]
  (cond  (= n 1) (move! l1 :to [x y])
         (= n 2) (move! l2 :to [x y])
         (= n 3) (move! l3 :to [x y])
         :else   (move! l4 :to [x y])))

(defn laser_loop []
  (if @end?
    nil
    (do
      (let [l @player_laser_state]
        (dosync
         (alter player_laser_state assoc :shot
                (vec (map (fn [[x y n]] [x (- y 10) n])
                          (filter (fn [[x y n]] (< -40 y)) (:shot l)))))
         (alter player_laser_state assoc :ready
                (reduce conj (:ready l) (vec  (filter (fn [[x y n]] (>= -40 y)) (:shot l)))))))

      (doall (for [x (:shot @player_laser_state)]
          (move_laser x)))
      
      (Thread/sleep 15)
      (recur))))

(defn move_enemies []
  (let [e @enemy_state]
    (do
      (move! e1 :to [(first (:1 e)) (second (:1 e))])
      (move! e2 :to [(first (:2 e)) (second (:2 e))])
      (move! e3 :to [(first (:3 e)) (second (:3 e))]))))

(defn enemy_loop []
  (if @end?
    nil
    (do
      (dosync
       (let [e @enemy_state]
         (ref-set enemy_state
                  (reduce merge
                          (map
                           (fn [[i [x y hp]]] (if (< y 900)
                                               {i [x (+ y 5) hp]}
                                               {i [(+ 30 (rand-int 470))
                                                   (- -100 (rand-int 800)) hp]})) e))))
       (move_enemies))
      (Thread/sleep 15)
      (recur))))


(defn abs [x]
  (if (<= 0 x)
    x
    (- x)))


;; currently not in use
(defn eval_props []
  (let [n (:net @player_state)]
      (text! properties
             (clojure.string/replace (pretty_v
                (apply str
                       (for [p (walker/prewalk-replace (clojure.set/map-invert (sim/hash_name_map n)) (sim/eval_properties n))]
                         (str p)))) #"(:type|:args)" ""))))

(defn collision_loop []
  (if @end?
    nil
    (do
      (dosync
       (doall
        (for [[x_1 y_1 n] (:shot @player_laser_state)]
          (doall (for [[k [x_2 y_2 hp]]  @enemy_state]
                   (if (and
                        (> 90 (abs (- (- y_2 50) y_1)))
                        (> 90 (abs (- (+ 50 x_2) (+ 7 x_1)))))
                     (dosync
                      (eval_props)
                      (alter enemy_state assoc k [-200 y_2 hp])
                      (alter player_laser_state assoc :shot
                             (vec (map (fn [[x y i]] (if (and (= x x_1)
                                                             (= y y_1)
                                                             (= i n))
                                                      [-200 -10 i]
                                                      [x y i])) (:shot @player_laser_state)))))))))))
      (Thread/sleep 20)
      (recur))))




(for [[x y hp] (:ready @player_laser_state)]
  [x y hp])

(defn pretty_t [input]
  (clojure.string/replace
   (clojure.string/replace input "[[" " [") "]]" "]"))

(defn update_hud_net [n]
  (do
    (config! current_net :text (str n))
    (text! transitions (pretty_v (:transitions (n @net/state))))
    (text! vertices (pretty_v (:vertices (n @net/state))))
    (config! current_trans_name :text "None Selected")
    (text! current_trans "")))

(defn player_switch_net [i]
  (dosync
   (let [p @player_state]
        (if (nil? (:net_i p) )
          (if-let [n (first (first @net/state))]
            (do
              (alter player_state assoc :net n)
              (alter player_state assoc :net_i 0)
              (update_hud_net n)))
          
          (let [x (mod (+ i (:net_i p)) (count @net/state))
                n (first (nth (vec @net/state) x))]
            (do (alter player_state assoc :net_i x)
                (alter player_state assoc :net n)
                (alter player_state assoc :trans nil)
                (alter player_state assoc :trans_i nil)
                (update_hud_net n)))))))


(defn pretty_trans [net t]
  (let [ts (:transitions (net @net/state))
        vs (:vertices (net @net/state))]
    (vec (map (fn [[t v x]] [(first (v vs)) x])  t))))




(defn update_hud_trans [n t]
  (do
    (config! current_trans_name :text (str t))
    (text! current_trans
           (str "COSTS:\n"(pretty_v
              (pretty_t
               (str 
                (pretty_trans n (net/edges_to_transition_hash (n @net/state) (first t))))))
                "\nGAINS:\n"
                (pretty_t
               (str 
                (pretty_trans n (net/edges_from_transition_hash (n @net/state) (first t)))))))))

(defn player_switch_trans [i]
  (dosync
   (let [p @player_state]
     (if-let [net (:net p)]
       (if (nil? (:trans p) )
         (if-let [t  (first (:transitions (net @net/state)))]
             (do
                (alter player_state assoc :trans t)
                (alter player_state assoc :trans_i 0)
                (update_hud_trans (:net p) t)))
           (do
             (let [ts (:transitions (net @net/state))
                   x  (mod (+ i (:trans_i p)) (count ts))
                   t  (nth (vec ts) x)]
                 (alter player_state assoc :trans_i x)
                 (alter player_state assoc :trans t)
                 (update_hud_trans (:net p) t))))))))



(defn pause []
  (if @end?  
    (do
      (dosync (ref-set end? false))
      (future (bg_loop))
      (future (player_loop))
      (future (laser_loop))
      (future (enemy_loop))
      (future (player_anim))
      (future (collision_loop))
      ;(future (play-url (clojure.java.io/resource
      ;"petri/nyan_cat.mid")))
      )
    (dosync (ref-set end? true))))

(listen f
        :key-pressed
        (fn [e]
          (do
            (dosync
             (alter player_state assoc :e   (.getKeyCode e)))
            (let [k (.getKeyCode e)]
              (cond
               (= 27 k) (pause)
               (= 81 k) (player_switch_net 1)
               (= 87 k) (player_switch_net -1)
               (= 65 k) (player_switch_trans 1)
               (= 83 k) (player_switch_trans -1)
               (= 32 k) (dosync (alter player_state assoc :shoot true))
               (= 37 k) (dosync (alter player_state assoc :l true))
               (= 38 k) (dosync (alter player_state assoc :u true))
               (= 39 k) (dosync (alter player_state assoc :r true))
               (= 40 k) (dosync (alter player_state assoc :d true)))))))







(listen f
        :key-released
        (fn [e]
          (let [k (.getKeyCode e)]
              (cond
               (= 32 k) (dosync (alter player_state assoc :shoot false))
               (= 37 k) (dosync (alter player_state assoc :l false))
               (= 38 k) (dosync (alter player_state assoc :u false))
               (= 39 k) (dosync (alter player_state assoc :r false))
               (= 40 k) (dosync (alter player_state assoc :d false))))))


@player_state

;(-> f show!)

(defn game []
  (do
    (-> f show!)
    ;(future (bg_loop))
    ;(future (player_loop))
    ;(future (laser_loop))
    ;(future (enemy_loop))
    ;(future (player_anim))
    ;(future (collision_loop))
    ;(future (play-url (clojure.java.io/resource "petri/nyan_cat.mid")))
    (dosync (ref-set end? true))
    (request-focus! f)))






;(game)

;@player_state


;(reset! net/state (read-string (slurp "net_state.txt")))



