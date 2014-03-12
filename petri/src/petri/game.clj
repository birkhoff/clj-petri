(ns petri.game
  (:use seesaw.core))


(require '[petri.petri_net_state :as net])
(require '[petri.simulator :as sim])
(require '[clojure.walk :only (prewalk-replace) :as walker])
(use '[petri.simulator :only (net_alive)])
(use '[petri.simulator :only (transition_alive)])
(use '[petri.simulator :only (non_empty)])


(def f (frame :title "Petri Netz Shooter 2014"))
(config! f :size [600 :by 800])

(defn display [content]
  (config! f :content content)
  content)

(def background_state (ref {:x 300 :y 0 :image "petri/galaxySmall.png"}))
(def player_state (ref {:x 300 :y 400 :u false :d false :r false :l false :shoot false :e ""}))
(def player_laser_state (ref {:ready [[-20 -20 1] [-20 -20 2] [-20 -20 3] [-20 -20 4]] :shot []}))
(def end? (ref false))


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
   :icon (clojure.java.io/resource "petri/spaceship.png")
   :bounds [300 400 100 100]))

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


(def game_panel
  (xyz-panel :items [l1 l2 l3 l4 player bg1 bg2]))

(display game_panel)

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


(defn player_fire_shot []
  (let [s @player_laser_state p @player_state]
    (if (not (empty? (:ready s)))
      (do
        (dosync
         (alter player_state assoc :shoot false)
         (alter player_laser_state assoc :ready (vec (rest (:ready s))))
         (alter player_laser_state assoc :shot (conj (:shot s) [(:x p) (:y p) (last (first (:ready s)))])))))))

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
      (Thread/sleep 15)
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

(listen f
        :key-pressed
        (fn [e]
          (do
            (dosync
             (alter player_state assoc :e   (.getKeyCode e)))
            (let [k (.getKeyCode e)]
              (cond
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

(-> f show!)

(do
 (dosync (ref-set end? true))
 (dosync (ref-set end? false)))


(future (bg_loop))
(future (player_loop))
(future (laser_loop))

@player_laser_state

