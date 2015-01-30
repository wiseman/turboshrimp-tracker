(ns com.lemondronor.turboshrimp-tracker
  (:require [clojure.core.incubator :as incu]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [com.lemondronor.turboshrimp :as ar-drone]
            [com.lemondronor.turboshrimp.at :as commands]
            [com.lemondronor.turboshrimp.pave :as pave]
            [com.lemondronor.turboshrimp.xuggler :as video]
            [com.lemondronor.turboshrimp-tracker.opencv :as vision]
            [seesaw.core :as seesaw])
  (:import [java.awt.event InputEvent KeyEvent MouseEvent]
           [java.awt Color Component Font Graphics Graphics2D]
           [java.awt.image BufferedImage]
           [java.net Socket]
           [javax.swing JPanel])
  (:gen-class))


(defn make-ui []
  (seesaw/frame
   :title "Turboshrimp Controller"
   :size [1280 :by 720]
   :content
   (seesaw/border-panel
    :id :video
    :border 0 :hgap 0 :vgap 0
    :size [1280 :by 720])))


;; This is our list of keyboard-based actions.
;;
;; Keys are WASD/QZ, with Shift to "strafe":
;; W / S  -  Move forward / Move backward
;; A / D  -  Yaw left / Yaw right
;; Q / Z  -  Climb / Descend
;; Shift-A / Shift D  - Move left / Move right.
;;
;; T / L  - Takeoff / Land
;; C / V  - Switch to forward- / down- facing camera.
;;
;; Each action is a key descriptor and a function.  The descriptor is
;; a keycode and optional modifiers.  All functions take at least one
;; argument--the drone.  Continuous functions (the default) are called
;; with a second argument, which is speed (0.0 - 1.0).  When their key
;; is pressed, they're called with the default speed; When their key
;; is released, they're called with a speed of 0.0.

(def key-actions
  ;; Checked in order as they appear here, so make sure to put keys
  ;; with mods first.
  [[{:code KeyEvent/VK_A :mod InputEvent/SHIFT_MASK} ar-drone/left]
   [{:code KeyEvent/VK_D :mod InputEvent/SHIFT_MASK} ar-drone/right]
   [{:code KeyEvent/VK_W} ar-drone/front]
   [{:code KeyEvent/VK_S} ar-drone/back]
   [{:code KeyEvent/VK_A} ar-drone/counter-clockwise]
   [{:code KeyEvent/VK_D} ar-drone/clockwise]
   [{:code KeyEvent/VK_Q} ar-drone/up]
   [{:code KeyEvent/VK_Z} ar-drone/down]
   [{:code KeyEvent/VK_T} ar-drone/takeoff :continuous? false]
   [{:code KeyEvent/VK_L} ar-drone/land :continuous? false]
   [{:code KeyEvent/VK_C} #(ar-drone/command % :switch-camera :forward)
    :continuous? false]
   [{:code KeyEvent/VK_V} #(ar-drone/command % :switch-camera :down)
    :continuous? false]])


(defn key-descriptor-matches?
  "Tests whether a key descriptor matches a KeyEvent."
  [descriptor ^KeyEvent evt]
  (and (= (:code descriptor) (.getKeyCode evt))
       (if-let [modifier (:mod descriptor)]
         (not (zero? (bit-and modifier (.getModifiers evt))))
         true)))


(defn find-key-action
  "Given a KeyEvent, returns the matching action."
  [evt actions]
  (if-let [e (some #(if (key-descriptor-matches? (first %) evt) %) actions)]
    (rest e)
    nil))


(def default-speed 0.5)


(defn key-controller
  "Makes a controller that mediates between key events and the drone."
  [model]
  (fn [^KeyEvent e]
    (let [id (.getID e)
          drone (:drone @model)]
      (when-let [action (find-key-action e key-actions)]
        (let [[action-fn & {:keys [continuous?]
                            :or {continuous? true}}] action]
          (let [action-args (cons
                             drone
                             (cond
                               (and (= id KeyEvent/KEY_PRESSED) continuous?)
                               (list default-speed)
                               (and (= id KeyEvent/KEY_RELEASED) continuous?)
                               (list 0.0)
                               :else
                               '()))]
            (when (or (= id KeyEvent/KEY_PRESSED)
                      (and (= id KeyEvent/KEY_RELEASED) continuous?))
              (println "Running" action-fn action-args)
              (apply action-fn action-args))))))))


(defn magnitude
  "Returns the magnitude of a vector."
  [v]
  (Math/sqrt (reduce + (map #(* % %) v))))


(defn deg2rad
  "Converts degrees to radians."
  [d]
  (* d (/ Math/PI 180.0)))


(defn draw-hud
  "Draws a simple HUD."
  [^JPanel view ^Graphics2D g drone]
  (let [navdata @(:navdata drone)
        batt (get-in navdata [:demo :battery-percentage])
        pitch (get-in navdata [:demo :theta])
        roll (get-in navdata [:demo :phi])
        hdg (get-in navdata [:magneto :heading :fusion-unwrapped])
        alt (get-in navdata [:demo :altitude])
        vel (get-in navdata [:demo :velocity])
        spd (magnitude (vals vel))
        lat (get-in navdata [:gps :lat-fuse])
        lon (get-in navdata [:gps :lon-fuse])]
    (.setFont g (Font. "Futura" Font/PLAIN 24))
    (.setColor g Color/WHITE)
    (if spd
      (.drawString g (format "SPD %.1f m/s" (/ spd 1000.0)) 10 30)
      (.drawString g "SPD unk" 10 30))
    (if alt
      (.drawString g (format "ALT %.1f m" (/ alt 1.0)) 10 60)
      (.drawString g "ALT unk" 10 60))
    (if hdg
      (.drawString g (str "HDG " (int hdg)) 10 90)
      (.drawString g "HDG unk" 10 90))
    (when (and lat lon)
      (.drawString g (format "LAT %.5f" lat) 10 120)
      (.drawString g (format "LON %.5f" lon) 10 150))
    (when batt
      (.drawString g (format "BATT %s %%" batt) 10 180))
    (when (and roll pitch)
      (let [h (.getHeight view)
            w (.getWidth view)
            h2 (/ h 2.0)
            ;; The value 1.65 was obtained by uh eyeballing.  Is sin
            ;; even the right thing to use here?
            ph (+ h2 (* 1.65 h (Math/sin (deg2rad pitch))))]
        (.rotate g (- (deg2rad roll)) (/ w 2.0) h2)
        (.drawLine g -500 ph (+ w 500) ph)
        (.rotate g (deg2rad roll) (/ w 2.0) h2)))))


(defn draw-selection
  "Draws a selection marquee on the view."
  [^JPanel view ^Graphics2D g selection]
  (when (and (:origin selection) (:cur selection))
    (let [[x1 y1] (:origin selection)
          [x2 y2] (:cur selection)
          w (- x2 x1)
          h (- y2 y1)]
      (.setColor g Color/WHITE)
      (.drawRect g x1 y1 w h))))


(defn view-point-xformer [^Component component ^BufferedImage video-frame]
  (if (and component video-frame)
    (let [cw (.getWidth component)
          ch (.getHeight component)
          iw (.getWidth video-frame)
          ih (.getHeight video-frame)
          xf (/ iw cw)
          yf (/ ih ch)]
      (fn [[x y]]
        [(* x xf) (* y yf)]))
    (fn [[x y]]
      [x y])))


(defn inv-view-point-xformer [^Component component ^BufferedImage video-frame]
  (if (and component video-frame)
    (let [cw (.getWidth component)
          ch (.getHeight component)
          iw (.getWidth video-frame)
          ih (.getHeight video-frame)
          xf (/ cw iw)
          yf (/ ch ih)]
      (fn [[x y]]
        [(* x xf) (* y yf)]))
    (fn [[x y]]
      [x y])))


(defn draw-tracker
  "Draws the tracking box."
  [^JPanel view ^Graphics2D g model]
  (if-let [tracked (:tracked (:tracker model))]
    (let [tracked (map (inv-view-point-xformer view (:video-frame model))
                       tracked)]
      (println "DRAWING TRACKER" (:tracked (:tracker model)))
      (.setColor g Color/RED)
      (let [x (int-array (map first tracked))
            y (int-array (map second tracked))]
        (.drawPolygon g x y 4)))
    (println "SKIPPING TRACKER")))


(defn draw-frame
  "Draws a video frame."
  [^JPanel view ^Graphics g ^BufferedImage image]
  (.drawImage g image 0 0 (.getWidth view) (.getHeight view) nil))


(defn get-video-input-stream [drone]
  (.getInputStream (Socket. (:hostname drone) ar-drone/default-video-port)))


(defn start-video-controller
  "Starts two threads: One reads video data from the drone, the other
  decodes it.  We keep these two processes asynchronous so we can drop
  frames if our decoding is too slow."
  [model]
  (let [fq (pave/make-frame-queue)]
    {:frame-reader
     (doto
         (Thread.
          (fn []
            (let [is (get-video-input-stream (:drone @model))]
              (loop [frame (pave/read-frame is)]
                (if frame
                  (pave/queue-frame fq frame)
                  (log/info "No frame?"))
                (recur (pave/read-frame is))))))
       (.setDaemon true)
       (.start))
     :frame-decoder
     (doto
         (Thread.
          (fn []
            (let [decoder (video/decoder)]
              (loop [frame (pave/pull-frame fq 1000)]
                (when frame
                  (let [^BufferedImage image (decoder frame)]
                    (swap! model assoc :video-frame image)))
                (recur (pave/pull-frame fq 1000))))))
       (.setDaemon true)
       (.start))}))


(defn make-view [ui]
  (let [^JPanel view (seesaw/select ui [:#video])]
    (fn render-model [model]
      (let [g (.getGraphics view)
            bi (BufferedImage.
                (.getWidth view)
                (.getHeight view)
                BufferedImage/TYPE_INT_ARGB)
            gbi (.createGraphics bi)
            video-frame (:video-frame model)]
        (draw-hud view gbi (:drone model))
        (when video-frame
          (vision/write video-frame)
          (seesaw/invoke-now
           (draw-frame view gbi video-frame)
           (draw-hud view gbi (:drone model))
           (draw-selection view gbi (:selection model))
           (println "WOO" model)
           (draw-tracker view gbi model)
           (.drawImage g bi 0 0 nil)))))))


(defn mouse-controller
  "Makes a controller function for mouse events."
  [model]
  (fn [^MouseEvent e]
    (let [id (.getID e)]
      (cond
        ;; Begin selection on MOUSE_PRESSED.
        (= id MouseEvent/MOUSE_PRESSED)
        (swap!
         model
         (fn [m]
           (-> m
               (assoc :tracker (vision/stop-tracker (:tracker model)))
               (assoc :selection {:origin [(.getX e) (.getY e)]}))))
        ;; Resize selection on MOUSE_DRAGGED.
        (= id MouseEvent/MOUSE_DRAGGED)
        (swap! model assoc-in [:selection :cur] [(.getX e) (.getY e)])
        ;; On MOUSE_RELEASED, initialize the video tracker and clear
        ;; the selection.
        (= id MouseEvent/MOUSE_RELEASED)
        (swap!
         model
         (fn [m]
           (println "STARTING TRACKER model=" m)
           (if-let [cur (:cur (:selection m))]
             (let [[x1 y1] (:origin (:selection m))
                   [x2 y2] cur
                   roi [[x1 y1] [(- x2 x1) (- y2 y1)]]]
               (-> m
                   (dissoc :selection)
                   (assoc
                    :tracker
                    (vision/start-tracker
                     (:video-frame m)
                     (map (view-point-xformer (.getComponent e) (:video-frame m))
                          roi)))))
             m)))
        :else nil))))


(defn add-sub-watch [ref keys id watch-fn]
  (add-watch
   ref
   id
   (fn [k r o n]
     (when (not (= (get-in o keys) (get-in n keys)))
       (watch-fn k r o n)))))


(defn update-tracker [model]
  (if-let [tracker (:tracker model)]
    (let [m (assoc
             model :tracker
             (vision/update-tracker tracker (:video-frame model)))]
      (println "UPDATED TRACKER" (:tracker m))
      (println (-> m :tracker :roi))
      (println (-> m :tracker :tracked))
      m)
    model))


(defn -main [& args]
  (vision/init)
  (let [ui (make-ui)
        drone (ar-drone/make-drone)
        model (atom {:drone drone
                     :video-frame nil
                     :navdata nil
                     :selection nil})
        view (make-view ui)]
    (-> ui seesaw/pack! seesaw/show!)
    (seesaw/listen ui :key (key-controller model))
    (let [mc (mouse-controller model)
          vid (seesaw/select ui [:#video])]
      (seesaw/listen vid :mouse mc)
      (seesaw/listen vid :mouse-motion mc))
    (add-watch model :render-view (fn [k r o n] (view n)))
    (add-sub-watch
     model [:video-frame] :tracker (fn [k r o n] (swap! r update-tracker)))
    (start-video-controller model)
    (ar-drone/connect! drone)
    (ar-drone/command drone :navdata-options commands/default-navdata-options)))
