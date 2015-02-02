(ns com.lemondronor.turboshrimp-tracker.opencv
  "Some simple wrappers around OpenCV functions."
  (:require [seesaw.core :as seesaw])
  (:import [java.awt.image BufferedImage DataBufferByte]
           [java.util ArrayList]
           [nu.pattern OpenCV]
           [org.opencv.core Core CvType Mat MatOfInt MatOfFloat MatOfPoint Point
            Rect RotatedRect Scalar TermCriteria]
           [org.opencv.imgproc Imgproc]
           [org.opencv.highgui Highgui VideoCapture]
           [org.opencv.video Video]))


(set! *warn-on-reflection* true)


(defn init
  "Need to call this before using OpenCV.

  Loads the native library we require."
  []
  (OpenCV/loadShared))


(defn mat-of-int [& args]
  (MatOfInt. (int-array args)))


(defn mat-of-float [& args]
  (MatOfFloat. (float-array args)))


(defn bounds->rect [bounds]
  (let [[[x1 y1] [w h]] bounds]
    (Rect. x1 y1 w h)))


(defn point->coords [^Point point]
  [(. point x) (. point y)])


(defn rect->bounds [^Rect rect]
  [[(. rect x) (. rect y)] [(. rect width) (. rect height)]])


(defn img->mat [^BufferedImage img]
  (let [^Mat mat (Mat. (.getHeight img) (.getWidth img) CvType/CV_8UC3)
        pixels (.getData ^DataBufferByte (.getDataBuffer (.getRaster img)))]
    (.put mat 0 0 pixels)
    mat))


(defn mat->img [^Mat m]
  (let [bufsiz (* (.channels m) (.cols m) (.rows m))
        b (byte-array bufsiz)
        img (BufferedImage.
             (.cols m) (.rows m)
             (if (> (.channels m) 1)
               BufferedImage/TYPE_3BYTE_BGR
               BufferedImage/TYPE_BYTE_GRAY))
        target-pixels (.getData (.getDataBuffer (.getRaster img)))]
    (.get m 0 0 b)
    (System/arraycopy b 0 target-pixels 0 (count b))
    img))


(defn imshow [m]
  (let [p (seesaw/frame
           :title "Image"
           :size [1280 :by 720]
           :content
           (seesaw/label
            :size [1280 :by 720]
            :icon (mat->img m)))]
    (-> p seesaw/pack! seesaw/show!)))


(defn make-roi-hist
  "Given an image and a ROI, returns a tracking histogram.

  Converts the image to HSV and returns a color histogram that can be
  used for meanshift or camshift tracking."
  [^BufferedImage img roi-bounds]
  (let [^Rect roi-rect (bounds->rect roi-bounds)
        ^Mat frame (img->mat img)
        ^Mat hsv-roi (Mat.)
        ^Mat mask (Mat.)
        ^Mat roi (Mat. frame roi-rect)
        ^Mat roi-hist (Mat.)]
    (Imgproc/cvtColor roi hsv-roi Imgproc/COLOR_BGR2HSV)
    ;; Creates a mask that contains any hue, saturation between
    ;; 10-255, value between 32-255.
    (Core/inRange
     hsv-roi
     (Scalar. 0.0 10.0 32.0)
     (Scalar. 360.0 255.0 250.0)
     mask)
    (Imgproc/calcHist
     [hsv-roi]
     (mat-of-int 0)
     mask
     roi-hist
     (mat-of-int 180)
     (mat-of-float 0.0 250.0))
    (Core/normalize roi-hist roi-hist 0 255 Core/NORM_MINMAX)
    roi-hist))


(defn start-tracker
  "Given an image and a ROI, returns a new tracker."
  [^BufferedImage img roi]
  {:roi roi
   :roi-hist (make-roi-hist img roi)})


(defn update-tracker
  "Updates a tracker with a new image."
  [tracker ^BufferedImage img]
  (let [^Mat frame (img->mat img)
        ^Mat hsv (Mat.)
        ^Mat back-projection (Mat.)]
    (Imgproc/cvtColor frame hsv Imgproc/COLOR_BGR2HSV)
    (Imgproc/calcBackProject
     [hsv]
     (mat-of-int 0)
     ^Mat (:roi-hist tracker)
     back-projection
     (mat-of-float 0.0 250.0)
     1.0)
    (let [^RotatedRect rot-rect
          (Video/CamShift
           back-projection
           ^Rect (bounds->rect (:roi tracker))
           (TermCriteria.
            (+ TermCriteria/COUNT TermCriteria/EPS) 10 1.0))
          points (make-array Point 4)]
      (.points rot-rect points)
      (assoc
       tracker
       :roi (rect->bounds (.boundingRect rot-rect))
       :tracked (map point->coords points)))))


(defn write [^BufferedImage img]
  (Highgui/imwrite "frame.png" (img->mat img)))
