(ns com.lemondronor.turboshrimp-tracker.opencv
  (:import [java.awt.image BufferedImage]
           [java.util ArrayList]
           [nu.pattern OpenCV]
           [org.opencv.core Core CvType Mat MatOfInt MatOfFloat MatOfPoint Point
            Rect RotatedRect Scalar TermCriteria]
           [org.opencv.imgproc Imgproc]
           [org.opencv.highgui Highgui VideoCapture]
           [org.opencv.video Video]))


(set! *warn-on-reflection* true)


(defn init []
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
  (let [mat (Mat. (.getHeight img) (.getWidth img) CvType/CV_8UC3)
        pixels (.getData (.getDataBuffer (.getRaster img)))]
    (.put mat 0 0 pixels)
    mat))


(defn make-roi-hist [^BufferedImage img roi-bounds]
  (let [^Rect roi-rect (bounds->rect roi-bounds)
        ^Mat frame (img->mat img)
        ^Mat hsv-roi (Mat.)
        ^Mat mask (Mat.)
        ^Mat roi (Mat. frame roi-rect)
        ^Mat roi-hist (Mat.)]
    (Imgproc/cvtColor roi hsv-roi Imgproc/COLOR_BGR2HSV)
    (Core/inRange
     hsv-roi
     (Scalar. 0.0 30.0 32.0)
     (Scalar. 180.0 255.0 255.0)
     mask)
    (Imgproc/calcHist
     [hsv-roi]
     (mat-of-int 0)
     mask
     roi-hist
     (mat-of-int 180)
     (mat-of-float 0.0 50.0))
    (Core/normalize roi-hist roi-hist 0 255 Core/NORM_MINMAX)
    roi-hist))


(defn start-tracker [^BufferedImage img roi]
  {:roi roi
   :roi-hist (make-roi-hist img roi)})


(defn update-tracker [tracker ^BufferedImage img]
  (let [^Mat frame (img->mat img)
        ^Mat hsv (Mat.)
        ^Mat back-projection (Mat.)]
    (Imgproc/cvtColor frame hsv Imgproc/COLOR_BGR2HSV)
    (Imgproc/calcBackProject
     [hsv]
     (mat-of-int 0)
     ^Mat (:roi-hist tracker)
     back-projection
     (mat-of-float 0.0 50.0)
     1.0)
    (let [^RotatedRect rot-rect
          (Video/CamShift
           back-projection
           ^Rect (bounds->rect (:roi tracker))
           (TermCriteria.
            (+ TermCriteria/COUNT TermCriteria/EPS) 80 1.0))
          points (make-array Point 4)]
      (.points rot-rect points)
      (println "WOO" rot-rect (.boundingRect rot-rect))
      (assoc
       tracker
       :roi (rect->bounds (.boundingRect rot-rect))
       :tracked (map point->coords points)))))


(defn stop-tracker [tracker]
  nil)


(defn write [^BufferedImage img]
  (Highgui/imwrite "frame.png" (img->mat img)))
