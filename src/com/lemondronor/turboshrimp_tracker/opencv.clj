(ns com.lemondronor.turboshrimp-tracker.opencv
  (:import [java.util ArrayList]
           [nu.pattern OpenCV]
           [org.opencv.core Core Mat MatOfInt MatOfFloat MatOfPoint Point Rect
            RotatedRect Scalar TermCriteria]
           [org.opencv.imgproc Imgproc]
           [org.opencv.highgui Highgui VideoCapture]
           [org.opencv.video Video]))


(defn tracker []
  {:roi-rect (atom nil)})


(defn img->mat [img mat]
  (let [pixels (.getdata (.getDataBuffer (.getRaster img)))]
    (.put mat 0 0 pixels)))


(defn update-tracker [tracker img]
  )

(defn write [img]
  (let [mat (Mat.)]
  (Highgui/imwrite "frame.png" (img->mat img mat))))
