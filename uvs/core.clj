(ns uvs.core
  (:require [clojure.string :as str]))

;; A tool I like to use - http://runexec.github.com

(defn inside-value
  [^String x 
   ^String start 
   ^String end]
  (if-not (and (.. x (contains start))
               (.. x (contains end)))
    false
    (let [make-pattern #(.. java.util.regex.Pattern (compile %))
          after-start (last
                       (str/split x 
                                  (make-pattern start)))
          before-end (first
                      (str/split after-start 
                                 (make-pattern end)))
          validator #(or (.. before-end 
                             (startsWith %))
                         (.. before-end
                             (endsWith %)))]
      (if (every? false? 
                  (map validator [">" "<"]))
        before-end
        false))))

(defn var-name
  "Returns name of html tag. False on failure"
  [_]
  (inside-value _ " name=\"" "\""))
       
(defn var-id
  "Returns id of html tag. False on failure"
  [_]
  (inside-value _ " id=\"" "\""))

(defn var-class
  "Returns class value of html tag. False on failure."
  [_]
  (inside-value _ " class=\"" "\""))

(defn var-value
  "Returns value value of html tag. False on failure."
  [_]
  (inside-value _ " value=\"" "\""))

(defn var-tag
  "Returns tag of html tag. False on failure."
  [_]
  (inside-value _ "<" " "))

(defn var-type
  "Returns type value of _. Failure on failure."
  [_]
  (inside-value _ " type=\"" "\""))

(defn tag-attributes
  "retuns {:type :tag :class :id :name :value} of html element.
 If all values are false/nil => throws an AssertionError."
  [_]
  (zipmap [:type :tag :class :id :name :value]
          [(var-type _)
           (var-tag _)
           (var-class _)
           (var-id _)
           (var-name _)
           (var-value _)]))

(defn has-value?
  "Returns false if the html element is not valid"
  [^String _]
  (boolean
   (:value 
    (tag-attributes _))))

(defn get-html-tags
  "Returns array of html tags from url. False on fail"
  [url]
  (with-open [rdr (clojure.java.io/reader url)]
    (let [_ (reduce conj 
                    [] 
                    (line-seq rdr))
          lines (->> _
                     (map #(.. (str %)
                               (replace ">" ">\n")
                               (replace "<" "\n<")
                               (replace "\n" "")))
                     (filter #(.. % (contains ">"))))]
      (if-not (seq? lines)
        false
        lines))))

(defn get-vars         
  [url]
  (map tag-attributes
       (filter has-value? 
               (get-html-tags url))))

(defn get-unset-vars
  [url]
  (filter #(= "" (:value %))
          (get-vars url)))


(defn get-hidden-vars
  "Returns any var with :type of value 'hidden'"
  [url]
  (filter #(= "hidden" (:type %))
          (get-vars url)))

(defn get-unset-input
  [url]
  (filter #(= "input" (:tag %))
          (get-unset-vars url)))

(defn get-report
  "Returns {:hidden :unset} vars properties from a url"
  [url]
    (clojure.pprint/pprint
     {:unset-input (get-unset-input url)
      :vars (get-vars url)
      :hidden-vars (get-hidden-vars url)}))
