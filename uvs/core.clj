(ns uvs.core)

(comment
  A tool I like to use - http://runexec.github.com)

(defn inside-value
  "Returns the first inside value between two points of a string.
False on failure.
"
  [x strStart strEnd]
  (try
    (let
        [x (str x)
         after-start (second
                      (.split x
                              strStart))
         before-end (first
                     (.split after-start
                             strEnd))
         ret before-end]
      (if (or
           (empty? ret)
           (false? ret)
           (nil? ret))
        false
        ret))
    (catch Exception ex false)))

(defn var-name
  "Returns name of html tag. False on failure"
  [html-element]
  (inside-value html-element
                " name=\""
                "\""))
       
(defn var-id
  "Returns id of html tag. False on failure"
  [html-element]
  (inside-value html-element
                " id=\""
                "\""))

(defn var-class
  "Returns class value of html-element. False on failure."
  [html-element]
  (inside-value html-element
                " class=\""
                "\""))

(defn var-value
  "Returns value of html-element. False on failure."
  [html-element]
  (inside-value html-element
                " value=\""
                "\""))

(defn var-tag
  "Returns tag of html-element. False on failure."
  [html-element]
  (inside-value html-element
                "<"
                " "))

(defn var-type
  "Returns type value of html-element. Failure on failure."
  [html-element]
  (inside-value html-element
                " type=\""
                "\""))

(defn tag-attributes
  "retuns {:type :tag :class :id :name :value} of html element.
 If all values are false/nil => throws an AssertionError."
  [html-element]
  {:post (= true
            (let
                [h html-element]
              (some #(not
                      (false? %))
                    [(var-type h)
                     (var-tag h)
                     (var-class h)
                     (var-name h)
                     (var-id h)
                     (var-value h)])))}
  (let
      [h html-element]
    {:type (var-type h)
     :tag (var-tag h)
     :class (var-class h)
     :id (var-id h)
     :name (var-name h)
     :value (var-value h)}))

(defn tag-values
  "Returns false if the html element is not valid"
  [html-element]
  (try
    (tag-attributes html-element)
    (catch AssertionError ae false)))

(defn get-html-tags
  "Returns array of html tags from url"
  [url]
  (with-open
      [rdr (clojure.java.io/reader url)]
    (let
        [lines (apply str
                      (reduce conj
                              []
                              (line-seq rdr)))]
      (.split
       (.replace lines
                 ">"
                 ">\n")
       "\n"))))

(defn get-vars
  "Returns non false list of (tag-values) from url"
  [url]
  (filter #(not
            (false? %))
          (map tag-values
               (get-html-tags url))))


(defn get-unset-vars
  "Returns any var with :name not null and :value False"
  [url]
  (filter #(and
            (not
             (false? (:name %)))
             (false? (:value %)))
          (get-vars url)))

(defn get-hidden-vars
  "Returns any var with :type of value 'hidden'"
  [url]
  (filter #(= "hidden"
              (:type %))
          (get-vars url)))

(defn get-unset-input
  [url]
  (filter #(= "input"
              (:tag %))
          (get-unset-vars url)))

(defn get-report
  "Returns {:hidden :unset} vars properties from a url"
  [url]
  {:unset (get-unset-input url)
   :hidden (get-hidden-vars url)})

(defn print-report
  "Pretty prints (get-report url)"
  [url]
  (println "\n={ " url " }=")
  (let
      [report (get-report url)
       unset (:unset report)
       hidden (:hidden report)]
    (println "\n===== Unset vars =====")
    (doseq [u unset]
      (println "\n-- -- --"
               (:name u) ": type" (:type u))
      (doseq [v (sort u)
              :when (not
                     (or (= :name (first v))
                         (= :type (first v))))]
          (println v)))
    (println "\n===== Hidden vars =====")
    (doseq [h hidden]
      (println "\n-- -- --"
               (:name h) ": value" (:value h))
      (doseq [v (sort h)
              :when (not
                     (or (= :name (first v))
                         (= :value (first v))))]
        (println v)))))

(defn report-list
  "(print-report url) on collection of urls"
  [coll-url]
  {:pre (= true
           (coll? coll-url))}
  (doseq [url coll-url
          :let [url (str url)]]
    (print-report url)
    (println "\n___ waiting 5 seconds ___")
    (Thread/sleep 5000)))