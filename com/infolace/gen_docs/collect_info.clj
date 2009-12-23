(ns com.infolace.gen-docs.collect-info
  (:require [clojure.contrib.str-utils2 :as string])
  (:use [clojure.contrib.pprint.utilities :only [prlabel]]
        [com.infolace.gen-docs.params :only (*namespaces-to-document* *trim-prefix*)]))

;; Build a single structure representing all the info we care about concerning
;; namespaces and their members 
;;
;; Assumes that all the relevant namespaces have already been loaded

;; namespace: { :full-name :short-name :doc :author :members :subspaces :see-also}
;; vars: {:name :doc :arglists :var-type :file :line}

(defn remove-leading-whitespace 
  "Find out what the minimum leading whitespace is for a doc block and remove it.
We do this because lots of people indent their doc blocks to the indentation of the 
string, which looks nasty when you display it."
  [s]
  (when s
    (let [lines (.split s "\\n") 
          prefix-lens (map #(count (re-find #"^ *" %)) 
                           (filter #(not (= 0 (count %))) 
                                   (next lines)))
          min-prefix (when (seq prefix-lens) (apply min prefix-lens))
          regex (when min-prefix (apply str "^" (repeat min-prefix " ")))]
      (if regex
        (apply str (interpose "\n" (map #(.replaceAll % regex "") lines)))
        s))))


(defn create-line-structure
  "For each line in doc, decides if it's part of a code block or running text.
Creates a vector of maps in the form of {:type :code :strs []} where :type is
either :code or :text and :strs is a vector of the lines of text in that block."
  [l]
  (loop [ret []
         [line :as lines] l
         last-type :blank]
    (if lines
      (if (< 0 (count line))
        (let [ch (string/get line 0)
              type (if (or (Character/isWhitespace ch) (= \( ch))
                         :pre
                         :p)
              new-ret (if (= type last-type)
                        (update-in ret [(dec (count ret)) :content] conj line)
                        (conj ret {:tag type :attr {} :content [line]}))]
          (recur new-ret (next lines) type))
        (recur ret (next lines) :blank))
      ret)))

;;; Thanks to Chouser for this regex
(defn expand-links
  "Return a seq of nodes with links expanded into anchor tags."
  [s]
  (when s
    (for [x (string/partition s #"(\w+://.*?)([.>]*(?: |$))")]
      (if (vector? x)
        [{:tag :a :attrs {:href (x 1)} :content [(x 1)]} (x 2)]
        x))))

(defn wrap-code-blocks
  "Using structure created from create-line-structure, araps each block in <p>
or <pre> tags depending on its type."
  [data]
  (let [blocks (map (fn [d]
                      (if (= (:tag d) :p)
                        (assoc d :content [(expand-links (apply str (interpose " " (:content d))))])
                        (assoc d :content [(expand-links (apply str (interpose "\n" (:content d))))]))
                      )
                 data)]
    ;(apply str (interpose "\n" blocks))
    blocks
    ))

(defn clean-doc
  "Clean up the docstring by removing whitespace and putting markup around running
text and code blocks."
  [s]
  (when-let [clean (remove-leading-whitespace s)]
    (let [lines (.split clean "\\n")
          structure (create-line-structure lines)
          wrapped (wrap-code-blocks structure)
        ]
      (if wrapped
        wrapped
        {:tag :p :content ["this is text"]}
      ))))

(defn var-type 
  "Determing the type (var, function, macro) of a var from the metadata and
return it as a string."
  [v]
  (cond (:macro (meta v)) "macro"
        (= (:tag (meta v)) clojure.lang.MultiFn) "multimethod"
        (:arglists (meta v)) "function"
        :else "var"))

(defn vars-for-ns [ns]
  (for [v (sort-by (comp :name meta) (vals (ns-interns ns)))
        :when (and (or (:wiki-doc (meta v)) (:doc (meta v))) (not (:skip-wiki (meta v))) (not (:private (meta v))))] v))

(defn vars-info [ns]
  (for [v (vars-for-ns ns)] 
    (merge (select-keys (meta v) [:arglists :file :line])
           {:name (name (:name (meta v)))
            :doc (clean-doc (:doc (meta v))),
            :var-type (var-type v)})))

(defn add-vars [ns-info]
  (merge ns-info {:members (vars-info (:ns ns-info))}))

(defn relevant-namespaces []
  (filter #(not (:skip-wiki (meta %)))
          (map #(find-ns (symbol %)) 
               (filter #(some (fn [n] (or (= % n) (.startsWith % (str n "."))))
                              *namespaces-to-document*)
                       (sort (map #(name (ns-name %)) (all-ns)))))))

(defn trim-ns-name [s]
  (if (and *trim-prefix* (.startsWith s *trim-prefix*))
    (subs s (count *trim-prefix*))
    s))

(defn base-namespace
  "A nasty function that finds the shortest prefix namespace of this one"
  [ns]
  (first 
   (drop-while 
    (comp not identity) 
    (map #(let [ns-part (find-ns (symbol %))]
            (if (not (:skip-wiki (meta ns-part)))
              ns-part))
         (let [parts (seq (.split (name (ns-name ns)) "\\."))]
           (map #(apply str (interpose "." (take (inc %) parts)))
                (range 0 (count parts)))))))) ;; TODO first arg to range was 0 for contrib

(defn base-relevant-namespaces []
  (filter #(= % (base-namespace %)) (relevant-namespaces)))

(defn sub-namespaces 
  "Find the list of namespaces that are sub-namespaces of this one. That is they 
have the same prefix followed by a . and then more components"
  [ns]
  (let [pat (re-pattern (str (.replaceAll (name (ns-name ns)) "\\." "\\.") "\\..*"))]
    (sort-by
     #(name (ns-name %))
     (filter #(and (not (:skip-wiki (meta %))) (re-matches pat (name (ns-name %)))) (all-ns)))))

(defn ns-short-name [ns]
  (trim-ns-name (name (ns-name ns))))

(defn build-ns-entry [ns]
  {:full-name (name (ns-name ns)) :short-name (ns-short-name ns)
   :doc (clean-doc (:doc (meta ns))) :author (:author (meta ns))
   :see-also (:see-also (meta ns)) :ns ns})

(defn build-ns-list [nss]
  (sort-by :short-name (map add-vars (map build-ns-entry nss))))

(defn add-subspaces [info]
     (assoc info :subspaces 
            (filter #(or (:doc %) (seq (:members %)))
                    (build-ns-list (sub-namespaces (:ns info))))))

(defn add-base-ns-info [ns]
  (assoc ns
    :base-ns ns
    :subspaces (map #(assoc % :base-ns ns) (:subspaces ns))))

(defn contrib-info []
  (map add-base-ns-info (map add-subspaces
                             (build-ns-list (base-relevant-namespaces)))))
