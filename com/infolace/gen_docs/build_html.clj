(ns com.infolace.gen-docs.build-html
  (:refer-clojure :exclude [empty complement]) 
  (:import [java.util.jar JarFile]
           [java.io File FileWriter BufferedWriter])
  (:require [clojure.contrib.str-utils :as str])
  (:use [net.cgrand.enlive-html :exclude (deftemplate)]
        [clojure.contrib.pprint :only (cl-format)]
        [clojure.contrib.pprint.examples.json :only (print-json)]
        [clojure.contrib.pprint.utilities :only (prlabel)]
        [clojure.contrib.duck-streams :only (with-out-writer)]
        [clojure.contrib.shell-out :only (sh)]
        [com.infolace.gen-docs.collect-info :only (contrib-info)]
        [com.infolace.gen-docs.params
         :only (*output-directory* *src-dir* *src-root* *web-src-dir* *web-home*
                *external-doc-tmpdir* *param-dir* *page-title* *copyright*
                *build-json-index*)]))

(def *layout-file* "layout.html")
(def *master-toc-file* "master-toc.html")
(def *local-toc-file* "local-toc.html")

(def *overview-file* "overview.html")
(def *namespace-api-file* "namespace-api.html")
(def *sub-namespace-api-file* "sub-namespace-api.html")
(def *index-html-file* "api-index.html")
(def *index-json-file* "api-index.json")

(defn template-for
  "Get the actual filename corresponding to a template. We check in the project
specific directory first, then in the base template directory."
  [base] 
  (let [custom-template (str *param-dir* "/templates/" base)]
    (if (.exists (File. custom-template))
      custom-template
      (str "templates/" base))))

(def memo-nodes
     (memoize
      (fn [source]
        (map annotate (select (html-resource (template-for source)) [:body :> any-node])))))

(defmacro deffragment
  [name source args & forms]
  `(def ~name
        (fn ~args
          (let [nodes# (memo-nodes ~source)]
            (flatmap (transformation ~@forms) nodes#)))))  

(def memo-html-resource
     (memoize
      (fn [source]
        (html-resource (template-for source)))))

(defmacro deftemplate
  "A template returns a seq of string:
   Overridden from enlive to defer evaluation of the source until runtime.
   Builds in \"template-for\""
  [name source args & forms] 
  `(def ~name
        (comp emit* 
              (fn ~args (let [nodes# (memo-html-resource ~source)]
                          (flatmap (transformation ~@forms) nodes#))))))

;;; Thanks to Chouser for this regex
(defn expand-links 
  "Return a seq of nodes with links expanded into anchor tags."
  [s]
  (when s
    (for [x (str/re-partition #"(\w+://.*?)([.>]*(?: |$))" s)]
      (if (vector? x)
        [{:tag :a :attrs {:href (x 1)} :content [(x 1)]} (x 2)]
        x))))

(deftemplate page *layout-file*
  [title prefix master-toc local-toc page-content]
  [:html :head :title] (content title)
  [:link] #(apply (set-attr :href (str prefix (:href (:attrs %)))) [%])
  [:a#page-header] (content *page-title*)
  [:div#leftcolumn] (content master-toc)
  [:div#right-sidebar] (content local-toc)
  [:div#content-tag] (content page-content)
  [:div#copyright] (content *copyright*))

(defn create-page [output-file title prefix master-toc local-toc page-content]
  (with-out-writer (str *output-directory* output-file) 
    (print
     (apply str (page title prefix master-toc local-toc page-content)))))

(defn ns-html-file [ns-info]
  (str (:short-name ns-info) "-api.html"))

(defn overview-toc-data 
  [ns-info]
  (for [ns ns-info] [(:short-name ns) (:short-name ns)]))

(defn var-tag-name [ns v] (str (:full-name ns) "/" (:name v)))

(defn var-toc-entries 
  "Build the var-name, <a> tag pairs for the vars in ns"
  [ns]
  (for [v (:members ns)] [(:name v) (var-tag-name ns v)]))

(defn ns-toc-data [ns]
  (apply 
   vector 
   ["Overview" "toc0" (var-toc-entries ns)]
   (for [sub-ns (:subspaces ns)]
     [(:short-name sub-ns) (:short-name sub-ns) (var-toc-entries sub-ns)])))

(defn var-url
  "Return the relative URL of the anchored entry for a var on a namespace detail page"
  [ns v] (str (ns-html-file (:base-ns ns)) "#" (var-tag-name ns v)))

(defn add-ns-vars [ns]
  (clone-for [v (:members ns)]
             #(at % 
                [:a] (do->
                      (set-attr :href (var-url ns v))
                      (content (:name v))))))

(defn process-see-also
  "Take the variations on the see-also metadata and turn them into a canonical [link text] form"
  [see-also-seq]
  (map 
   #(cond
      (string? %) [% %] 
      (< (count %) 2) (repeat 2 %)
      :else %) 
   see-also-seq))

(defn see-also-links [ns]
  (if-let [see-also (seq (:see-also ns))]
    #(at %
       [:span.see-also-link] 
       (clone-for [[link text] (process-see-also (:see-also ns))]
         (fn [t] 
           (at t
             [:a] (do->
                   (set-attr :href link)
                   (content text))))))))

(defn external-doc-links [ns external-docs]
  (if-let [ns-docs (get external-docs (:short-name ns))]
    #(at %
       [:span#external-doc-link] 
       (clone-for [[link text] ns-docs]
         (fn [t] 
           (at t
             [:a] (do->
                   (set-attr :href (str "doc/" link))
                   (content text))))))))

(defn namespace-overview [ns template]
  (at template
    [:.namespace-tag] (set-attr :id (:short-name ns))
    [:.author] (content (or (:author ns) "unknown author"))
    [:a.api-link] 
    (do->
     (set-attr :href (ns-html-file ns))
     (content (:short-name ns)))
    [:pre.namespace-docstr] (html-content (expand-links (:doc ns)))
    [:span.var-link] (add-ns-vars ns)
    [:span.subspace] (if-let [subspaces (seq (:subspaces ns))]
                       (clone-for [s subspaces]
                         #(at % 
                            [:span.subspace-name] (content (:short-name s))
                            [:span.sub-var-link] (add-ns-vars s))))
    [:span.see-also] (see-also-links ns)))

(deffragment make-overview-content *overview-file* [ns-info]
  [:div.namespace-entry] (clone-for [ns ns-info] #(namespace-overview ns %)))

(deffragment make-master-toc *master-toc-file* [ns-info]
  [:ul#left-sidebar-list :li] (clone-for [ns ns-info]
                                #(at %
                                   [:a] (do->
                                         (set-attr :href (ns-html-file ns))
                                         (content (:short-name ns))))))

(deffragment make-local-toc *local-toc-file* [toc-data]
  [:.toc-section] (clone-for [[text tag entries] toc-data]
                    #(at %
                       [:a] (do->
                             (set-attr :href (str "#" tag))
                             (content text))
                       [:.toc-entry] (clone-for [[subtext subtag] entries]
                                       (fn [node]
                                         (at node
                                           [:a] (do->
                                                 (set-attr :href (str "#" subtag))
                                                 (content subtext))))))))

(defn make-overview [ns-info master-toc]
  (create-page "index.html"
               (str *page-title* " - Overview")
               nil
               master-toc
               (make-local-toc (overview-toc-data ns-info))
               (make-overview-content ns-info)))

;;; TODO: redo this so the usage parts can be styled
(defn var-usage [v]
  (if-let [arglists (:arglists v)]
    (cl-format nil
               "~<Usage: ~:i~@{~{(~a~{ ~a~})~}~^~:@_~}~:>~%"
               (map #(vector %1 %2) (repeat (:name v)) arglists))
    (if (= (:var-type v) "multimethod")
      "No usage documentation available")))

(def commit-hash-cache (ref {}))

(defn get-last-commit-hash
  "Gets the commit hash for the last commit that included this file. We
do this for source links so that we don't change them with every commit (unless that file
actually changed). This reduces the amount of random doc file changes that happen."
  [file]
  (dosync
   (if-let [hash (get @commit-hash-cache file)]
     hash
     (let [hash (.trim (sh "git" "rev-list" "--max-count=1" "HEAD" file 
                           :dir (str *src-dir* "/" *src-root*)))]
       (alter commit-hash-cache assoc file hash)
       hash))))

(defn web-src-file [file]
  (cl-format nil "~a~a/~a/~a" *web-src-dir* (get-last-commit-hash file) *src-root* file))

(defn var-src-link [v]
  (when (and (:file v) (:line v))
    (cl-format nil "~a#L~d" (web-src-file (:file v)) (:line v))))

;;; TODO: factor out var from namespace and sub-namespace into a separate template.
(defn var-details [ns v template]
  (at template 
    [:.var-tag] 
    (do->
     (set-attr :id (var-tag-name ns v))
     (content (:name v)))
    [:span.var-type] (content (:var-type v))
    [:pre.var-usage] (content (var-usage v))
    [:pre.var-docstr] (content (expand-links (:doc v)))
    [:a.var-source] (set-attr :href (var-src-link v))))

(declare common-namespace-api)

(deffragment render-sub-namespace-api *sub-namespace-api-file*
 [ns external-docs]
  (common-namespace-api ns external-docs))

(deffragment render-namespace-api *namespace-api-file*
 [ns external-docs]
  (common-namespace-api ns external-docs))

(defn make-ns-content [ns external-docs]
  (render-namespace-api ns external-docs))

(defn common-namespace-api [ns external-docs]
  (fn [node]
    (at node
      [:.namespace-name] (content (:short-name ns))
      [:span.author] (content (or (:author ns) "Unknown"))
      [:span.long-name] (content (:full-name ns))
      [:pre.namespace-docstr] (html-content (expand-links (:doc ns)))
      [:span.see-also] (see-also-links ns)
      [:span.external-doc] (external-doc-links ns external-docs)
      [:div.var-entry] (clone-for [v (:members ns)] #(var-details ns v %))
      [:div.sub-namespaces]
        (substitute (map #(render-sub-namespace-api % external-docs) (:subspaces ns))))))

(defn make-ns-page [ns master-toc external-docs]
  (create-page (ns-html-file ns)
               (str (:short-name ns) " API reference (" *page-title* ")")
               nil
               master-toc
               (make-local-toc (ns-toc-data ns))
               (make-ns-content ns external-docs)))

(defn vars-by-letter 
  "Produce a lazy seq of two-vectors containing the letters A-Z and Other with all the 
vars in ns-info that begin with that letter"
  [ns-info]
  (let [chars (conj (into [] (map #(str (char (+ 65 %))) (range 26))) "Other")
        var-map (apply merge-with conj 
                       (into {} (for [c chars] [c [] ]))
                       (for [v (mapcat #(for [v (:members %)] [v %]) ns-info)]
                         {(or (re-find #"[A-Z]" (-> v first :name .toUpperCase))
                              "Other")
                          v}))]
    (for [c chars] [c (sort-by #(-> % first :name .toUpperCase) (get var-map c))])))

(defn doc-prefix [v n]
  "Get a prefix of the doc string suitable for use in an index"
  (let [doc (:doc v)
        len (min (count doc) n)
        suffix (if (< len (count doc)) "..." ".")]
    (str (.replaceAll (.substring doc 0 len) "\n *" " ") suffix)))

(defn gen-index-line [v ns]
  (let [var-name (:name v)
        overhead (count var-name)
        short-name (:short-name ns)
        doc-len (+ 50 (min 0 (- 18 (count short-name))))]
    #(at %
         [:a] (do->
               (set-attr :href
                         (str (ns-html-file ns) "#" (:full-name ns) "/" (:name v)))
               (content (:name v)))
         [:.line-content] (content 
                           (cl-format nil "~vt~a~vt~a~vt~a~%"
                                      (- 29 overhead)
                                      (:var-type v) (- 43 overhead)
                                      short-name (- 62 overhead)
                                      (doc-prefix v doc-len))))))

;; TODO: skip entries for letters with no members
(deffragment make-index-content *index-html-file* [vars-by-letter]
  [:span.project-name-span] (content *page-title*)
  [:div.index-body] (clone-for [[letter vars] vars-by-letter]
                      #(at %
                         [:h2] (set-attr :id letter)
                         [:span.section-head] (content letter)
                         [:span.section-content] (clone-for [[v ns] vars]
                                                   (gen-index-line v ns)))))

(defn make-index-html [ns-info master-toc]
  (create-page *index-html-file*
               (str *page-title* " - Index")
               nil
               master-toc
               nil
               (make-index-content (vars-by-letter ns-info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Make the JSON index
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ns-file 
  "Get the file name (relative to src/ in clojure.contrib) where a namespace lives" 
  [ns]
  (let [ns-name (.replaceAll (:full-name ns) "-" "_")
        ns-file (.replaceAll ns-name "\\." "/")]
    (str ns-file ".clj")))

(defn namespace-index-info [ns]
  (assoc (select-keys ns [:doc :author])
    :name (:full-name ns)
    :wiki-url (str *web-home* (ns-html-file ns))
    :source-url (web-src-file (ns-file ns))))

(defn var-index-info [v ns]
  (assoc (select-keys v [:name :doc :author :arglists])
    :namespace (:full-name ns)
    :wiki-url (str *web-home* "/" (var-url ns v))
    :source-url (var-src-link v)))

(defn structured-index 
  "Create a structured index of all the reference information about contrib"
  [ns-info]
  (let [namespaces (concat ns-info (mapcat :subspaces ns-info))
        all-vars (mapcat #(for [v (:members %)] [v %]) namespaces)]
     {:namespaces (map namespace-index-info namespaces)
      :vars (map #(apply var-index-info %) all-vars)}))


(defn make-index-json
  "Generate a json formatted index file that can be consumed by other tools"
  [ns-info]
  (when *build-json-index*
    (with-out-writer (BufferedWriter. (FileWriter. (str *output-directory* *index-json-file*)))
                     (print-json (structured-index ns-info)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Wrap the external doc
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-href-prefix [node prefix]
  (at node
    [:a] #(apply (set-attr :href (str prefix (:href (:attrs %)))) [%])))
(defmacro select-content-text [node selectr]
  `(first (:content (first (select [~node] ~selectr)))))

(defn get-title [node]
  (or (select-content-text node [:title])
      (select-content-text node [:h1])))

(defn external-doc-map [v]
  (apply 
   merge-with concat
   (map 
    (partial apply assoc {}) 
    (for [[offset title :as elem] v]
      (let [[_ dir nm] (re-find #"(.*/)?([^/]*)\.html" offset)
            package (if dir (apply str (interpose "." (into [] (.split dir "/")))))]
        (if dir
          [package [elem]]
          [nm [elem]]))))))

(defn wrap-external-doc [staging-dir target-dir master-toc]
  (external-doc-map
   (doall ; force the side effect (generating the xml files
    (for [file (filter #(.isFile %) (file-seq (java.io.File. staging-dir)))]
      (let [source-path (.getAbsolutePath file)
            offset (.substring source-path (inc (.length staging-dir)))
            target-path (str target-dir "/" offset)
            page-content (first (html-resource (java.io.File. source-path)))
            title (get-title page-content)
            prefix (apply str (repeat (count (.split offset "/")) "../"))]
        (create-page target-path title prefix (add-href-prefix master-toc prefix) nil page-content)
        [offset title])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Put it all together
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-all-pages []
  (let [ns-info (contrib-info)
        master-toc (make-master-toc ns-info)
        external-docs (wrap-external-doc *external-doc-tmpdir* "doc" master-toc)]
    (make-overview ns-info master-toc)
    (doseq [ns ns-info]
      (make-ns-page ns master-toc external-docs))
    (make-index-html ns-info master-toc)
    (make-index-json ns-info)))

