(ns gmark.tei-elems)

(defrecord ContainerEType [contains options])
(defrecord ChunkEType [contains line-start options])
(defrecord MultiChunkEType [contains options])

;;; no contains: what it can contain is determined by parent type
(defrecord InnerEType [begin end options])      
(defrecord EmptyEType [sym options])

(defprotocol EditableType
  (text-edit-as-child? [etype])
  (can-contain-text? [etype]))

(extend-type MultiChunkEType
  EditableType
  (text-edit-as-child? [_] false)
  (can-contain-text? [_] true))

(extend-type ContainerEType
  EditableType
  (text-edit-as-child? [_] false)
  (can-contain-text? [_] false))

(extend-type InnerEType
  EditableType
  (text-edit-as-child? [_] true)
  (can-contain-text? [_] true))

(extend-type ChunkEType
  EditableType
  (text-edit-as-child? [_] true)
  (can-contain-text? [_] true))

(extend-type EmptyEType
  EditableType
  (text-edit-as-child? [_] true)
  (can-contain-text? [_] false))

(defprotocol TextType
  (begin-text [etype])
  (end-text [etype])
  (attribute-default [etype])
  (to-gmark [etype elem tagtypes]))

(defprotocol SubChunkType
  (as-token-mapval [etype tag]))

(declare inner-to-text chunk-to-text multi-chunk-to-text)

(defn att-default [etype]
  (if-let [d-a (get-in etype [:options :attribute-default])]
      d-a
      (throw (#+cljs js/Error. #+clj IllegalStateException.
               "No default attribute defined"))))

(extend-type InnerEType
  TextType
  (begin-text [etype] (:begin etype))
  (end-text [etype] (:end etype))
  (attribute-default [etype] (att-default etype))
  (to-gmark [etype elem tagtypes] (inner-to-text etype elem tagtypes))
  SubChunkType
  (as-token-mapval [etype tag] [(begin-text etype)
                                {:tag tag
                                 :closing-tag (end-text etype)}]))

(extend-type MultiChunkEType
  TextType
  (begin-text [_] "\n")
  (end-text [_] "\n\n")
  (attribute-default [etype] (att-default etype))
  (to-gmark [etype elem tagtypes] (multi-chunk-to-text etype elem tagtypes)))

(extend-type ChunkEType
  TextType
  (begin-text [etype] (str "\n" (:line-start etype) " "))
  (end-text [etype] (if (pos? (count (:line-start etype)))
                      ""
                      "\n"))
  (attribute-default [etype] (att-default etype))
  (to-gmark [etype elem tagtypes] (chunk-to-text etype elem tagtypes)))

(extend-type EmptyEType
  TextType
  (begin-text [etype] (:sym etype))
  (end-text [_] nil)
  (attribute-default [etype] (att-default etype))
  (to-gmark [etype elem _] (begin-text etype))
  SubChunkType
  (as-token-mapval [etype tag] [(begin-text etype)
                                {:tag tag
                                 :no-content true}]))

(defn container-type [contains] (ContainerEType. contains))
(defn multi-chunk-type
  ([contains] (MultiChunkEType. contains {}))
  ([contains options] (MultiChunkEType. contains options)))
(defn chunk-type
  ([contains line-start] (ChunkEType. contains line-start {}))
  ([contains line-start options] (ChunkEType. contains line-start options)))
(defn inner-type
  ([begin end] (InnerEType. begin end {}))
  ([begin end options] (InnerEType. begin end options)))
(defn empty-type
  ([sym]  (EmptyEType. sym {}))
  ([sym options] (EmptyEType. sym options)))

;;; Functions for dealing with collections of tagtypes

(defn tagdesc-to-type [tagdesc]
  "Takes a map describing an element type and returns the correct type
  of Record."
  {:pre [(map? tagdesc)]}
  (case (:type tagdesc)
    :container (container-type (:contains tagdesc))
    :multi-chunk (multi-chunk-type (:contains tagdesc))
    :chunk (chunk-type (:contains tagdesc) (:line-token tagdesc))
    :inner (inner-type (:begin-token tagdesc) (:end-token tagdesc))
    :empty (empty-type (:begin-token tagdesc))))

(defn tagtypes [description-map]
  "Define tagtypes from general tag description map. The tag
  description is a map of maps where each type looks something 
  like this:

:rhyme {:type :inner
        :contains []
        :begin-token \"??\"
        :end-token \"??\"}
"
  (->>
    description-map
    (map
     (fn [[elem-name descrip]]
       [elem-name (tagdesc-to-type descrip)]))
    (into {})))

(defn chunk-line-starts-types [tagtypes]
  "Given a tagtypes map (ie. tags mapped to element type records),
returns a map, where keys are line start strings and values are
element types. This is a map to be used when parsing multi-chunk
elements. "
  (->> tagtypes
    (filter (fn [[_ etype]]
              (instance? ChunkEType etype)))
    (map (fn [[tag etype]]
           [(:line-start etype) tag]))
    (into {})))


(defn tagtypes-to-token-map [tagtypes]
  (->>
    tagtypes
    (filter (fn [[tag etype]]
              (or (instance? InnerEType etype)
                (instance? EmptyEType etype))))
    (map (fn [[tag etype]] (as-token-mapval etype tag)))
    (into {})))

;;; Function for dealing with attributes

(defn attributes-to-text [attrs]
  "Given a map of attributes, returns a string formatted like this: 

#[attr1:val1::attr2:val2]"
  (if (empty? attrs)
    ""
    (apply str
      (concat ["#["]
        (interpose "::"
          (map (fn [[att val]]
                 (str (name att) ":" val))
            attrs))
        ["]"]))))

(defn inner-to-text
  [etype elem tagtypes]
  (str
    (begin-text etype)
    (attributes-to-text (:attrs elem))
    (apply str (map
                 #(if (string? %)
                    %
                    (inner-to-text % tagtypes))
                 (:content elem)))
    (end-text etype)))

(declare elem-to-text)
(defn chunk-to-text
  [etype elem tagtypes]
  (str (apply str (map #(elem-to-text % tagtypes) (:content elem)))))

(defn child-chunk-to-text
  "To be called when parsing a multi-chunk. This function is different
  from the others because we don't know the type yet."
  [elem tagtypes]
  (let [etype ((:tag elem) tagtypes)]
    (str (begin-text etype) (chunk-to-text etype elem tagtypes))))

(defn multi-chunk-to-text [etype elem tagtypes]
  (str "\n"
    (apply str
      (map
        #(child-chunk-to-text % tagtypes)
        (filter map? (:content elem))))
    "\n\n"))

(defn elem-to-text [elem tagtypes]
  (cond
    (nil? elem)  nil
    (string? elem) elem

    (contains? tagtypes (:tag elem))
    (let [elem-type ((:tag elem) tagtypes)]
      (to-gmark elem-type elem tagtypes))

    true
    (throw
      (#+cljs js/Error. #+clj IllegalArgumentException.
        (str "unknown type: " (type elem))))))


(def tei
  {:body    (container-type [:div])
   :text    (container-type [:div])
   :caesura (empty-type "|")
   :rhyme   (inner-type "//" "//")
   :div     (container-type [:div :head :lg])
   :head    (chunk-type [:note] "*")
   :lg      (multi-chunk-type [:l])
   :l       (chunk-type [:rhyme :caesura :note] "-")
   :note    (inner-type "{" "}")})
