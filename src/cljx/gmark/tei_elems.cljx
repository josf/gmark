(ns gmark.tei-elems)

(defrecord ContainerEType [contains])
(defrecord ChunkEType [contains line-start])
(defrecord MultiChunkEType [contains])

;;; no contains: what it can contain is determined by parent type
(defrecord InnerEType [begin end])      
(defrecord EmptyEType [sym])

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
  (to-gmark [etype elem tagtypes]))

(declare inner-to-text chunk-to-text multi-chunk-to-text)

(extend-type InnerEType
  TextType
  (begin-text [etype] (:begin etype))
  (end-text [etype] (:end etype))
  (to-gmark [etype elem tagtypes] (inner-to-text etype elem tagtypes)))

(extend-type MultiChunkEType
  TextType
  (begin-text [_] "\n")
  (end-text [_] "\n\n")
  (to-gmark [etype elem tagtypes] (multi-chunk-to-text etype elem tagtypes)))

(extend-type ChunkEType
  TextType
  (begin-text [etype] (str "\n" (:line-start etype) " "))
  (end-text [etype] (if (pos? (count (:line-start etype)))
                      ""
                      "\n"))
  (to-gmark [etype elem tagtypes] (chunk-to-text etype elem tagtypes)))

(extend-type EmptyEType
  TextType
  (begin-text [etype] (:sym etype))
  (end-text [_] nil)
  (to-gmark [etype elem _] (begin-text etype)))

(defn container-type [contains] (ContainerEType. contains))
(defn multi-chunk-type [contains] (MultiChunkEType. contains))
(defn chunk-type [contains line-start] (ChunkEType. contains line-start))
(defn inner-type [begin end] (InnerEType. begin end))
(defn empty-type [sym]  (EmptyEType. sym))


(defn tagdesc-to-type [tagdesc]
  "Takes a map describing an element type and returns the correct type
  of Record."
  {:pre [(map? tagdesc)]}
  (case (:type tagdesc)
    :container (container-type (:contains tagdesc))
    :multi-chunk (multi-chunk-type (:contains tagdesc))
    :chunk (chunk-type (:contains tagdesc) (:line-token "-"))
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


(defn inner-to-text
  [etype elem tagtypes]
  (str (begin-text etype)
    (apply str (map
                 #(if (string? %)
                    %
                    (inner-to-text % tagtypes))
                 (:content elem)))
    (end-text etype)))

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
    (apply str (map #(child-chunk-to-text % tagtypes) (:content elem)))
    "\n\n"))

(defn elem-to-text [elem tagtypes]
  (cond
    (nil? elem)  nil
    (string? elem) elem

    ;; TODO all of this logic needs to use MultiChunkEType, InnerEType etc.)
    (contains? tagtypes (:tag elem))
    (let [elem-type ((:tag elem) tagtypes)]

      ;; TODO this is a mess! Should the element itself be an
      ;; additional argument to the to-gmark methods? Or should
      ;; elements "know" what kind of tagtype they are? THINK, man!
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
