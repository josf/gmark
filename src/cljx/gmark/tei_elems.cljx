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
  (end-text [etype]))

(extend-type InnerEType
  TextType
  (begin-text [etype] (:begin etype))
  (end-text [etype] (:end etype)))

(extend-type MultiChunkEType
  TextType
  (begin-text [_] "\n")
  (end-text [_] "\n\n"))

(extend-type ChunkEType
  TextType
  (begin-text [etype] (str "\n" (:line-start etype)))
  (end-text [etype] (if (pos? (count (:line-start etype)))
                      ""
                      "\n")))

(extend-type EmptyEType
  TextType
  (begin-text [etype] (:sym etype))
  (end-text [_] nil))

(defn container-type [contains] (ContainerEType. contains))
(defn multi-chunk-type [contains] (MultiChunkEType. contains))
(defn chunk-type [contains line-start] (ChunkEType. contains line-start))
(defn inner-type [begin end] (InnerEType. begin end))
(defn empty-type [sym]  (EmptyEType. sym))


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
       [elem-name
        (case (:type descrip)
          :container (container-type (:contains descrip))
          :multi-chunk (multi-chunk-type (:contains descrip))
          :chunk (chunk-type (:contains descrip) (:line-token "-"))
          :inner (inner-type (:begin-token descrip) (:end-token descrip))
          :empty (empty-type (:begin-token descrip)))]))
    (into {})))


(defn inner-to-text
  [elem tagtypes]
  (if (string? elem)
    elem
    (let [etype ((:tag elem) tagtypes)
          content (:content elem)]
      (if (= :empty (:type etype))
        (:begin-token etype)
        (str (:begin-token etype)
          (apply str (map
                       #(if (string? %)
                          %
                          (inner-to-text % tagtypes))
                       content))
          (:end-token etype))))))

(defn chunk-to-text
  "from-multi? arg determines whether the chunk has a parent element
  or not. If not, there is no line end prefix."
  ([elem tagtypes] (chunk-to-text elem tagtypes nil))
  ([elem tagtypes from-multi?]
   (let [token (when from-multi?
                 (str "\n"
                  (or
                    (get-in tagtypes [(:tag elem) :line-token])
                    "\n")            ; extra \n for bare paragraphs
                  " "))]                ; space after line-token
     (str token
       (apply str (map #(inner-to-text % tagtypes) (:content elem)))))))


(defn multi-chunk-to-text [elem tagtypes]
  (str "\n"
    (apply str (map #(chunk-to-text % tagtypes true) (:content elem)))
    "\n"))

(defn elem-to-text [elem tagtypes]
  (cond
    (nil? elem)  nil
    (string? elem) elem
    
    (map? elem)
    (let [etype (get-in tagtypes [(:tag elem) :type])]
      (cond
        (or (= etype :inner) (= etype :empty))
        (inner-to-text elem tagtypes)

        (= etype :chunk)
        (chunk-to-text elem tagtypes)

        (= etype :multi-chunk)
        (multi-chunk-to-text elem tagtypes)

        (= etype :container)
        (throw
          (#+cljs js/Error.
           #+clj IllegalArgumentException.
            "container elements cannot be rendered as text"))

        (nil? etype)
        (throw (#+cljs js/Error.
                #+clj IllegalArgumentException.
                (str "Strange arg. Tag is " (:tag elem))))

        true
        (throw
          #+cljs (js/Error. (str "No match " etype))
          #+clj (IllegalArgumentException. (str "No match " etype)))))

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
