(in-package :user-comment-web-service)

(defparameter *db-name* "user-comments")
(defparameter *comment-db-collection* "user-comments")
(defparameter *user-db-collection* "users")

;TODO: i dont know how to get all slots of a class obj, so add a slots member when convert obj and doc.
(defclass t_base ()
  ((id :accessor id :initarg :id :initform 0)
   (create-time :accessor :create-time :initarg create-time :initform 1);TODO: create and update generate 
   (update-time :accessor :update-time :initarg update-time :initform 1)
   (valid :accessor valid :initarg :valid :initform 1)
   (slots :reader slots :initform (list `id  `create-time `valid `update-time)))
  (:documentation "base entity class include basic properties and CURD functions"))

(defclass t_user (t_base)
  ((name  :initarg :name :accessor name :initform "")
   (type  :initarg :type :accessor type :initform "normal")
   (slots :reader slots :initform (list `id `name `type `create-time `valid `update-time))))

(defclass t_comment (t_base)
  ((type :accessor type :initarg :type)
   (content :accessor content :initarg :content)
   (origin-info :accessor origin-info :initarg :origin-info)
   (target :accessor target :initarg :target)
   (slots :reader slots :initform (list `id `content `type `target `origin-info `create-time `valid `update-time))))

(defparameter  *test-user* (make-instance `t_user :name "dao test" :type "normal" :valid 0))

(defun find-key (key mappings) (dolist (c mappings) (if (string-equal key (car c)) (return (cdr c))) ))
;TODO: entity class and  mongo db collection mapping
(defparameter *collection-mapping* (list (cons "t_user" "users") (cons "t_comment" "comment")))
(defun get-collection-name (class) (find-key class *collection-mapping*))

;TODO: entity class to symbol mapping 
(defparameter *symbol-mapping* (list (cons "t_user" `t_user) (cons "t_comment" `t_comment)))
(defun get-class-symbol (class) (find-key class *symbol-mapping*))

(defun object->doc (object)
  (let ((ret nil))
    (progn
      (dolist (slot (slots object))
      (progn
        (if (eq ret nil)
            (setf ret (cl-mongo:$ (string-downcase (string slot)) (slot-vaLue object slot)))
            (setf ret (cl-mongo:$ ret (cl-mongo:$ (string-downcase (string slot)) (slot-vaLue object slot)))))
        ))
      (cl-mongo:$ ret))
    ))


(defun doc->object (doc class)
  (let ((object (make-instance class)))
    (progn
      (mapcar
       (lambda (slot)
         (setf (slot-value object slot) (get-element (string-downcase (string slot)) doc)))
       (slots object))
      (block  nil object))))

(defun get-entity (class  &key (query ()))
  (let ((found-objects (cl-mongo:docs (cl-mongo:db.find (get-collection-name class) query :limit 1000))))
    (when found-objects
      (mapcar (lambda (doc) (doc->object doc (get-class-symbol class))) found-objects))
    ))

;TODO: simple id generate function
(defun get-new-id () (+ (* 1000 (get-internal-real-time)) (random 1000)))

(defun create-entity (new-object)
  (progn
    (setf (slot-vaLue new-object `id) (get-new-id))
    (cl-mongo:db.insert
     (get-collection-name (class-name (class-of new-object))) (object->doc new-object))
    ))

(defun update-entity (object)
  (let ((doc (object->doc object)))
    (cl-mongo:db.update
     (get-collection-name (class-of (class-of object)))
     (cl-mongo:$  "id" (id object)) doc)
    ))

(defun delete-entity (id class)
  (cl-mongo:db.delete
   (get-collection-name class)
   (car (cl-mongo:docs (cl-mongo:db.find (get-collection-name class) (cl-mongo:$ "id" id)))))) 


;;dao and bussiness level functions
(defun unique-index-on (field collection-name)
  (cl-mongo:db.ensure-index collection-name
                   (cl-mongo:$ field 1)
                   :unique t))

(defun setup-mongodb (&optional (db-name *db-name*))
 (cl-mongo:db.use db-name))






;; web servcie relatied functions
(defclass servcie ()
  ((db-name :reader db-name )
   (db-address :reader db-address)
   (db-port :reader db-port)))

(defun publish-static-content ()
  (push (hunchentoot:create-static-file-dispatcher-and-handler
         "/" "static/") hunchentoot:*dispatch-table*)
  )

;; DSL for our web pages
;; =====================

;; Here we grow a small domain-specific language for
;; creating dynamic web pages.

; Control the cl-who output format (default is XHTML, we
; want HTML5):
(setf (html-mode) :html5)

(defmacro standard-page ((&key title script) &body body)
  "The macro also accepts an optional script argument. When present, the
   script form is expected to expand into valid JavaScript."
  `(with-html-output-to-string
    (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
           (:head
            (:meta :charset "utf-8")
            (:title ,title)
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/retro.css")
            ,(when script
               `(:script :type "text/javascript"
                         (str ,script))))
           (:body
            (:div :id "header" ; Retro games header
                  (:img :src "/logo.jpg"
                        :alt "Commodore 64"
                        :class "logo")
                  (:span :class "strapline"
                         "Vote on your favourite Retro Game"))
            ,@body))))

;; HTML
;; ====

;; The functions responsible for generating the actual pages of our app go here.
;; We use the Hunchentoot macro define-easy-handler to automatically
;; push our uri to the dispatch table of the server and associate the
;; request with a function that will handle it.

(define-easy-handler (retro-games :uri "/retro-games") ()
  (standard-page (:title "Top Retro Games")
     (:h1 "Vote on your all time favourite retro games!")
     (:p "Missing a game? Make it available for votes " (:a :href "new-game" "here"))
     (:h2 "Current stand")
     (:div :id "chart" ; Used for CSS styling of the links.
       (:ol
	(dolist (game (games))
	 (htm
	  (:li (:a :href (format nil "vote?name=~a" (url-encode ; avoid injection attacks
                                                     (name game))) "Vote!")
	       (fmt "~A with ~d votes" (escape-string (name game))
                                       (votes game)))))))))

(define-easy-handler (new-game :uri "/new-game") ()
  (standard-page (:title "Add a new game"
                         :script (ps  ; client side validation
                                  (defvar add-form nil)
                                  (defun validate-game-name (evt)
                                    "For a more robust event handling
                                     mechanism you may want to consider
                                     a library (e.g. jQuery) that encapsulates
                                     all browser-specific quirks."
                                    (when (= (@ add-form name value) "")
                                      (chain evt (prevent-default))
                                      (alert "Please enter a name.")))
                                  (defun init ()
                                    (setf add-form (chain document
                                                          (get-element-by-id "addform")))
                                    (chain add-form
                                           (add-event-listener "submit" validate-game-name false)))
                                  (setf (chain window onload) init)))
     (:h1 "Add a new game to the chart")
     (:form :action "/game-added" :method "post" :id "addform"
            (:p "What is the name of the game?" (:br)
                (:input :type "text" :name "name" :class "txt"))
            (:p (:input :type "submit" :value "Add" :class "btn")))))

(define-easy-handler (game-added :uri "/game-added") (name)
  (unless (or (null name) (zerop (length name))) ; In case JavaScript is turned off.
    (add-game name))
  (redirect "/retro-games")) ; back to the front page

(define-easy-handler (vote :uri "/vote") (name)
  (when (game-stored? name)
    (vote-for (game-from-name name)))
  (redirect "/retro-games")) ; back to the front page


(defparameter *db-instance* nil
  "global db connection instance")

(defun start-server (&optional (port 8080) (mongo-address "127.0.0.1") (mongo-port 27017) (mongo-collection-name "user-comments"))
  (progn
   (cl-mongo:db.use mongo-collection-name)
   (if (null *acceptor*)
       (setf *acceptor* (make-instance 'easy-acceptor :port port)))
   (publish-static-content)
   (start *acceptor*)
   ))




(defun stop-server () ((stop *acceptor*)))

;(start-server "112.74.208.205")