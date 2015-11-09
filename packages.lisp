(in-package :cl-user)

(defpackage :user-comment-web-service
  (:use :cl :cl-who :hunchentoot :parenscript :cl-mongo)
  (:local-nicknames (:h :hunchentoot) )
  (:export
   ;class
   :t_user
   :t_comment
   :user_comment_service


   :start-service
   :stop-service
   :restart-service
   )
  )