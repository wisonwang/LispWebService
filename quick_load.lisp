(defvar import-system
  (list "cl-who" "hunchentoot" "parenscript" "cl-mongo" ))

;;
(dolist (sys import-system)
  (ql:quickload sys)
  )