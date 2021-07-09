(in-package :cl-user)
(defpackage cl-trello-clone.web
  (:use :cl
        :caveman2
        :cl-trello-clone.config
        :cl-trello-clone.view
        :cl-trello-clone.db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :cl-trello-clone.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
