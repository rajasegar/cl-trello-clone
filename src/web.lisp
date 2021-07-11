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

;; Boards
(defvar *board* '((:name "To Do" :id 1 :cards ((:id 1 :label "First Card" :list 1)
                                                (:id 2 :label "Second Card" :list 1)))
                   (:name "Doing" :id 2 :cards ((:id 1 :label "First Card" :list 2)
                                                (:id 2 :label "Second Card" :list 2)))))

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html" (list :board *board*)))

(defroute "/lists/add" ()
  (render #P"_add-list.html"))

(defroute "/lists/cancel" ()
  (render #P"_new-list.html"))

(defroute ("/lists" :method :POST) (&key _parsed)
  (let ((name (cdr (assoc "name" _parsed :test #'string=))))
    (push (list :name name :id (+ 1 (length *board*)) :cards ()) *board*)
    (render #P"_board.html" (list :board (reverse *board*)))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
