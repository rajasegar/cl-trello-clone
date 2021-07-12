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
                   (:name "Doing" :id 2 :cards ((:id 3 :label "First Card" :list 2)
                                                (:id 4 :label "Second Card" :list 2)))))

;; Utils
(defun find-list (id)
  "find list from board based on id"
  (car (remove-if #'(lambda (item)
		 (if (= (getf item :id) (parse-integer id))
		     nil
		     t)) *board*)))

(defun find-card (list-id card-id)
  "Find card from list"
  (let* ((x-list (find-list list-id))
	(cards (getf x-list :cards)))
    (car (remove-if #'(lambda (item)
		   (if (= (getf item :id) (parse-integer card-id))
		       nil
		       t
		       )) cards))))

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

;; New card
(defroute ("/cards/new/:list-id" :method :POST) (&key list-id _parsed)
  (format t "~a~%" (cdr (assoc (concatenate 'string "label-" list-id) _parsed :test #'string=)))
  (let* ((label (cdr (assoc (concatenate 'string "label-" list-id) _parsed :test #'string=)))
         (card (list :label label :list-id list-id :id (get-universal-time)))
         (x-list (find-list list-id))
         (cards (getf x-list :cards)))
    (if (last cards)
        (push card (cdr (last cards)))
        (push card cards))
    (setf (getf x-list :cards) cards)
    (render #P"_new-card.html" (list :card card :list (find-list list-id)))))


;; Edit card
(defroute "/cards/edit/:list-id/:id" (&key list-id id)
  (render #P"_edit-card.html" (list
			       :id id
			       :list (find-list list-id)
			       :card (find-card list-id id))))

;; Cancel edit card
(defroute "/cards/cancel-edit/:list-id/:id" (&key list-id id)
  (render #P"_card.html" (list
			  :id id
			  :list (find-list list-id)
			  :card (find-card list-id id))))

;; Update card
(defroute ("/cards/:list-id/:id" :method :PUT) (&key list-id id _parsed)
  (let ((label (cdr (assoc "label" _parsed :test #'string=)))
        (card (find-card list-id id)))
    (setf (getf card :label) label)
    (render #P"_card.html" (list :card card))))

;; Delete card
(defroute ("/cards/:list-id/:id" :method :DELETE) (&key list-id id)
  (let* ((x-list (find-list list-id))
        (cards (remove-if #'(lambda (item)
                              (if (= (parse-integer id) (getf item :id))
                                  t
                                  nil)) (getf x-list :cards))))
    (setf (getf x-list :cards) cards)
    ""))

;; Move cards
(defroute ("/cards/move" :method :POST) (&key _parsed)
  (format t "~a~%" _parsed)
  (render #P"_board.html" (list :board (reverse *board*))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
