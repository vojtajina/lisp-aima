(load "lib/aima/aima.fas")
(load "src/hide-seek-assignments.lisp")

(defstructure (jinavojt-agent
  (:include hs-agent
    (body (make-jinavojt-body))
    (program 'jinavojt-program)))
  "My agent for hide-seek world.")

(defstructure (jinavojt-body
  (:include hs-agent-body))
  in-action ; current state of the agent
  (map (create-map 10 10))
)

(defun create-map (x y)
  (setq map (make-array (list x y)))
  (loop for i from 0 below x do
    (progn (setf (aref map i 0) 'SEEN)
	   (setf (aref map i (- y 1)) 'SEEN)))

  (loop for i from 1 below (- y 1) do
    (progn (setf (aref map 0 i) 'SEEN)
	   (setf (aref map (- x 1) i) 'SEEN)))
  (values map)
)

(defun jinavojt-program (percept)
  (print (jinavojt-body-in-action (first percept)))
  (sleep 1)
  (setq body (first percept)
	perc (rest percept))
  ; todo: ignore learn, when last step was forw
  (learn body perc)
  (decide body perc)
)

; the main logic is here, this function is unit tested...
(defun decide (body percept)
  (setq first-seen (first percept))
  (if (null first-seen)
      ; can see list (couple of nils and then object
      (if (decide-see-person? percept)
	  ; can see person - ignore everything, go for it
	  (prog1 'FORW (set-in-action body NIL))

	  ; can't see any person
          (case (jinavojt-body-in-action body)
	    ; last step was forward, let's check left/right
	    ('CLEAN (if (= 0 (random 2))
	      (prog1 'TURNLEFT (set-in-action body 'BACK-RIGHT))
	      (prog1 'TURNRIGHT (set-in-action body 'BACK-LEFT))))
	    (otherwise (if (= 0 (random 3))
	      (prog1 'FORW (set-in-action body 'CLEAN))
	      (case (jinavojt-body-in-action body)
	        ('BACK-LEFT (prog1 'TURNLEFT (set-in-action body NIL)))
		('BACK-RIGHT (prog1 'TURNRIGHT (set-in-action body NIL)))
		(otherwise (prog1 'FORW (set-in-action body 'CLEAN))))))))

      ; facing to object directly
      (if (eq first-seen 'PERSON)
	  ; facing to person
	  'PYKY
	  (if (or (eq first-seen 'BUSH) (eq first-seen 'WALL))
	      ; facing to bush or wall
	      (case (jinavojt-body-in-action body)
		('NO-TURN-RIGHT 'TURNLEFT)
		('NO-TURN-LEFT 'TURNRIGHT)
		('BACK-LEFT (prog1 'TURNLEFT (set-in-action body NIL)))
		('BACK-RIGHT (prog1 'TURNRIGHT (set-in-action body NIL)))
		(otherwise (cond
		  ; todo: local var for what-is-on-* calls
		  ((equal (what-is-on-left? body) 'HOPE)
		   (prog1 'TURNLEFT (set-in-action body 'NO-TURN-RIGHT)))
		  ((equal (what-is-on-right? body) 'HOPE)
		   (prog1 'TURNRIGHT (set-in-action body 'NO-TURN-LEFT)))
		  ((equal (what-is-on-left? body) 'SEEN)
		   (prog1 'TURNRIGHT (set-in-action body 'NO-TURN-LEFT)))
		  ((equal (what-is-on-right? body) 'SEEN)
		   (prog1 'TURNLEFT (set-in-action body 'NO-TURN-RIGHT)))
		  ((= 0 (random 2))
	           (prog1 'TURNLEFT (set-in-action body 'NO-TURN-RIGHT)))
		  (T  (prog1 'TURNRIGHT (set-in-action body 'NO-TURN-LEFT))))))
	      ; facing to something else
	      (prog1 'FORW (set-in-action body 'CLEAN)))))
)

(defun set-in-action (body action)
  (setf (jinavojt-body-in-action body) action))

(defun decide-see-person? (percept)
  (let ((first-seen (first percept)))
  (if (null first-seen)
	    (decide-see-person? (rest percept))
            (if (eq first-seen 'PERSON) T NIL))))

; rotate 2d orientation (heading) to left
; wrapper for tleft (no mutation)
(defun rotate-heading-left (heading)
  (tleft (copy-xy heading)))

; rotate 2d orientation (heading) to right
; wrapper for tright (no mutation)
(defun rotate-heading-right (heading)
  (tright (copy-xy heading)))

; next location on my left
; location = current location
; heading = current orientation
(defun loc-on-left (location heading)
  (xy-add location (rotate-heading-left heading)))

; next location on my right
; location = current location
; heading = current orientation
(defun loc-on-right (location heading)
  (xy-add location (rotate-heading-right heading)))

(defun what-is-on-left? (body)
  (let ((map (jinavojt-body-map body))
        (heading (jinavojt-body-heading body))
        (loc (jinavojt-body-loc body)))
       (what-is-on-loc? (loc-on-left loc heading) map (rotate-heading-left heading))))

(defun what-is-on-right? (body)
  (let ((map (jinavojt-body-map body))
        (heading (jinavojt-body-heading body))
        (loc (jinavojt-body-loc body)))
       (what-is-on-loc? (loc-on-right loc heading) map (rotate-heading-right heading))))

; returns
; - NIL = unknown
; - HOPE = bush, not seen from this direction yet
; - SEEN = wall, seen bush
; todo: when bush, store directions already seen, when all, change into SEEN
(defun what-is-on-loc? (location map heading)
  (setq item (aref map (xy-x location) (xy-y location)))
  (cond
    ((null item) NIL)
    ((listp item)
     (if (find-equal heading item) 'HOPE 'SEEN))
    (T item)))

; find, but using equal for comparing
; todo: could we use member instead ?
(defun find-equal (item list)
  (cond ((null (first list)) NIL)
	((equal item (first list)) T)
	(T (find-equal item (rest list)))))

; remove, but using equal for comparing
(defun remove-equal (item list)
  (setq first-item (first list)
	rest-items (rest list))
  (cond
    ((null first-item) NIL)
    ((equal first-item item) (remove-equal item rest-items))
    (T (cons first-item (remove-equal item rest-items)))))
  
; remember first seen object
(defun learn (body percept)
  (setq map (jinavojt-body-map body)
	location (jinavojt-body-loc body)
	heading (jinavojt-body-heading body))
  (multiple-value-bind (object location)
                       (first-seen-object-and-loc location heading percept)
  (cond
    ((or (equal object 'WALL) (equal object 'PERSON))
     (setf (aref map (xy-x location) (xy-y location)) 'SEEN))
    ((equal object 'BUSH)
     (progn
     (let ((old-map (aref map (xy-x location) (xy-y location))))
          (when (not (equal old-map 'SEEN))
	        (setf (aref map (xy-x location) (xy-y location))
	        (remove-heading-from-list old-map heading)))))))))


; removes given heading (direction) from list and returns it
; if list NIL inits new one
; todo: should change empty list into SEEN
(defun remove-heading-from-list (list heading)
  (when (null list)
        (setq list (list '(0 1) '(0 -1) '(1 0) '(-1 0))))
  (remove-equal heading list))

(defun first-seen-object-and-loc (location heading percept)
  (setq first-seen (first percept))
  (if (null first-seen)
      (first-seen-object-and-loc (xy-add location heading) heading (rest percept))
      (values first-seen (xy-add location heading))))
