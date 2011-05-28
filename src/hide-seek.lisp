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
  (values map)
)

(defun jinavojt-program (percept)
  (print (jinavojt-body-in-action (first percept)))
  (sleep 1)
  (decide (first percept) (rest percept))
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
		(otherwise (if (= 0 (random 2))
	          (prog1 'TURNLEFT (set-in-action body 'NO-TURN-RIGHT))
		  (prog1 'TURNRIGHT (set-in-action body 'NO-TURN-LEFT)))))
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
; (declarative - not tested)
(defun rotate-heading-left (heading)
  (cond
    ((equal heading '(0 1)) '(-1 0))
    ((equal heading '(1 0)) '(0 1))
    ((equal heading '(0 -1)) '(1 0))
    ((equal heading '(-1 0)) '(0 -1))))

; rotate 2d orientation (heading) to right
; (declarative - not tested)
(defun rotate-heading-right (heading)
  (cond
    ((equal heading '(0 1)) '(1 0))
    ((equal heading '(1 0)) '(0 -1))
    ((equal heading '(0 -1)) '(-1 0))
    ((equal heading '(-1 0)) '(0 1))))

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
        (loc (jinavojt-body-location body)))
       (what-is-on-loc (loc-on-left loc heading))))

(defun what-is-on-right? (body)
  (let ((map (jinavojt-body-map body))
        (heading (jinavojt-body-heading body))
        (loc (jinavojt-body-location body)))
       (what-is-on-loc (loc-on-right loc heading))))
