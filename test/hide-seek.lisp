(load "lisp-unit.fas")
(use-package :lisp-unit)

(load "src/hide-seek.lisp")

;;; HELPERS

; Run given lambda couple of times (stress test)
(defun stress (times fn)
  (loop for i from 1 to times do (funcall fn)))

; Hack/fix - lisp-unit's fail does not fail the test...
(defun fail_ (msg)
  (assert-true nil msg))

;;; TESTS
(define-test should-pyky-when-heading-to-person
  (assert-equal 'PYKY
		(decide (make-jinavojt-body) (list 'PERSON))))

(define-test should-go-forward-when-see-person
  (assert-equal 'FORW
		(decide (make-jinavojt-body) (list NIL NIL 'PERSON)))
  (assert-equal 'FORW
		(decide (make-jinavojt-body) (list NIL 'PERSON)))
  (assert-equal 'FORW
		(decide (make-jinavojt-body) (list NIL NIL NIL NIL NIL NIL 'PERSON))))

(define-test should-go-forward-when-see-person-after-checking-turn
  (setq body (make-jinavojt-body))
  (decide body (list NIL NIL 'WALL)) ; should go forward
  (decide body (list NIL 'WALL)) ; should check left/right
  (assert-equal 'FORW (decide body (list NIL NIL NIL 'PERSON))))

(define-test should-turn-when-heading-to-obstacle
  (stress 10 (lambda ()
    (let ((decision (decide (make-jinavojt-body) (list 'BUSH))))
      (assert-true (or (eq decision 'TURNLEFT) (eq decision 'TURNRIGHT))))

    (let ((decision (decide (make-jinavojt-body) (list 'WALL))))
      (assert-true (or (eq decision 'TURNLEFT) (eq decision 'TURNRIGHT))))))
)

(define-test should-not-return-after-turning-from-obstacle
  (stress 10 (lambda ()
    (let ((body (make-jinavojt-body)))
      (case (decide body (list 'BUSH))
        ('TURNLEFT (assert-false (eq (decide body (list 'BUSH)) 'TURNRIGHT)))
        ('TURNRIGHT (assert-false (eq (decide body (list 'BUSH)) 'TURNLEFT)))
        (otherwise (fail_ "Should turn left/right when facing obstacle"))
      ))))
)
; todo: it should turn any direction next step after turning because of obstacle

(define-test should-check-left-or-right-every-forward-step
  (stress 10 (lambda ()
    (setq body (make-jinavojt-body))
    (decide body (list nil nil 'WALL)) ; should go forward
    (case (decide body (list nil nil 'WALL))
      ; nothing interested (no person), should turn back
      ('TURNLEFT (assert-equal 'TURNRIGHT (decide body (list nil nil 'WALL))))
      ('TURNRIGHT (assert-equal 'TURNLEFT (decide body (list nil nil 'WALL))))
      (otherwise (fail_ "Should check left or right after each forw step")))))
)

(define-test should-return-from-checking-when-obstacle
  (stress 10 (lambda ()
    (setq body (make-jinavojt-body))
    (decide body (list NIL NIL 'WALL)) ; should go forward
    (case (decide body (list NIL NIL 'WALL)) ; should check left/right
      ; should return back, there is an obstacle
      ('TURNLEFT (assert-equal 'TURNRIGHT (decide body (list 'WALL))))
      ('TURNRIGHT (assert-equal 'TURNLEFT (decide body (list 'WALL))))
      (otherwise (fail_ "Should check left or right after each forw step")))))
)

(define-test decide-see-person?
  (assert-true (decide-see-person? (list NIL 'PERSON)))
  (assert-true (decide-see-person? (list NIL NIL NIL 'PERSON)))
  (assert-false (decide-see-person? (list NIL NIL 'WALL)))
  (assert-false (decide-see-person? (list NIL 'BUSH)))
  (assert-false (decide-see-person? (list NIL NIL NIL NIL NIL NIL NIL 'WALL)))
)

;;; FEATURES
; - when see person, always go to pyky him
; - if obstacle, turn left/right (and do not turn back)
; - every forward step - check left/right (and turn back if no person)

; let's not turn back when checking (change direction) - sometimes
