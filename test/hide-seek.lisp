(load "lisp-unit.fas")
(use-package :lisp-unit)

(load "src/load-aima.lisp")
(load "src/hide-seek.lisp")

(define-test hide-seek-basic
  (assert-equal 1 1 )
)

(run-environment (make-hs-world :max-steps 10))