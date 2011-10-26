#!/usr/bin/env racket
#lang racket

(require rackunit)
(require rackunit/text-ui)

(require (only-in "manufactoria-dot.rkt"
                  to-dot))
;; (require/expose "manufactoria-dot.rkt" (token-id))

;; (define manufactoria-dot-internals-test-suite
;;   (test-suite "Tests for manufactoria-dot.rkt internals"
;;               (test-case "token-id"
;;                          (let ((token (list '(1 2) 'basket null)))
;;                            (check-equal? (token-id token) 'basket-1-2) "Test 1")
;;                          )))

(define manufactoria-dot-test-suite 
  (test-suite "Tests for manufactoria-dot.rkt"

              (test-case "Robotoast! l1 - Accept everything"
                         (let ((machine "?lvl=1&code=c12:6f3;c12:7f3;c12:8f3;"))
                           (check-equal? (to-dot machine) "TODO!")
                           ))
              )
  )

;; (run-tests manufactoria-dot-internals-test-suite)
(run-tests manufactoria-dot-test-suite)

