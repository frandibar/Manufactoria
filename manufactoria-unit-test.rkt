#! /usr/local/bin/racket
#lang racket

(require rackunit)
(require rackunit/text-ui)
(require/expose "manufactoria.rkt" (
                                    token-id
                                    ))

(define manufactoria-test-suite 
  (test-suite 
   "Tests for manufactoria.rkt"

   (test-case
    "token-id"
     (let ([token '((1 2) 'basket null)])
       (check-equal? (token-id token) "basket-1-2") "Test 1")
       ))


(run-tests manufactoria-test-suite)

