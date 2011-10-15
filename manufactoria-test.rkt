#!/usr/bin/env racket
#lang racket

(require rackunit)
(require rackunit/text-ui)

(require (only-in "manufactoria.rkt"
                  build-machine
                  run))
(require/expose "manufactoria.rkt" (token-id))

(define manufactoria-internals-test-suite
  (test-suite "Tests for manufactoria.rkt internals"
              (test-case "token-id"
                         (let ([token '((1 2) 'basket null)])
                           (check-equal? (token-id token) "basket-1-2") "Test 1")
                         )))

(define manufactoria-test-suite 
  (test-suite "Tests for manufactoria.rkt"

              (test-case "Robotoast! l1 - Accept everything"
                         (let ([machine (build-machine "?lvl=1&code=c12:6f3;c12:7f3;c12:8f3;")])
                           (check-equal? (run machine '()) '(accepted ()) "Test 1")
                           (check-equal? (run machine '(b)) '(accepted (b)) "Test 2")
                           (check-equal? (run machine '(r)) '(accepted (r)) "Test 3")
                           (check-equal? (run machine '(b r)) '(accepted (b r)) "Test 4")
                           ))

              (test-case "Robocoffee! l2 - Accept if starts with blue"
                         (let ([machine (build-machine "?lvl=2&code=p12:6f3;c11:6f3;c11:7f3;c11:8f3;c11:9f2;")])
                           (check-equal? (run machine '()) '(rejected ()) "Test 1")
                           (check-equal? (run machine '(b)) '(accepted ()) "Test 2")
                           (check-equal? (run machine '(b r)) '(accepted (r)) "Test 3")
                           (check-equal? (run machine '(b b)) '(accepted (b)) "Test 4")
                           (check-equal? (run machine '(r b)) '(rejected (b)) "Test 5")
                           ))

              (test-case "Robolamp! l3 - Accept if three or more blues"
                         (let ([machine (build-machine "?lvl=3&code=c12:5f3;p12:6f3;c13:5f0;c13:6f1;c11:6f0;p10:7f3;c9:7f3;c10:6f3;c11:7f1;c9:9f2;c10:9f2;c9:8f3;c11:8f3;c11:9f2;c12:8f0;p12:9f2")])
                           (check-equal? (run machine '()) '(rejected ()) "Test 1")
                           (check-equal? (run machine '(b)) '(rejected ()) "Test 2")
                           (check-equal? (run machine '(b b)) '(rejected ()) "Test 3")
                           (check-equal? (run machine '(b b b)) '(accepted ()) "Test 4")
                           (check-equal? (run machine '(r b b b)) '(accepted ()) "Test 5")
                           (check-equal? (run machine '(r b b b r b)) '(accepted (r b)) "Test 6")
                           (check-equal? (run machine '(r b b r r)) '(rejected ()) "Test 7")
                           ))
              
              (test-case "Robofish! l4 - Accept if no red"
                         (let ([machine (build-machine "?lvl=4&code=p12:8f3;c12:6f3;c12:7f3;c11:8f1;c11:7f2;")])
                           (check-equal? (run machine '()) '(accepted ()) "Test 1")
                           (check-equal? (run machine '(b)) '(accepted ()) "Test 2")
                           (check-equal? (run machine '(b r)) '(rejected ()) "Test 3")
                           (check-equal? (run machine '(b b)) '(accepted ()) "Test 4")
                           (check-equal? (run machine '(r b)) '(rejected (b)) "Test 5")
                           ))

              (test-case "Robobugs! l5 - Accept alternating colors"
                         (let ([machine (build-machine "?lvl=5&code=c12:4f3;c12:8f3;c12:9f3;c12:10f3;p12:6f3;c12:7f3;c12:5f3;c11:6f0;p10:6f0;p10:7f0;c9:6f3;c9:7f3;c9:8f3;c9:9f2;c10:9f2;c11:9f2;c13:6f2;p14:6f2;p14:7f2;c15:6f3;c15:7f3;c15:8f3;c15:9f0;c14:9f0;c13:9f0;")])
                           (check-equal? (run machine '()) '(accepted ()) "Test 1")
                           (check-equal? (run machine '(b)) '(accepted ()) "Test 2")
                           (check-equal? (run machine '(b r)) '(accepted ()) "Test 3")
                           (check-equal? (run machine '(b b)) '(rejected ()) "Test 4")
                           (check-equal? (run machine '(r r)) '(rejected ()) "Test 5")
                           (check-equal? (run machine '(r b)) '(accepted ()) "Test 6")
                           (check-equal? (run machine '(r b r)) '(accepted ()) "Test 7")
                           (check-equal? (run machine '(b r b r)) '(accepted ()) "Test 8")
                           ))

              (test-case "Robocats! l6 - Accept if ends with 2 blues"
                         (let ([machine (build-machine "?lvl=6&code=c10:10f3;c10:11f2;c11:11f2;c11:5f3;c12:4f3;p12:5f3;c13:4f0;c13:5f1;c13:6f1;c11:6f3;p11:7f3;c12:7f2;c9:7f2;c13:7f1;c9:9f1;p10:9f3;c9:8f1;c10:8f3;c11:9f2;c12:9f2;c13:8f1;c13:9f1;c10:7f3;")])
                           (check-equal? (run machine '()) '(rejected ()) "Test 1")
                           (check-equal? (run machine '(b)) '(rejected ()) "Test 2")
                           (check-equal? (run machine '(b r)) '(rejected ()) "Test 3")
                           (check-equal? (run machine '(b b)) '(accepted ()) "Test 4")
                           (check-equal? (run machine '(r r)) '(rejected ()) "Test 5")
                           (check-equal? (run machine '(r b)) '(rejected ()) "Test 6")
                           (check-equal? (run machine '(r b r)) '(rejected ()) "Test 7")
                           (check-equal? (run machine '(b r b r)) '(rejected ()) "Test 8")
                           (check-equal? (run machine '(b r b b)) '(accepted ()) "Test 9")
                           ))

              (test-case "Robobears! l7 - Accept if starts and ends with same color"
                         (let ([machine (build-machine "?lvl=7&code=p12:4f3;c12:10f3;c11:4f0;c13:4f2;c12:5f3;c12:6f3;c12:7f3;c12:8f3;c12:9f3;c10:4f0;c8:4f3;c8:5f3;c9:4f0;c11:7f1;c11:5f1;c16:4f3;c16:5f3;c14:4f2;c15:4f2;c13:7f1;c13:5f1;c8:6f3;c8:7f3;c8:8f2;c9:6f3;c9:7f2;c9:8f2;c9:9f1;p10:6f1;c10:7f1;p10:8f2;c10:9f0;c11:6f1;c11:8f2;c13:6f1;c13:8f0;p14:6f1;c14:7f1;p14:8f0;c14:9f2;c15:6f3;c15:7f0;c15:8f0;c15:9f1;c16:6f3;c16:7f3;c16:8f0;")])
                           (check-equal? (run machine '()) '(accepted ()) "Test 1")
                           (check-equal? (run machine '(b)) '(accepted ()) "Test 2")
                           (check-equal? (run machine '(b r)) '(rejected ()) "Test 3")
                           (check-equal? (run machine '(b b)) '(accepted ()) "Test 4")
                           (check-equal? (run machine '(r r)) '(accepted ()) "Test 5")
                           (check-equal? (run machine '(r b)) '(rejected ()) "Test 6")
                           (check-equal? (run machine '(r b r)) '(accepted ()) "Test 7")
                           (check-equal? (run machine '(b r b r)) '(rejected ()) "Test 8")
                           (check-equal? (run machine '(b r b b)) '(accepted ()) "Test 9")
                           ))

              (test-case "RC Cars! l8 - The input with the first symbol at the end"
                         (let ([machine (build-machine "?lvl=8&code=c12:8f3;c12:7f3;p12:6f3;c11:7f2;c13:7f0;b11:6f3;r13:6f3;")])
                           (check-equal? (run machine '()) '(accepted ()) "Test 1")
                           (check-equal? (run machine '(b)) '(accepted (b)) "Test 2")
                           (check-equal? (run machine '(b r)) '(accepted (r b)) "Test 3")
                           (check-equal? (run machine '(b b)) '(accepted (b b)) "Test 4")
                           (check-equal? (run machine '(r r)) '(accepted (r r)) "Test 5")
                           (check-equal? (run machine '(r b)) '(accepted (b r)) "Test 6")
                           (check-equal? (run machine '(r b r)) '(accepted (b r r)) "Test 7")
                           (check-equal? (run machine '(b r b r)) '(accepted (r b r b)) "Test 8")
                           (check-equal? (run machine '(b r b b)) '(accepted (r b b b)) "Test 9")
                           ))

              (test-case "Robocars! l9 - Replace blue with green and red with yellow"
                         (let ([machine (build-machine "?lvl=9&code=g11:6f3;p12:6f3;y13:6f3;c12:7f3;c12:8f3;c12:9f3;c10:6f1;c14:6f1;c14:5f0;c13:5f0;c10:5f2;c11:5f2;c12:5f3;c11:7f0;c10:7f1;c14:7f1;c13:7f2;")])
                           (check-equal? (run machine '()) '(accepted ()) "Test 1")
                           (check-equal? (run machine '(b)) '(accepted (g)) "Test 2")
                           (check-equal? (run machine '(b r)) '(accepted (g y)) "Test 3")
                           (check-equal? (run machine '(b b)) '(accepted (g g)) "Test 4")
                           (check-equal? (run machine '(r r)) '(accepted (y y)) "Test 5")
                           (check-equal? (run machine '(r b)) '(accepted (y g)) "Test 6")
                           (check-equal? (run machine '(r b r)) '(accepted (y g y)) "Test 7")
                           (check-equal? (run machine '(b r b r)) '(accepted (g y g y)) "Test 8")
                           (check-equal? (run machine '(b r b b)) '(accepted (g y g g)) "Test 9")
                           ))

              (test-case "Robostilts! l10 - Put a green at the beginning and a yellow at the end"
                         (let ([machine (build-machine "?lvl=10&code=g12:4f2;p12:6f3;b11:6f1;r13:6f1;c11:5f2;c13:5f0;c12:5f3;y12:7f3;c12:8f3;c12:9f3;c12:10f3;c13:4f3;")])
                           (check-equal? (run machine '()) '(accepted (g y)) "Test 1")
                           (check-equal? (run machine '(b)) '(accepted (g b y)) "Test 2")
                           (check-equal? (run machine '(b r)) '(accepted (g b r y)) "Test 3")
                           (check-equal? (run machine '(b b)) '(accepted (g b b y)) "Test 4")
                           (check-equal? (run machine '(r r)) '(accepted (g r r y)) "Test 5")
                           (check-equal? (run machine '(r b)) '(accepted (g r b y)) "Test 6")
                           (check-equal? (run machine '(r b r)) '(accepted (g r b r y)) "Test 7")
                           (check-equal? (run machine '(b r b r)) '(accepted (g b r b r y)) "Test 8")
                           (check-equal? (run machine '(b r b b)) '(accepted (g b r b b y)) "Test 9")
                           ))

              ;; (test-case "Milidogs! l11 - With blue as 1 and red as 0 accept odd binary strings"
              ;;   (let ([machine (build-machine "?lvl=11&code=p11:8f2;c12:8f3;c12:9f3;c10:5f3;c10:6f3;c10:7f3;c12:5f0;c11:5f0;c10:9f1;c10:8f2;c11:9f0;c11:7f2;c12:7f2;p13:7f2;c13:8f2;c14:5f0;c13:5f0;c15:6f1;c15:7f1;c15:8f1;c14:8f2;c15:5f0;c13:6f0;c12:6f3;")])
              ;;     (check-equal? (run machine '()) '(accepted (g y)) "Test 1")
              ;;     (check-equal? (run machine '(b)) '(accepted (g b y)) "Test 2")
              ;;     (check-equal? (run machine '(b r)) '(accepted (g b r y)) "Test 3")
              ;;     (check-equal? (run machine '(b b)) '(accepted (g b b y)) "Test 4")
              ;;     (check-equal? (run machine '(r r)) '(accepted (g r r y)) "Test 5")
              ;;     (check-equal? (run machine '(r b)) '(accepted (g r b y)) "Test 6")
              ;;     (check-equal? (run machine '(r b r)) '(accepted (g r b r y)) "Test 7")
              ;;     (check-equal? (run machine '(b r b r)) '(accepted (g b r b r y)) "Test 8")
              ;;     (check-equal? (run machine '(b r b b)) '(accepted (g b r b b y)) "Test 9")
              ;;     ))

              (test-case "Roborockets! l25 - Swap blue for red and red for blue"
                         (let ([machine (build-machine "?lvl=25&code=c12:5f3;c12:9f3;p12:6f3;g11:6f1;c11:5f2;y13:6f1;c13:5f0;c12:7f3;q12:8f3;c13:7f0;c11:7f2;r13:8f1;b11:8f1;")])
                           (check-equal? (run machine '()) '(accepted ()) "Test 1")
                           (check-equal? (run machine '(b)) '(accepted (r)) "Test 2")
                           (check-equal? (run machine '(b r)) '(accepted (r b)) "Test 3")
                           (check-equal? (run machine '(b b)) '(accepted (r r)) "Test 4")
                           (check-equal? (run machine '(r r)) '(accepted (b b)) "Test 5")
                           (check-equal? (run machine '(r b)) '(accepted (b r)) "Test 6")
                           (check-equal? (run machine '(r b r)) '(accepted (b r b)) "Test 7")
                           (check-equal? (run machine '(b r b r)) '(accepted (r b r b)) "Test 8")
                           (check-equal? (run machine '(b r b b)) '(accepted (r b r r)) "Test 9")
                           ))

              (test-case "Roboplanes! l26 - Remove red"
                         (let ([machine (build-machine "?lvl=26&code=p12:6f3;g11:6f1;c13:5f0;c12:5f3;c13:6f1;c11:5f2;q12:8f3;c12:7f3;b13:8f1;c13:7f0;c12:9f3;")])
                           (check-equal? (run machine '()) '(accepted ()) "Test 1")
                           (check-equal? (run machine '(b)) '(accepted (b)) "Test 2")
                           (check-equal? (run machine '(b r)) '(accepted (b)) "Test 3")
                           (check-equal? (run machine '(b b)) '(accepted (b b)) "Test 4")
                           (check-equal? (run machine '(r r)) '(accepted ()) "Test 5")
                           (check-equal? (run machine '(r b)) '(accepted (b)) "Test 6")
                           (check-equal? (run machine '(r b r)) '(accepted (b)) "Test 7")
                           (check-equal? (run machine '(b r b r)) '(accepted (b b)) "Test 8")
                           (check-equal? (run machine '(b r b b)) '(accepted (b b b)) "Test 9")
                           ))
              )
  )

(run-tests manufactoria-internals-test-suite)
(run-tests manufactoria-test-suite)

