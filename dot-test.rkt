#!/usr/bin/env racket
#lang racket

(require rackunit)
(require rackunit/text-ui)

(require "dot.rkt")
(require/expose "dot.rkt" (dot-name
                           dot-label
                           nodes->dot
                           edges->dot
                           undirected-edges->dot
                           graph->dot
                           dot->png
                           ))

(define dot-internals-test-suite
  (test-suite "Tests for dot.rkt internals"
              (test-case "dot-name"
                         (check-equal? (dot-name 'my-living-room) "my_living_room"))
              (test-case "dot-label"
                         (check-equal? (dot-label "123456789012345678901234567890") "123456789012345678901234567890")
                         (check-equal? (dot-label "1234567890123456789012345678901") "123456789012345678901234567890..."))
              (test-case "nodes->dot"
                         (check-equal? (nodes->dot '((a "node-a") (b "node-b"))) ""))
              ))
              ;; (test-case "edges->dot")
              ;; (test-case "undirected-edges->dot")
              ;; (test-case "graph->dot")
              ;; (test-case "dot->png")


;; (define dot-test-suite 
;;   (test-suite "Tests for dot.rkt"

;;               (test-case "graph->png"
;;                          (let ((nodes '())
;;                                (edges '()))
;;                            (check-true (graph->png "test.png" nodes edges false))
;;                            ))
;;               )
;;   )

(run-tests dot-internals-test-suite)
;; (run-tests dot-test-suite)

