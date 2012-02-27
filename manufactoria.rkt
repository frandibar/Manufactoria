(module manufactoria racket
        (provide build-machine
                 run)

(require "hardware-parser.rkt")

;; These definitions go into the machine's code
(define (helper-defs)
  '((define (blue? color)
      (eq? color 'b))
    (define (red? color)
      (eq? color 'r))
    (define (green? color)
      (eq? color 'g))
    (define (yellow? color)
      (eq? color 'y))
    (define (next tape)
      (if (null? tape) null (car tape)))
    (define (tail tape)
      (if (null? tape) null (cdr tape)))
    (define (accept tape)
      (list 'accepted tape))
    (define (reject tape)
      (list 'rejected tape))
    ))

;; Returns the code for a branch (tokens 'p and 'q)
(define (branch-code token tokens)
  (define (branch color tape)
    (let ((tok (get-token (next-pos (token-pos token)
                                    (next-branch-dir (list color) token))
                          tokens)))
      (if (null? tok) 
          ;; TODO I think tape should go instead of tape-tail, since the else clause in conditionals doesn't eat input
          '(reject tape-tail) 
          (if (basket? tok)
              `(accept ,tape) 
              `(,(token-id (if (carrier? tok) 
                               (advance tok tokens) 
                               tok))
                ,tape)))))

  `(define (,(token-id token) tape)
     (let ((color (next tape))
           (tape-tail (tail tape)))
       ,(cond ((blue-red-branch? token)
               `(cond ((blue? color) ,(branch 'b 'tape-tail))
                      ((red? color) ,(branch 'r 'tape-tail))
                      (else ,(branch null 'tape))))  ; maybe green or yellow
              ((green-yellow-branch? token)
               `(cond ((green? color) ,(branch 'g 'tape-tail))
                      ((yellow? color) ,(branch 'y 'tape-tail))
                      (else ,(branch null 'tape)))) ; maybe red or green
              (else raise("Invalid token color"))))))

;; Returns the code for the writers (tokens 'b 'r 'g and 'y)
(define (writer-code token tokens)
  `(define (,(token-id token) tape)
     ,(let ((tok (get-token (next-pos (token-pos token) (one-way-dir (token-dir token)))
                            tokens)))
        (if (null? tok) 
            '(reject tape) 
            (if (basket? tok)
                '(accept tape) 
                `(,(token-id (if (carrier? tok) 
                                 (advance tok tokens) 
                                 tok))
                  (append tape (list ',(token-type token)))))))))

;; Returns a list with the machine's code
(define (build-machine hardware-desc)

  (define (branch-defs tokens)
    (map (lambda (tok) (branch-code tok tokens)) (filter branch? tokens)))

  (define (writer-defs tokens)
    (map (lambda (tok) (writer-code tok tokens)) (filter writer? tokens)))

  (let* ((level (get-level hardware-desc))
         (start-pos (get-start-pos level))
         (tokens (hardware-tokens hardware-desc))
         (token (get-token start-pos tokens)))
    (append '(lambda (tape))
            (helper-defs)
            (branch-defs tokens)
            (writer-defs tokens)
            `((define (,(token-id (basket-tok level)) tape)
                (accept tape)))
            `((,(token-id (if (carrier? token) 
                              (advance token tokens) 
                              token))
               tape)))))

(define (run machine tape)
  ;; see http://docs.racket-lang.org/guide/eval.html#(part._namespaces)
  ;; for help on make-base-namespace
  ((eval machine (make-base-namespace)) tape))
  ;; ((eval machine) tape))

)     ; module


;; Sample usage:
;;
;; (require "manufactoria.rkt")
;;
;; (define hw "?lvl=25&code=c12:5f3;c12:9f3;p12:6f3;g11:6f1;c11:5f2;y13:6f1;c13:5f0;c12:7f3;q12:8f3;c13:7f0;c11:7f2;r13:8f1;b11:8f1;")
;; (define machine (build-machine hw))
;;
;; Run it!
;; (run machine '(b r b))
;;
;; Show machine's code
;; machine
