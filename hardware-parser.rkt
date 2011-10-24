(module hardware-parser racket
        (provide (except-out (all-defined-out)
                             *branch-dirs*
                             *levels*))

;; Elements
;; Writers: b (blue)
;;          r (red) 
;;          g (green)
;;          y (yellow)
;; Carrier: c
;; Directions:
;;  f0 right to left
;;  f1 bottom to top
;;  f2 left to right
;;  f3 top to bottom

;; Branches: p (blue & red)
;;           q (yellow & green)
;; Directions:
;;  f0 left
;;  f1 up
;;  f2 right
;;  f3 down
;;  f6 inverted right
;;  [element][column]:[row][direction];
;;  in clockwise direction at angle 0
;;  f0 for branches (in yellow none green) (in blue none red)
;;  f1 rotate clockwise (yellow none green in) (blue none red in)
;;  f4 (flip) (in green none yellow) (in red none blue) 
;;  f5 rotate clockwise (green none yellow in) (red none blue in)
;;  f8

(define *branch-dirs* `((p ((f0 ((b up) (,null left) (r down)))
                            (f1 ((b right) (,null up) (r left)))
                            (f2 ((b down) (,null right) (r up)))
                            (f3 ((b left) (,null down) (r right)))
                            (f4 ((r up) (,null left) (b down)))
                            (f5 ((r right) (,null up) (b left)))
                            (f6 ((r down) (,null right) (b up)))
                            (f7 ((r left) (,null down) (b right)))))
                        (q ((f0 ((y up) (,null left) (g down)))
                            (f1 ((y right) (,null up) (g left)))
                            (f2 ((y down) (,null right) (g up)))
                            (f3 ((y left) (,null down) (g right)))
                            (f4 ((g up) (,null left) (y down)))
                            (f5 ((g right) (,null up) (y left)))
                            (f6 ((g down) (,null right) (y up)))
                            (f7 ((g left) (,null down) (y right)))))))

 ;; The starting position depends on the the level
 ;; Each entry contains (level (nrows (start-col start-row) (end-col end-row)))
(define *levels* '((1 (5 (12 6) (12 9)))
                   (2 (5 (12 6) (12 9)))
                   (3 (7 (12 5) (12 10)))
                   (4 (5 (12 6) (12 9)))
                   (5 (9 (12 4) (12 11)))
                   (6 (9 (12 4) (12 11)))
                   (7 (9 (12 4) (12 11)))
                   (8 (5 (12 6) (12 9)))
                   (9 (7 (12 5) (12 10)))
                   (10 (9 (12 4) (12 11)))
                   (25 (7 (12 5) (12 10)))
                   (26 (7 (12 5) (12 10)))
                   ))

;; Returns a list of tokens extracted from the machine's string representation
;; hardware-desc is a string such as "?lvl=1&code=c12:6f3;c12:7f3;c12:8f3;"
(define (hardware-tokens hardware-desc)
  ;; convert "c12:6f3" into ((12 6) c f3)
  (define (conv-notation str)
    (if (equal? str "")
        null
        (let* ((toks (regexp-split #rx":" str))
               (fst (car toks))
               (sec (second toks)))
          (let ((type (string->symbol (string (string-ref fst 0))))
                (col (string->number (substring fst 1 (string-length fst))))
                (row (string->number (substring sec 0 (- (string-length sec) 2))))
                (dir (string->symbol (substring sec (- (string-length sec) 2) (string-length sec)))))
            (list (list col row) type dir)))))

  (let ((toks (cdddr (regexp-split #rx"[&;=]" hardware-desc))))   ; remove first 3 elements
    ;; add basket token (ending token)
    (filter (lambda (x) (not (null? x)))
            (append (map conv-notation toks) (list (basket-tok (get-level hardware-desc)))))))

(define (get-start-pos lvl)
  (second (second (assoc lvl *levels*))))

(define (get-finish-pos lvl)
  (third (second (assoc lvl *levels*))))

;; pre: hardware-desc is a string
(define (get-level hardware-desc)
  (string->number (second (regexp-split #rx"[&;=]" hardware-desc)))) ; split string by any of &;= and return the 2nd item

;; Returns the token at position pos
(define (get-token pos tokens)
  (if (null? tokens)
      null
      (if (equal? (token-pos (car tokens)) pos)
          (car tokens)
          (get-token pos (cdr tokens)))))

;; Returns the next position in direction dir from position pos
(define (next-pos pos dir)
  (let ((col (first pos))
        (row (second pos)))
    (cond ((or (eq? dir 'right->left)
               (eq? dir 'left))
           (list (- col 1) row))
          ((or (eq? dir 'bottom->top)
               (eq? dir 'up)) 
           (list col (- row 1)))
          ((or (eq? dir 'left->right) 
               (eq? dir 'right)) 
           (list (+ col 1) row))
          ((or (eq? dir 'top->bottom) 
               (eq? dir 'down)) 
           (list col (+ row 1)))
          (else null))))

;; Returns the first non carrier token, when starting at token one-way-token.
;; one-way-token should be of type carrier or writer.
(define (advance one-way-token tokens)
  (define (next-token one-way-token)
    (if (null? one-way-token)
        null
        (get-token (next-pos (token-pos one-way-token) 
                             (one-way-dir (token-dir one-way-token)))
                   tokens)))

  (let ((next (next-token one-way-token)))
    (if (null? next)
        null
        (if (carrier? next)
            (advance next tokens)
            next))))

;; Returns the basket token
(define (basket-tok level)
  (list (get-finish-pos level)
        'basket
        null))

;; Returns a unique name for the token, used for function calls.
;; i.e. token in position (12 3) of type 'p gets called "p-12-3"
(define (token-id token)
  (string->symbol (string-join (append (list (symbol->string (token-type token)))
                                       (map number->string (token-pos token)))
                               "-")))

;; Returns the token's position
(define (token-pos token)
  (car token))

;; Returns the token's type
;; Valid types are 'c 'p 'q 'b 'r 'g 'y and 'basket.
(define (token-type token)
  (second token))

;; Returns the token's direction
;; Valid types are f0 to f3
(define (token-dir token)
  (last token))

;; The basket is the final token, when reached the tape is accepted.
(define (basket? token)
  (eq? (token-type token) 'basket))

(define (writer? token)
  (if (member (token-type token) '(b r g y)) true false))

(define (carrier? token)
  (eq? (token-type token) 'c))

(define (blue-red-branch? token)
  (eq? (token-type token) 'p))

(define (green-yellow-branch? token)
  (eq? (token-type token) 'q))

(define (branch? token)
  (or (blue-red-branch? token)
      (green-yellow-branch? token)))

;; Translates the f0..f4 into more verbose symbols
(define (one-way-dir dir)
  (cond ((eq? dir 'f0) 'right->left)
        ((eq? dir 'f1) 'bottom->top)
        ((eq? dir 'f2) 'left->right)
        ((eq? dir 'f3) 'top->bottom)
        (else null)))

;; Returns the direction to take based on the token's orientation and the
;; first color in tape
(define (next-branch-dir tape token)
  (cadr (assoc (if (null? tape) 
                   null 
                   (car tape))
               (cadr (assoc (token-dir token)
                            (cadr (assoc (token-type token) *branch-dirs*)))))))

)    ; module