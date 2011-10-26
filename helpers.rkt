(module helpers racket
        (provide maplist
                 to-string
                 )

;; supposed to imitate clisp maplist
;; eg.: (maplist list '(a b c)) => ((a b c)) ((b c)) ((c))
(define (maplist fn lst)
  (if (pair? lst)
      (cons (fn lst) (maplist fn (cdr lst)))
      '()))

(define (to-string x)
  (cond [(pair? x)
         (let ([str (string-append "(" (to-string (car x)))])
           (set! x (cdr x))
           (let loop ()
             (when (pair? x)
                   (begin
                     (set! str (string-append str " " (to-string (car x))))
                     (set! x (cdr x))
                     (loop))))
           (if (not (null? x))
               (string-append str " . " (to-string x) ")")
               (string-append str ")")))]
        [(boolean? x) (if x "#t" "#f")]
        [(symbol? x) (symbol->string x)]
        [(number? x) (number->string x)]
        [(string? x) (string-append "\"" x "\"")]
        [else "null"]))

) ; module

