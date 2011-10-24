(module manufactoria-dot racket
        (provide to-dot)

(require "hardware-parser.rkt")

;; TODO: Finish all

;; Returns the dot code for a branch (tokens 'p and 'q)
(define (branch-dot token tokens)
  (define (branch color id)
    (let ((tok (get-token (next-pos (token-pos token)
                                    (next-branch-dir (list color) token))
                          tokens)))
      (if (null? tok) 
          (list id 'reject color) 
          (if (basket? tok)
              (list id 'accept color) 
              (list id 
                    (token-id (if (carrier? tok) 
                                  (advance tok tokens) 
                                  tok))
                    color)))))

  (let ((id (token-id token)))
    (cond ((blue-red-branch? token)
           (list (branch 'b id)
                 (branch 'r id)
                 (branch null id)))  ; maybe green or yellow
          ((green-yellow-branch? token)
           (list (branch 'g id)
                 (branch 'y id)
                 (branch null id))) ; maybe red or green
          (else raise "Invalid token color"))))

;; Returns the dot code for the writers (tokens 'b 'r 'g and 'y)
(define (writer-dot token tokens)
  (let ((id (token-id token))
        (tok (get-token (next-pos (token-pos token) (one-way-dir (token-dir token)))
                        tokens)))
    (if (null? tok) 
        (list id 'reject) 
        (if (basket? tok)
            (list id 'accept) 
            (list id (token-id (if (carrier? tok) 
                                   (advance tok tokens) 
                                   tok)))))))

;; Returns a list with the machine's code
(define (to-dot hardware-desc)

  (define (branch-defs tokens)
    (map (lambda (tok) (branch-dot tok tokens)) (filter branch? tokens)))

  (define (writer-defs tokens)
    (map (lambda (tok) (writer-dot tok tokens)) (filter writer? tokens)))

  (let* ((level (get-level hardware-desc))
         (spos (get-start-pos level))
         (tokens (hardware-tokens hardware-desc))
         (token (get-token spos tokens)))
    (append '(graph)
            (branch-defs tokens)
            (writer-defs tokens)
            (list 'start (token-id (if (carrier? token) 
                                       (advance token tokens) 
                                       token))))))
)    ; module
