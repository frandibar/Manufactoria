Manufactoria is a game about programming.
http://pleasingfungus.com/#!/Manufactoria

You must build a testing machine with robots consisting on symbols (red, blue, green and yellow) and conveyor belts that transport those symbols in a fixed direction and branch conveyors that read a symbol and move the robot in the corresponding direction.
The symbols are fed into the machine in a tape. 
The machine is programmed to accept certain patterns. All tapes that do not conform to the pattern should be rejected by the machine.


This project aims to generate the scheme code defined by a specific hardware configuration (a "solution" to the game).

This generated code recieves a "tape" as input and accepts/rejects it when evaluated.

To see it in action, let's suppose we have built a machine that converts red symbols to blue ones and vice versa.
This machine should accept any input.

From a scheme prompt type in the following:

(require "manufactoria.rkt")
(define hardware "?lvl=25&code=c12:5f3;c12:9f3;p12:6f3;g11:6f1;c11:5f2;y13:6f1;c13:5f0;c12:7f3;q12:8f3;c13:7f0;c11:7f2;r13:8f1;b11:8f1;")
(define machine (build-machine hardware))
(define tape '(b r b))       ; b stands for blue and r for red
(run machine tape)

which returns: '(accepted (r b r))

The code for machine in this case is:

'(lambda (tape)
   (define (blue? color) (eq? color 'b))
   (define (red? color) (eq? color 'r))
   (define (green? color) (eq? color 'g))
   (define (yellow? color) (eq? color 'y))
   (define (next tape) (if (null? tape) null (car tape)))
   (define (tail tape) (if (null? tape) null (cdr tape)))
   (define (accept tape) (list 'accepted tape))
   (define (reject tape) (list 'rejected tape))
   (define (p-12-6 tape)
     (let ((color (next tape)) (tape-tail (tail tape)))
       (cond
        ((blue? color) (g-11-6 tape-tail))
        ((red? color) (y-13-6 tape-tail))
        (else (q-12-8 tape)))))
   (define (q-12-8 tape)
     (let ((color (next tape)) (tape-tail (tail tape)))
       (cond
        ((green? color) (r-13-8 tape-tail))
        ((yellow? color) (b-11-8 tape-tail))
        (else (basket-12-10 tape)))))
   (define (g-11-6 tape) (p-12-6 (append tape (list 'g))))
   (define (y-13-6 tape) (p-12-6 (append tape (list 'y))))
   (define (r-13-8 tape) (q-12-8 (append tape (list 'r))))
   (define (b-11-8 tape) (q-12-8 (append tape (list 'b))))
   (define (basket-12-10 tape) (accept tape))
   (p-12-6 tape))

