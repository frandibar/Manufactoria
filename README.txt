Manufactoria is a game about programming.
http://pleasingfungus.com/#!/Manufactoria

You must build a testing machine that is a sort of production line, fed with a "tape" of symbols (red, blue, green and yellow) and conveyor belts that transport these symbols in a fixed direction and branch conveyors that change the direction based on the current symbol.
The machine is designed to accept tapes that follow certain patterns, and reject those that do not.


This project aims to generate the scheme code defined by a specific hardware configuration (a "solution" to the game).

The hardware configuration was specified by the game designer.
It's a string consisting of the game level and the code. The code is a semi colon separated list of tokens, each token indicates the type of element (carrier, branch or writer) and it's position and direction, in the format [element][column]:[row][direction];

For example: 
"?lvl=25&code=c12:5f3;c12:9f3;p12:6f3;g11:6f1;c11:5f2;y13:6f1;c13:5f0;c12:7f3;q12:8f3;c13:7f0;c11:7f2;r13:8f1;b11:8f1;"


The generated code recieves a "tape" as input and accepts/rejects it when evaluated.

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

