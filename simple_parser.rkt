#lang racket

(require "simple_scanner.rkt")

(define-token keyword
  (my-if "if")
  (my-let "let")
  (my-proc "proc")
  (my-in "in")
  )
(define source
    "let a = 3443 in let b = 4 in let k = if (x) 3 4")
(define w
    (simple-scanner (string->list source)))
(define token-list
    (tokenization w keyword))


;;; define-node
(define-syntax (define-node stx)
  (syntax-case stx ()
    [(_ name a ...)
     #`(begin
         #,@(for/list ([x (syntax->list #'(a ...))])
              (with-syntax ((n-name (car (syntax->list x)))
                            (n-member (cadr (syntax->list x))))
                #`(begin (struct n-name n-member)))))]))

(define-node aaa
  (if-exp (a b c))
  (let-exp (b c d))
  (num-exp (a))
  (unknown-exp (a)))
;;; simple-parser : Listof(token) -> node
(define simple-parser
  (lambda (t coin)
    (cond [(null? t) '()]
          [(zero? coin) t]
          [else (let ((current (car t)))
                  (let ((ct (token-kind current))
                        (cs (token-string current)))
                    (cond [(or (unknown? ct) (my-in? ct)) (simple-parser (cdr t) coin)]
                          [(int? ct) (cons (num-exp cs) (simple-parser (cdr t) (- coin 1)))]
                          [(my-if? ct) (let ((nodes (simple-parser (cdr t) 3)))
                                         (cons (if-exp (car nodes)
                                                       (cadr nodes)
                                                       (caddr nodes))
                                               (simple-parser (cdddr nodes) (- coin 1))))]
                          [(my-let? ct) (let ((nodes (simple-parser (cdr t) 2)))
                                          (let-exp (car nodes)
                                                   (cadr nodes)
                                                   (simple-parser (cddr nodes) (- coin 1))))]
                          [else (cons (unknown-exp cs) (simple-parser (cdr t) (- coin 1)))]
                    )))])))
                        
(define wee
  (simple-parser token-list 100))

