#lang racket

(provide (all-defined-out))
;;;;;;;;;;;;; Lex Program ;;;;;;;;;;;;
;;;; source code(string) to token ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CharType ;;;
;;;;;;;;;;;;;;;;
;;; isX? : Char -> bool
;;; usage : for string and number literal
(define isAlphabet?
  (lambda (char)
    (let ((i (char->integer char)))
      (or (and (>= i 65)
               (<= i 90))
          (and (>= i 97)
               (<= i 122))))))
(define isInteger?
  (lambda (char)
    (let ((i (char->integer char)))
      (and (>= i 48)
           (<= i 57)))))
(define isSkipable?
  (lambda (char)
    (or (equal? char #\space)
        (equal? char #\newline))))

;;; simple-scanner : Listof(Char) -> Listof(Char)
(define simple-scanner
  (lambda (sl)
    (cond [(null? sl) '()]
          [else
           (let ((slh (car sl)))
             (cond [(isSkipable? slh) (simple-scanner (cdr sl))]
                   [(isAlphabet? slh) (let ((a (string-literal sl)))
                                       (cons a (simple-scanner (drop-n sl (length a)))))]
                   [(isInteger? slh) (let ((a (number-literal sl)))
                                       (cons a (simple-scanner (drop-n sl (length a)))))]
                   [else (cons (list slh) (simple-scanner (cdr sl)))]))])))

;;; X-literal : Listof(Char) -> Listof(Char)
;;; usage : Returns the first checked string
(define string-literal
  (lambda (sl)
    (cond [(null? sl) '()]
          [else
           (let ((slh (car sl)))
             (if (isAlphabet? slh)
                 (cons slh (string-literal (cdr sl)))
                 '()))])))
(define number-literal
  (lambda (sl)
    (cond [(null? sl) '()]
          [else
           (let ((slh (car sl)))
             (if (isInteger? slh)
                 (cons slh (number-literal (cdr sl)))
                 '()))])))

;;; Aux
(define drop-n
  (lambda (a n)
    (cond [(null? a) '()]
          [(= n 0) a]
          [else (drop-n (cdr a) (- n 1))])))

;;; token := kind * string
(struct token (kind string))

;;; tokenization : Listof(Char) -> Listof(token)
(define tokenization
  (lambda (string-list table)
    (cond [(null? string-list) '()]
          [else
           (let ((h (car string-list)))
             (cond [(isAlphabet? (car h))
                    (let ((lookup-keyword (assoc (list->string h) table)))
                      (if lookup-keyword
                          (cons (token (cdr lookup-keyword) (list->string h))
                                (tokenization (cdr string-list) table))
                          (cons (token "identifier" (list->string h))
                                (tokenization (cdr string-list) table))))]
                   [(isInteger? (car h))
                    (cons (token "int" (list->string h))
                          (tokenization (cdr string-list) table))]
                   [else (cons (token "unknown" (list->string h))
                               (tokenization (cdr string-list) table))]))])))

;;; define-token
(define-syntax (define-token stx)
  (syntax-case stx ()
    [(_ name a ...)
     #`(begin
         (define name
           '())
         #,@(for/list ([x (syntax->list #'(a ...))])
              (with-syntax ((t-name (car (syntax->list x)))
                            (t-val (cadr (syntax->list x))))
                #`(begin (struct t-name ())
                         (set! name (cons (cons t-val t-name)
                                          name))))))]))

;;; check-token : Listof(token) -> void
(define check-token
  (lambda (a)
    (cond [(null? a) 'done]
          [else
           (display (token-kind (car a)))
           (display " : ")
           (display (token-string (car a)))
           (newline)
           (check-token (cdr a))])))


;;; List supported keyword
(define-token keyword
  (my-if "if")
  (my-let "let")
  (my-struct "struct"))


;(define f "let a = 3 in let b = 4 in let c = 5")
;(define w (simple-scanner (string->list f)))
;(define rr (tokenization w keyword))
;(check-token rr)