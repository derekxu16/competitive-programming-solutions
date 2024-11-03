#lang racket

(define parse
  (case-lambda
    [(token port)
     (case token
       [(#\+ #\-) (string-join (list (read port) (read port) (string token)))]
       [else (string token)])]
    [(ch port src line col pos) (assert-unreachable)]))
(define arguments
  (for/fold ([args (list)])
            ([codepoint (in-list
                         (cons (char->integer #\+)
                               (cons (char->integer #\-)
                                     (range (char->integer #\0) (add1 (char->integer #\9))))))])
    (cons (integer->char codepoint) (cons 'terminating-macro (cons parse args)))))
(define readtable (apply make-readtable (cons #f arguments)))
(for ([line (in-lines)])
  (if (equal? line "0")
      (void)
      (parameterize ([current-readtable readtable])
        (displayln (read (open-input-string line))))))
