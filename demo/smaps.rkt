#lang racket/base
(require racket/string racket/flonum racket/format)
(provide table)
(define record (vector #f #f #f))

(define (inspect-smaps p)
  (call-with-input-file (format "/proc/~a/smaps" p)
    (lambda (in)
      (define-values (s r p)
        (for/fold ((swap 0) (rss 0) (pss 0)) ((l (in-lines in)))
          (cond ((string-prefix? l "Swap:")
                 (values (+ swap (string->number (cadr (string-split l))))
                         rss
                         pss))
                ((string-prefix? l "Rss:")
                 (values swap
                         (+ rss (string->number (cadr (string-split l))))
                         pss))
                ((string-prefix? l "Pss:")
                 (values swap
                         rss
                         (+ pss (string->number (cadr (string-split l))))))
                (else (values swap rss pss)))))
      (vector-set! record 0 s)
      (vector-set! record 1 r)
      (vector-set! record 2 p))))

(define (filesize size)
  (define n (* 1024 size))
  (if (< n 1100) (format "~aB" n)
      (let* ([fn (exact->inexact n)]
             [p (flfloor (fl/ (fllog (fl/ fn 1100.0)) (fllog 1024.0)))])
        (define s (~r (fl/ fn (flexpt 1024.0 (fl+ 1.0 p))) #:precision '(= 1)))
        (define unit (string-ref "KMGT" (inexact->exact p)))
        (format "~a~aiB" s unit))))

(define table (list (list "Swap:" (lambda (p) (inspect-smaps p) (filesize (vector-ref record 0))))
                    (list "Rss:" (lambda (_) (filesize (vector-ref record 1))))
                    (list "Pss:" (lambda (_) (filesize (vector-ref record 2))))))
