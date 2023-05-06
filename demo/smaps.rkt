#lang racket/base
(require racket/string racket/flonum racket/format)
(provide table)
(define record (vector #f #f #f))

(define (inspect-smaps p)
  (call-with-input-file (format "/proc/~a/smaps" p)
    (lambda (in)
      (for ((l (in-lines in)))
        (cond ((string-prefix? l "Swap:")
               (vector-set! record 0 (string->number (cadr (string-split l)))))
              ((string-prefix? l "Rss:")
               (vector-set! record 1 (string->number (cadr (string-split l)))))
              ((string-prefix? l "Pss:")
               (vector-set! record 2 (string->number (cadr (string-split l))))))))))

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
