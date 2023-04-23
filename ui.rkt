#lang racket/base
(require racket/gui/base racket/class racket/contract racket/list)
(provide (contract-out (main-window%
                        (class/c
                         (init-field (mod-path (or/c module-path? #f))
                                     (interval (or/c positive? #f)))))))

(define main-window%
  (class frame%
    (init-field mod-path interval)

    (super-new [label "racket performance visualizer"])

    (define ep (new vertical-panel% (parent this)(alignment '(center center))))
    (define hp (new horizontal-panel% (parent ep)(alignment '(left center))))
    (define ip (new vertical-panel% (parent hp)(alignment '(center center))))

    (define green (make-object color% "green"))
    (define white (make-object color% "white"))
    (define red (make-object color% "red"))

    (define mc (make-custodian))
    (define/augment (on-close)
      (custodian-shutdown-all mc))

    (define vec (vector #f #f #f #f))
    (define bx (box #f))
    (define lb (new list-box%
                    [label "view"][choices null]
                    [parent ep][min-width 600][min-height 400]
                    [style '(single column-headers vertical-label)]
                    [columns '("entry" "value")]))
    (define (set-data)
      (send lb set
            (list "running?" "terminated?" "blocked?" "bytes in use" "processor time" )
            (list
             (if (vector-ref vec 0) "yes" "no")
             (if (vector-ref vec 1) "yes" "no")
             (if (vector-ref vec 2) "yes" "no")
             (cond ((vector-ref vec 3) => number->string) (else "unknown"))
             (number->string (unbox bx)))))
    (define (get-data thd)
      (vector-set-performance-stats! vec thd)
      (set-box! bx (current-process-milliseconds thd)))
    
    (define but (new button%
                     [label "Ok"] [parent hp][enabled #f]
                     [callback (lambda _
                                 (send hp enable #f)

                                 (define tg (make-thread-group))

                                 (define thd
                                   (parameterize ((current-custodian mc)
                                                  (current-thread-group tg))
                                     (thread
                                      (lambda ()
                                        (dynamic-require (list-ref (params-lst) 0) #f)))))
                                 
                                 (let loop ((n 0))
                                   (get-data thd)
                                   (set-data)
                                   (cond ((sync/timeout (list-ref (params-lst) 1) thd)
                                          (displayln (format "~a samples are taken" n)))
                                         (else (loop (add1 n))))))]))
    
    (define params-lst (make-parameter '(#f #f)))
    
    (define (make-field lbl pd pos)
      (define t
        (new text-field%
             [label lbl][parent ip]
             [callback
              (lambda (f _)
                (let ((v (send f get-value)))
                  (with-handlers ((exn:fail:read? (lambda (e) (send f set-field-background red))))
                    (define r (read (open-input-string v)))
                    (if (pd r)
                        (begin (send f set-field-background green)
                               (params-lst (list-set (params-lst) pos r))
                               (cond ((andmap values (params-lst)) (send but enable #t))))
                        (begin (send f set-field-background red)
                               (send but enable #f))))))]))
      (send t set-field-background white)
      t)

    (if mod-path
        (params-lst (list-set (params-lst) 0 mod-path))
        (make-field "module path" module-path? 0))

    (if interval
        (params-lst (list-set (params-lst) 1 interval))
        (make-field "interval" positive? 1))

    (inherit show)
    (show #t)))
