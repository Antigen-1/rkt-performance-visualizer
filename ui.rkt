#lang racket/base
(require racket/gui/base racket/class racket/contract racket/list)
(provide (contract-out (main-window%
                        (class/c
                         (init-field (mod-path (or/c module-path? #f))
                                     (args (vectorof string?))
                                     (thread-group thread-group?)
                                     (interval (or/c positive? #f)))))))

(define main-window%
  (class frame%
    (init-field mod-path interval (args (current-command-line-arguments)) (thread-group (current-thread-group)))

    (super-new [label "racket performance visualizer"])

    (define ep (new vertical-panel% (parent this)(alignment '(center center))))
    (define hp (new horizontal-panel% (parent ep)(alignment '(left center))))
    (define ip (new vertical-panel% (parent hp)(alignment '(center center))))

    (define green (make-object color% "green"))
    (define white (make-object color% "white"))
    (define red (make-object color% "red"))

    (define mc (make-custodian))
    (define ac (make-custodian mc))
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
             (cond ((vector-ref vec 3) => (lambda (v) (format "thread @ ~a" v)))
                   (else (format "custodian @ ~a" (current-memory-use ac))))
             (number->string (unbox bx)))))
    (define (get-data thd)
      (vector-set-performance-stats! vec thd)
      (set-box! bx (current-process-milliseconds thd)))

    (define switch (make-semaphore 0))
    (define but (new button%
                     [label "Ok"] [parent hp][enabled #f]
                     [callback (lambda _
                                 (send hp enable #f)
                                 (semaphore-post switch))]))
    
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

    (cond ((and mod-path interval)
           (params-lst (list mod-path interval))
           (send but enable #t))
          (mod-path
           (params-lst (list mod-path #f))
           (make-field "interval" positive? 1))
          (interval
           (params-lst (list #f interval))
           (make-field "module path" module-path? 0))
          (else (make-field "module path" module-path? 0)
                (make-field "interval" positive? 1)))

    (inherit show)
    (show #t)

    (void
     (parameterize ((current-custodian mc))
       (thread
        (lambda ()
          (when (sync switch)
            (define thd
              (parameterize ((current-thread-group thread-group)
                             (current-custodian ac))
                (thread
                 (lambda ()
                   (parameterize ((current-command-line-arguments args))
                     (dynamic-require (list-ref (params-lst) 0) #f))))))
            
            (let loop ((n 0))
              (collect-garbage 'incremental)
              (cond ((sync/timeout (list-ref (params-lst) 1) thd)
                     (get-data thd)
                     (set-data)
                     (displayln (format "~a samples are taken" (add1 n))))
                    (else
                     (get-data thd)
                     (set-data)
                     (loop (add1 n))))))))))))
