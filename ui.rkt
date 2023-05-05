#lang racket/base
(require racket/gui/base racket/class racket/contract racket/list racket/function racket/format)
(provide (contract-out (main-window%
                        (class/c
                         (init-field (mod-path (or/c module-path? #f))
                                     (args (listof string?))
                                     (interval (or/c positive? #f)))))))

(define main-window%
  (class frame%
    (init-field mod-path interval args)

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

    (define lb (new list-box%
                    [label "view"][choices null]
                    [parent ep][min-width 600][min-height 400]
                    [style '(single column-headers vertical-label)]
                    [columns '("entry" "value")]))

    (define ch (make-channel))
    (define but (new button%
                     [label "Ok"] [parent hp][enabled #f]
                     [callback (lambda _
                                 (send hp enable #f)
                                 (send ep delete-child hp)
                                 (channel-put ch (unbox params-lst)))]))
    
    (define params-lst (box '(#f #f)))
    
    (define (make-field lbl pd pos)
      (define t
        (new text-field%
             [init-value (cond ((list-ref (unbox params-lst) pos) => ~s) (else ""))]
             [label lbl][parent ip]
             [callback
              (lambda (f _)
                (let ((v (send f get-value)))
                  (with-handlers (((disjoin exn:fail:read? exn:fail:contract?) (lambda (_)
                                                                                 (send f set-field-background red)
                                                                                 (send but enable #f)
                                                                                 (set-box! params-lst (list-set (unbox params-lst) pos #f)))))
                    (define r (read (open-input-string v)))
                    (if (pd r)
                        (begin (send f set-field-background green)
                               (set-box! params-lst (list-set (unbox params-lst) pos r))
                               (cond ((andmap values (unbox params-lst)) (send but enable #t))))
                        (begin (send f set-field-background red)
                               (set-box! params-lst (list-set (unbox params-lst) pos #f))
                               (send but enable #f))))))]))
      (send t set-field-background white)
      t)

    (cond ((and mod-path interval)
           (set-box! params-lst (list mod-path interval)))
          (mod-path
           (set-box! params-lst (list mod-path #f)))
          (interval
           (set-box! params-lst (list #f interval))))

    (make-field "module path" module-path? 0)
    (make-field "interval" positive? 1)

    (inherit show)
    (show #t)

    (void
     (parameterize ((current-custodian mc))
       (thread
        (lambda ()
          (define params (sync ch))
          
          (define/contract mappings (listof (list/c string? (-> exact-nonnegative-integer? string?))) (dynamic-require (car params) 'table))
          
          (define (get-and-set pid)
            (send lb set (map car mappings) (map (lambda (m) ((cadr m) pid)) mappings)))

          (define-values (sb out in err) (apply subprocess (current-output-port) (current-input-port) (current-error-port) #f args))
          
          (let loop ((n 0))
            (cond ((sync/timeout (cadr params) sb)
                   (displayln (format "~a samples are taken" n)))
                  (else
                   (get-and-set (subprocess-pid sb))
                   (loop (add1 n)))))))))))
