#lang info
(define collection "rpv")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "gui-lib"))
(define scribblings '(("scribblings/rpv.scrbl" ())))
(define pkg-desc "A simple performance visualizer for racket")
(define version "0.0")
(define pkg-authors '(hin))
(define license '(Apache-2.0 OR MIT))
