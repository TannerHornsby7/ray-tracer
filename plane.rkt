#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; plane.rkt module
;; Tanner Hornsby
;;
;; This module contains the implementation of the plane object
;;

;; load custom definitions
;;
(require "../include/cs151-core.rkt")

;; load image library definitions
;;
(require "../include/cs151-image.rkt")

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; project modules
;;
(require "util.rkt")
(require "math-util.rkt")
(require "material.rkt")
(require "hit.rkt")
(require "object.rkt")

(: make-plane : Float3 Float3 Material -> Object)
;; (make-plane pt perp material) makes a plane object.  The plane
;; contains the point pt and its orientation is defined by the
;; vector perp, which is perpendicular to the plane.  The third
;; argument specifies the plane's surface material.
;; Note that the perpendicular vector does not have to be unit length.
(define (make-plane pt perp material)
  (Object (lambda ([ray : Ray] [min-t : Float])
            (local
              {;;defining the normalized perp
               (define Nnorm : Float3 (fl3-normalize perp))

               ;;defining the direction of the ray
               (define direction : Float3 (Ray-dir ray))

               ;;dot of perp and direction
               (define dperp : Float (fl3-dot perp direction))

               ;;defining the origin of the ray
               (define point-P : Float3 (Ray-origin ray))

               ;;defining the N
               (define n : Float (fl3-dot (fl3- pt point-P) Nnorm))

               ;;defining behind the plane
               (: behind-plane : Ray -> Boolean )
               (define (behind-plane r)
                 (<= (fl3-dot Nnorm (fl3- (Ray-origin r) pt)) 0))

               ;;defining t
               (define t : Float (/ n (fl3-dot direction Nnorm)))

               ;;defining intersection
               (define intersection : Float3 (fl3+ point-P (fl3-scale t direction)))}
              (cond
                [(<= -.0001 dperp .0001) (if (behind-plane ray) (list (Hit 'OUT +inf.0 fl3-zero fl3-zero flat-black)) miss)]
                [(> dperp 0.0001) (if (<= min-t t) (list (Hit 'OUT t intersection Nnorm material)) miss)]
                [else (if (<= min-t t) (list (Hit 'IN t intersection Nnorm material) (Hit 'OUT +inf.0 fl3-zero fl3-zero flat-black)) miss)])))))
                                                 
                                              
                                           
;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide make-plane)
