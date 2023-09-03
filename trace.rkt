#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; trace.rkt module
;; Tanner Hornsby
;;
;; Ray casting and recursive ray tracing
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
(require "color.rkt")
(require "camera.rkt")
(require "material.rkt")
(require "object.rkt")
(require "hit.rkt")

;; A (Scene obj bg) packages up the description of a scene, where obj
;; is an object representing the geometric objects in the scene and
;; bg is a function that computes a background color based on a ray
(define-struct Scene
  ([world : Object]
   [background : (Ray -> RGB)]))

(: cast-ray-in-world : Scene -> Ray -> RGB)
;; ray caster for testing purposes
(define (cast-ray-in-world scene)
  (match scene
    [(Scene world ray->background)
     (lambda ([ray : Ray])
       (match (first-entry (hit-test world ray 0.001))
         [(Some hit)
          (match (get-reflection ray hit)
            ['None (ray->background ray)]
            [(Some (Reflect-Info rgb _)) rgb])]
         ['None (ray->background ray)]))]))

(: trace-ray-in-world : Scene Natural -> Ray -> RGB)
;; Given a world and a maximum tracing depth, this function returns
;; a function that will recursively trace a ray through the world
;; to compute a color
(define (trace-ray-in-world scene limit)
  (lambda ([ray : Ray])
    (local
      {(: recur_trace : Ray Natural -> RGB)
       (define (recur_trace r n)
         (if (= n 0) rgb-black
         (match (first-entry (hit-test (Scene-world scene) r .001)) 
           ['None ((Scene-background scene) ray)]
           [(Some hit)
            (match (get-reflection r hit)
              ['None ((Material-emit (Hit-material hit)) r (Hit-norm hit))]
              [(Some (Reflect-Info att ref)) (rgb+ (rgb* att (recur_trace ref (- n 1)))  ((Material-emit (Hit-material hit)) r (Hit-norm hit)))])])))}
      (recur_trace ray limit)  
      )))

(: ray-tracer : Camera Scene Natural -> Image)
;; Given a camera, world object, and max depth, render a scene
;; using the given depth limit.
(define (ray-tracer cam scene depth)
  (foreach-pixel cam
               (make-pixel-renderer
                (antialias-pixel->rgb cam
                                      (trace-ray-in-world scene depth))
                gamma-rgb->color)))

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide Scene)

(provide cast-ray-in-world
         trace-ray-in-world
         ray-tracer)

