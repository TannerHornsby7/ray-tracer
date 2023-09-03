#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; camera.rkt module
;; Tanner Hornsby
;;
;; This module implements the Camera abstraction
;;

;; load custom definitions
;;
(require "../include/cs151-core.rkt")

;; load image library definitions
;;
(require "../include/cs151-image.rkt")

;; project modules
;;
(require "math-util.rkt")
(require "color.rkt")

;; the representation of a Camera for the ray tracer.
(define-struct Camera
  [(wid : Natural)           ;; width of image
   (ht : Natural)            ;; height of image
   (n-samples : Natural)     ;; number of samples per pixel
   (origin : Float3)         ;; where the camera is located
   (ulc : Float3)            ;; upper-left-corner of image plane
   (h-vec : Float3)          ;; horizontal pixel-wide vector parallel to image
                             ;; pointing right
   (v-vec : Float3)])        ;; vertical pixel-wide vector parallel to image
                             ;; pointing down

(: simple-camera : Natural Natural Natural Float -> Camera)
;; make a camera that is equivalent to a Project 1 camera (for
;; testing purposes)
(define (simple-camera wid ht ns flen)
  (local
    {(define pw : Float (/ 2.0 (->fl wid)))}
    (Camera wid ht ns
            fl3-zero
            (Float3 -1.0
                    (/ (->fl ht) (->fl wid))
                    (- flen))
            (Float3 pw 0.0 0.0)
            (Float3 0.0 (- pw) 0.0))))

;Make Camera
(: make-camera : Natural Natural Natural Float3 Float3 Float3 Float -> Camera)
; make a camera.  The arguments are (in order):
;   - width of image
;   - height of image
;   - number of samples per pixel
;   - origin of camera in the world
;   - point that the camera is looking at
;   - up vector
;   - horizontal field of view (in degrees)
(define (make-camera wid ht ns pos look-at up fov)
   (local
     {;;Dcam= (fl3-normalize (fl3- look-at pos))
        ;;Up= (Float3 0 1 0)
        ;;Rcam= (fl3-normalize (fl3-cross Dcam Up))
        ;;Ucam= (fl3-normalize (fl3-cross Rcam Dcam))
        ;;flen= (/ 1 (tan (/ fov wid)))
        ;;Cimg= (fl+ Pcam (fl3-scale flen Dcam))
        ;;ULC= (fl3+ Cimg (fl3- (fl3-scale (/ h w) Ucam) Rcam))
        ;;(define pw : Float (/ 2.0 (->fl wid)))
        ;;h-vect= (fl3-scale pw Rcam)
        ;;v-vect= (fl3-scale (- pw) Ucam)
      (define pw : Float (/ 2.0 (->fl wid)))
      (define Dcam : Float3 (fl3-normalize (fl3- look-at pos)))
      (define Up : Float3 (Float3 0.0 1.0 0.0))
      (define Rcam : Float3 (fl3-normalize (fl3-cross Dcam Up)))
      (define Ucam : Float3 (fl3-normalize (fl3-cross Rcam Dcam)))
      (define flen : Float (/ 1.0 (tan (degrees->radians (/ fov 2)))))
      (define Cimg : Float3 (fl3+ pos (fl3-scale flen Dcam)))
      (define ULC : Float3 (fl3+ Cimg (fl3- (fl3-scale (/ (->fl ht) (->fl wid)) Ucam) Rcam)))
      (define h-vect : Float3 (fl3-scale pw Rcam))
      (define v-vect : Float3 (fl3-negate (fl3-scale pw Ucam)))}
       (Camera
   wid
   ht
   ns
   pos
   ULC
   h-vect
   v-vect
   )))

;; A Pixel-Renderer is a function that takes the row and column of a pixel
;; and produces a Racket Image-Library Color
(define-type Pixel-Renderer (Natural Natural -> Color))

(: foreach-pixel : Camera Pixel-Renderer -> Image)
;; given a camera and a pixel renderer, generate an image.
;;
(define (foreach-pixel cam pixel-renderer)
  (match cam
    [(Camera wid ht _ _ _ _ _)
     (if (or (= wid 0) (= ht 0))
         empty-image
         (local
           {(: for-rows : Natural (Listof Color) -> (Listof Color))
            ;; iterate over the rows of the image from bottom to top
            (define (for-rows row pixels)
              (if (= 0 row)
                  pixels
                  (for-cols (- row 1) wid pixels)))
            (: for-cols :  Natural Natural (Listof Color) -> (Listof Color))
            ;; iterate over the columns of a row from right to left
            (define (for-cols row col pixels)
              (if (= 0 col)
                  (for-rows row pixels)
                  (for-cols
                   row
                   (- col 1)
                   (cons (pixel-renderer row (- col 1)) pixels))))}
           (color-list->bitmap
            (for-rows ht '())
            wid ht)))]))

(: make-pixel-renderer : (Natural Natural -> RGB) (RGB -> Color) -> Pixel-Renderer)
;; compose a function that maps pixel coordinates to RGB values with
;; an RGB to Image-Library Color converter
(define (make-pixel-renderer pixel->rgb rgb->color)
  (lambda ([row : Natural] [col : Natural]) (rgb->color (pixel->rgb row col))))

;; Ray for Pixel Function
(: ray-for-pixel : Camera -> (Natural Natural -> Ray))
;;Takes in a Camera and returns a function for generating a ray for a pixel specified by its row and column
;;pw= 2/w
;;pw/2 - 1 = center of pixel laterally
;;(cam height/ cam width) - (pixel width/2) = center of pixel horizontally.
(define (ray-for-pixel var_cam)
  (lambda ([c : Natural] [r : Natural])
    (local
      {;;pw = pixel_width = 2/w
       (define pw : Float (/ 2.0 (->fl (Camera-wid var_cam))))
       ;;given a pixel, calculate its center and returns its float3 location
       (: pixel_center Float3)
       (define pixel_center
         (fl3+ (fl3+ (fl3+ (Camera-ulc var_cam) (fl3-scale .5 (fl3+ (Camera-v-vec var_cam) (Camera-h-vec var_cam))))
               (fl3-scale (->fl r) (Camera-v-vec var_cam)))
               (fl3-scale (->fl c) (Camera-h-vec var_cam))))}
    (make-ray (Camera-origin var_cam)
         (fl3- pixel_center (Camera-origin var_cam))))))
;;check that the last element of the array is properly converted to a Ray
;;(check-within ((ray-for-pixel cam) 0 0) (Ray (Float3 0.0 0.0 0.0)
             ;; (Float3 (+ (/ 399.0 400.0) 2) (- (- (/ 1.0 400) .5) 1) (-(Camera-focal-len cam)))) .05)


;; Rays for Pixel Function
(: rays-for-pixel : Camera -> (Natural Natural -> (Listof Ray)))
;;Rays for pixel takes in a camera and outputs a function for finding the list of rays at pixel(c, r)
(define (rays-for-pixel cam)
  (lambda ([c : Natural] [r : Natural])
    (local
      {;;pw = pixel_width = 2/w
       (define pw : Float (/ 2.0 (->fl (Camera-wid cam))))
       (: comp-ray-list : Natural (Listof Ray) -> (Listof Ray))
       (define (comp-ray-list n prod)
         (local
           {(define rx : Float (random))
            (define ry : Float (random))
            (define ray_dir : Float3
              (fl3- (fl3+ (fl3+ (Camera-ulc cam)
                          (fl3-scale (+ rx (->fl c)) (Camera-h-vec cam)))
                    (fl3-scale (+ ry (->fl r)) (Camera-v-vec cam))) (Camera-origin cam)))}
         (if (< n (Camera-n-samples cam))
             (comp-ray-list (+ n 1) (cons (make-ray (Camera-origin cam) ray_dir) prod))
             prod)))}
      (comp-ray-list 0 '()))))

;(fl3+ (fl3+ (fl3+ (Camera-ulc var_cam) (fl3-scale .5 (fl3+ (Camera-v-vec var_cam) (Camera-h-vec var_cam))))
;               (fl3-scale (->fl r) (Camera-v-vec var_cam)))
;               (fl3-scale (->fl r) (Camera-v-vec var_cam)))


;;pixel to rgb
(: pixel->rgb : Camera (Ray -> RGB) -> Natural Natural -> RGB)
;; given a camera and a ray-tracing function, return a function that
;; traces the ray for the given pixel
(define (pixel->rgb var_cam2 ray_trace)
    (local
      {(: ray_funct : Natural Natural -> Ray)
       (define ray_funct (ray-for-pixel var_cam2))
       }
      
      (lambda ([c : Natural] [r : Natural]) (ray_trace (ray_funct r c)))))

;;Anti alias pixel to rgb
(: antialias-pixel->rgb : Camera (Ray -> RGB) -> Natural Natural -> RGB)
;; given a camera and a ray-tracing function, return a function that
;; traces a list of rays for the given pixel and returns their average
(define (antialias-pixel->rgb cam ray_trace)
   (local
      {(define scale_factor : Float (if (= (->fl (Camera-n-samples cam)) 0.0) 1.0 (/ 1.0 (->fl (Camera-n-samples cam)))))
       (: ray_funct : Natural Natural -> (Listof Ray))
       (define ray_funct (rays-for-pixel cam))}
      (lambda ([c : Natural] [r : Natural]) (rgb-scale scale_factor
      (foldl (lambda ([ray : Ray] [rgb : RGB]) (rgb+ (ray_trace ray) rgb)) (RGB 0.0 0.0 0.0) (ray_funct r c))))))


(: ray->rgb : Ray -> RGB)
;; a function for testing ray generation.  It maps a ray to a color in
;; the white-to-blue range based on the Y component of the ray's direction
;; vector.
(define (ray->rgb ray)
  (match ray
    [(Ray _ dir)
     (local
       {(define t : Float (* 0.5 (+ 1.0 (Float3-y dir))))}
       (rgb-lerp rgb-white t (RGB 0.5 0.7 1.0)))]))

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide Camera)

(provide make-camera
         simple-camera
         foreach-pixel
         make-pixel-renderer
         pixel->rgb
         antialias-pixel->rgb
         ray->rgb)
