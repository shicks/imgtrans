(define (script-fu-creeping-gradient
         inImage
         inDrawable
         inRadius)
  (gimp-undo-push-group-start inImage)
  (let ((layer (car (gimp-layer-copy inDrawable 1)))
        (channel (car (gimp-selection-save inImage))))
    (gimp-image-add-layer inImage layer -1)
    (gimp-edit-clear layer)
    (script-fu-incremental-blur-step inImage layer channel 0 inIncrement inRadius inSteps)
    ;(gimp-drawable-delete layer)
    (gimp-selection-load channel)
    (gimp-image-remove-channel inImage channel)
    (script-fu-incremental-blur-merge inImage inSteps)
  )
  (gimp-undo-push-group-end inImage)
)

(define (script-fu-incremental-blur-step
         image
         layer
         channel
         step
         increment
         radius
         steps)
  (if (> steps 0)
    (let ((copy (car (gimp-layer-copy layer 1)))
          (blur (floor (* (+ 1 step) increment)))
          (rad (floor (* (+ 1 step) radius))))
      (gimp-image-add-layer image copy -1)
      (gimp-selection-load channel)
      (gimp-selection-grow image rad)
      (gimp-edit-clear copy)
      (if (> steps 1) ;
          (begin ;
            (gimp-selection-grow image (floor (+ rad radius increment))) ;
            (gimp-selection-invert image)
            (gimp-edit-clear copy))) ;
      (gimp-selection-none image) ;
      (plug-in-gauss-iir2 1 image copy blur blur)
      ; (gimp-selection-invert image)
      (gimp-image-raise-layer-to-top image copy)
      (script-fu-incremental-blur-step image layer channel (+ 1 step) increment radius (- steps 1)))))

(define (script-fu-incremental-blur-merge image count)
  (if (> count 0)
      (begin
        (gimp-image-merge-down image (vector-ref (car (cdr (gimp-image-get-layers image))) 0) 0)
        (script-fu-incremental-blur-merge image (- count 1)))))

(script-fu-register
 "script-fu-incremental-blur"                ;func name
 "Incremental Blur..."                       ;menu label
 "Incrementally blurs around the selection." ;description
 "Stephen Hicks"                             ;author
 "Copyright 2013 Stephen Hicks"              ;copyright notice
 "May 14, 2013"                              ;date created
 "RGB*"                     ;image type that the script works on
 SF-IMAGE       "Image"         0
 SF-DRAWABLE    "Drawable"      0
 SF-ADJUSTMENT  "Increment"     '(1 0.1 100 0.1 1 1 1)
 SF-ADJUSTMENT  "Radius"        '(2 1 500 1 10 1 1)
 SF-ADJUSTMENT  "Steps"         '(10 1 50 1 5 0 1)
 )
(script-fu-menu-register "script-fu-incremental-blur" "<Image>/Filters/Blur")


;; NOTE: incremental-blur w/ .5/2 * 40
;;       then feather 20, invert, fill black, soft light
;;       then grow 20, feather 40, invert, fill black, hard black 85%
;; Might want to blurry-grow right-hand margin a bit first
;;       so can fade to 100% black at some point...?
