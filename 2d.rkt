#lang racket

(require racket/gui)

(define (clamp lower value upper)
  (max lower (min value upper)))

(struct planet (x y type strength shield-strength owner) #:mutable #:transparent)

(define *planets* null)
(define *tick-number* 0)
(define *planet-rotation-speed* (degrees->radians 45/60))  ; radians per frame
(define *planet-size* 40)

(define (planet-image plt)
  (let* ([-size/2 (- (/ *planet-size* 2))]
         [bm (make-bitmap *planet-size* *planet-size*)]
         [dc (send bm make-dc)]
         [color (planet-owner plt)])
    (let-values ([(w h) (send dc get-size)])
      (send dc set-origin (/ w 2) (/ h 2)))
    (send dc set-rotation (* *tick-number* *planet-rotation-speed* *animation-speed*))
    (case (planet-type plt)
      [(normal)
       (send dc set-brush color 'solid)
       (send dc draw-ellipse -size/2 -size/2 *planet-size* *planet-size*)]
      [(economic)
       (send dc set-brush color 'solid)
       (send dc draw-ellipse -size/2 -size/2 *planet-size* *planet-size*)
       (send dc set-brush "" 'transparent)
       (send dc set-pen "white" (/ *planet-size* 5) 'solid)
       (let* ([xy (* -3/10 *planet-size*)]
              [wh (+ *planet-size* xy xy)])
         (send dc draw-rounded-rectangle xy xy wh wh -1/4))]
      [(defensive)
       (send dc set-brush (format "dark ~a" color) 'solid)
       (for ([start '(0 1/3 2/3)])
         (send dc draw-arc -size/2 -size/2 *planet-size* *planet-size* start (+ start 1/6)))
       (send dc set-brush color 'solid)
       (let* ([xy (* -3/8 *planet-size*)]
              [wh (+ *planet-size* xy xy)])
         (send dc draw-ellipse xy xy wh wh))])
    (send dc set-rotation 0)
    (send dc set-font (send the-font-list find-or-create-font
                            (/ *planet-size* 3) "Fira Sans" 'swiss 'normal 'bold))
    (send dc set-text-foreground "black")
    (let*-values ([(text) (number->string (truncate (planet-strength plt)))]
                  [(w h _ __) (send dc get-text-extent text #f #t)])
      (send dc draw-text text (- (/ w 2)) (- (/ h 2))))
    bm))

(define (tick-planet p)
  (define type (planet-type p))
  (define strength
    (+ (planet-strength p)
       (* *animation-speed*
          (case type [(normal) 1/10] [(economic) 1/5] [(defensive) 1/15]))))
  (set-planet-shield-strength!
   p (+ (planet-shield-strength p)
        (* *animation-speed*
           (case type [(normal) 1/15] [(economic) 0] [(defensive) 1/5]))))
  (cond
    [(and (eq? type 'normal) (>= strength 10))
     (set-planet-type! p 'economic)
     (set-planet-strength! p (- strength 10))
     (set-planet-shield-strength! p 0)]
    [(and (eq? type 'economic) (>= strength 20))
     (set-planet-type! p 'defensive)
     (set-planet-strength! p (- strength 20))
     (set-planet-shield-strength! p 10)]))

(define (closest-planet x y)
  (for/fold ([dist^2 +inf.0] [closest #f] #:result (values (sqrt dist^2) closest))
            ([p *planets*])
    (let ([d^2 (+ (sqr (- x (planet-x p))) (sqr (- y (planet-y p))))])
      (if (< dist^2 d^2)
          (values dist^2 closest)
          (values d^2 p)))))

(define (draw canvas dc)
  (set! *tick-number* (add1 *tick-number*))
  (for-each tick-planet *planets*)
  (define-values (canvas-width canvas-height) (send canvas get-virtual-size))
  (for ([p *planets*])
    (let* ([planet-bitmap (planet-image p)])
      (send dc draw-bitmap planet-bitmap
            (clamp 0 (- (planet-x p) (round (/ (send planet-bitmap get-width) 2))) canvas-width)
            (clamp 0 (- (planet-y p) (round (/ (send planet-bitmap get-height) 2))) canvas-height)))))

(define *currently-inserting-planet-type* 'normal)
(define *animation-speed* 1)

(define *gui-toplevel* (new frame% [label "Star Empire"]))
(void
 (let* ([top-container (new vertical-pane% [parent *gui-toplevel*])]
        [toolbar (new horizontal-panel% [parent top-container] [stretchable-height #f])]
        [speed-label (new message% [parent toolbar] [auto-resize #t]
                          [label (string-append (real->decimal-string *animation-speed* 1) "×")])])
   ;; speed-label should be at the end of the toolbar
   (send toolbar delete-child speed-label)

   (new radio-box% [parent toolbar] [label "Planet type"]
        [choices '("normal" "economic" "defensive")]
        [style '(horizontal horizontal-label)]
        [callback (lambda (rb evt)
                    (set! *currently-inserting-planet-type*
                          (case (send rb get-selection)
                            [(#f 0) 'normal]
                            [(1) 'economic]
                            [(2) 'defensive])))])

   ;; slider values in tenths as it needs integers
   (new slider% [parent toolbar] [label "Speed"] [min-value 1] [max-value 40] [init-value (* 10 *animation-speed*)]
        [style '(plain horizontal horizontal-label)] [stretchable-width #t]
        [callback (lambda (sl evt)
                    (set! *animation-speed* (/ (send sl get-value) 10))
                    (send speed-label set-label
                          (string-append (real->decimal-string *animation-speed* 1) "×")))])

   ;; add speed-label to end of toolbar
   (send toolbar add-child speed-label)

   (new (class canvas%
          (inherit refresh)

          (define/override (on-event event)
            (cond
              [(send event button-up? 'right)
               ;; place a new planet
               (let-values ([(dist _) (closest-planet (send event get-x) (send event get-y))])
                 (when (> dist 100)
                   (set! *planets* (cons (planet (send event get-x) (send event get-y)
                                                 *currently-inserting-planet-type* 0 0 "green")
                                         *planets*))))])
            (refresh))

          (define/override (on-char event)
            (cond
              [(or (equal? (send event get-key-code) #\q)
                   (equal? (send event get-key-code) 'escape))
               (send *gui-toplevel* show #f)])
            (refresh))

          (super-new))
        [parent top-container] [style '(resize-corner)] [paint-callback draw])))

(module* main #f
  (send *gui-toplevel* show #t))
