#lang typed/racket

(require pict3d
         pict3d/universe
         typed/racket/gui)

(define shiny-material
  (material #:ambient 0.01 #:diffuse 0.39 #:specular 0.6 #:roughness 0.2))
(define planet-material
  (material #:ambient 0.1 #:diffuse 0.3 #:specular 0.6 #:roughness 0.4))
(current-material planet-material)

(define static
  (freeze (combine (sunlight (dir 1 1 1) (emitted "azure" 500))
                   (sphere (pos +16 +16 +2) 1)
                   (sphere (pos +16 -16 -2) 1)
                   (sphere (pos -16 +16 -2) 1)
                   (sphere (pos -16 -16 +2) 1))))

(struct game
  ([run-state : (U 'continue 'pause 'stop)]
   [camera-pos : Pos])
  #:type-name State)

(define (continue [s : State]) : State
  (struct-copy game s [run-state 'continue]))

(define (pause [s : State]) : State
  (struct-copy game s [run-state 'pause]))

(define (stop [s : State]) : State
  (struct-copy game s [run-state 'stop]))

(define (paused? [s : State] [n : Natural] [t : Flonum]) : Boolean
  (eq? (game-run-state s) 'pause))

(define (stopped? [s : State] [n : Natural] [t : Flonum]) : Boolean
  (eq? (game-run-state s) 'stop))

(define (move-camera [s : State] [move-dir : Dir]) : State
  (struct-copy game s [camera-pos (pos+ (game-camera-pos s) move-dir)]))

(define (on-frame [s : State] [n : Natural] [t : Flonum]) : State
  s)

(define (on-draw [s : State] [n : Natural] [t : Flonum]) : Pict3D
  (combine (basis 'camera (point-at (game-camera-pos s) origin))
           static
           (rotate-z (cube origin 2) (/ t 10))))

(define (on-key [s : State] [n : Natural] [t : Flonum] [k : String]) : State
  ;(printf "key: ~a~%" k)
  (match k
    ["q" (stop s)]
    [" " (if (paused? s n t) (continue s) (pause s))]
    ;; ignore further key events when not running
    [_ #:when (or (paused? s n t) (stopped? s n t)) s]
    ["up" (move-camera s +y)]
    ["down" (move-camera s -y)]
    ["left" (move-camera s -x)]
    ["right" (move-camera s +x)]
    [_ s]))

(define (on-mouse [s : State] [n : Natural] [t : Flonum] [x : Integer] [y : Integer] [e : String]) : State
  ;(printf "mouse: ~a (~a, ~a)~%" e x y)
  (match e
    ["left-down"
     (match (camera-transform static)
       [(and t (affine _ _ _ camera-pos))
        (writeln (trace/data static camera-pos ((camera-ray-dir t) x y)))]
       [_ (error "got invalid (camera-transform static)")])]
    [_ s])
  s)

(module* main #f
  ((inst big-bang3d State)
   (game 'continue (pos 0 0 64))
   #:name "Star Empire"
   #:cursor (let ([c (make-object cursor% 'hand)]) (and (send c ok?) c))
   #:frame-delay 1000/30  ; 30 FPS
   #:on-frame on-frame #:on-draw on-draw #:on-key on-key #:on-mouse on-mouse
   #:pause-state? paused? #:stop-state? stopped?))
