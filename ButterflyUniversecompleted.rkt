;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ButterflyUniversecompleted) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;;;;;;;Butterfly Warmup
;;;;;Burleigh Charlton
;;;;;Owen Russel Lanning
(require 2htdp/image)
(require 2htdp/universe)
(require hostname)


;;;;Constants                                                                          

(define butterfly (bitmap/url "http://files.fellowhuman.com/butterfly.png"))
(define BGW 200)
(define BGH 50)
(define BG (empty-scene BGW BGH))
(define restworld 'resting)
(define startworld (make-posn 10 25))
(define velocity 5)



;;;;A handlerresult
; Posn
; ClientMessage is 'done
; ServerMessage is 'go



; Universe World -> universe
; adds world w to the universe, when in state u

(define (add-world u w)
  (local ((define u* (append u (list w))))
    (make-bundle u*
                 (list (make-mail (first u*) 'go))
                 '())))

(check-expect
 (add-world '() iworld1)
 (make-bundle (list iworld1)
              (list (make-mail iworld1 'go))
              '()))

; Universe world message -> Bundle
; world iw sent message m when server is in state u
(define (switch u w m)
  (local ((define u* (append (rest u) (list (first u)))))
    (make-bundle u*
                 (list (make-mail (first u*) 'go))
                 '())))

(check-expect
 (switch (list iworld1 iworld2) iworld1 'done)
 (make-bundle (list iworld2 iworld1)
              (list (make-mail iworld2 'go))
              '()))


;;;;Creates the Universe
;;Allows others to connect to server
#;
(universe '() (on-new add-world) (on-msg switch))





;;;;Worldstate

; world message -> world
; receives go message
(define (receive w m)
  (cond
    [(symbol? w) startworld]
    [else w]))

(check-expect (receive 'resting 'go) startworld)



;;;;World


;;;The world is either
;; -  a posn
;; - 'resting


;;Funcforworld: world ->
;;Processes world
(define (funcforworld w)
  (cond [(symbol=? w 'go) ...]
        [(posn? w)
         (... (posn-x w) ...
              (posn-y w) ...)]))


;;;A Worldresult is either
;; - World
;; – (make-package WorldState ClientMessage)



;;;;Functions
;;tickmove
;;takes a world and produces a world with the added movement. If reaches ond of screen does nothing.
;;If no butterfly does nothing
;;world-> handlerresult
(define (tickmove w)
  (cond [(symbol? w) w]
        [(posn? w)
         (cond [(offscreenx? w)
                (make-package 'resting 'done)]
               [else (make-posn (+ (posn-x w) velocity)
                                (cond [(and
                                        (offscreeny? w)
                                        (>= (posn-y w) BGH))
                                       (- (posn-y w) 5)]
                                      [(and
                                        (offscreeny? w)
                                        (<= (posn-y w) (/ (image-height butterfly) 2)))
                                       (+ (posn-y w) 5)]
                                      [else
                                       (+ (posn-y w) (- (random 10) 5))]))])]))

(check-expect (tickmove 'resting) 'resting)

(check-expect (tickmove (make-posn BGW 25)) (make-package 'resting 'done))


(check-within (tickmove (make-posn 20 25))
              (make-posn 25 25) 10)

(check-within (tickmove (make-posn 300 25))
              (make-package 'resting 'done) 10)

;;offscreenx?
;;takes posn produces true if x is offscreen
;;posn->boolean

(define (offscreenx? posn)
  (not (and
        (< (posn-x posn) BGW)
        (> (posn-x posn) 0))))

(check-expect (offscreenx? (make-posn 20 25))
              #false)
(check-expect (offscreenx? (make-posn 300 25))
              #true)


;;offscreeny?
;;takes posn produces true if y is offscreen
;;posn->boolean
(define (offscreeny? posn)
  (not (and
        (< (posn-y posn) BGH)
        (> (posn-y posn) (/ (image-height butterfly) 2)))))

(check-expect (offscreeny? (make-posn 20 25))
              #false)
(check-expect (offscreeny? (make-posn 20 300))
              #true)
(check-expect (offscreeny? (make-posn 20 -45))
              #true)
;;drawworld
;;takes a posn and produces an image of butterfly based on posn
;;posn->image

(define (drawworld w)
  (cond
    [(symbol? w) (underlay/xy BG 10 10 (text "resting" 10 "blue"))]
    [(posn? w)
     (place-image/align butterfly
                        (posn-x w)
                        (posn-y w)
                        "middle"
                        "middle"
                        BG)]))

(check-expect (drawworld 'resting)
              (underlay/xy BG 10 10 (text "resting" 10 "blue")))

(check-expect (drawworld (make-posn 20 25))
              (place-image/align butterfly
                                 20
                                 25
                                 "middle"
                                 "middle"
                                 BG))






;;;;Hook Up                                                  
;universe: n IP -> World
;takes a name and makes a world
(define (universer n ip)
  (big-bang restworld
            [on-receive receive]
            [on-tick tickmove]
            [to-draw drawworld]
            [name n]
            [register ip]))