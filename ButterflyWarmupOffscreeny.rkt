;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ButterflyWarmupOffscreeny) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;;;;;;;Butterfly Warmup
;;;;;Burleigh Charlton
;;;;;Owen Russel Lanning
(require 2htdp/image)
(require 2htdp/universe)

;                                                                          
;                                                                          
;                                                                          
;                                                                          
;     ;;;;                                                                 
;    ;   ;                           ;                       ;             
;   ;        ;;;;   ;; ;;    ;;;;;  ;;;;;    ;;;;   ;; ;;   ;;;;;    ;;;;; 
;   ;       ;    ;   ;;  ;  ;    ;   ;      ;    ;   ;;  ;   ;      ;    ; 
;   ;       ;    ;   ;   ;   ;;;;    ;       ;;;;;   ;   ;   ;       ;;;;  
;   ;       ;    ;   ;   ;       ;   ;      ;    ;   ;   ;   ;           ; 
;    ;   ;  ;    ;   ;   ;  ;    ;   ;   ;  ;   ;;   ;   ;   ;   ;  ;    ; 
;     ;;;    ;;;;   ;;; ;;; ;;;;;     ;;;    ;;; ;; ;;; ;;;   ;;;   ;;;;;  
;                                                                          
;                                                                          
;                                                                          
;                                                                          

(define BUTTERFLY (bitmap/url "http://files.fellowhuman.com/butterfly.png"))
(define BGW 200)
(define BGH 50)
(define BG (empty-scene BGW BGH))
(define STARTWORLD (make-posn 20 25))
(define VELOCITY 5)




;                                          
;                                          
;                                          
;                                          
;   ;;; ;;;                   ;;        ;; 
;    ;   ;                     ;         ; 
;    ;   ;   ;;;;   ;; ;;;     ;     ;;; ; 
;    ; ; ;  ;    ;   ;;        ;    ;   ;; 
;    ; ; ;  ;    ;   ;         ;    ;    ; 
;    ; ; ;  ;    ;   ;         ;    ;    ; 
;    ; ; ;  ;    ;   ;         ;    ;   ;; 
;     ; ;    ;;;;   ;;;;;    ;;;;;   ;;; ;;
;                                          
;                                          
;                                          
;
;;The world is a structure called a posn

;                                                                          
;                                                                          
;                                                                          
;   ;;;;;;                                     ;                           
;    ;   ;                           ;                                     
;    ; ;    ;;  ;;  ;; ;;    ;;; ;  ;;;;;    ;;;     ;;;;   ;; ;;    ;;;;; 
;    ;;;     ;   ;   ;;  ;  ;   ;;   ;         ;    ;    ;   ;;  ;  ;    ; 
;    ; ;     ;   ;   ;   ;  ;        ;         ;    ;    ;   ;   ;   ;;;;  
;    ;       ;   ;   ;   ;  ;        ;         ;    ;    ;   ;   ;       ; 
;    ;       ;  ;;   ;   ;  ;    ;   ;   ;     ;    ;    ;   ;   ;  ;    ; 
;   ;;;       ;; ;; ;;; ;;;  ;;;;     ;;;    ;;;;;   ;;;;   ;;; ;;; ;;;;;  
;                                                                          
;                                                                          
;                                                                          
;
;;tickmove
;;takes a world (state) and produces a world (state) with the added movement
;;world->world
(define (tickmove posn)
  (cond [(offscreenx? posn)
         (make-posn (- 0 (image-width BUTTERFLY))
                    (/ BGH 2))]
        [else (make-posn (+ (posn-x posn) VELOCITY)
                         (cond [(and
                                 (offscreeny? posn)
                                 (>= (posn-y posn) BGH))
                                (- (posn-y posn) 5)]
                               [(and
                                 (offscreeny? posn)
                                 (<= (posn-y posn) (/ (image-height BUTTERFLY) 2)))
                                (+ (posn-y posn) 5)]
                               [else
                                (+ (posn-y posn) (- (random 10) 5))]))]))

(check-within (tickmove (make-posn 20 25))
              (make-posn 25 25) 10)

(check-within (tickmove (make-posn 300 25))
              (make-posn -21 25) 10)

(check-within (tickmove (make-posn 200 51))
                        (make-posn 200 46) 10)

(check-within (tickmove (make-posn 200 -1))
                        (make-posn 200 4) 10)

;;offscreenx?
;;takes posn produces true if x is offscreen
;;posn->boolean

(define (offscreenx? posn)
  (not (and
        (< (posn-x posn) (+ (image-width BUTTERFLY) BGW))
        (> (posn-x posn) (- 0 (image-width BUTTERFLY) BGW)))))

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
        (> (posn-y posn) (/ (image-height BUTTERFLY) 2)))))

(check-expect (offscreeny? (make-posn 20 25))
              #false)
(check-expect (offscreeny? (make-posn 20 300))
              #true)
(check-expect (offscreeny? (make-posn 20 -45))
              #true)
;;drawworld
;;takes a posn and produces an image of butterfly based on posn
;;posn->image

(define (drawworld posn)
  (place-image/align BUTTERFLY
                     (posn-x posn)
                     (posn-y posn)
                     "middle"
                     "middle"
                     BG))

(check-expect (drawworld (make-posn 20 25))
              (place-image/align BUTTERFLY
                                 20
                                 25
                                 "middle"
                                 "middle"
                                 BG))






;                                                  
;                                                  
;                                                  
;                                                  
;   ;;; ;;;                 ;;                     
;    ;   ;                   ;                     
;    ;   ;   ;;;;    ;;;;    ; ;;;; ;;  ;;  ;; ;;  
;    ;;;;;  ;    ;  ;    ;   ;  ;    ;   ;   ;;  ; 
;    ;   ;  ;    ;  ;    ;   ;;;     ;   ;   ;   ; 
;    ;   ;  ;    ;  ;    ;   ; ;     ;   ;   ;   ; 
;    ;   ;  ;    ;  ;    ;   ;  ;    ;  ;;   ;   ; 
;   ;;; ;;;  ;;;;    ;;;;   ;;  ;;;   ;; ;;  ;;;;  
;                                            ;     
;                                           ;;;    
;                                                  
;                                                  

(define (main w)
  (big-bang w
            [on-tick tickmove]
            [to-draw drawworld]))
