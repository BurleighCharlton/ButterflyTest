;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Warm up Butterfly part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define butterfly (bitmap/url "http://files.fellowhuman.com/butterfly.png"))
(define BGW 200)
(define BGH 50)
(define BG (empty-scene BGW BGH))
(define startworld (make-posn 20 25))
(define velocity 5)




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
         (make-posn (/ (image-width butterfly) 2)
                    (/ BGH 2))]
        [else (make-posn (+ (posn-x posn) velocity)
                         (+ (posn-y posn) (- (random 10) 5)))]))

(check-within (tickmove (make-posn 20 25))
              (make-posn 25 25) 10)

(check-within (tickmove (make-posn 300 25))
              (make-posn 10.5 25) 10)

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


;;drawworld
;;takes a posn and produces an image of butterfly based on posn
;;posn->image

(define (drawworld posn)
  (place-image/align butterfly
                     (posn-x posn)
                     (posn-y posn)
                     "middle"
                     "middle"
                     BG))

(check-expect (drawworld (make-posn 20 25))
              (place-image/align butterfly
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

(big-bang startworld
          [on-tick tickmove]
          [to-draw drawworld])