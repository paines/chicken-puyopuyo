  ;;TODO
  ;;du musst um paus eund so  besser zu handlen als erstes events abfragen
  ;;dann sachen machen

  ;;auch dropThemDown aufrufen wenn einer hängen bleibt

;;restart nach gameover
;;gameover und ganzes game flackert flackert !!



(use sdl)
(use posix)
(use srfi-69)
(use srfi-18)
(use defstruct)
(use loops)

;;width/height of bitmaps is 32x32, anyway this should be determinded after loading those images
(define *puyoW* 32)
(define *puyoH* 32)

(define *fieldW* 6)
(define *fieldH* 12)

(define *maxCols* 4)

(define *points* 0)
(define *level* 1)

(define *field* (make-vector (* *fieldW* *fieldH*) -1))
(define *solveField* (make-vector (* *fieldW* *fieldH*) -1))

(define *state* 0)
(define *run* 0)
(define *puyos* #f)

;;a puyo consists of colors 0-3 (0=blue, 1=red, 2=green and 3=yellow), a state (dropping or landed) and a position
(defstruct puyo
  x
  y
  col
  state) ;;dropping / landed

;;we need two stones, 1st and 2nd
;;these need to be global, so that the dropPuyo-thread can access the stones
(define *first* 0)
(define *second* 0)

;;sdl surface pictures for blitting
(define *surface* 0)
(define *font* 0)
(define *blue* 0)
(define *red* 0)
(define *green* 0)
(define *yellow* 0)
(define *gameover* 0)

;i added joystick support for the GP2X
(define *joystick* 0)


(define (clearField)
  (do-for y (0 *fieldH*)
	  (do-for x (0 *fieldW*)
		  (vector-set! *field* (getOffset x y) -1))))

(define (printField f)
  (do-for y (0 *fieldH*)
	  (do-for x (0 *fieldW*)
		  (printf "~X " (vector-ref f (getOffset x y))))
	  (printf "~%")))

(define (countElements f elem)
  (let ((n 0))
    (do-for counter (0 (vector-length f))
	    (if (= (vector-ref f counter) elem)
		(set! n (+ n 1))))
    n))

(define (copyField src dst)
  (do-for i (0 (vector-length src))
	  (vector-set! dst i (vector-ref src i))))

(define (drawField f)
  (let ((x 0)
	(y 0)
	(count 0)
	(col 0))
    
    (do-for i (0 (vector-length f))
	    (set! col (vector-ref f i))
	    (cond
	     ((= col 0 )
	      (sdl-blit-surface *blue* #f *surface* (make-sdl-rect x y 32 32)))
	     ((= col 1 )
	      (sdl-blit-surface *red* #f *surface* (make-sdl-rect x y 32 32)))
	     ((= col 2 )
	      (sdl-blit-surface *green* #f *surface* (make-sdl-rect x y 32 32)))
	     ((= col 3 )
	      (sdl-blit-surface *yellow* #f *surface* (make-sdl-rect x y 32 32))))

	    (set! x (+ x 32))
	    (set! count (+ count 1))
	    (if (= (modulo count 6) 0)
		(begin
		  (set! y (+ y 32))
		  (set! x 0))))))
		 
(define (moveToLeft f s)
  (set! *state* 'pause)
  (let (
	(fx (puyo-x f))
	(fy (puyo-y f))
	(sy (puyo-y s))
	(sx (puyo-x s))
	(fstate (puyo-state f))
	(sstate (puyo-state s)))
    (if (and (eq? sstate 'dropping) (eq? fstate 'dropping))
	(cond 
	  ;;stones lay on each other
	  ((= fx sx)
	   (if (and (= (vector-ref *field* (getOffset (- fx 1) fy)) -1) (>= (- fx 1) 0)
		   (= (vector-ref *field* (getOffset (- sx 1) sy)) -1) (>= (- sx 1) 0))
	       (begin
		 (puyo-x-set! f (- fx 1))
		 (puyo-x-set! s (- sx 1)))))
	  ;;s is left from f
	  ((< sx fx)
	   (if (and (= (vector-ref *field* (getOffset (- sx 1) sy)) -1) (>= (- sx 1) 0))
	       (begin
		 (puyo-x-set! s (- sx 1))
		 (puyo-x-set! f (- fx 1)))))
	  ;;f is left from s
	  ((< fx sx)
	     (if (and (= (vector-ref *field* (getOffset (- fx 1) fy)) -1) (>= (- fx 1) 0))
		 (begin
		   (puyo-x-set! f (- fx 1))	       
		   (puyo-x-set! s (- sx 1))))))))
  (set! *state* 'unpause))

(define (moveToRight f s)
  (set! *state* 'pause)
  (let ((fx (puyo-x f))
	(fy (puyo-y f))
	(sy (puyo-y s))
	(sx (puyo-x s))
	(fstate (puyo-state f))
	(sstate (puyo-state s)))
    (if (and (eq? sstate 'dropping) (eq? fstate 'dropping))
	(cond 
	  ;;stones lay on each other
	  ((= fx sx)
	   (begin
	     (if (and
		  (= (vector-ref *field* (getOffset (+ fx 1) fy)) -1) (< (+ fx 1 ) *fieldW*)
		  (= (vector-ref *field* (getOffset (+ sx 1) sy)) -1) (< (+ sx 1 ) *fieldW*))
		 (begin
		   (puyo-x-set! f (+ fx 1))
		   (puyo-x-set! s (+ sx 1))))))
	  ;;fx is left from sx
	  ((< fx sx)
	   (begin
	     (if (and (= (vector-ref *field* (getOffset (+ sx 1) sy)) -1) (< (+ sx 1) *fieldW*))
		 (begin
		   (puyo-x-set! s (+ sx 1)) 
		   (puyo-x-set! f (+ fx 1))))))	       
	  ;;sx is left from sx
	  ((< sx fx)
	   (begin
	     (if (and (= (vector-ref *field* (getOffset (+ fx 1) fy)) -1) (< (+ fx 1) *fieldW*))
		 (begin
		   (puyo-x-set! f (+ fx 1))
		   (puyo-x-set! s (+ sx 1)))))))))
  (set! *state* 'unpause))


(define (rotateStones f s)
;  (set! *state* 'pause)
  (let ((fx (puyo-x f))
	(fy (puyo-y f))
	(sy (puyo-y s))
	(sx (puyo-x s)))
    (cond
      ((< fx sx)
       (if (not (eq? sy 0))
	   (begin
	     (puyo-y-set! f (- fy 1))
	     (puyo-x-set! f sx))
	   (begin
	     (puyo-y-set! f (+ fy 1))
	     (puyo-x-set! f sx))))
      ((< fy sy)(= fx sx)
       (if (= fx 0)
	   (begin
	     (puyo-x-set! f (+ fx 1))
	     (puyo-y-set! f (+ fy 1)))
	   (begin
	     (puyo-y-set! s (- sy 1))
	     (puyo-x-set! s (- sx 1)))))
      ((> fx sx) (= fy sy)
       (if (not (eq? sy 0))
	   (begin
	     (puyo-x-set! s (+ sx 1))
	     (puyo-y-set! s (- sy 1)))
	   (begin
	     (puyo-x-set! s (+ sx 1))
	     (puyo-y-set! f (+ fy 1)))))
      ((> fy sy)(= fx sx)
       (if (= fx 0)
	   (begin
	     (puyo-x-set! s (+ sx 1))
	     (puyo-y-set! s (+ sy 1)))
	   (begin
	     (puyo-y-set! s (+ sy 1))
	     (puyo-x-set! f (- sx 1)))))))
 ; (set! *state* 'unpause))
)
(define (getOffset x y)
  (+ (* y *fieldW*) x))

(define (dropPuyos)
  (let ((landed 0))
    (set! *puyos* (sort *puyos* (lambda (x y) (> (puyo-y x) (puyo-y y)))))
    (do-list p *puyos*
	     (puyosToField)
	     (if (eq? (puyo-state p) 'dropping)
		 (if (and (< (+ (puyo-y p) 1) *fieldH*)
			  (= (vector-ref *field* (getOffset (puyo-x p) (+ (puyo-y p) 1))) -1))
		     (puyo-y-set! p (+ (puyo-y p) 1))
		     (puyo-state-set! p 'landed))))

    (do-list p *puyos*
	     (if (eq? (puyo-state p) 'landed)
		 (set! landed (+ landed 1))))
    
    (if (eq? landed (length *puyos*))
	0
	1)))

(define (backtrack f x y col)
  (if (and (>= x 0) (>= y 0) (< x *fieldW*) (< y *fieldH*))
      (if (= (vector-ref f (getOffset x y)) col)
	  (begin
	    (vector-set! f (getOffset x y) 255)
	    (if (or
		 (backtrack f (+ x 1) y col)
		 (backtrack f x (+ y 1) col)
		 (backtrack f (- x 1) y col)
		 (backtrack f x (- y 1) col)
		 #f)
		#f)
	    #f)
	  #f)
      #f))

(define (sweeping)

  (set! *state* 'pause)
;  (pp "sweeping called")
  (let ((did-we-swept? 0)
	(done 0))
    (printf "~%sweeping") 
    (do-for i (0 (vector-length *solveField*))
	    (if (= (vector-ref *solveField* i) 255)
		(begin
		  (set! did-we-swept 1)
		  (set! *points* (+ *points* 2))	  
		  (if (< *level* 10)	      
		      (if (= (modulo *points* 50) 0)
			  (set! *level* (+ *level* 1))))
		  
		  (vector-set! *field* i -1))))
    
    (do-list p *puyos*
	     (set! *puyos* (cdr *puyos*)))
    
    (fieldToPuyos)
    
    (do-while (eq? (dropPuyos) 1)
    	      (set! done (backtrackPuyos)))

;    (backtrackPuyos)
	      
    ;; (let loop ((done 1))
    ;;   (dropPuyos)
    ;;   (loop (set! done (backtrackPuyos))))
    
;;    did-we-swept?))
    (set! *state* 'unpause)
  done))
    

(define (dropPuyo-thread)
  (let loop ()
    (unless (= 0 *run*)
	    (if (not (eq? *state* 'pause))
		(begin
		  (thread-sleep! (- 1 (/ *level* 10)))
		  (if (= 0 (dropPuyos))
		      (set! *state* 'newPuyos))))
	    (loop))))

(define (fieldToPuyos)
  "this helper function will take a field->f and fill the lisp of puyos->p. this is only for testing purpose"
  (do-for x (0 *fieldW*)
	  (do-for y (0 *fieldH*)
		  (if (not (= (vector-ref *field* (getOffset x y)) -1))
		      (begin
			(set! *puyos* (cons (make-puyo x: x y: y col: (vector-ref *field* (getOffset x y)) state: 'dropping) *puyos*)))))))

(define (puyosToField)
  "this helper function will take a list of puyos->p and place them into a field ->f"
  (do-for x (0 (vector-length *field*))
	       (vector-set! *field* x -1))

  (do-list p *puyos*
       (vector-set! *field* (getOffset (puyo-x p) (puyo-y p)) (puyo-col p))))

(define (fileToField f)
  "this helper function will read in a file called test.txt and construct a playing field->f. this is for testing purposes..."
  (let ((in (open-input-file "test.txt"))
	(l 0)
	(data 0))    
    (do-for i  (0 (vector-length f))
;	    (set! data (digit-char-p (read-char in)))
	    (set! data (read-char in))
	    (if (eq? data #\*)
		(set! data -1))
	    (if (eq? data #\1)
		(set! data 1))
	    (if (eq? data #\2)
		(set! data 2))
	    (if (eq? data #\3)
		(set! data 3))
	    (vector-set! f i data) 
	    (set! l (+ l 1))
	    (if (eq? l 6)
		(begin
	       (read-char in)
	       (set! l 0))))
    (close-input-port in)))

(define (backtrackPuyos)
  (printf "~%backtrackPuyos")
  (printf "~%puyos count = ~A" (length *puyos*))
  (if (>= (length *puyos*) 4 )
      (do-list p *puyos*
	       (let ((col (puyo-col p))
		     (x (puyo-x p))
		     (y (puyo-y p)))
		 (if (member col '(0 1 2 3))
		     (begin
		       (copyField *field* *solveField*)
		       (backtrack *solveField* x y col)
		       (printField *solveField*)
		       (if (>= (countElements *solveField* 255) 4)
			   (if (eq? (sweeping) 1)
			       (begin
				 (printf "~%sweeping returning 1")		     
				 1)		   
			       (begin
				 (printf "~%sweeping returning 0")))
			   0))))))
  0)


(define (draw-sdl-string str x y)
  (define tmp-surface (sdl#ttf-render-text-blended *font* str (sdl#make-sdl-color 0 0 0)))
  (sdl#sdl-blit-surface tmp-surface #f *surface* (make-sdl-rect x y (sdl#sdl-surface-width tmp-surface) (sdl#sdl-surface-height tmp-surface)))
  (sdl#sdl-free-surface tmp-surface))
  
;;this is used to drop first and second down by one click!!!
(define (dropThemDown)
  (let ((done #f))
    (do-while (not done)
	      (let 
		  ((fstate (puyo-state *first*))
		   (sstate (puyo-state *second*)))
		(if (and (eq? fstate 'landed) (eq? sstate 'landed))
		    (set! done #t))
		(dropPuyos)))))

;;start / main
(define (main)
  ;;sdl
  (sdl#sdl-init SDL_INIT_EVERYTHING)
  (sdl#ttf-init)
;  (sdl#img-init IMG_INIT_PNG)
  (set! event (sdl#make-sdl-event))
  (set! *font*(sdl#ttf-open-font "font.ttf" 32))


  (set! *joystick* (sdl#sdl-joystick-open 0))

  ;;is this sufficient to generate random colors/numbers ?
  (set! *random-state* (randomize (sdl#sdl-get-ticks)))

  ;;test stuff
  (set! *first*  (make-puyo x: 3 y: 1 col: 3 state: 'dropping))
  (set! *second* (make-puyo x: 3 y: 2 col: 1 state: 'dropping))

;;  (set! *first*  (make-puyo x: 2 y: 10 col: (random *maxCols*) state: 'dropping))
;;  (set! *second* (make-puyo x: 3 y: 10 col: (random *maxCols*) state: 'dropping))
  (set! *puyos* (list *first* *second*))

  
  (set! *level* 0)
  (set! *points* 0)
  (set! *run* 1)
  (set! *state* 'unpause)



    
  ;;the puyo sprites are 32x32 pixels, so the playfield is 6*32 wide  and 12*32 heigh
  ;;we need double with, for displaying points and next puyos right from the playfield
					;(SET! (SDL:FRAME-RATE) 60)
  (set! *surface* (sdl#sdl-set-video-mode (* (* *puyoW* *fieldW*) 2) (* *puyoH* *fieldH*) 16 0))
  (sdl#sdl-show-cursor #f)

  (define *surface-rect* (sdl#make-sdl-rect 0 0 (* (* *puyoW* *fieldW*) 2) (* (* *puyoH* *fieldH*) 2))) 

  (sdl#sdl-fill-rect *surface* *surface-rect* (sdl#sdl-map-rgb (sdl#sdl-surface-pixel-format *surface*) 255 255 255))

  (clearField)

  (thread-start! (make-thread dropPuyo-thread 'dropPuyo-thread))

  ;;test stuff
  (fileToField *field*)
  (fieldToPuyos)
  
  ;;sdl surface pictures for blitting
  (set! *blue* (sdl#img-load "puyo_blue.jpg"))
  (set! *red* (sdl#img-load "puyo_red.jpg"))
  (set! *green* (sdl#img-load "puyo_green.jpg"))
  (set! *yellow* (sdl#img-load "puyo_yellow.jpg"))
  (set! *gameover* (sdl#img-load "gameover.jpg"))

  ;;game-loop 
  (let loop ()
    (unless (= 0 *run*)
    (define event (sdl#make-sdl-event))
    (if (sdl#sdl-poll-event! event)
	(begin
	  (set! event-type (sdl-event-type event))
	  (cond
	   ((= event-type SDL_QUIT) #f)
	   ((= event-type SDL_JOYBUTTONDOWN)
	    (let ((j (sdl-event-button event)))
	      (printf "~%button  ~A pressed" j)
	      (cond 
	       ;; ((= j 11)
	       ;; 	(sdl-save-bmp *surface* "screenshot.bmp"))
	       ((= j 14) ; SPACE / X BUTTON
		(dropThemDown))
	       ((= j 9) ;ESCAPE / SELECT BUTTON
		(set! *run* 0))
	       ((= j 8) ;PAUSE / START BUTTON       
		(cond 
		 ((eq? *state* 'pause)
		  (set! *state* 'unpause))
		 ((eq? *state* 'unpause)
		  (set! *state* 'pause))))
	       ((= j 2) ;left button
		(moveToLeft *first* *second*)
		(puyosToField))       
	       ((= j 6) ;right button
		(moveToRight *first* *second*)
		(puyosToField))
	       ((= j 13) ;b button
		(rotateStones *first* *second*)
		(puyosToField))	 
	       ((= j 4) ;down button
		(dropPuyos)))))

	   ((= event-type SDL_KEYDOWN)
	    (let* ((i (sdl-event-sym event))
		   (c (integer->char i)))
	      (print "key =~A" i)
	      (newline)
	      (cond 
	       ((= i 115) ;s -> screenshot
		(sdl-save-bmp *surface* "screenshot.bmp"))
	       ((= i 32) ; SPACE
		(dropThemDown))
	       ((= i 27)
		(set! *run* 0))
	       ((= i 112) ;p       
		(cond 
		 ((eq? *state* 'pause)
		  (set! *state* 'unpause))
		 ((eq? *state* 'unpause)
		  (set! *state* 'pause))))
	       ((= i 276) ;left key
		(moveToLeft *first* *second*)
		(puyosToField))       
	       ((= i 275) ;right key
		(moveToRight *first* *second*)
		(puyosToField))
	       ((= i 273) ;upper key
		(rotateStones *first* *second*)
		(puyosToField))	 
	       ((= i 274) ;down key
		(dropPuyos))))))))
    
    (sdl#sdl-fill-rect *surface* *surface-rect* (sdl#sdl-map-rgb (sdl#sdl-surface-pixel-format *surface*) 255 255 255))

    ;; (lispbuilder-sdl:draw-string-solid-* (format 0 "Puyopuyo") (+ (* *fieldW* *puyoW*) 20) 10 :color (sdl:color :r 0 :g 0 :b 0))
    ;; (lispbuilder-sdl-gfx:draw-line-* (* *fieldW* *puyoW*) 0 (* *fieldW* *puyoW*) (* *fieldH* *puyoH*) :surface sdl:*default-display*  :color (sdl:color :r 0 :g 0 :b 0))

   
    (puyosToField)	       

    (if (= *run* 1)
	(begin
	  (if (eq? *state* 'gameover)
	      (sdl#sdl-blit-surface *gameover* #f *surface* (sdl#make-sdl-rect 0 0 (* *puyoW* *fieldW*) (* *puyoH* *puyoH*)))
	      (drawField *field*))

	
			  ;; (define event (sdl#make-sdl-event))
			  ;; (if (sdl#sdl-poll-event! event)
			  ;;     (set! event-type (sdl-event-type event))
			  ;;     (if (or
			  ;; 	   (= event-type SDL_QUIT)
			  ;; 	   (= event-type SDL_JOYBUTTONDOWN)
			  ;; 	   (= event-type SDL_JOYBUTTONDOWN)
			  ;; 	   (= event-type SDL_KEYDOWN))
			  ;; 	  (begin
			  ;; 	    (set! *points* 0)
			  ;; 	    (set! *level* 0)
			  ;; 	    (set! *state* 'unpause)
			  ;; 	    (set! done 1)))))

	
	(if (not (eq? *state* 'pause))
	      (draw-sdl-string (format #f "~A" *points*) (+ (* *puyoW* *fieldW*) 10) (/ (sdl#sdl-surface-height *surface*) 2))
	      (draw-sdl-string "Paused" (+ (* *puyoW* *fieldW*) 10) (/ (sdl#sdl-surface-height *surface*) 2)))))
    

    (sdl#sdl-flip *surface*)

    (if (eq? *state* 'newPuyos)
	(begin
	  (if (or (not (= (vector-ref *field* (getOffset 2 0)) -1))
		  (not (= (vector-ref *field* (getOffset 3 0)) -1)))
	      (set! *state* 'gameover)
	      (begin
		(if (backtrackPuyos)
		    (backtrackPuyos))
		;; we need to create new objects. what is the impact on memory / gc
		(set! *first*  (make-puyo x: 2 y: 0 col: (random *maxCols*) state: 'dropping))
		(set! *second*  (make-puyo x: 3 y: 0 col: (random *maxCols*) state: 'dropping))		     
		(set! *puyos* (cons *first* *puyos*))
		(set! *puyos* (cons *second* *puyos*))
		(set! *state* 'unpause)))))
    
    (loop))))

(main)
;;when escape is pressed we drop out here. set run to zero so that the thread stops
;;we are done. bye bye
(sdl#sdl-quit)
(exit)



