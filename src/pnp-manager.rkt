#lang racket/gui

(require racket/match)
(require racket/hash)
(require "pnp-utils.rkt")
(require "gcode-gen.rkt")

(define config
  (hash 'window-w 1280
        'window-h 860
        'tape-part-spacing 4
        'zoom 9
        
        'feedrate 3000
        'z-drive-position 7
        'z-pick-position 3
        'z-place-position 1
        'rest-time 100
        'correction-x 3
        'correction-y 2))

(define (cfg k)
  (: config k))

(define global-st
  (make-hash '((parts . ())
               (parts-filtered . ())
               (bed-size . "219*219")
               (board-size . '(100 . 100))
               (watchers . ())
               (tape-width . 8)
               (rows-columns . "1*2")
               (tray-horizontal . ())
               (tray-vertical . ())
               (tray-combined . ())
               (boards . ())
               (exclusion . ()))))

(define (st-update state k v)
  (let ([x (: state k)])
    (hash-set! state k v)
    (for* ([w (: state 'watchers)])
      (when (eq? (car w) k) ((cdr w) v x state)))))

(define (st-refresh st)
  (hash-for-each st (lambda (k v) (st-update st k v))))

(define (st-watch state k f)
  (hash-set! state 'watchers (cons (cons k f) (: state 'watchers))))

(define part-regex #px"(\\S+?)\\s+(\\S+?)\\s+(\\S+?)\\s+(\\S+?)\\s+\\((\\S+?)\\s(\\S+?)\\)\\s+([A-Z]+)(\\S+?)$")

;;load all parts
(define (load-parts parts in-port)
  (let ([line (read-line in-port)])
    (if (eof-object? line)
        (begin (close-input-port in-port) parts)
        (let ([match (regexp-match part-regex line)])
          (load-parts (if (pair? match)
                          (cons (apply parse-part (cdr match)) parts)
                          parts)
                      in-port)))))

(define (parse-part name val package lib x y rot deg)
  (list name val package lib (inch->mm (string->number x)) (inch->mm (string->number y)) rot deg))

(define (translate-y y zero)
  (- zero y))

(define frame (new frame% [label "Eagle Partlist to GCode Compiler"]
                   [width (cfg 'window-w)]
                   [height (cfg 'window-h)]))

(define panel (new horizontal-panel% [parent frame]
                   [style '(border)]
                   [vert-margin 10]
                   [horiz-margin 10]))

(send frame show #t)

(define (top-layer? part)
  (equal? (list-ref part 6) "R"))

(define bm1 (make-bitmap (/ (cfg 'window-w) 2) (cfg 'window-h)))
(define bmdc1 (new bitmap-dc% [bitmap bm1]))
(define bm2 (make-bitmap (/ (cfg 'window-w) 2) (cfg 'window-h)))
(define bmdc2 (new bitmap-dc% [bitmap bm2]))

(define white-brush (new brush% [color "white"]))
(define transparent-brush (new brush% [style 'transparent]))
(define top-brush (new brush% [color (make-object color% 200 97 44)]))
(define bottom-brush (new brush% [color (make-object color% 56 130 166)]))
(define yellow-brush (new brush% [color (make-object color% 177 214 150)]))
(define thick-pen (new pen% [color (make-object color% 0 0 0 0.9)]))
(define faint-pen (new pen% [color (make-object color% 0 0 0 0.3)]))

(define (draw-part dc max-coord name val package library x-inch y-inch rotation angle)
  (let ((x (* (cfg 'zoom) x-inch))
        (y (* (cfg 'zoom) y-inch))
        (t (send dc get-transformation)))
    (send* dc
      (transform (vector 1 0 0
                         -1 0 (+ 10 (* (cfg 'zoom) (cdr max-coord)))))
      (set-origin x y)
      (rotate (degrees->radians (string->number angle)))
      (set-brush (if (equal? "MR" rotation) bottom-brush top-brush))
      (draw-rectangle -5 2.5 10 5)
      (draw-text name 5 -2.5)
      (set-transformation t))))

(define (exclude-exclusion parts exclusion)
  (filter (lambda (p) (not (member (car p) exclusion))) parts))

(define left-panel (new vertical-panel% [parent panel]
                        [min-width (/ (cfg 'window-w) 2)]
                        [stretchable-width #f]))

(define cv 
  (new canvas% 
       [parent left-panel]
       [paint-callback 
        (lambda (cv dc) 
          (send dc draw-bitmap bm1 0 0))]))

(define ctrl (new vertical-panel% [parent left-panel]
                   [style '(border)]
                   [alignment '(left bottom)]
                   [stretchable-height #f]))

(define cv-right
  (new canvas% 
       [parent panel]
       [paint-callback
        (lambda (cv dc) 
          (send dc draw-bitmap bm2 0 0))]))

(define chk-ignore-bottom
  (new check-box% [parent ctrl]
       [label "show bottom layer"]
       [value #t]
       [callback (lambda (cb ce)
                   (if (send cb get-value)
                       (st-update global-st 'parts-filtered (: global-st 'parts))
                       (st-update global-st 'parts-filtered (filter top-layer? (: global-st 'parts)))))]))


(define nop (lambda (e) '()))

(define bed-s-txb
  (new text-field% 
       [parent ctrl]
       [label "Bed Size X*Y"]
       [init-value (: global-st 'bed-size)]
       [callback (lambda (tf e)
                   (with-handlers ([exn:fail? nop])
                     (st-update global-st 'bed-size (send tf get-value))))]))

(define board-s-txb
  (new text-field% 
       [parent ctrl]
       [label "Tape Width"]
       [init-value (number->string (: global-st 'tape-width))]
       [callback (lambda (tf e)
                   (with-handlers ([exn:fail? nop])
                     (st-update global-st 'tape-width (string->number (send tf get-value)))))]))

(define row-col-txb
  (new text-field%
       [parent ctrl]
       [label "rows*colums"]
       [init-value (: global-st 'rows-columns)]
       [callback (lambda (tf e)
                   (with-handlers ([exn:fail? nop])
                     (st-update global-st 'rows-columns (send tf get-value))))]))

(define exclusion-txb
  (new text-field%
       [parent ctrl]
       [label "exclude"]
       [init-value ""]
       [callback (lambda (tf e)
                   (with-handlers ([exn:fail? nop])
                     (st-update global-st 'exclusion (string-split (send tf get-value) ","))))]))

(define go-btn
  (new button%
       [parent ctrl]
       [label "Generate GCode"]
       [callback (lambda (btn e)
                   (send* bmdc2
                     (set-brush transparent-brush)
                     (set-pen faint-pen))
                   (let ([t (send bmdc2 get-transformation)])
                     (send* bmdc2
                       (transform (vector 1 0 0 -1 0 (send cv-right get-height)))
                       (draw-path (generate-gcode (hash-union config global-st) (get-file "Save gcode as" frame (find-system-path 'home-dir) ".gcode")))
                       (set-transformation t)))
                   (send bmdc2 set-pen thick-pen)
                   (send cv-right on-paint))]))

(define (clear-half dc x)
  (send dc set-brush white-brush)
  (send dc draw-rectangle x 0 (/ (cfg 'window-w) 2)  (cfg 'window-h)))

(define draw-left-half 
  (lambda (nw old st)
    (let* ([max-coord (foldl (lambda (p acc) 
                               (cons (max (car acc) (list-ref p 4))
                                     (max (cdr acc) (list-ref p 5)))) '(0 . 0) (: st 'parts))]
           [brd-size (cons (+ (car max-coord) 5) (+ (cdr max-coord) 2.5))]
           [t (send bmdc1 get-transformation)])
      (clear-half bmdc1 0)
      (send bmdc1 transform (vector 1 0 0 
                                    1 20 200))
      (for-each (lambda (p) (apply draw-part (cons bmdc1 (cons max-coord p)))) (: st 'parts-filtered))
      (send cv on-paint)
    
      ;;board outline
      (st-update global-st 'board-size brd-size)
      (send bmdc1 set-brush transparent-brush)
      (send bmdc1 draw-rectangle
            0 0
            (* (car brd-size) (cfg 'zoom)) (* (cdr brd-size) (cfg 'zoom)))
      (displayln (list 0 (translate-y (* (cdr brd-size) (cfg 'zoom)) (- (cfg 'window-h) 150))
            (* (car brd-size) (cfg 'zoom)) (* (cdr brd-size) (cfg 'zoom))))
      (send bmdc1 set-transformation t)
      (send cv on-paint))))

(define (inch->mm x)
  (* x 25.4))

(define draw-right-half
  (lambda (nw old st)
    (let* ([ww (send cv-right get-width)]
           [wh (send cv-right get-height)]
           [bed-size (map string->number (string-split (: st 'bed-size) "*"))]
           [ratio (/ (- ww 20) (car bed-size))]
           [rc   (map string->number (string-split (: st 'rows-columns) "*"))]
           [brd-size (: st 'board-size)]
           [h-tray (: st 'tray-horizontal)]
           [v-tray (: st 'tray-vertical)]
           [tr (send bmdc2 get-transformation)])
      (clear-half bmdc2 0)
      ;(send bmdc2 set-origin (/ ww 2) (/ wh 2))
      (send bmdc2 transform (vector 1 0 0 -1 0 wh))
      (send bmdc2 draw-rectangle 
            10 10
            (* (car bed-size)  ratio)
            (* (cadr bed-size) ratio))
      (send bmdc2 draw-text (number->string (car bed-size)) (/ ww 2) 0)
      (send bmdc2 draw-text (number->string (cadr bed-size)) 0 (/ wh 3))
      
      (for ([b (: st 'boards)])
        (send bmdc2 set-brush yellow-brush)
        (send bmdc2 draw-rectangle 
              (+ (* (car b) ratio) 10) (+ (* (cdr b) ratio) 10)
              (* (car brd-size) ratio)
              (* (cdr brd-size) ratio))
        (send bmdc2 draw-text (string-append "(" (number->string (floor (car b))) "," (number->string (floor (cdr b))) ")")
              (+ (* (car b) ratio) 20) (+ (* (cdr b) ratio) 20)))
      
      (for ([part h-tray]
            [i (in-naturals)])
        (let* ([tw (: st 'tape-width)]
              [y-pos (- (* (cadr bed-size) ratio) (* tw ratio i) 10)])
          (send* bmdc2
            (set-brush bottom-brush)
            (draw-rectangle
             10 y-pos
             (* (car bed-size) ratio) (* tw ratio))
            (draw-text (~a (hash-values part))
                       20 (+ y-pos 1.5)))))
      (for ([part v-tray]
            [i (in-naturals)])
        (let* ([tw (* ratio (: st 'tape-width))]
               [x-pos (- (* (car bed-size) ratio) (* tw i) 10)]
               [t (send bmdc2 get-transformation)])
          (send* bmdc2
            (set-brush bottom-brush)
            (draw-rectangle
             x-pos 10
             tw (* (cadr bed-size) ratio))
            (set-origin (+ x-pos 15) 10)
            (rotate (* (/ pi 2) 3))
            (draw-text (~a (hash-values part)) 0 0)
            (set-transformation t))))

      (send bmdc2 set-transformation tr)
      (send cv-right on-paint))))

(define (not-f f)
  (lambda (x) (not (f x))))

(define (reduce-parts parts)
  (foldl (lambda (p ps) 
           (let ([idx (index-where 
                       ps
                       (lambda (q)
                         (and (equal? (: q 'val)     (cadr p))
                              (equal? (: q 'package) (caddr p)))))])
             (if idx
                 (list-update ps idx (λ (item) (hash-update item 'parts (λ (pts) (cons (car p) pts)))))
                 (cons
                  (hasheq 'val     (cadr p)
                          'package (caddr p)
                          'parts   (list (car p)))
                  ps))))
         '() parts))


(define (prepare-tray parts old st)
  (let* ([f (lambda (p)  (and (regexp-match? "^0|180$" (list-ref p 7))))]
         [horizontal (for/list ([p (reduce-parts (filter f parts))]
                                [i (in-naturals)])
                       (hash-set* p 'orientation 'horizontal
                                    'order i))]
         [vertical   (for/list ([p (reduce-parts (filter (not-f f) parts))]
                                [i (in-naturals)])
                       (hash-set* p 'orientation 'vertical
                                    'order i ))])
    (st-update global-st 'tray-horizontal horizontal)
    (st-update global-st 'tray-vertical vertical)
    (st-update global-st 'tray-combined (append horizontal vertical))))

(define (prepare-boards nw old st)
  (let* ([rc (map string->number (string-split (: st 'rows-columns) "*"))]
         [bed-size (map string->number (string-split (: st 'bed-size) "*"))]
         [h-tray (: st 'tray-horizontal)]
         [v-tray (: st 'tray-vertical)])
    (st-update global-st 'boards
               (for*/list ([i (car rc)]
                           [j (cadr rc)])
                 (cons
                  (* i (/ (- (car  bed-size) (* (length v-tray) (: st 'tape-width))) (car rc)))
                  (* j (/ (- (cadr bed-size) (* (length h-tray) (: st 'tape-width))) (cadr rc))))))))

(define *draw-thread*
  (thread (lambda ()
            (let loop ()
              (sleep 0.1)
              (when (thread-try-receive)
                (draw-left-half #f #f global-st)
                (draw-right-half #f #f global-st))
              (loop)))))

(define (draw-all nw old st)
  (thread-send *draw-thread* 'go (λ () (displayln "thread-send fail"))))

(define (start)
  (let* ([fpath (get-file "Open Eagle Partlist" frame (find-system-path 'home-dir) #f ".txt")]
         [in-port (open-input-file fpath)]
         [parts (load-parts '() in-port)])
    (for-each displayln parts)
    (st-watch global-st 'parts-filtered prepare-tray)
    (st-watch global-st 'parts-filtered draw-all)
    (st-watch global-st 'bed-size       draw-all)
    (st-watch global-st 'tape-width     draw-all)
    (st-watch global-st 'tape-width     prepare-boards)
    (st-watch global-st 'rows-columns   prepare-boards)
    (st-watch global-st 'exclusion      (lambda (nw old st)
                                          (st-update global-st 'parts-filtered
                                                     (exclude-exclusion (: st 'parts-filtered) (: st 'exclusion)))))

    (st-refresh global-st)
    (st-update global-st 'parts parts)
    (st-update global-st 'parts-filtered parts)))

(start)