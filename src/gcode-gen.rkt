#lang racket

(require racket/draw)

(provide generate-gcode :)

(define (asscdr k xs)
  (cdr (assq k xs)))

(define : hash-ref)

(define (find-tape part tray)
  (lambda (cc)
    (for ([tape tray])
      (let ([i (index-of (: tape 'parts) (car part))])
        (if i (cc (cons i tape)) #f)))))

(define (brd->scr st x y)
  (let* ([ww (/ (: st 'window-w) 2)]
         [bed-size (map string->number (string-split (: st 'bed-size) "*"))]
         [ratio (/ (- ww 40) (car bed-size))])
    (cons (+ (* x ratio) 10)
          (+ (* y ratio) 10))))

(define-syntax-rule (my-send dc msg coords)
  (begin
    (send dc msg (car coords) (cdr coords))))

(define (generate-gcode setting path)
  (let ([dcp (new dc-path%)]
        [p (open-output-file path #:exists 'truncate/replace)]
        [t-p-s (: setting 'tape-part-spacing)]
        [n-boards (length (: setting 'boards))])
    (display p)
    (my-send dcp move-to (brd->scr setting 0 0))
    (fprintf p "G1 F~a~n" (: setting 'feedrate))
    (fprintf p "G28~n") ;home all axis
    (fprintf p "G1 X0 Y0~n")
    ;(fprintf p "G28 X0 Y0~n")
    ;(fprintf p "G92 X0 Y0~n")
    (my-send dcp line-to (brd->scr setting 0 0))
    (for ([part (: setting 'parts-filtered)]
          [i (in-naturals)])
      (let* ([tape-info (call/cc (find-tape part (: setting 'tray-combined)))]
             [tape (cdr tape-info)]
             [tape-i (car tape-info)]
             [bed-size (map string->number (string-split (: setting 'bed-size) "*"))])
        (fprintf p ";------~n")
        (fprintf  p ";Part ~a~n" (car part))
        (fprintf p "G1 Z~a~n" (: setting 'z-drive-position))
        ;(fprintf p "G1 X0 Y0~n")
        ;(fprintf p "G28 X0 Y0~n")
        ;(fprintf p "G92 X0 Y0~n")
        ;(my-send dcp line-to (brd->scr setting 0 0))
        (for ([board (: setting 'boards)]
              [j (in-naturals 1)])
          (fprintf p "G1 Z~a~n" (: setting 'z-drive-position))
          (fprintf p ";--- move to tray ---~n")
          (let* ([h? (eq? (: tape 'orientation) 'horizontal)]
                 [f (if h? car cadr)]
                 [a (+ (* j t-p-s) (* n-boards tape-i t-p-s))]
                 [b (- (f bed-size) (* (: tape 'order) (: setting 'tape-width)) 5.25)])
            (fprintf p "G1 X~a Y~a~n" 
                    (+ (if h? a b) (: setting 'correction-x))
                    (+ (if h? b a) (: setting 'correction-y)))
            (my-send dcp line-to (brd->scr setting (if h? a b) (if h? b a))))
          (fprintf p "G1 Z~a~n" (: setting 'z-pick-position))
          (fprintf p "G4 P~a~n" (: setting 'rest-time))
          (fprintf p "M106~n") ;turn pump on
          (fprintf p "G4 P~a~n" (: setting 'rest-time))
          (fprintf p "G1 Z~a~n" (: setting 'z-drive-position))
          (fprintf p ";---move to board---~n")
          (fprintf p "G1 X~a Y~a~n" 
                  (+ (car board) (list-ref part 4) (: setting 'correction-x))  ;;**part-x-pos-in-a-board**
                  (+ (cdr board) (list-ref part 5) (: setting 'correction-y))) ;;**part-y-pos-in-a-board**
          (my-send dcp line-to (brd->scr setting (+ (car board) (list-ref part 4)) (+ (cdr board) (list-ref part 5))))
          (fprintf p "G1 Z~a~n" (: setting 'z-place-position))
          (fprintf p "G4 P~a~n" (: setting 'rest-time))
          (fprintf p "M107~n") ;turn pump off
          (fprintf p "G4 P~a~n" (: setting 'rest-time)))))
    (fprintf p "G1 X0 Y0~n")
    (close-output-port p)
    dcp))