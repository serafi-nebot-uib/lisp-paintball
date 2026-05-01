;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer del mòdul gràfic.
;; <Descripció de les funcions d'aquest fitxer>

; **************************************************
; CONSTANTS
; **************************************************

(defconstant XLISP-WINDOW-HEIGHT   375)
(defconstant XLISP-WINDOW-WIDTH    640)
(defconstant CELL-MIN-SIZE         6)   ; px
(defconstant CELL-MAX-SIZE         18)
(defconstant CELL-BORD-THCK        1)
(defconstant SB-MIN-WIDTH          480) ; Sidebar minimum width
(defconstant SB-FIXED-WIDTH        t)   ; Sidebar adjustment policy, t expands board container, nil prioritizes symmetrical board margins expanding sidebar
(defconstant BOARD-DELIM-W         2)   ; delimiter width
(defconstant BOARD-MAX-COLS        60)
(defconstant BLACK                 '(0 0 0))
(defconstant RED                   '(255 0 0))
(defconstant GREEN                 '(0 255 0))
(defconstant BLUE                  '(0 0 255))
(defconstant WHITE                 '(255 255 255))
(defconstant SOFT-WHITE            '(240 240 240))
(defconstant WATER-COL             '(205 205 255))
(defconstant LAND-COL              '(216 163 133))
(defconstant PAINT-CELL-COL        t) ; t paints cell color, nil paints land color
(defconstant LAND-COL-RED          '(255 128 109));'(255 160 136))
(defconstant LAND-COL-GREEN        '(205 168 126))
(defconstant LAND-COL-BLUE         '(166 139 207))
(defconstant DEF-TEXT-COL          BLACK)
(defconstant DEF-MARG-COL          BLACK)
(defconstant BACKGROUND-COL        WHITE)
(defconstant SHOW-WATERMARK        t)

; **************************************************
; UTILITIES
; **************************************************

;; Els modificadors de la funció format es poden consultar a:
;; https://ad.uib.es/estudis2526/pluginfile.php/237066/mod_resource/content/1/XLISPPLUS3.pdf#page=107
;;
;; ~A mínim espai possible
;; ~3A el text ocupa 3 espais i alínia el text a l'left
;; ~3@A el text ocupa 3 espais i alínia el text a la right
;; ~3,'0@A	el text ocupa 3 espais i alínia el text a la right i els espais es substitueixen per 0's per exemple, "005"
(defun to-string (var &optional (f "~A"))
    "Converteix a string amb el format indicat un valor 'var' passat per paràmetre"
    (format nil f var))

(defun make-str (size &optional (value #\space))
    "Crea un string de tamany 'size' d'un caràcter concret"
    (format nil "~V@{~A~:*~}" size value))

; &rest agafa tots els paràmetres que segueixen i els fica dins una llista
; ~{ ~} itera sobre els elements de la llista i aplica el format "~A" a cada un d'ells
; Podria emprar-se també concatenate
(defun strcat (&rest str-list) 
    "Crea un string a partir de la concatenació dels n valors passats per paràmetre"
    (format nil "~{~A~}" str-list))

; retorna l'element de major longitud
(defun maxlen (&rest str) (reduce 'max (mapcar 'length str)))

; p = (x y)
;; (defun cx (p) (car p))                                   ; coordx
;; (defun cy (p) (cadr p))                                  ; coordy
;; (defun csum (p x y) (list (+ (car p) x) (+ (cadr p) y))) ; coordsum
;; (defun ccomp (p1 p2) (equal p1 p2))                      ; coordcomp

; **************************************************
; PRIMITIVES
; **************************************************

(defun set-color (c) (color (car c) (cadr c) (caddr c)))

(defun key2color (keys &key colors color-class (col-idx nil))
    "Transforma una clau o llista de claus 'key' en el seu corresponent color i/o índex"
    (if keys
        (let* ((atm-typ (atom keys))
               (act-key (if atm-typ keys (car keys)))
               (rem-key (if atm-typ nil  (cdr keys)))
               (rgb-col (cond ((eq act-key RGB-R) (list (if (eq color-class LAND-COL) LAND-COL-RED RED) 0))
                              ((eq act-key RGB-G) (list (if (eq color-class LAND-COL) LAND-COL-GREEN GREEN) 1))
                              ((eq act-key RGB-B) (list (if (eq color-class LAND-COL) LAND-COL-BLUE BLUE) 2))))
               (act-col (if col-idx rgb-col (car rgb-col))))
            (key2color rem-key :colors (append colors (if atm-typ act-col (list act-col)))
                               :color-class class
                               :col-idx col-idx))
        colors))

(defun gradient-rect (x y w h &optional col)
    "Genera una gradient de lluminositat des del color 'col' fins a blanc"
    (cond ((<= h 0) nil)
          (t (let ((new-col (mapcar '(lambda (a) (if (< a 255) (1+ a) 255)) col)))
                (set-color new-col)
                (fill-rect x y w 1)
                (gradient-rect x (1+ y) w (1- h) new-col)))))

; TODO: &key col for all shape functions
(defun fill-rect (x y w h)
    "Omple un rectangle a la posició (x, y), amplada w i alçada h."
    (cond ((<= h 0) nil)
          (t (move x y)
             (draw (+ x w) y)
             (fill-rect x (1+ y) w (1- h)))))

(defun fill-rect-rel (w h)
    "Omple un rectangle a la posició relativa, amplada w i alçada h."
    (cond ((<= h 0) nil)
          (t (moverel 0 (1- h))
             (drawrel w 0)
             (moverel (- w) (- (1- h)))
             (fill-rect-rel w (1- h)))))

(defun draw-rect-rel (w h)
    "Dibuixa un rectangle d'amplada `w` i alçada 'h' a la posició actual."
    (drawrel w 0)
    (drawrel 0 h)
    (drawrel (- w) 0)
    (drawrel 0 (- h)))

(defun rect-outline-rel (w h &optional (g CELL-BORD-THCK))
    "Dibuixa un contorn rectangular d'amplada `w - 1`, alçada 'h - 1' i gruix `g`, a la posició actual. SENSE FONS"
    (cond ((plusp g) ; si gruix > 0
           (draw-rect-rel (- w 1) (- h 1))
           (moverel 1 1)
           (rect-outline-rel (- w 2) (- h 2) (- g 1))
           (moverel -1 -1))))

(defun square-outline-rel (size &optional (g CELL-BORD-THCK)) (rect-outline-rel size size g))
; dibuixa un contorn rectangular d'amplada 'w', alçada 'h' i gruix 'g' a la posició (x, y)
(defun rect-outline (x y w h &optional (g CELL-BORD-THCK)) (move x y) (rect-outline-rel (1+ w) (1+ h) g))

; TODO: variable height triangles [reduction ratio (per line) = height / base]
(defun draw-triangle-iso (b &optional (ttype 'UPT))
    "Dibuixa un triangle isòsceles de tipus 'ttype' de base 'b' i altura 'b'/2"
    (let* ((c (round (/ b 2)))
           (l (cond ((eq ttype 'UPT) (list 'LRT c 0 'LLT))    ; 🞁 Up-Pointing Triangle
                    ((eq ttype 'DPT) (list 'URT c 0 'ULT))    ; 🞃 Down-Pointing Triangle
                    ((eq ttype 'LPT) (list 'URT 0 c 'LRT))    ; 🞀  Left-Pointing Triangle
                    ((eq ttype 'RPT) (list 'ULT 0 c 'LLT))))) ; 🞂  ⁠​​Right-Pointing Triangle
        (draw-triangle c (car l))
        (moverel (cadr l) (caddr l))
        (draw-triangle c (cadddr l))
        (moverel (- (cadr l)) (- (caddr l)))))

(defun draw-triangle (c &optional (ttype 'LLT)) ;cateto = altura = base 
    "Dibuixa un triangle rectangle de tipus 'ttype' i costats de mida 'c'"
    (let ((l (cond ((eq ttype 'LLT) '(0 0 1 0 1 0 0 0))     ; ◣ Lower Left Triangle
                   ((eq ttype 'ULT) '(0 0 1 0 1 1 1 0))     ; ◤ Upper Left Triangle
                   ((eq ttype 'LRT) '(0 1 0 1 0 1 1 0))     ; ◢ Lower Right Triangle
                   ((eq ttype 'URT) '(1 1 0 1 1 0 0 1)))))  ; ◥ Upper Right Triangle
        (draw-triangle-internal c (car l) (cadr l) (caddr l) (cadddr l) (nth 4 l) (nth 5 l) (nth 6 l) (nth 7 l))))
        
(defun draw-triangle-internal (c ay bx by cx cy dy ey offy)
    (cond ((< c 0) nil)
          (t (moverel 0  (+ (* ay c) (- offy)))
             (drawrel (* bx c) (* by c))
             (moverel (* (- cx) c) (- (* (- cy) c) (- offy)))
             (moverel 1  dy)
             (draw-triangle-internal (1- c) ay bx by cx cy dy ey offy)
             (moverel -1 (- ey)))))

; **************************************************
; BITMAP/STRING
; **************************************************
; strings are drawn by having a bitmap representation of every possible character and drawing it.
; a bitmap is an array where each cell represents a pixel.
;    1 means the pixel should be painted
;    0 means the pixel should not be painted
; with this representation it is very easy to define a 5x6 pixel character representation
; an empty row and column is left empty for every character to provide spacing
; this is done through the FONT-WIDTH and FONT-HEIGHT (5x6 +1 -> 6x7) which dictate the total
; width and height of the final drawn character.

; https://fontstruct.com/fontstructions/show/2122607/5x6-font-3
(load "font.lsp")

(defun draw-bitmap (bm x y scale)
    "Dibuixa un bitmap bm a la posició (x, y) escalat per scale"
    (draw-bitmap-rows (reverse bm) x y scale))

(defun draw-bitmap-rows (bm x y scale)
    "Dibuixa totes les files del bitmap bm a la posició (x, y) escalat per scale"
    (cond ((null bm) nil)
          (t (draw-bitmap-row (car bm) x y scale)
             (draw-bitmap-rows (cdr bm) x (+ y scale) scale))))

(defun draw-bitmap-row (bm-row x y scale)
    "Dibuixa una unica fila bm-row d'un bitmap a la posició (x, y) escalat per scale"
    (cond ((null bm-row) nil)
          (t (when (= (car bm-row) 1) (fill-rect x y scale scale))
             (draw-bitmap-row (cdr bm-row) (+ x scale) y scale))))

(defun draw-chars (chars x y scale)
    "Dibuixa una llista de caràcters a la posició (x, y) escalat per scale"
    (cond ((null chars) nil)
          (t (let* ((char-info (cdr (assoc (car chars) FONT)))
                    (width (car char-info))
                    (bm (cdr char-info)))
                   (draw-bitmap bm x y scale)
                   (draw-chars (cdr chars) (+ x (* (1+ width) scale)) y scale)))))

(defun draw-str (str x y scale &key max-cs bcol tcol)
    "Dibuixa un string a la posició (x, y) escalat per scale amb color 'tcol',
     repinta el contenidor de color 'bcol' i dimensions 'max-cs' si aquest s'indica per paràmetre"
    (cond (max-cs
        (set-color (if bcol bcol BACKGROUND-COL))
        (fill-rect x y (car max-cs) (cadr max-cs))))
    (set-color (if tcol tcol DEF-TEXT-COL))
    (draw-chars (coerce str 'list) x y scale))

(defun get-str-width (str scale &optional (substring-w 0))
    "Retorna l'amplada real del bitmap corresponent a l'string 'str' escalat per 'scale'"
    (cond ((= (length str) 0) (* substring-w scale))
          (t (let* ((chars (coerce str 'list))
                    (char-info (cdr (assoc (car chars) FONT)))
                    (width (car char-info)))
                (get-str-width (coerce (cdr chars) 'string) scale (+ substring-w (1+ width)))))))

; ******************************************************************************************

;TODO: add optional shadow to board
(defun graphics-upd (state &optional (updates nil))
    (let* ((m (state-map state))
           (map-h (map-height m))
           (map-w (map-width m))
           (cell-ratio (min (/ XLISP-WINDOW-HEIGHT map-h) (/ SB-MIN-WIDTH map-w)))
           (cell-size  (min CELL-MAX-SIZE (max CELL-MIN-SIZE (truncate cell-ratio)))) ; board cell size
           (board-h  (+ (* map-h (- cell-size CELL-BORD-THCK)) CELL-BORD-THCK))
           (board-w  (+ (* map-w (- cell-size CELL-BORD-THCK)) CELL-BORD-THCK))
           (y-margin (floor (/ (- XLISP-WINDOW-HEIGHT board-h) 2)))
           (x-margin (if (or SB-FIXED-WIDTH (> (+ board-w (* y-margin 2)) SB-MIN-WIDTH))
                         (floor (/ (- SB-MIN-WIDTH board-w) 2)) y-margin))
           (board-container-size (if SB-FIXED-WIDTH SB-MIN-WIDTH (min SB-MIN-WIDTH (+ board-w (* x-margin 2)))))
           (xi x-margin)
           (yi (+ (- board-h cell-size) y-margin)))
        (cond ((= (state-turn state) 0)
                  (cls)
                  (when SHOW-WATERMARK (draw-watermark 0 0 board-container-size XLISP-WINDOW-HEIGHT 6 t))
                  (gradient-rect board-container-size 0 (- XLISP-WINDOW-WIDTH board-container-size) XLISP-WINDOW-HEIGHT '(20 20 20))
                  (when SHOW-WATERMARK (draw-watermark (+ board-container-size 35) 100 (- XLISP-WINDOW-WIDTH board-container-size) XLISP-WINDOW-HEIGHT 5))
                  (set-color BLACK)
                  (fill-rect board-container-size 0 BOARD-DELIM-W XLISP-WINDOW-HEIGHT)
                  (draw-map m xi yi 0 cell-size))
              (updates (paint-changes xi yi cell-size updates))
              (t nil))
         (status-sidebar-upd state board-container-size updates)))

(defun turn-counter-panel (xi yi container-w team turn)
    (let* ((tn-scale0 2)     (tn-txt0 "JUGADA ") ; (tn-txt0 "TORN ")
           (tn-scale1 2)     (tn-txt1 "N*")
           (tn-num-scale 3)  (tn-num-txt (to-string turn))
           (tn-x-margin (floor (/ (- container-w (get-str-width tn-txt0 tn-scale0)) 2)))
           (tn-x0 (+ xi tn-x-margin))
           (tn-y0 yi)
           (tn-x1 tn-x0)
           (tn-y1 (- tn-y0 20))
           (tn-num-x (+ (* (length tn-txt1) tn-scale1 BITMAP-WIDTH) tn-x1))
           (tn-num-y (- tn-y1 (* (abs (- tn-num-scale tn-scale1)) BITMAP-HEIGHT)))
           (tn-num-max-cs (list (* (length tn-num-txt) tn-num-scale BITMAP-WIDTH)
                                (* tn-num-scale BITMAP-HEIGHT))))
        (cond ((= turn 0)
            (draw-str tn-txt0 tn-x0 tn-y0 tn-scale0)
            (draw-str tn-txt1 tn-x1 tn-y1 tn-scale1))
         (t (draw-str tn-num-txt  tn-num-x  tn-num-y  tn-num-scale  :max-cs tn-num-max-cs)))))

(defun team-panel (xi yi container-w team turn)
    (let* ((td-scale0 2)      (td-txt0 "Torn de")
           (td-scale1 2)      (td-txt1 "l'equip ")
           (td-team-scale 2)  (td-team-txt (if (eq team TEAM-1) "1" "2"))
           (td-x-margin (floor (/ (- container-w (get-str-width td-txt0 td-scale0)) 2)))
           (td-x0 (+ xi td-x-margin (- 2))) 
           (td-y0 yi)
           (td-x1 td-x0)
           (td-y1 (- td-y0 18))
           (td-team-x (+ td-x1 (get-str-width td-txt1 td-scale1) (- 2)))
           (td-team-y td-y1)
           (td-max-cs (list (* (length td-team-txt) td-team-scale BITMAP-WIDTH)
                            (* td-team-scale BITMAP-HEIGHT))))
        (cond ((= turn 0)
            (draw-str td-txt0 td-x0 td-y0 td-scale0)
            (draw-str td-txt1 td-x1 td-y1 td-scale1))
         (t (draw-str td-team-txt td-team-x td-team-y td-team-scale :max-cs td-max-cs)))))

(defun base-color-upd (colbox-x0 colbox-y0 colbox-size colbox-bord-thck &optional rgb-col)
    (let* ((colbox-inner-size (- colbox-size (* colbox-bord-thck 2)))
           (colbox-inner-x0 (+ colbox-x0 colbox-bord-thck))
           (colbox-inner-y0 (+ colbox-y0 colbox-bord-thck))
           (colbox-inner-offsx (- colbox-size colbox-bord-thck))
           (col-idx (key2color rgb-col :col-idx t)))
        (cond (rgb-col
                 (set-color (car col-idx))
                 (fill-rect (+ colbox-inner-x0 (* colbox-inner-offsx (cadr col-idx))) colbox-inner-y0
                            colbox-inner-size colbox-inner-size))
              (t (move colbox-x0 colbox-y0)
                 (set-color WHITE)
                 (fill-rect-rel (+ colbox-size (* (- colbox-size colbox-bord-thck) 2)) colbox-size)
                 (set-color BLACK)
                 (square-outline-rel colbox-size colbox-bord-thck)
                 (moverel (- colbox-size colbox-bord-thck) 0)
                 (square-outline-rel colbox-size colbox-bord-thck)
                 (moverel (- colbox-size colbox-bord-thck) 0)
                 (square-outline-rel colbox-size colbox-bord-thck)))))

(defun base-color-panel (x0 y0 container-w &optional updates turn)
    (let* ((act-upd  (car updates))
           (action   (car act-upd))
           (src-new  (cadr act-upd))
           (src-cell (caddr src-new))
           (dst-new  (caddr act-upd))
           (dst-cell (caddr dst-new))
           (base-cell-size 18)
           (colbox-size (1- base-cell-size))
           (base-bord-thck 2)
           (colbox-bord-thck 1)
           (base-colbox-sep 2)
           (base-sep-y 20)
           (base-x-margin (floor (/ (- container-w (+ base-cell-size base-colbox-sep colbox-size
                                    (* (- colbox-size colbox-bord-thck) 2))) 2)))
           (base-t1-x (+ x0 base-x-margin)) (base-t1-y (+ y0 base-sep-y))
           (base-t2-x (+ x0 base-x-margin)) (base-t2-y y0)
           (colbox-t1-x (+  base-t1-x base-cell-size base-colbox-sep))
           (colbox-t1-y (1+ base-t1-y))
           (colbox-t2-x (+  base-t2-x base-cell-size base-colbox-sep))
           (colbox-t2-y (1+ base-t2-y)))
        (cond ((and (eq action ACTION-PAINT) (cell-has-base dst-cell))
                 (let* ((new-color (cell-unit-color src-cell))
                        (base-team (cell-unit-team dst-cell))
                        (colbox-x  (if (eq base-team TEAM-1) colbox-t1-x colbox-t2-x))
                        (colbox-y  (if (eq base-team TEAM-1) colbox-t1-y colbox-t2-y)))
                    (base-color-upd colbox-x colbox-y colbox-size colbox-bord-thck new-color)))
              ((= turn 0)
                 (move base-t1-x base-t1-y)
                 (set-color BLACK)
                 (square-outline-rel base-cell-size base-bord-thck)
                 (move (1+ base-t1-x) (1+ base-t1-y))
                 (draw-base (list 0 0 0 TEAM-1 0 0) base-cell-size)
                 (move base-t2-x base-t2-y)
                 (set-color BLACK)
                 (square-outline-rel base-cell-size base-bord-thck)
                 (move (1+ base-t2-x) (1+ base-t2-y))
                 (draw-base (list 0 0 0 TEAM-2 0 0) base-cell-size)
                 (base-color-upd colbox-t1-x colbox-t1-y colbox-size colbox-bord-thck)
                 (base-color-upd colbox-t2-x colbox-t2-y colbox-size colbox-bord-thck)))))

;; TODO: Marco que canvii amb es color de s'equip actual, fer mètode draw-rect-rel
;; (square-outline-rel (get-str-width (strcat td-txt1 td-team-txt) td-scale1) (+ td-y0 10)) 
(defun stats-table (xi container-w state turn team)
    (let* ((m (state-map state))
           (e1-paint  (state-paint state TEAM-1))
           (e2-paint  (state-paint state TEAM-2))
           (team-labs (map-count m (lambda (cell) (cell-lab-team cell team))))
           (header-scale 1) (stats-scale 1)
           (lin-thck 1)
           (outer-sep (* 3 stats-scale))
           (inner-sep (* 2 stats-scale))
           (table-y 210)
           (pq-txt0 "Paint") (pm-txt0 "Mult") (lq-txt0 "Labs") (lt-txt0 "TOTAL ")
           (row0-y (- table-y  lin-thck (* header-scale BITMAP-HEIGHT) (* inner-sep 2)))
           (row1-y (- row0-y   lin-thck (* stats-scale  BITMAP-HEIGHT) (* inner-sep 1) 1))
           (row2-y (- row1-y   lin-thck (* stats-scale  BITMAP-HEIGHT) (* inner-sep 1)))
           (col0-w (+ lin-thck (get-str-width pq-txt0 header-scale) outer-sep inner-sep))
           (col1-w (+ lin-thck (get-str-width pm-txt0 header-scale) outer-sep inner-sep)) ;(* inner-sep 2)))
           (col2-w (+ lin-thck (get-str-width lq-txt0 header-scale) outer-sep inner-sep)) ;(* lin-thck 2)
           (table-x-margin (floor (/ (- container-w (+ col0-w col1-w col2-w)) 2))) 
           (table-x (+ xi table-x-margin))
           (col0-x  (+ table-x lin-thck)) ;col refers to first px after column's left vertical border
           (col1-x  (+ col0-x col0-w))
           (col2-x  (+ col1-x col1-w))
           ;headers
           (pq-x0 (+ col0-x outer-sep))  (pq-y0 (+ row0-y inner-sep))
           (pm-x0 (+ col1-x outer-sep))  (pm-y0 (+ row0-y inner-sep))  ; (pm-x0 (+ col1-x inner-sep)) 
           (lq-x0 (+ col2-x outer-sep))  (lq-y0 (+ row0-y inner-sep))
           (lt-x0 (- pm-x0 (* stats-scale 2)))
           (lt-y0 (- row2-y outer-sep inner-sep (* BITMAP-HEIGHT header-scale))) ;TOTAL LAB QTY
           (pq-t1-txt (strcat "" e1-paint))
           (pq-t2-txt (strcat "" e2-paint))
           (pm-t-txt  (strcat "x" (+ 1 (* team-labs 0.5))))
           (lq-t-txt  (strcat team-labs))
           (stats-max-cs-h  (* stats-scale BITMAP-HEIGHT))
           (stats-base-cs-w (* stats-scale BITMAP-WIDTH))
           (pq-t-max-cs (list (* (min 4 (1+ (maxlen pq-t1-txt pq-t2-txt))) stats-base-cs-w)
                               stats-max-cs-h))
           (pm-t-max-cs (list 16 stats-max-cs-h))
           (lq-t-max-cs (list (* 2 stats-base-cs-w) stats-max-cs-h))
           (pq-t-txt (if (> (length pq-t1-txt) (length pq-t2-txt)) pq-t1-txt pq-t2-txt))
           (pq-t-margin-x (round (/ (- col0-w (get-str-width "XXXX" stats-scale))  2)))
           (pm-t-margin-x (round (/ (- col1-w (get-str-width pm-t-txt stats-scale)) 2)))
           (lq-t-margin-x (round (/ (- col2-w (get-str-width lq-t-txt stats-scale)) 2)))
           (table-w (- (+ col2-x col2-w) lin-thck table-x))
           (table-bott-y (- row2-y inner-sep))
           (table-h (+ (- table-y table-bott-y) lin-thck))
           (header-cell-h (- table-y row0-y))
           (footer-cell-h (- row2-y lt-y0 (* 2 lin-thck)))
           (t-bcol SOFT-WHITE))
        (cond ((= turn 0)
               (let* ((labs (map-count m (lambda (cell) (cell-has-lab cell))))
                      (lt-qty-txt (strcat labs)))
                    (set-color t-bcol)
                    (fill-rect table-x (1- lt-y0) table-w (+ table-h footer-cell-h))
                    (set-color BLACK)
                    (draw-str pq-txt0 pq-x0 pq-y0 header-scale)
                    (draw-str pm-txt0 pm-x0 pm-y0 header-scale)
                    (draw-str lq-txt0 lq-x0 lq-y0 header-scale)
                    (draw-str lt-txt0 lt-x0 lt-y0 header-scale)
                    (draw-str lt-qty-txt (+ lq-x0 lq-t-margin-x) lt-y0 stats-scale)
                    (rect-outline table-x row0-y table-w header-cell-h)
                    (rect-outline table-x     (1- table-bott-y) col0-w table-h) 
                    (rect-outline (1- col1-x) (1- table-bott-y) col1-w table-h)
                    (rect-outline (1- col2-x) (1- table-bott-y) col2-w table-h)
                    (rect-outline table-x     (1- lt-y0)        table-w footer-cell-h)))
            (t (let ((t-y (if (eq team TEAM-1) row1-y row2-y)))
                (draw-str pq-t1-txt (+ pq-x0 pq-t-margin-x) row1-y stats-scale :max-cs pq-t-max-cs :bcol t-bcol)
                (draw-str pq-t2-txt (+ pq-x0 pq-t-margin-x) row2-y stats-scale :max-cs pq-t-max-cs :bcol t-bcol)
                (draw-str pm-t-txt  (+ pm-x0)               t-y    stats-scale :max-cs pm-t-max-cs :bcol t-bcol)
                (draw-str lq-t-txt  (+ lq-x0 lq-t-margin-x) t-y    stats-scale :max-cs lq-t-max-cs :bcol t-bcol))))))
                                            ;;  pm-t-margin-x)

; full-COORDS-length '(XX,YY) (XX,YY)' = 15 ; pA-full-length ' (XX,YY)' = 8 ; number width = 5 ; (XX,YY) width = 25
(defun log-update (x y container-h updates &key (src-y y) bcol)
    (if updates
        (let* ((act-upd (car updates))
               (action (car act-upd))
               (pA (cadr act-upd))
               (pB (caddr act-upd))
               (cell (caddr pA))
               (ccolor (cell-unit-color cell))
               (txt-scale 1)
               (action-txt (strcat (if (eq action ACTION-MOVE) "MOURE" action)))
               (pA-txt               (strcat "(" (car pA) "," (cadr pA) ")"))
               (pB-txt    (if pB     (strcat "(" (car pB) "," (cadr pB) ")") ""))
               (color-txt (if ccolor (strcat "(" ccolor ")") ""))
               (margin-x 10)
               (line-spacing 2)
               (line-h      (+ (* txt-scale BITMAP-HEIGHT) line-spacing))
               (in-bounds   (and src-y (<= (+ y (* line-h 2)) (+ src-y container-h))))
               (action-y    (+ (if in-bounds y src-y) line-h)) ; (- y line-h)
               (action-x    (+ x margin-x))
               (pB-x        (+ action-x 70))
               (pA-x        (if (eq action ACTION-CREATE-BALL) pB-X (+ action-x 35)))
               (color-x     (+ action-x 107))
               (line-w      (- (+ color-x 12) action-x))
               (line-max-cs (list line-w line-h)))
            (draw-str action-txt action-x action-y txt-scale :max-cs line-max-cs :bcol bcol)
            (draw-str pA-txt pA-x action-y txt-scale)
            (draw-str pB-txt pB-x action-y txt-scale)
            (draw-str color-txt color-x action-y txt-scale)
            (log-update x action-y container-h (cdr updates) :src-y src-y :bcol bcol))
        nil))

(defun log-panel (frame-x frame-y frame-w frame-h updates turn)
    (let* ((frame-bord-thck 2)
           (shadow-thck 2)
           (inner-frame-h (- frame-h (* frame-bord-thck 2)))
           (inner-frame-w (- frame-w (* frame-bord-thck 2))))
        (cond ((= turn 0)
               (rect-outline frame-x frame-y frame-w frame-h frame-bord-thck)
               (set-color '(100 100 100))
               (fill-rect (+ frame-x shadow-thck) (- frame-y shadow-thck) frame-w shadow-thck)
               (fill-rect (+ frame-x frame-w) (- frame-y shadow-thck) shadow-thck (- frame-h shadow-thck))
               (set-color SOFT-WHITE) ;upd block
               (fill-rect (+ frame-x frame-bord-thck) (+ frame-y frame-bord-thck) inner-frame-w inner-frame-h)
                ; TEMPORARY!!
                ;;   (let* ((use-custom-cell-size t)
                ;;          (tst-cell-size (if use-custom-cell-size 10 cell-size)))
                ;;       (set-color BLACK)
                ;;       (move (+ 100 board-container-size) 180)
                ;;       (square-outline-rel tst-cell-size)
                ;;       (move (1+ (+ 100 board-container-size)) (1+ 180)) 
                ;;       (draw-ball (list 0 0 0 TEAM-1 0 (list ) 'r 0 0) tst-cell-size)
                ;;     ;; Bolla: (TERRA COLOR BOLLA EQUIP ID COLORS-PINTAT COLOR-PROPI TR-PINTAR TR-MOURE)
                ;;       (draw-str (strcat "this cell-size:    " tst-cell-size) (+ 58 board-container-size) 168 1)
                ;;   (draw-str (strcat "board cell-size: " (- cell-size (* 2 CELL-BORD-THCK)))  (+ 47 board-container-size) 135 1))
                ;; (set-color BACKGROUND-COL)
                ;; (fill-rect (+ frame-x frame-bord-thck)       (+ frame-y frame-bord-thck) 
                ;;           (- frame-w (* frame-bord-thck 2)) (- frame-h (* frame-bord-thck 2)))
                ;; ;; (test-draw-base (+ board-container-size 20) 45 0 0 (- CELL-MIN-SIZE (* 2 CELL-BORD-THCK)) (- CELL-MAX-SIZE (* 2 CELL-BORD-THCK)))
                ;; (test-draw-base (+ xi 20) 35 0 0 CELL-MIN-SIZE CELL-MAX-SIZE)
               )
            (t (set-color SOFT-WHITE) ;upd block
               (fill-rect (+ frame-x frame-bord-thck) (+ frame-y frame-bord-thck) inner-frame-w inner-frame-h)
               (set-color DEF-TEXT-COL)
               (log-update frame-x frame-y inner-frame-h updates :bcol SOFT-WHITE) ;;(log-update frame-x (+ frame-y frame-h (- frame-bord-thck) (- 5)) update-log)
               ))))

(defun status-sidebar-upd (state xi &optional updates)
    (let* ((turn (state-turn state))
           (team (if (oddp turn) TEAM-1 TEAM-2))
           (sidebar-w (- XLISP-WINDOW-WIDTH xi))
           (sidebar-h XLISP-WINDOW-HEIGHT)
           (log-frame-w 140)
           (log-frame-h 130))
        (turn-counter-panel xi 345 sidebar-w team turn)
        (team-panel xi 295 sidebar-w team turn)
        (base-color-panel xi 225  sidebar-w (if (= turn 0) nil updates) turn)
        (stats-table xi sidebar-w state turn team)
        (log-panel (+ xi (round (/ (- sidebar-w log-frame-w) 2))) 20 log-frame-w log-frame-h updates turn)

        ;; LAB  ;; (set-color BLACK) (move (+ 10 xi) 65) (square-outline-rel 18 18) (move (1+ (+ 10 xi)) (1+ 65)) (draw-lab (list 0 0 0 TEAM-1 0 0) 18)
                ;; (let* ((rand (random 3)) (col (cond ((= rand 0) 'r) ((= rand 1) 'g) ((= rand 2) 'b))) (rand2 (random 2)) (team (if (= rand2 0) TEAM-1 TEAM-2)))
                ;; (sidebar-color-manager (+ 10 xi) 45 (list (list ACTION-PAINT (list 0 0 (list LAND 'r BALL TEAM-1 12 (list col) col 0 0)) (list 0 0 (list LAND 'r BASE team 12))))))
        
        (when (= turn 0)
            (let* ((ver-txt "LISP Paintball v1.0.0")
                   (txt-scale 1) (txt-len (get-str-width ver-txt txt-scale)))
                (draw-str ver-txt (+ xi (round (/ (- sidebar-w txt-len) 2))) 3 txt-scale :tcol SOFT-WHITE)))))

(defun paint-changes (xi yi cell-size updates)
    (if updates
        (let* ((act-upd (car updates))
               (action (car act-upd))
               (pA (if (neq action ACTION-PAINT) (cadr act-upd) (caddr act-upd)))
               (pB (if (neq action ACTION-PAINT) (caddr act-upd) nil)))
            ;; (when (eq action ACTION-MOVE) (draw-arrow xi yi (car pA) (cadr pA) (car pB) (cadr pB) cell-size))
            (repaint-cell xi yi (car pA) (cadr pA) (caddr pA) cell-size)
            (when pB (repaint-cell xi yi (car pB) (cadr pB) (caddr pB) cell-size))
            ;; (princ (strcat "updates: " updates))(terpri)
            ;; (set-color BLACK)
            ;; (princ (strcat "act-udp: " act-upd))(terpri)
            ;; (princ (strcat "action: " action)) (terpri)
            ;; ;; (princ (strcat "pA:" pA)) 
            ;; (princ (strcat " -> pAx=" (car pA) " pAy=" (cadr pA))) (terpri)
            ;; ;; (princ (strcat "pB:" pB))
            ;; (princ (strcat " -> pBx=" (car pB) " pAy=" (cadr pB))) (terpri)
            (paint-changes xi yi cell-size (cdr updates)))
        nil))

; xi, yi, posicionament del tauler; tx,ty numero de columna i fila
(defun repaint-cell (xi yi tx ty cell cell-size)
    (let ((x (+ xi (* tx (- cell-size CELL-BORD-THCK))))
          (y (- yi (* ty (- cell-size CELL-BORD-THCK)))))
        (move x y)
        (draw-cell cell cell-size)))

; TODO: fix arrow drawing
(defun draw-arrow (xi yi ux uy tx ty cell-size)
    (let* ((orig-x (+ xi (* ux (- cell-size CELL-BORD-THCK))))
           (orig-y (- yi (* uy (- cell-size CELL-BORD-THCK))))
           (dest-x (+ xi (* tx (- cell-size CELL-BORD-THCK))))
           (dest-y (- yi (* ty (- cell-size CELL-BORD-THCK))))
           (innr-s (- cell-size (* 2 CELL-BORD-THCK)))
           (offs (round (/ innr-s 2)))
           (sep-lin-y (- (round (/ (- cell-size CELL-BORD-THCK) 2)) 2))
           (dif-lin-x 4))
        (set-color BLACK)
        ;; (princ (strcat "origin arrow coords: (" ux "," uy ")"))(terpri)
        ;; (princ (strcat "destin arrow coords: (" tx "," ty ")"))(terpri)
        ;; (princ (strcat "orig: x="  orig-x "  y=" orig-y))(terpri)
        ;; (princ (strcat "dest: x="  dest-x "  y=" dest-y))(terpri)
        ;; (princ (strcat "off-dir: " off-dir "  (car off-dir):" (car off-dir) "  (cadr off-dir):" (cadr off-dir))) (terpri)
        (move (+ orig-x offs) (+ orig-y offs))
        ;; (fill-rect x y 2 2)
        ;; (draw (+ dest-x (* offs (car off-dir))) (+ dest-y (* offs (cadr off-dir))))
        (draw (+ dest-x offs) (+ dest-y offs))
        (move (+ orig-x offs dif-lin-x) (+ orig-y offs sep-lin-y))
        (draw (+ dest-x offs) (+ dest-y offs sep-lin-y))
        (move (+ orig-x offs dif-lin-x) (+ orig-y offs (- sep-lin-y)))
        (draw (+ dest-x offs) (+ dest-y offs (- sep-lin-y)))
        ;; (moverel (- dest-x) (- dest-y))
        ))

(defun draw-map (m xi yi row cell-size)
    (cond ((null m) nil)
          (t (move xi (- yi (* row (- cell-size CELL-BORD-THCK)))) ; Adjusted to avoid double margin in-between cells
             (draw-row (car m) cell-size)
             (draw-map (cdr m) xi yi (+ row 1) cell-size))))

(defun draw-row (l cell-size)
    "Pinta la fila `l`, amb índex `x` (número de cel·la dins del conjunt)"
    (cond ((null l) nil)
          (t (draw-cell (car l) cell-size)
             (moverel (- cell-size CELL-BORD-THCK) 0)
             (draw-row (cdr l) cell-size))))

; Requereix posicionament absolut previ
(defun draw-cell (cell cell-size)
    "Dibuixa la cel·la passada per paràmetre amb el tamany cell-size"
        (set-color DEF-MARG-COL)
        (square-outline-rel cell-size) ; pinta el quadrat
        (set-color (if (cell-type-water cell) WATER-COL (if PAINT-CELL-COL (key2color (cell-color cell) :color-class LAND-COL) LAND-COL)))
        (moverel CELL-BORD-THCK CELL-BORD-THCK)
        (fill-rect-rel (- cell-size (* CELL-BORD-THCK 2)) (- cell-size (* CELL-BORD-THCK 2)))
        (cond ((cell-has-base cell) (draw-base cell cell-size))
              ((cell-has-lab  cell) (draw-lab cell cell-size) ) ;(draw-ball cell cell-size)) 
              ((cell-has-ball cell) (draw-ball cell cell-size))
              (t nil))
        (moverel (- CELL-BORD-THCK) (- CELL-BORD-THCK)))

; TODO: FIX lab painting left offset
(defun draw-lab (cell cell-size)
        (let* ((lab-t1 (cell-owned-by cell TEAM-1))
               (lab-t2 (cell-owned-by cell TEAM-2)) 
               (NEUTRAL-WHITE '(220 222 221))
               (SHADOW-WHITE  '(176 176 176))
               (SHADOW-RED  (if lab-t1 '(110 110 0) (if lab-t2 '(0 116 117) '(179 34 37))))
               (NEUTRAL-RED (if lab-t1 '(123 123 0) (if lab-t2 '(0 136 136) '(222 31 33)))) ; Could be RED?
               (PINKIER-RED (if lab-t1 '(135 135 0) (if lab-t2 '(0 156 156) '(204 56 59))))
               (BRIGHT-RED  (if lab-t1 '(155 155 0) (if lab-t2 '(0 176 177) '(242 61 64))))
               (max-inner-sp (- CELL-MAX-SIZE (* CELL-BORD-THCK 2)))
               (lab-size (max 4 (- cell-size (* CELL-BORD-THCK 2))))
               (g (/ lab-size max-inner-sp))
               (pixl-size              (round g))
               (tile-size              (round (* g 8)))
               (refl-size              (round (* g 2)))
               (cent-white-sq-w        (round (* g 12))) (cent-white-sq-h (round (* g 13)))
               (no-refl-size           (round (* g 6)))
               (no-refl-no-shadow-size (round (* g 4)))
            ;;    (ul-tile-bcol-w (round (* g 4)))   (ul-tile-bcol-h (round (* g 6)))
            ;;    (ul-tile-bcol-x (round (* g 2)))   (ul-tile-bcol-y (round (* g 10)))
            ;;    (ul-tile-refl-w (round (* g 6)))   (ul-tile-refl-h tile-size)
            ;;    (lr-tile-bcol-w (round (* g 6)))   (lr-tile-bcol-h tile-size)
            ;;    (lr-tile-bcol-x tile-size)         (lr-tile-bcol-y 0)
            ;;    (lr-tile-refl-x (round (* g 14)))  (lr-tile-refl-y 0)
               )
            ;; (set-color BLACK)
            ;; (princ (strcat "ls=" lab-size)) (terpri) (princ (strcat "g=" g))(terpri)
            ;; (princ (strcat "pixl-size= " pixl-size)) (terpri)
            ;; (princ (strcat "tile-size= " tile-size "   refl-size= " refl-size)) (terpri)
            (set-color SHADOW-WHITE)
            (fill-rect-rel tile-size tile-size)
            (moverel tile-size tile-size)
            (set-color WHITE)
            (fill-rect-rel tile-size tile-size)
            (moverel (- no-refl-size) (- pixl-size tile-size))
            (set-color NEUTRAL-WHITE)
            (fill-rect-rel cent-white-sq-w cent-white-sq-h)
            (moverel (- refl-size) (- tile-size pixl-size))
            (set-color SHADOW-RED)
            (fill-rect-rel 2 tile-size)
            (moverel refl-size 0)
            (set-color PINKIER-RED)
            (fill-rect-rel no-refl-size tile-size)
            (moverel 0 refl-size)
            (set-color NEUTRAL-RED)
            (fill-rect-rel no-refl-no-shadow-size no-refl-size)
            (moverel no-refl-size (- (+ tile-size refl-size)))
            (fill-rect-rel no-refl-size tile-size)
            (moverel no-refl-size 0)
            (set-color BRIGHT-RED)
            (fill-rect-rel refl-size tile-size)
            (moverel (- (+ tile-size no-refl-size)) 0)))

;;; !! TEMPORARY !!
(defun test-draw-base (x y row col lower-bound upper-bound)
    (when (<= lower-bound upper-bound)
        (let* ((sep 30)
               (act-cs lower-bound)
               (col-qty 4)
               (next-col (if (= col (1- col-qty)) 0 (1+ col)))
               (next-row (if (= col (1- col-qty)) (1+ row) row))
               (act-x (+ x (* sep col)))
               (act-y (+ y (* sep row))))
            (set-color BLACK)
            (move act-x act-y)
            (square-outline-rel act-cs)
            (move (+ act-x CELL-BORD-THCK) (+ act-y CELL-BORD-THCK)) 
            (draw-ball (list 0 0 0 TEAM-1 0 (list 'g 'b) 'r 0 0) act-cs)
            (draw-str (to-string act-cs) (+ act-x 2) (- act-y 10) 1)
            (test-draw-base x y next-row next-col (1+ lower-bound) upper-bound))))

; TODO: fix small px size ball resizing
(defun draw-ball (cell cell-size)
    (let* ((max-inner-sp (- CELL-MAX-SIZE (* CELL-BORD-THCK 2)))
           (base-size (max 4 (- cell-size (* CELL-BORD-THCK 2))))
           (g (/ base-size max-inner-sp))
           (diag-size-std (round (* g 7)));8
           (diag-size-sm  (round (* g 6)));7
           (dist-diag-sep (round (* g 9)))
           (spacing (round (* g 2)))
           (spacing4 (round (* g 4)))
           (border-type 2) ; 0: None | 1: Single | 2: Double ;TEMPORARY

        ;;    (line-thck (round (* cell-size 0.0625))) 
        ; V1 Thicker Background color strip
        ;;    (delim-x (round (* g 5))) (delim-sep (round (* g 6))) (delim-len (round (* g 12))) (delim-y   (round (* g 2)))
        ; V2 Slimmer Background color strip
        ;;    (delim-x (round (* g 6))) (delim-sep (round (* g 4))) (delim-len (round (* g 14))) (delim-y   (round (* g 1)))
        ;;    ; V1 Color Distribution
        ;;    (tile1-x (round (* g 2)))  (tile1-y (round (* g 4))) (tile2-x (round (* g 10))) (tile2-y (round (* g 4))) (tiles-h (round (* g 8)))
           
                                    ; tile1-y = 1 for cell-size 15
           (tile1-x (round (* g 1))) (tile1-y (round (* g 2))) ; V2 Color Distribution
           (tile2-x (round (* g 9))) (tile2-y (round (* g 2)))
           (tiles-h (round (* g 12)))

        ;;    ; Central Triangles small (V1)
        ;;    (cent-triang-l-x (round (* g 5))) (cent-triang-l-y (round (* g 5))) (cent-triang-r-x (round (* g 8))) (cent-triang-r-y (round (* g 5))) (cent-triang-h  (round (* g 6)))
        ;cs 15 ;;(cent-triang-l-x (round (* g 5))) (cent-triang-l-y (round (* g 5))) ; Central Triangles big (V2)
        ;;    (cent-triang-r-x (round (* g 9))) (cent-triang-r-y (round (* g 5)))

           (cent-triang-l-x (round (* g 4))) (cent-triang-l-y (round (* g 4))) ; Central Triangles big (V2)
           (cent-triang-r-x (round (* g 8))) (cent-triang-r-y (round (* g 4)))
           (cent-triang-h   (round (* g 8)))
           (cell-own-col (cell-unit-color cell))
           (cell-oth-col (remove cell-own-col (cell-unit-paint cell)))
           (ball-backg-col  (key2color cell-own-col)) ; COLOR PROPI
           (ball-paint-cols (key2color cell-oth-col)) ; COLOR PINTATS
           (ball-paint-col1 (if (car ball-paint-cols)  (car ball-paint-cols)  ball-backg-col))
           (ball-paint-col2 (if (cadr ball-paint-cols) (cadr ball-paint-cols) ball-backg-col))
           (team-col (if (cell-owned-by cell TEAM-1) '(118 118 0) '(136 254 255)))

           ; TEMPORARY
           (ver1 t)  ; V1 Colors pintats laterals, central color bolla | V2 Colors pintats centrals, laterals color bolla
        ;;    (ver1 (cell-owned-by cell TEAM-1))
           (cent-l-triang-col (if ver1 ball-backg-col  ball-paint-col1))
           (cent-r-triang-col (if ver1 ball-backg-col  ball-paint-col2))
           (lsided-triang-col (if ver1 ball-paint-col1 ball-backg-col))
           (rsided-triang-col (if ver1 ball-paint-col2 ball-backg-col)))

        ;; (princ (strcat "dball cs=" base-size)) (terpri) (princ (strcat "g=" g)) (terpri)
        ;; (princ (strcat "backg-col: " ball-backg-col)) (terpri)
        ;; (princ (strcat "own-col: " cell-own-col)) (princ (strcat "  oth-col: " cell-oth-col)) (terpri)
        ;; (princ (strcat "paint-col: " ball-paint-cols)) (terpri)
        
        (cond ((>= border-type 1)
            (set-color team-col)
            (moverel (- diag-size-std 1) 0)
            (drawrel (- diag-size-std) diag-size-std)
            (moverel 1 spacing)

            ;; (moverel 0 -1) ;cs 15
            (drawrel diag-size-std diag-size-std)
            ;; (moverel 0 1)
            
            (moverel (+ diag-size-std spacing -1) (- diag-size-std))
            (drawrel (- diag-size-std) diag-size-std)
            (moverel 1 (- (+ (* diag-size-std 2) spacing)))

            ;; (moverel -1 0) ;cs 15
            (drawrel diag-size-std diag-size-std)
            ;; (moverel 1 0)

            (moverel (- (+ (* diag-size-std 2) spacing)) (- diag-size-std))
            (cond ((>= border-type 2) 
                (moverel (- diag-size-sm 1) 0) 
                (drawrel (- diag-size-sm) diag-size-sm)
                (moverel 1 spacing4)
                (drawrel diag-size-sm diag-size-sm)
                (moverel (+ diag-size-sm spacing4 -1) (- diag-size-sm))
                (drawrel (- diag-size-sm) diag-size-sm)
                (moverel  1 (- (+ (* diag-size-sm 2) spacing4)))
                (drawrel diag-size-sm diag-size-sm)
                (moverel (- (+ (* diag-size-sm 2) spacing4)) (- diag-size-sm))))))
        ;; (moverel 1 1)
        (set-color ball-backg-col) ; Fill Ball with background color
        (draw-triangle-iso base-size 'LPT)
        (moverel (round (/ base-size 2)) 0)
        (draw-triangle-iso (1+ base-size) 'RPT)
        (moverel (- (round (/ base-size 2))) 0)
        ;; (moverel -1 -1)
        (cond (ver1 (set-color lsided-triang-col) ; 1st color paint
                    (moverel tile1-x tile1-y)
                    (draw-triangle-iso tiles-h 'LPT)
                    (moverel (- tile1-x) (- tile1-y))
                    (set-color rsided-triang-col) ; 2nd color paint
                    (moverel tile2-x tile2-y)
                    (draw-triangle-iso tiles-h 'RPT)
                    (moverel (- tile2-x) (- tile2-y))))
        (set-color cent-l-triang-col) ; Central triangles 
        (moverel cent-triang-l-x cent-triang-l-y)
        ;; (moverel 1 0) ;cs 15
        (draw-triangle-iso cent-triang-h 'LPT)
        ;; (moverel -1 0)
        (moverel (- cent-triang-l-x) (- cent-triang-l-y))
        (set-color cent-r-triang-col)
        (moverel cent-triang-r-x cent-triang-r-y)
        ;; (moverel -1 0) ;cs 15
        (draw-triangle-iso cent-triang-h 'RPT)
        ;; (moverel 1 0)
        (moverel (- cent-triang-r-x) (- cent-triang-r-y))))

;; Move all file colors to an additional file? (load "colors.lsp")
(defun draw-base (cell cell-size)
    (let* ((max-inner-sp (- CELL-MAX-SIZE (* CELL-BORD-THCK 2)))
           (base-size (max 4 (- cell-size (* CELL-BORD-THCK 2))))
           (g (/ base-size max-inner-sp)) ; base-size / reference-size ratio (decimal)
           (hires-icon (> base-size 8))
           (base-team1 (cell-owned-by cell TEAM-1))
           (BASE-COL1 (if base-team1 '(118 118 0)   '(0 117 118)))   ; Darkest
           (BASE-COL2 (if base-team1 '(189 189 0)   '(0 188 189)))
           (BASE-COL3 (if base-team1 '(229 229 0)   '(0 228 229)))
           (BASE-COL4 (if base-team1 '(240 240 125) '(124 240 240)))
           (BASE-COL5 (if base-team1 '(255 255 137) '(136 254 255))) ; Lightest
           )
        ;;   (princ (strcat "bs=" base-size)) (terpri) (princ (strcat "g =" g)) (terpri)
          (if hires-icon
            ; Original Version
            (let ((corn-sm-sq (round (* cell-size 0.1875)))
                  (corn-bg-sq (round (* cell-size 0.3125)))
                  (cent-sm-sq (round (* cell-size 0.3333)))
                  (cent-bg-sq (round (* cell-size 0.6250)))
                  (dist-corn-sm-sq (round (* g 13))) ; Distance in-between small squares
                  (dist-corn-bg-sq (round (* g 10)));11
                  (dist-cent-sm-sq (round (* g 5)))
                  (dist-cent-bg-sq (round (* g 3))))
                (set-color BASE-COL3)   ; background square
                (fill-rect-rel base-size base-size)
                (set-color BASE-COL5)   ; clearest square
                (moverel dist-cent-bg-sq dist-cent-bg-sq)
                (fill-rect-rel cent-bg-sq cent-bg-sq)
                (moverel (- dist-cent-bg-sq) (- dist-cent-bg-sq))
                (set-color BASE-COL4)   ; central square
                (moverel dist-cent-sm-sq dist-cent-sm-sq)
                (fill-rect-rel cent-sm-sq cent-sm-sq)
                (moverel (- dist-cent-sm-sq) (- dist-cent-sm-sq))
                (set-color BASE-COL2)   ; corners big squares
                (fill-rect-rel corn-bg-sq corn-bg-sq)
                (moverel 0 dist-corn-bg-sq)
                (fill-rect-rel corn-bg-sq corn-bg-sq)
                (moverel dist-corn-bg-sq 0)
                (fill-rect-rel corn-bg-sq corn-bg-sq)
                (moverel 0 (- dist-corn-bg-sq))
                (fill-rect-rel corn-bg-sq corn-bg-sq)
                (moverel (- dist-corn-bg-sq) 0)
                (set-color BASE-COL1)  ; corners small squares
                (fill-rect-rel corn-sm-sq corn-sm-sq)
                (moverel 0 dist-corn-sm-sq)
                (fill-rect-rel corn-sm-sq corn-sm-sq)
                (moverel dist-corn-sm-sq 0)
                (fill-rect-rel corn-sm-sq corn-sm-sq)
                (moverel 0 (- dist-corn-sm-sq))
                (fill-rect-rel corn-sm-sq corn-sm-sq)
                (moverel (- dist-corn-sm-sq) 0))
            ; Simple Version
            (let ((corn-sq (round (* cell-size 0.25)))
                  (cent-sq (round (* cell-size 0.50)))
                  (dist-corn-sq (round (* g 12)))
                  (dist-cent-sq (round (* g 4))))
                (set-color BASE-COL2)     ; background square
                (fill-rect-rel base-size base-size)
                (set-color BASE-COL4)     ; central square
                (moverel dist-cent-sq dist-cent-sq)
                (fill-rect-rel cent-sq cent-sq)
                (moverel (- dist-cent-sq) (- dist-cent-sq))
                (set-color BASE-COL1)     ; corners small squares
                (fill-rect-rel corn-sq corn-sq)
                (moverel 0 dist-corn-sq)
                (fill-rect-rel corn-sq corn-sq)
                (moverel dist-corn-sq 0)
                (fill-rect-rel corn-sq corn-sq)
                (moverel 0 (- dist-corn-sq))
                (fill-rect-rel corn-sq corn-sq)
                (moverel (- dist-corn-sq) 0)))))

(defun draw-watermark (xi yi container-w container-h scale &optional (b nil))
    (let* ((col    (if b '(220 220 220) '(245 245 245)))
           (col2   (if b '(200 200 200) '(240 240 240)))
           (sh-col (if b '(200 200 200) '(240 240 240)))
           (sh-offs 2)
           (sh-offs0   (if (< scale 3) 1 sh-offs))
           (ball-offs  (get-str-width "LI" scale))
           (paint-offs (get-str-width "LIS" scale))
           (line-offsy (round (- (* BITMAP-HEIGHT scale) (+ (* 1.5 scale) -1))))
           (total-h    (+ (* 4 line-offsy) (* BITMAP-HEIGHT scale)))
           (x (+ xi (round (/ (- container-w (get-str-width "LISPLL" scale)) 2))))
           (y (+ yi (round (/ (- container-h total-h) 2)) (* 4 line-offsy)))
           )
        (draw-str "LISP" x y scale :tcol sh-col)
        (draw-str "LISP" (+ x sh-offs0) y scale :tcol col)
        (draw-str "BALL" (+ x ball-offs) (- y line-offsy) scale :tcol sh-col)
        (draw-str "BALL" (+ x ball-offs sh-offs0) (- y line-offsy) scale :tcol col)
        (draw-str "P"    (+ x paint-offs) y scale :tcol sh-col)
        (draw-str "P"    (+ x paint-offs sh-offs) y scale :tcol col2)
        (draw-str "A"    (+ x paint-offs) (- y line-offsy) scale :tcol sh-col)
        (draw-str "A"    (+ x paint-offs sh-offs) (- y line-offsy) scale :tcol col2)
        (draw-str "I"    (+ x paint-offs scale) (- y (* line-offsy 2)) scale :tcol sh-col)
        (draw-str "I"    (+ x paint-offs scale sh-offs) (- y (* line-offsy 2)) scale :tcol col2)
        (draw-str "N"    (+ x paint-offs) (- y (* line-offsy 3)) scale :tcol sh-col)
        (draw-str "N"    (+ x paint-offs sh-offs) (- y (* line-offsy 3)) scale :tcol col2)
        (draw-str "T"    (+ x paint-offs) (- y (* line-offsy 4)) scale :tcol sh-col)
        (draw-str "T"    (+ x paint-offs sh-offs) (- y (* line-offsy 4)) scale :tcol col2)))