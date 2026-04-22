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
(defconstant SB-MIN-WIDTH          500) ; Sidebar minimum width
(defconstant SB-FIXED-WIDTH        t)   ; Sidebar adjustment policy, t expands board container, nil prioritizes symmetrical board margins expanding sidebar
(defconstant BOARD-DELIM-W         2)   ; delimiter width
(defconstant BOARD-MAX-COLS        60)
(defconstant BLACK                 '(0 0 0))
(defconstant RED                   '(255 0 0))
(defconstant GREEN                 '(0 255 0))
(defconstant BLUE                  '(0 0 255))
(defconstant WHITE                 '(255 255 255))
(defconstant WATER-COL             '(205 205 255))
(defconstant LAND-COL              '(216 163 133))
(defconstant DEF-TEXT-COL          BLACK)
(defconstant DEF-MARG-COL          BLACK)
(defconstant BACKGROUND-COL        WHITE)


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

; &rest agafa tots els paràmetres que segueixen i els fica dins una llista
; ~{ ~} itera sobre els elements de la llista i aplica el format "~A" a cada un d'ells
; Podria emprar-se també concatenate
(defun strcat (&rest str-list) 
    "Crea un string a partir de la concatenació dels n valors passats per paràmetre"
    (format nil "~{~A~}" str-list))

; p = (x y)
;; (defun cx (p) (car p))                                   ; coordx
;; (defun cy (p) (cadr p))                                  ; coordy
;; (defun csum (p x y) (list (+ (car p) x) (+ (cadr p) y))) ; coordsum
;; (defun ccomp (p1 p2) (equal p1 p2))                      ; coordcomp

; **************************************************
; PRIMITIVES
; **************************************************

(defun set-color (c) (color (car c) (cadr c) (caddr c)))

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
             
(defun draw-square (mida)
    "Dibuixa un quadrat de mida `mida`, a la posició actual."
    (drawrel mida 0)
    (drawrel 0 mida)
    (drawrel (- mida) 0)
    (drawrel 0 (- mida)))
    
(defun square-outline (mida &optional (gruix CELL-BORD-THCK))
    "Dibuixa un square-outline de mida `mida` - 1, i de gruix `gruix`, a la posició actual. SENSE FONS"
    (cond ((plusp gruix) ; si gruix > 0
           (draw-square (- mida 1))
           (moverel 1 1)
           (square-outline (- mida 2) (- gruix 1))
           (moverel -1 -1))))

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
             (moverel (- 1) (- ey)))))

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

(defun draw-str (str x y scale &key max-cs tcol)
    "Dibuixa un string a la posició (x, y) escalat per scale amb color 'tcol',
     repinta el contenidor de dimensions 'max-cs' si aquest s'indica per paràmetre"
    (cond (max-cs
        (set-color BACKGROUND-COL)
        (fill-rect x y (car max-cs) (cdr max-cs))))
    (set-color (if tcol tcol DEF-TEXT-COL))
    (draw-chars (coerce str 'list) x y scale))
; ******************************************************************************************

(defun graphics-init (m)
    (let* ((map-h (map-height m))
           (map-w (map-width m))
           (cell-ratio (min (/ XLISP-WINDOW-HEIGHT map-h) (/ SB-MIN-WIDTH map-w)))
           (cell-size (min CELL-MAX-SIZE (max CELL-MIN-SIZE (truncate cell-ratio)))) ; board cell size
           (board-h (+ (* map-h (- cell-size CELL-BORD-THCK)) CELL-BORD-THCK)) ; (board-h (- (* map-h cell-size) (* (1- map-h) CELL-BORD-THCK)));
           (board-w (+ (* map-w (- cell-size CELL-BORD-THCK)) CELL-BORD-THCK)) ; (board-w (- (* map-w cell-size) (* (1- map-w) CELL-BORD-THCK)));
           (y-margin (floor (/ (- XLISP-WINDOW-HEIGHT board-h) 2)))
           (x-margin (if (or SB-FIXED-WIDTH (> (+ board-w (* y-margin 2)) SB-MIN-WIDTH))
                         (floor (/ (- SB-MIN-WIDTH board-w) 2)) y-margin))
           (board-container-size (if SB-FIXED-WIDTH SB-MIN-WIDTH (min SB-MIN-WIDTH (+ board-w (* x-margin 2)))))
           (xi x-margin)
           (yi (+ (- board-h cell-size) y-margin)))
          (cls)
          (draw-map m xi yi 0 cell-size)
          (set-color BLACK)
          (fill-rect board-container-size 0 BOARD-DELIM-W XLISP-WINDOW-HEIGHT)
          (color 0 0 0)
          

          ; !! TEMPORARY !!
          (let* ((half-cs (/ cell-size 2))
                 (padded-cs (+ cell-size half-cs))
                 (marker-cell-size 18)
                 (marker-bord-thck 1)
                 (cell-inner-size (- (1- marker-cell-size) (* marker-bord-thck 2))))
            ;LAB
            (move (+ 10 board-container-size) 90)
            (square-outline marker-cell-size marker-bord-thck)
            (move (+ 11 board-container-size) 91)
            (draw-lab (list 0 0 0 TEAM-1 0 0) marker-cell-size)
            (moverel (+ marker-cell-size 1) 0)
            (set-color BLACK)
            (square-outline (1- marker-cell-size) marker-bord-thck)
            ;; (set-color RED) (fill-rect-rel marker-cell-size cell-inner-size)
            (moverel (- marker-cell-size (* marker-bord-thck 2)) 0)
            (set-color BLACK)
            (square-outline (1- marker-cell-size) marker-bord-thck)
            ;; (set-color GREEN) (fill-rect-rel marker-cell-size cell-inner-size)
            (moverel (- marker-cell-size (* marker-bord-thck 2)) 0)
            (set-color BLACK)
            (square-outline (1- marker-cell-size) marker-bord-thck) 
            ;; (set-color BLUE) (fill-rect-rel marker-cell-size cell-inner-size)

            ; BASE
            (move (+ 10 board-container-size) 70)
            (square-outline marker-cell-size marker-bord-thck)
            (move (+ 11 board-container-size) 71)
            (draw-base (list 0 0 0 TEAM-1 0 0) marker-cell-size) ;; emprar draw_row
            (moverel (+ marker-cell-size 1) 0)
            (set-color BLACK)
            (square-outline (1- marker-cell-size) marker-bord-thck)
            ;; (set-color RED) (fill-rect-rel marker-cell-size cell-inner-size)
            (moverel (- marker-cell-size (* marker-bord-thck 2)) 0)
            (set-color BLACK)
            (square-outline (1- marker-cell-size) marker-bord-thck)
            ;; (set-color GREEN) (fill-rect-rel marker-cell-size cell-inner-size)
            (moverel (- marker-cell-size (* marker-bord-thck 2)) 0)
            (set-color BLACK)
            (square-outline (1- marker-cell-size) marker-bord-thck) 
            ;; (set-color BLUE) (fill-rect-rel marker-cell-size cell-inner-size)

            ; BASE
            (move (+ 10 board-container-size) 50)
            (square-outline marker-cell-size marker-bord-thck)
            (move (+ 11 board-container-size) 51)
            (draw-base (list 0 0 0 TEAM-2 0 0) marker-cell-size)
            (moverel (+ marker-cell-size 1) 0)
            (set-color BLACK)
            (square-outline (1- marker-cell-size) marker-bord-thck)
            ;; (set-color RED) (fill-rect-rel marker-cell-size cell-inner-size)
            (moverel (- marker-cell-size (* marker-bord-thck 2)) 0)
            (set-color BLACK)
            (square-outline (1- marker-cell-size) marker-bord-thck)
            ;; (set-color GREEN) (fill-rect-rel marker-cell-size cell-inner-size)
            (moverel (- marker-cell-size (* marker-bord-thck 2)) 0)
            (set-color BLACK)
            (square-outline (1- marker-cell-size) marker-bord-thck) 
            ;; (set-color BLUE) (fill-rect-rel marker-cell-size cell-inner-size)

            ;DRAW BALL
            (let* ((cell (list 0 0 0 TEAM-1 0 0 0 0 0))
                    (cell-inner-size (- cell-size (* CELL-BORD-THCK 2))))
                (set-color BLACK)
                (move (+ 10 board-container-size) 110)
                (square-outline marker-cell-size marker-bord-thck)
                (move (+ 11 board-container-size) 111)
                ;; (set-color GREEN) (fill-rect-rel marker-cell-size cell-inner-size)
                (draw-ball cell marker-cell-size))

            (let* ((cell (list 0 0 0 TEAM-2 0 0 0 0 0)))
                (set-color BLACK)
                (move (+ 10 board-container-size) 150)
                (square-outline (1- 50) 1)
                (draw-ball cell 50))
          )
          
          (let* ((triangle-size 20)
                 (half-triangle-size (round (/ triangle-size 2)))
                 (h-sep 1)
                 (all-bord-px (* 3 h-sep))
                 (cateto half-triangle-size)) ; altura (h)
            (set-color BLACK)
            (move (+ 9 board-container-size) 249) ; Left vertical guide
            (drawrel 0 (+ triangle-size h-sep))
            (move (+ 9 board-container-size triangle-size h-sep) 249) ; Right vertical guide
            (drawrel 0 (+ triangle-size h-sep))
            (move (+ 9 board-container-size) 249) ; Lower horizontal guide
            (drawrel (+ triangle-size (* h-sep 2)) 0)
            (move (+ 9 board-container-size) (+ 249 triangle-size h-sep)) ; Upper horizontal guide
            (drawrel (+ triangle-size (* h-sep 2)) 0)
            (set-color RED)
            (move (+ 10 board-container-size) 250)
            (draw-triangle-iso triangle-size 'LPT)
            (move (+ 10 board-container-size half-triangle-size) 250)
            (draw-triangle-iso triangle-size 'RPT))

          (draw-str (strcat "board-container-size=" board-container-size) (+ 10 board-container-size) 360 1)
          (draw-str (strcat "y-margin: " y-margin)                        (+ 10 board-container-size) 340 2)
          (draw-str (strcat "x-margin: " x-margin)                        (+ 10 board-container-size) 320 2)          
          (draw-str (strcat "yi: " yi)                                    (+ 10 board-container-size) 300 2)
          (draw-str (strcat "Max cell size for a ")                       (+ 10 board-container-size) 15 1)
          (draw-str (strcat map-w "x" map-h " board is " cell-size " px.")       (+ 10 board-container-size) 5 1)
    )
)

;; S’ha de mostrar, com a mínim, la següent informació:
;;   CHAT:
;;    ◆ Quantitat de pintura de cada equip (pot ser en text).
;;    ◆ Comptador de torn actual i de quin equip és el torn (pot ser en text).
;;   BOARD:
;; OK ◆ Matriu de caselles que representen el tauler. Caselles d’aigua senyalitzades.
;; OK ◆ Bases de cada equip, diferenciades d’alguna manera, amb indicació visual dels colors
;;       dels quals estan pintades.
;; OK  ◆ Bolles de cada equip, diferenciades d’alguna manera, amb indicació visual del seu
;;       color i dels colors dels quals estan pintades.
;; OK  ◆ Laboratoris, amb indicació de per quin equip estan capturats (o que no estan capturats).

;; Pintar una base dels 3 colors és destruirla, no obstant es pot indicar de quin color estan pintades
;; al panell lateral ja que sino no és visualment factible

(defun draw-map (m xi yi row cell-size)
    (cond ((null m) nil)
          (t ;;  (move xi (- yi (* row cell-size)))
             (move xi (- yi (* row (- cell-size CELL-BORD-THCK)))) ; Adjusted to avoid double margin in-between cells
             (draw-row (car m) cell-size)
             (draw-map (cdr m) xi yi (+ row 1) cell-size))))

(defun draw-row (l cell-size)
    "Pinta la fila `l`, amb índex `x` (número de cel·la dins del conjunt)"
    (cond ((null l) nil)
          (t (draw-cell (car l) cell-size)
             (moverel (- cell-size CELL-BORD-THCK) 0) ;;  (moverel cell-size 0)
             (draw-row (cdr l) cell-size))))

; Requereix posicionament absolut previ
(defun draw-cell (cell cell-size)
    "Dibuixa la cel·la passada per paràmetre amb el tamany cell-size"
        (set-color DEF-MARG-COL)
        (square-outline cell-size) ; pinta el quadrat
        (set-color (if (cell-type-water cell) WATER-COL LAND-COL))
        (moverel CELL-BORD-THCK CELL-BORD-THCK)
        (fill-rect-rel (- cell-size (* CELL-BORD-THCK 2)) (- cell-size (* CELL-BORD-THCK 2)))
        (cond ((cell-has-base cell) (draw-base cell cell-size))
              ((cell-has-lab  cell) (draw-lab  cell cell-size)) ;(draw-ball cell cell-size)) 
              ((cell-has-ball cell) (draw-ball cell cell-size))
              (t nil))
        (moverel (- CELL-BORD-THCK) (- CELL-BORD-THCK)))

; TODO:
;      FIX lab painting left offset
;      Extract HSL Diff of actual color palette
(defun draw-lab (cell cell-size)
        (let* (
               (NEUTRAL-WHITE '(220 222 221))
               (SHADOW-WHITE  '(176 176 176))
               (SHADOW-RED    '(179 34 37)  )
               (NEUTRAL-RED   '(222 31 33)  ) ; Could be RED?
               (PINKIER-RED   '(204 56 59)  )
               (BRIGHT-RED    '(242 61 64)  )
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

            (set-color BLACK)
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

; TODO: Apply color changes; fix small px size ball resizing
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
           
           (tile1-x (round (* g 1))) (tile1-y (round (* g 2))) ; V2 Color Distribution
           (tile2-x (round (* g 9))) (tile2-y (round (* g 2)))
           (tiles-h (round (* g 12)))

        ;;    ; Central Triangles small (V1)
        ;;    (cent-triang-l-x (round (* g 5))) (cent-triang-l-y (round (* g 5))) (cent-triang-r-x (round (* g 8))) (cent-triang-r-y (round (* g 5))) (cent-triang-h  (round (* g 6)))
           
           (cent-triang-l-x (round (* g 4))) (cent-triang-l-y (round (* g 4))) ; Central Triangles big (V2)
           (cent-triang-r-x (round (* g 8))) (cent-triang-r-y (round (* g 4)))
           (cent-triang-h   (round (* g 8)))

        ;;    (ball-backg-col  (cell-unit-color cell))
        ;;    (ball-paint-cols (cell-unit-paint cell))
           (ball-backg-col  GREEN)
           (ball-paint-cols (list RED BLUE))
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

        (cond ((>= border-type 1)
            (set-color team-col)
            (moverel (- diag-size-std 1) 0)
            (drawrel (- diag-size-std) diag-size-std)
            (moverel 1 spacing)
            (drawrel diag-size-std diag-size-std)
            (moverel (+ diag-size-std spacing (- 1)) (- diag-size-std))
            (drawrel (- diag-size-std) diag-size-std)
            (moverel 1 (- (+ (* diag-size-std 2) spacing)))
            (drawrel diag-size-std diag-size-std)
            (moverel (- (+ (* diag-size-std 2) spacing)) (- diag-size-std))
            (cond ((>= border-type 2) 
                (moverel (- diag-size-sm 1) 0) 
                (drawrel (- diag-size-sm) diag-size-sm)
                (moverel 1 spacing4)
                (drawrel diag-size-sm diag-size-sm)
                (moverel (+ diag-size-sm spacing4 (- 1)) (- diag-size-sm))
                (drawrel (- diag-size-sm) diag-size-sm)
                (moverel  1 (- (+ (* diag-size-sm 2) spacing4)))
                (drawrel diag-size-sm diag-size-sm)
                (moverel (- (+ (* diag-size-sm 2) spacing4)) (- diag-size-sm))))))
        (set-color ball-backg-col) ; Fill Ball with background color
        (draw-triangle-iso base-size 'LPT)
        (moverel (round (/ base-size 2)) 0)
        (draw-triangle-iso base-size 'RPT)
        (moverel (- (round (/ base-size 2))) 0)
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
        (draw-triangle-iso cent-triang-h 'LPT)
        (moverel (- cent-triang-l-x) (- cent-triang-l-y))
        (set-color cent-r-triang-col)
        (moverel cent-triang-r-x cent-triang-r-y)
        (draw-triangle-iso cent-triang-h 'RPT)
        (moverel (- cent-triang-r-x) (- cent-triang-r-y))))

;; Move all file colors to an additional file? (load "colors.lsp")
; TODO: PROPORTIONAL COLOR FILTER
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