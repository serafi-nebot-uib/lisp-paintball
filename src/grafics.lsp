;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: Serafí Nebot Ginard, Jaume Galmés Ramis.
;; Professor: Miquel Cabot.
;; Lliurament: primera convocatòria.
;; Fitxer del mòdul gràfic.
;; Dins aquest fitxer es troben tots els mètodes que permeten graficar el joc LISP Paint Ball
;; cada un d'ells conté una breu explicació de la funció que realitza i l'ús dels paràmetres més rellevants

; **************************************************
; CONSTANTS
; **************************************************

(defconstant XLISP-WINDOW-HEIGHT   375)
(defconstant XLISP-WINDOW-WIDTH    640)
(defconstant CELL-MIN-SIZE         6)   ; px
(defconstant CELL-MAX-SIZE         18)
(defconstant CELL-BORD-THCK        1)

; Política d'ajustament de la barra lateral,
; t manté el tamany fixe, expandint el contenidor del tauler
; nil prioritza els marges simètrics del tauler, expandint la barra lateral
(defconstant SB-FIXED-WIDTH        t)
(defconstant SB-MIN-WIDTH          480) ; amplada mínima de la barra lateral
(defconstant BOARD-DELIM-W         2)   ; amplada del delimitador entre contenidors
(defconstant BOARD-MAX-COLS        60)

; colors que s'empren de forma global al llarg del codi
(defconstant BLACK                 '(0 0 0))
(defconstant RED                   '(255 0 0))
(defconstant GREEN                 '(0 255 0))
(defconstant BLUE                  '(0 0 255))
(defconstant WHITE                 '(255 255 255))
(defconstant SOFT-WHITE            '(240 240 240))
(defconstant WATER-COL             '(205 205 255))
(defconstant LAND-COL              '(216 163 133))
(defconstant LAND-COL-RED          '(255 128 109))
(defconstant LAND-COL-GREEN        '(205 168 126))
(defconstant LAND-COL-BLUE         '(166 139 207))

; t pinta el color de la cel·la, nil pinta color de terra
(defconstant PAINT-CELL-COL        t)
(defconstant DEF-TEXT-COL          BLACK)
(defconstant DEF-MARG-COL          BLACK)
(defconstant BACKGROUND-COL        WHITE)

; desactiva el dibuixat global de la marca d'aigua del joc
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

; &rest agafa tots els paràmetres que segueixen i els fica dins una llista
; ~{ ~} itera sobre els elements de la llista i aplica el format "~A" a cada un d'ells
; Podria emprar-se també concatenate
(defun strcat (&rest str-list) 
    "Crea un string a partir de la concatenació dels n valors passats per paràmetre"
    (format nil "~{~A~}" str-list))

; retorna l'element de major longitud
(defun maxlen (&rest str) (reduce 'max (mapcar 'length str)))

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
    "Genera una gradient de lluminositat des del color 'col' fins a blanc a la posició (x,y) amb dimensions (w,h)"
    (cond ((<= h 0) nil)
          (t (let ((new-col (mapcar '(lambda (a) (if (< a 255) (1+ a) 255)) col)))
                (set-color new-col)
                (fill-rect x y w 1)
                (gradient-rect x (1+ y) w (1- h) new-col)))))

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
    "Dibuixa un rectangle d'amplada 'w' i alçada 'h' a la posició actual."
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

(defun draw-triangle (c &optional (ttype 'LLT)) ; cateto = altura = base 
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
; el strings es dibuixen a partir d'una representació en forma de mapa de bits de cada possible caràcter.
; un mapa de bits és un array on cada cel·la representa un píxel.
;    1 indica que el píxel ha de ser pintat
;    0 indica que el píxel no ha de ser pintat
; a partir d'aquest format resulta senzill definir una representació de caràcter de 5x6 píxels.
; cal destacar que amb la finalitat de espaiar els caràcters s'ha deixat una fila i una columna
; buides al mapa de bits de cada caràcter, això es fa a partir de FONT-WIDTH i FONT-HEIGHT que dicten
; l'amplada i l'alçada del caràcter final dibuixat.
;
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

; mètode general d'actualització del fitxer "grafics.lsp", s'encarrèga d'obtenir el mapa, cridar als mètodes
; d'actualització generals (barra lateral i mapa) amb els canvis realitzats, i calcular a partir de
; la política seleccionada de la barra lateral [fixa (tamany mínim de barra lateral) o mòvil]:
; - El tamany de cel·la, intentant que aquesta sigui la màxima possible sense sobrepassar l'espai disponible.
; - El posicionament absolut del tauler o mapa dins de l'interfície de mode que aquesta es trobi centrada
(defun graphics-upd (state &optional (updates nil))
    "Actualitza els gràfics a partir de l'estat 'state' i la llista de actualitzacions 'update'"
    (let* ((m (state-map state))
           (map-h (map-height m))
           (map-w (map-width m))
           (cell-ratio (min (/ XLISP-WINDOW-HEIGHT map-h) (/ SB-MIN-WIDTH map-w)))
           (cell-size  (min CELL-MAX-SIZE (max CELL-MIN-SIZE (truncate cell-ratio)))) ; tamany de cel·la del tauler
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
 
(defun graphics-end (winner)
    "Rep i grafica el guanyador 'winner' de la partida"
    (let* ((msg-scale 2)
           (num-scale (+ msg-scale 1))
           (msg-x 150)  (msg-y 250)
           (msg-w 150)  (msg-h 80)
           (msg-txt0 "GUANYA")
           (msg-txt1 "L'EQUIP")
           (msg-num0 (if (eq winner TEAM-1) "1" "2"))
           (msg-txt0-x (+ msg-x (round (/ (- msg-w (get-str-width msg-txt0 msg-scale)) 2))))
           (msg-txt1-x (+ msg-x (round (/ (- msg-w (get-str-width msg-txt1 msg-scale)) 2))))
           (msg-num0-x  (+ msg-x (round (/ (- msg-w (get-str-width msg-num0 num-scale)) 2))))
           (line-spacing (* msg-scale 2))
           (line-h (+ (* BITMAP-HEIGHT msg-scale) line-spacing))
           (line-w (get-str-width "L'EQUIP" msg-scale))
           (outer-border-thck 1))
        (set-color BLACK)
        (princ (strcat "winner: " winner)) (terpri)
        (fill-rect (- msg-x outer-border-thck) (- msg-y outer-border-thck)
                   (+ msg-w (* outer-border-thck 2)) (+ msg-h (* outer-border-thck 2)))
        (set-color SOFT-WHITE)
        (fill-rect msg-x msg-y msg-w msg-h)
        (draw-str msg-txt0 msg-txt0-x (+ msg-y (* line-h 3) (- 10)) msg-scale)
        (draw-str msg-txt1 msg-txt1-x (+ msg-y (* line-h 2) (- 10)) msg-scale)
        (draw-str msg-num0 msg-num0-x (+ msg-y (* line-h 0) 0) num-scale)))

(defun turn-counter-panel (xi yi container-w turn)
    "Dibuixa i actualitza el comptador de torn de dimensions 'container-w' ubicat a la posició (xi,yi)"
    (let* ((tn-scale0 2)     (tn-txt0 "JUGADA ")
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
    "Dibuixa i actualitza el panell on s'informa de l'equip 'team' que està jugant al torn actual"
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

; actualitza (pinta) el color dels requadres que representen els colors dels quals està pintada una base al panell
(defun base-color-upd (colbox-x0 colbox-y0 colbox-size colbox-bord-thck &optional rgb-col)
    "Actualitza el requadre ubicat a 'colbox-(x0,y0)' amb tamany 'colbox-size' i contorn 'colbox-bord-thck' del color 'rgb-col'"
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

; dibuixa i gestiona els marcadors de colors pintats de cada una de les bases
(defun base-color-panel (x0 y0 container-w &optional updates turn)
    "Dibuixa el panell a la posició '(x0,y0)' centrat a partir de l'amplada del contenidor 'container-w'
     quan la llista 'updates' conté una acció de pintar una base"
    (if (or (= turn 0) updates)
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
            (cond 
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
                (base-color-upd colbox-t2-x colbox-t2-y colbox-size colbox-bord-thck))
            (t (when (or (and (eq action ACTION-PAINT) (cell-has-base dst-cell)) (eq action 'ELIMINA-BASE))
                    (let* ((new-color (cell-unit-color src-cell))
                           (team (cell-unit-team dst-cell))
                           (base-team (if (eq action 'ELIMINA-BASE) (opp-team team) team))
                           (colbox-x  (if (eq base-team TEAM-1) colbox-t1-x colbox-t2-x))
                           (colbox-y  (if (eq base-team TEAM-1) colbox-t1-y colbox-t2-y)))
                        (base-color-upd colbox-x colbox-y colbox-size colbox-bord-thck new-color)))
                (base-color-panel x0 y0 container-w (cdr updates) turn)
            )))
        nil))

; dibuixa i actualitza a cada torn una graella amb algunes de les estadístiques a destacar durant el joc com són
; la quantitat de pintura, el multiplicador de pintura per torn i el nombre de laboratoris capturats per equip
(defun stats-table (xi container-w state turn team)
    "Dibuixa la taula a la posició 'xi', centrada respecte el contenidor amb amplada 'container-w',
     rep informació com l'estat 'state', el torn 'turn' i l'equip del torn actual 'team'"
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
           (col1-w (+ lin-thck (get-str-width pm-txt0 header-scale) outer-sep inner-sep))
           (col2-w (+ lin-thck (get-str-width lq-txt0 header-scale) outer-sep inner-sep))
           (table-x-margin (floor (/ (- container-w (+ col0-w col1-w col2-w)) 2))) 
           (table-x (+ xi table-x-margin))
           (col0-x  (+ table-x lin-thck))
           (col1-x  (+ col0-x col0-w))
           (col2-x  (+ col1-x col1-w))
           (pq-x0 (+ col0-x outer-sep))  (pq-y0 (+ row0-y inner-sep))
           (pm-x0 (+ col1-x outer-sep))  (pm-y0 (+ row0-y inner-sep))
           (lq-x0 (+ col2-x outer-sep))  (lq-y0 (+ row0-y inner-sep))
           (lt-x0 (- pm-x0 (* stats-scale 2)))
           (lt-y0 (- row2-y outer-sep inner-sep (* BITMAP-HEIGHT header-scale)))
           (pq-t1-txt (strcat "" e1-paint))
           (pq-t2-txt (strcat "" e2-paint))
           (pm-t-txt  (strcat "x" (+ 1 (* team-labs 0.5))))
           (lq-t-txt  (strcat team-labs))
           (stats-max-cs-h  (* stats-scale BITMAP-HEIGHT))
           (stats-base-cs-w (* stats-scale BITMAP-WIDTH))
           (pq-t-max-cs (list (* (min 4 (1+ (maxlen pq-t1-txt pq-t2-txt))) stats-base-cs-w)
                               stats-max-cs-h))
           (pm-t-max-cs (list 20 stats-max-cs-h))
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

; mètode que formata i dibuixa en pantalla la llista d'actualitzacions dutes a terme a cada torn, incloent
; la destrucció d'una base. Cada línia es composa per:
; - l'identificador de l'acció
; - les cel·la on s'aplica l'acció (origen i destí si l'acció involucra dues cel·les)
; - el color (R)/(G)/(B) del qual ha estat pintada una casella/unitat si es tracta d'una acció PINTA
(defun log-update (x y container-h updates &key (src-y y) bcol)
    "Imprimeix el registre de les accions contingudes dins la llista 'updates' a partir de la posició (x,y)
     i respectant l'alçada del panell 'container-h'"
    (if updates
        (let* ((act-upd (car updates))
               (action (car act-upd))
               (pA (cadr act-upd))
               (pB (caddr act-upd))
               (src-cell (caddr pA))
               (dst-cell (caddr pB))
               (ccolor (cell-unit-color src-cell))
               (txt-scale 1)
               (del-act (eq action 'ELIMINA-BASE))
               (action-txt (strcat (cond ((eq action ACTION-MOVE) "MOURE") (del-act "BASE ELIM.") (t action))))
               (pA-txt    (if (not del-act) (strcat "(" (car pA) "," (cadr pA) ")") ""))
               (pB-txt    (if pB            (strcat "(" (car pB) "," (cadr pB) ")") ""))
               (color-txt (if ccolor        (strcat "(" ccolor ")") ""))
               (margin-x 10)
               (line-spacing 2)
               (line-h      (+ (* txt-scale BITMAP-HEIGHT) line-spacing))
               (in-bounds   (and src-y (<= (+ y (* line-h 2)) (+ src-y container-h))))
               (action-y    (+ (if in-bounds y src-y) line-h))
               (action-x    (+ x margin-x))
               (pB-x        (+ action-x 70))
               (pA-x        (if (or (eq action ACTION-CREATE-BALL) del-act) pB-X (+ action-x 35)))
               (color-x     (+ action-x 107))
               (line-w      (- (+ color-x 12) action-x))
               (line-max-cs (list line-w line-h)))
             (draw-str action-txt action-x action-y txt-scale :max-cs line-max-cs :bcol bcol)
             (draw-str pA-txt pA-x action-y txt-scale)
             (draw-str pB-txt pB-x action-y txt-scale)
             (draw-str color-txt color-x action-y txt-scale)
             (log-update x action-y container-h (cdr updates) :src-y src-y :bcol bcol))
        nil))

; mètode que dibuixa i gestiona el contenidor de les accions dutes a terme
(defun log-panel (frame-x frame-y frame-w frame-h updates turn)
    "Dibuixa el contenidor del registre a la posició 'frame-(x,y)' amb dimensions 'frame-(w,h)' quan el torn 'turn' és 0,
     i actualitza el mateix passant la llista d'actualitzacions 'updates' al mètode log-update"
    (let* ((frame-bord-thck 2)
           (shadow-thck 2)
           (inner-frame-h (- frame-h (* frame-bord-thck 2)))
           (inner-frame-w (- frame-w (* frame-bord-thck 2))))
        (cond ((= turn 0)
               (rect-outline frame-x frame-y frame-w frame-h frame-bord-thck)
               (set-color '(100 100 100))
               (fill-rect (+ frame-x shadow-thck) (- frame-y shadow-thck) frame-w shadow-thck)
               (fill-rect (+ frame-x frame-w) (- frame-y shadow-thck) shadow-thck (- frame-h shadow-thck))
               (set-color SOFT-WHITE)
               (fill-rect (+ frame-x frame-bord-thck) (+ frame-y frame-bord-thck) inner-frame-w inner-frame-h))
            (t (set-color SOFT-WHITE)
               (fill-rect (+ frame-x frame-bord-thck) (+ frame-y frame-bord-thck) inner-frame-w inner-frame-h)
               (set-color DEF-TEXT-COL)
               (log-update frame-x frame-y inner-frame-h updates :bcol SOFT-WHITE)))))

; controla l'actualització de la barra lateral mitjançant el posicionament i crida de tots els panells que el conformen
(defun status-sidebar-upd (state xi &optional updates)
    "Rep l'estat actual 'state', la posició absoluta de la barra lateral 'xi' i la llista d'accions 'updates' del torn actual"
    (let* ((turn (state-turn state))
           (team (if (oddp turn) TEAM-1 TEAM-2))
           (sidebar-w (- XLISP-WINDOW-WIDTH xi))
           (sidebar-h XLISP-WINDOW-HEIGHT)
           (log-frame-w 140)
           (log-frame-h 130))
        (turn-counter-panel xi 345 sidebar-w turn)
        (team-panel xi 295 sidebar-w team turn)
        (base-color-panel xi 225 sidebar-w (if (= turn 0) nil updates) turn)
        (stats-table xi sidebar-w state turn team)
        (log-panel (+ xi (round (/ (- sidebar-w log-frame-w) 2))) 20 log-frame-w log-frame-h updates turn)
        (when (= turn 0)
            (let* ((ver-txt "LISP Paintball v1.0.0")
                   (txt-scale 1) (txt-len (get-str-width ver-txt txt-scale)))
                (draw-str ver-txt (+ xi (round (/ (- sidebar-w txt-len) 2))) 3 txt-scale :tcol SOFT-WHITE)))))

; desencapsula el llistat d'actualizacions per a obtenir les cel·les a repintar en funció de l'acció realitzada
(defun paint-changes (xi yi cell-size updates)
    "Pinta els canvis de la llista 'updates' sobre el mapa ubicat a (xi,yi) amb cel·les de tamany 'cell-size'"
    (if updates
        (let* ((act-upd (car updates))
               (action (car act-upd))
               (pA (if (neq action ACTION-PAINT) (cadr act-upd) (caddr act-upd)))
               (pB (if (neq action ACTION-PAINT) (caddr act-upd) nil)))
            (repaint-cell xi yi (car pA) (cadr pA) (caddr pA) cell-size)
            (when pB (repaint-cell xi yi (car pB) (cadr pB) (caddr pB) cell-size))
            (paint-changes xi yi cell-size (cdr updates)))
        nil))

(defun repaint-cell (xi yi tx ty cell cell-size)
    "Repinta la cel·la 'cell' a les coordenades (tx,ty) del mapa ubicat a (xi,yi) amb tamany 'cell-size'"
    (let ((x (+ xi (* tx (- cell-size CELL-BORD-THCK))))
          (y (- yi (* ty (- cell-size CELL-BORD-THCK)))))
        (move x y)
        (draw-cell cell cell-size)))

(defun draw-map (m xi yi row cell-size)
    "Recorr de forma recursiva el mapa 'm', mou el punter a la posició d'inici de cada fila i crida a la funció de dibuixat"
    (cond ((null m) nil)
          (t (move xi (- yi (* row (- cell-size CELL-BORD-THCK))))
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
    "Dibuixa la cel·la 'cell' passada per paràmetre amb el tamany 'cell-size'"
        (set-color DEF-MARG-COL)
        (square-outline-rel cell-size) ; pinta el quadrat
        (set-color (if (cell-type-water cell) WATER-COL (if PAINT-CELL-COL (key2color (cell-color cell) :color-class LAND-COL) LAND-COL)))
        (moverel CELL-BORD-THCK CELL-BORD-THCK)
        (fill-rect-rel (- cell-size (* CELL-BORD-THCK 2)) (- cell-size (* CELL-BORD-THCK 2)))
        (cond ((cell-has-base cell) (draw-base cell cell-size))
              ((cell-has-lab  cell) (draw-lab cell cell-size) )
              ((cell-has-ball cell) (draw-ball cell cell-size))
              (t nil))
        (moverel (- CELL-BORD-THCK) (- CELL-BORD-THCK)))

(defun draw-lab (cell cell-size)
    "Dibuixa una estructura de tipus laboratori de tamany 'cell-size'"
        (let* ((lab-t1 (cell-owned-by cell TEAM-1))
               (lab-t2 (cell-owned-by cell TEAM-2)) 
               (NEUTRAL-WHITE '(220 222 221))
               (SHADOW-WHITE  '(176 176 176))
               (SHADOW-RED  (if lab-t1 '(110 110 0) (if lab-t2 '(0 116 117) '(179 34 37))))
               (NEUTRAL-RED (if lab-t1 '(123 123 0) (if lab-t2 '(0 136 136) '(222 31 33))))
               (PINKIER-RED (if lab-t1 '(135 135 0) (if lab-t2 '(0 156 156) '(204 56 59))))
               (BRIGHT-RED  (if lab-t1 '(155 155 0) (if lab-t2 '(0 176 177) '(242 61 64))))
               (max-inner-sp (- CELL-MAX-SIZE (* CELL-BORD-THCK 2)))
               (lab-size (max 4 (- cell-size (* CELL-BORD-THCK 2))))
               (g (/ lab-size max-inner-sp))
               (exc-lst '(11 15 13))
               (func (if (member cell-size exc-lst) 'ceiling 'round))
               (pixl-size              (funcall func g))
               (tile-size              (funcall func (* g 8)))
               (refl-size              (funcall func (* g 2)))
               (cent-white-sq-w        (funcall func (* g 12))) (cent-white-sq-h (funcall func (* g 13)))
               (no-refl-size           (funcall func (* g 6)))
               (no-refl-no-shadow-size (funcall func (* g 4)))
               (sc-offs1 (if (or (= cell-size 11) (= cell-size 13)) pixl-size 0)))
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
            (moverel 0 (- refl-size sc-offs1))
            (set-color NEUTRAL-RED)
            (fill-rect-rel no-refl-no-shadow-size no-refl-size)
            (moverel no-refl-size (+ (- (+ tile-size refl-size)) sc-offs1))
            (fill-rect-rel no-refl-size tile-size)
            (moverel (- no-refl-size sc-offs1) 0)
            (set-color BRIGHT-RED)
            (fill-rect-rel refl-size tile-size)
            (moverel (+ (- (+ tile-size no-refl-size)) sc-offs1) 0)
            (set-color BLACK)
            (moverel -1 -1)
            (square-outline-rel cell-size CELL-BORD-THCK)
            (moverel 1 1)))

(defun draw-ball (cell cell-size)
    "Dibuixa una unitat de tipus bolla de tamany 'cell-size'"
    (let* ((max-inner-sp (- CELL-MAX-SIZE (* CELL-BORD-THCK 2)))
           (cs (if (or (eq cell-size 12) (eq cell-size 16)) (1- cell-size) cell-size))
           (base-size (max 4 (- cs (* CELL-BORD-THCK 2))))
           (g (/ base-size max-inner-sp))
           (norm-lst '(14 17 18))
           (func (if (member cell-size norm-lst) 'round 'truncate))
           (ver1 (member cell-size norm-lst))
           (diag-size-std (funcall func (* g 7)))
           (diag-size-sm  (funcall func (* g 6)))
           (dist-diag-sep (funcall func (* g 9)))
           (spacing (funcall func (* g 2)))
           (spacing4 (funcall func (* g 4)))
           (border-type 2)
           (tile1-x (funcall func (* g 1))) (tile1-y (funcall func (* g 2)))
           (tile2-x (funcall func (* g 9))) (tile2-y (funcall func (* g 2)))
           (tiles-h (funcall func (* g 12)))
           (cent-triang-l-x (funcall func (* g 4))) (cent-triang-l-y (funcall func (* g 4)))
           (cent-triang-r-x (funcall func (* g 8))) (cent-triang-r-y (funcall func (* g 4)))
           (cent-triang-h   (funcall func (* g 8)))
           (cell-own-col (cell-unit-color cell))
           (cell-oth-col (remove cell-own-col (cell-unit-paint cell)))
           (ball-backg-col  (key2color cell-own-col)) ; COLOR PROPI
           (ball-paint-cols (key2color cell-oth-col)) ; COLOR PINTATS
           (ball-paint-col1 (if (car ball-paint-cols)  (car ball-paint-cols)  ball-backg-col))
           (ball-paint-col2 (if (cadr ball-paint-cols) (cadr ball-paint-cols) ball-backg-col))
           (team-col (if (cell-owned-by cell TEAM-1) '(118 118 0) '(136 254 255)))
           (cent-l-triang-col (if ver1 ball-backg-col  ball-paint-col1))
           (cent-r-triang-col (if ver1 ball-backg-col  ball-paint-col2))
           (lsided-triang-col (if ver1 ball-paint-col1 ball-backg-col))
           (rsided-triang-col (if ver1 ball-paint-col2 ball-backg-col)))
        (cond ((>= border-type 1)
            (set-color team-col)
            (moverel (- diag-size-std 1) 0)
            (drawrel (- diag-size-std) diag-size-std)
            (moverel 1 spacing)
            (drawrel diag-size-std diag-size-std)
            (moverel (+ diag-size-std spacing -1) (- diag-size-std))
            (drawrel (- diag-size-std) diag-size-std)
            (moverel 1 (- (+ (* diag-size-std 2) spacing)))
            (drawrel diag-size-std diag-size-std)
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
        (set-color ball-backg-col)
        (draw-triangle-iso base-size 'LPT)
        (moverel (funcall func (/ base-size 2)) 0)
        (draw-triangle-iso (1+ base-size) 'RPT)
        (moverel (- (funcall func (/ base-size 2))) 0)
        (cond (ver1 (set-color lsided-triang-col)
                    (moverel tile1-x tile1-y)
                    (draw-triangle-iso tiles-h 'LPT)
                    (moverel (- tile1-x) (- tile1-y))
                    (set-color rsided-triang-col)
                    (moverel tile2-x tile2-y)
                    (draw-triangle-iso tiles-h 'RPT)
                    (moverel (- tile2-x) (- tile2-y))))
        (set-color cent-l-triang-col)
        (moverel cent-triang-l-x cent-triang-l-y)
        (draw-triangle-iso cent-triang-h 'LPT)
        (moverel (- cent-triang-l-x) (- cent-triang-l-y))
        (set-color cent-r-triang-col)
        (moverel cent-triang-r-x cent-triang-r-y)
        (draw-triangle-iso cent-triang-h 'RPT)
        (moverel (- cent-triang-r-x) (- cent-triang-r-y))
        (set-color BLACK)
        (moverel -1 -1)
        (square-outline-rel cell-size CELL-BORD-THCK)
        (moverel 1 1))))

(defun draw-base (cell cell-size)
    "Dibuixa una unitat de tipus base de tamany 'cell-size'"
    (let* ((max-inner-sp (- CELL-MAX-SIZE (* CELL-BORD-THCK 2)))
           (base-size (max 4 (- cell-size (* CELL-BORD-THCK 2))))
           (g (/ base-size max-inner-sp)) ; tamany de la base / tamany de referència (decimal)
           (hires-icon (> base-size 8))
           (base-team1 (cell-owned-by cell TEAM-1))
           (BASE-COL1 (if base-team1 '(118 118 0)   '(0 117 118)))
           (BASE-COL2 (if base-team1 '(189 189 0)   '(0 188 189)))
           (BASE-COL3 (if base-team1 '(229 229 0)   '(0 228 229)))
           (BASE-COL4 (if base-team1 '(240 240 125) '(124 240 240)))
           (BASE-COL5 (if base-team1 '(255 255 137) '(136 254 255))))
          (if hires-icon
            (let ((corn-sm-sq (round (* cell-size 0.1875)))
                  (corn-bg-sq (round (* cell-size 0.3125)))
                  (cent-sm-sq (round (* cell-size 0.3333)))
                  (cent-bg-sq (round (* cell-size 0.6250)))
                  (dist-corn-sm-sq (round (* g 13)))
                  (dist-corn-bg-sq (round (* g 10)))
                  (dist-cent-sm-sq (round (* g 5)))
                  (dist-cent-bg-sq (round (* g 3))))
                (set-color BASE-COL3)
                (fill-rect-rel base-size base-size)
                (set-color BASE-COL5)
                (moverel dist-cent-bg-sq dist-cent-bg-sq)
                (fill-rect-rel cent-bg-sq cent-bg-sq)
                (moverel (- dist-cent-bg-sq) (- dist-cent-bg-sq))
                (set-color BASE-COL4)
                (moverel dist-cent-sm-sq dist-cent-sm-sq)
                (fill-rect-rel cent-sm-sq cent-sm-sq)
                (moverel (- dist-cent-sm-sq) (- dist-cent-sm-sq))
                (set-color BASE-COL2)
                (fill-rect-rel corn-bg-sq corn-bg-sq)
                (moverel 0 dist-corn-bg-sq)
                (fill-rect-rel corn-bg-sq corn-bg-sq)
                (moverel dist-corn-bg-sq 0)
                (fill-rect-rel corn-bg-sq corn-bg-sq)
                (moverel 0 (- dist-corn-bg-sq))
                (fill-rect-rel corn-bg-sq corn-bg-sq)
                (moverel (- dist-corn-bg-sq) 0)
                (set-color BASE-COL1)
                (fill-rect-rel corn-sm-sq corn-sm-sq)
                (moverel 0 dist-corn-sm-sq)
                (fill-rect-rel corn-sm-sq corn-sm-sq)
                (moverel dist-corn-sm-sq 0)
                (fill-rect-rel corn-sm-sq corn-sm-sq)
                (moverel 0 (- dist-corn-sm-sq))
                (fill-rect-rel corn-sm-sq corn-sm-sq)
                (moverel (- dist-corn-sm-sq) 0))
            (let ((corn-sq (round (* cell-size 0.25)))
                  (cent-sq (round (* cell-size 0.50)))
                  (dist-corn-sq (round (* g 12)))
                  (dist-cent-sq (round (* g 4))))
                (set-color BASE-COL2)
                (fill-rect-rel base-size base-size)
                (set-color BASE-COL4)
                (moverel dist-cent-sq dist-cent-sq)
                (fill-rect-rel cent-sq cent-sq)
                (moverel (- dist-cent-sq) (- dist-cent-sq))
                (set-color BASE-COL1)
                (fill-rect-rel corn-sq corn-sq)
                (moverel 0 dist-corn-sq)
                (fill-rect-rel corn-sq corn-sq)
                (moverel dist-corn-sq 0)
                (fill-rect-rel corn-sq corn-sq)
                (moverel 0 (- dist-corn-sq))
                (fill-rect-rel corn-sq corn-sq)
                (moverel (- dist-corn-sq) 0)))))

; dibuixa la marca d'aigua representativa del joc LISP Paintball (pura estètica)
(defun draw-watermark (xi yi container-w container-h scale &optional (b nil))
    "Dibuixa la marca d'aigua amb tamany 'scale' centrada al contenidor ubicat a la posició (xi,yi)
     amb dimensions 'container-(w,h)'"
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
           (y (+ yi (round (/ (- container-h total-h) 2)) (* 4 line-offsy))))
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