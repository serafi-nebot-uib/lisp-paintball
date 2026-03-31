;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer del mòdul gràfic.
;; <Descripció de les funcions d'aquest fitxer>

; **************************************************
; PRIMITIVES
; **************************************************

(defun fill-rect (x y w h)
    "Omple un rectangle a la posició (x, y), amplada w i alçada h."
    (cond ((<= h 0) nil)
          (t (move x y)
             (draw (+ x w) y)
             (fill-rect x (1+ y) w (1- h)))))

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

(defconstant FONT-WIDTH 6)
(defconstant FONT-HEIGHT 7)
(defconstant FONT (list
    (cons #\0 '((0 1 1 1 0)(1 0 0 0 1)(1 0 0 1 1)(1 0 1 0 1)(1 1 0 0 1)(1 0 0 0 1)(0 1 1 1 0)))
    (cons #\1 '((0 0 1 0 0)(0 1 1 0 0)(0 0 1 0 0)(0 0 1 0 0)(0 0 1 0 0)(0 0 1 0 0)(0 1 1 1 0)))
    (cons #\2 '((0 1 1 1 0)(1 0 0 0 1)(0 0 0 0 1)(0 0 1 1 0)(0 1 0 0 0)(1 0 0 0 0)(1 1 1 1 1)))
    (cons #\3 '((1 1 1 1 0)(0 0 0 0 1)(0 0 0 0 1)(0 1 1 1 0)(0 0 0 0 1)(0 0 0 0 1)(1 1 1 1 0)))
    (cons #\4 '((0 0 0 1 0)(0 0 1 1 0)(0 1 0 1 0)(1 0 0 1 0)(1 1 1 1 1)(0 0 0 1 0)(0 0 0 1 0)))
    (cons #\5 '((1 1 1 1 1)(1 0 0 0 0)(1 0 0 0 0)(1 1 1 1 0)(0 0 0 0 1)(0 0 0 0 1)(1 1 1 1 0)))
    (cons #\6 '((0 0 1 1 0)(0 1 0 0 0)(1 0 0 0 0)(1 1 1 1 0)(1 0 0 0 1)(1 0 0 0 1)(0 1 1 1 0)))
    (cons #\7 '((1 1 1 1 1)(0 0 0 0 1)(0 0 0 1 0)(0 0 1 0 0)(0 1 0 0 0)(0 1 0 0 0)(0 1 0 0 0)))
    (cons #\8 '((0 1 1 1 0)(1 0 0 0 1)(1 0 0 0 1)(0 1 1 1 0)(1 0 0 0 1)(1 0 0 0 1)(0 1 1 1 0)))
    (cons #\9 '((0 1 1 1 0)(1 0 0 0 1)(1 0 0 0 1)(0 1 1 1 1)(0 0 0 0 1)(0 0 0 1 0)(0 1 1 0 0)))
    (cons #\A '((0 0 1 0 0)(0 1 0 1 0)(1 0 0 0 1)(1 0 0 0 1)(1 1 1 1 1)(1 0 0 0 1)(1 0 0 0 1)))
    (cons #\B '((1 1 1 1 0)(1 0 0 0 1)(1 0 0 0 1)(1 1 1 1 0)(1 0 0 0 1)(1 0 0 0 1)(1 1 1 1 0)))
    (cons #\C '((0 1 1 1 0)(1 0 0 0 1)(1 0 0 0 0)(1 0 0 0 0)(1 0 0 0 0)(1 0 0 0 1)(0 1 1 1 0)))
    (cons #\D '((1 1 1 1 0)(1 0 0 0 1)(1 0 0 0 1)(1 0 0 0 1)(1 0 0 0 1)(1 0 0 0 1)(1 1 1 1 0)))
    (cons #\E '((1 1 1 1 1)(1 0 0 0 0)(1 0 0 0 0)(1 1 1 1 0)(1 0 0 0 0)(1 0 0 0 0)(1 1 1 1 1)))
    (cons #\F '((1 1 1 1 1)(1 0 0 0 0)(1 0 0 0 0)(1 1 1 1 0)(1 0 0 0 0)(1 0 0 0 0)(1 0 0 0 0)))
    (cons #\G '((0 1 1 1 0)(1 0 0 0 1)(1 0 0 0 0)(1 0 1 1 1)(1 0 0 0 1)(1 0 0 0 1)(0 1 1 1 1)))
    (cons #\H '((1 0 0 0 1)(1 0 0 0 1)(1 0 0 0 1)(1 1 1 1 1)(1 0 0 0 1)(1 0 0 0 1)(1 0 0 0 1)))
    (cons #\I '((0 1 1 1 0)(0 0 1 0 0)(0 0 1 0 0)(0 0 1 0 0)(0 0 1 0 0)(0 0 1 0 0)(0 1 1 1 0)))
    (cons #\J '((0 0 1 1 1)(0 0 0 1 0)(0 0 0 1 0)(0 0 0 1 0)(0 0 0 1 0)(1 0 0 1 0)(0 1 1 0 0)))
    (cons #\K '((1 0 0 0 1)(1 0 0 1 0)(1 0 1 0 0)(1 1 0 0 0)(1 0 1 0 0)(1 0 0 1 0)(1 0 0 0 1)))
    (cons #\L '((1 0 0 0 0)(1 0 0 0 0)(1 0 0 0 0)(1 0 0 0 0)(1 0 0 0 0)(1 0 0 0 0)(1 1 1 1 1)))
    (cons #\M '((1 0 0 0 1)(1 1 0 1 1)(1 0 1 0 1)(1 0 1 0 1)(1 0 0 0 1)(1 0 0 0 1)(1 0 0 0 1)))
    (cons #\N '((1 0 0 0 1)(1 1 0 0 1)(1 0 1 0 1)(1 0 0 1 1)(1 0 0 0 1)(1 0 0 0 1)(1 0 0 0 1)))
    (cons #\O '((0 1 1 1 0)(1 0 0 0 1)(1 0 0 0 1)(1 0 0 0 1)(1 0 0 0 1)(1 0 0 0 1)(0 1 1 1 0)))
    (cons #\P '((1 1 1 1 0)(1 0 0 0 1)(1 0 0 0 1)(1 1 1 1 0)(1 0 0 0 0)(1 0 0 0 0)(1 0 0 0 0)))
    (cons #\Q '((0 1 1 1 0)(1 0 0 0 1)(1 0 0 0 1)(1 0 0 0 1)(1 0 1 0 1)(1 0 0 1 0)(0 1 1 0 1)))
    (cons #\R '((1 1 1 1 0)(1 0 0 0 1)(1 0 0 0 1)(1 1 1 1 0)(1 0 1 0 0)(1 0 0 1 0)(1 0 0 0 1)))
    (cons #\S '((0 1 1 1 1)(1 0 0 0 0)(1 0 0 0 0)(0 1 1 1 0)(0 0 0 0 1)(0 0 0 0 1)(1 1 1 1 0)))
    (cons #\T '((1 1 1 1 1)(0 0 1 0 0)(0 0 1 0 0)(0 0 1 0 0)(0 0 1 0 0)(0 0 1 0 0)(0 0 1 0 0)))
    (cons #\U '((1 0 0 0 1)(1 0 0 0 1)(1 0 0 0 1)(1 0 0 0 1)(1 0 0 0 1)(1 0 0 0 1)(0 1 1 1 0)))
    (cons #\V '((1 0 0 0 1)(1 0 0 0 1)(1 0 0 0 1)(1 0 0 0 1)(0 1 0 1 0)(0 1 0 1 0)(0 0 1 0 0)))
    (cons #\W '((1 0 0 0 1)(1 0 0 0 1)(1 0 0 0 1)(1 0 1 0 1)(1 0 1 0 1)(1 1 0 1 1)(1 0 0 0 1)))
    (cons #\X '((1 0 0 0 1)(1 0 0 0 1)(0 1 0 1 0)(0 0 1 0 0)(0 1 0 1 0)(1 0 0 0 1)(1 0 0 0 1)))
    (cons #\Y '((1 0 0 0 1)(1 0 0 0 1)(0 1 0 1 0)(0 0 1 0 0)(0 0 1 0 0)(0 0 1 0 0)(0 0 1 0 0)))
    (cons #\Z '((1 1 1 1 1)(0 0 0 0 1)(0 0 0 1 0)(0 0 1 0 0)(0 1 0 0 0)(1 0 0 0 0)(1 1 1 1 1)))
    (cons #\space '((0 0 0 0 0)(0 0 0 0 0)(0 0 0 0 0)(0 0 0 0 0)(0 0 0 0 0)(0 0 0 0 0)(0 0 0 0 0)))))

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
          (t (draw-bitmap (cdr (assoc (car chars) FONT)) x y scale)
             (draw-chars (cdr chars) (+ x (* FONT-WIDTH scale)) y scale))))

(defun draw-str (str x y scale)
    "Dibuixa un string a la posició (x, y) escalat per scale"
    (draw-chars (coerce str 'list) x y scale))

(draw-str "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" 5 120 1)
(draw-str "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" 5 100 2)
(draw-str "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" 5 70 3)