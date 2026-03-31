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

(defun draw-str (str x y scale)
    "Dibuixa un string a la posició (x, y) escalat per scale"
    (draw-chars (coerce str 'list) x y scale))

(draw-str "0123456789" 10 160 3)
(draw-str "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 10 110 3)
(draw-str "abcdefghijklmnopqrstuvwxyz" 10 60 3)
(draw-str " .,'?!_*%&$#()+-/:;<>=^|~"  10 10 3)