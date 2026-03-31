;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer del mòdul gràfic.
;; <Descripció de les funcions d'aquest fitxer>

(defun fill-rect (x y w h)
    "Omple un rectangle a la posició (x, y), amplada w i alçada h."
    (cond ((<= h 0) nil)
          (t (move x y)
             (draw (+ x w) y)
             (fill-rect x (1+ y) w (1- h)))))
    
(defconstant FONT-WIDTH 6)
(defconstant FONT-HEIGHT 7)
(defconstant FONT (list
    (cons #\0 '((0 1 1 1 0) (1 0 0 0 1) (1 0 0 1 1) (1 0 1 0 1) (1 1 0 0 1) (1 0 0 0 1) (0 1 1 1 0)))
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
    (draw-bitmap-rows (reverse bm) x y scale))

(defun draw-bitmap-rows (bm x y scale)
    (cond ((null bm) nil)
          (t (draw-bitmap-row (car bm) x y scale)
             (draw-bitmap-rows (cdr bm) x (+ y scale) scale))))

(defun draw-bitmap-row (bm-row x y scale)
    (cond ((null bm-row) nil)
          ((= (car bm-row) 0) (draw-bitmap-row (cdr bm-row) (+ x scale) y scale))
          (t (fill-rect x y scale scale)
             (draw-bitmap-row (cdr bm-row) (+ x scale) y scale))))

(defun draw-str-iter (str x y scale i len)
    (cond ((>= i len) nil)
          (t (draw-bitmap (cdr (assoc (char-upcase (char str i)) FONT)) x y scale)
             (draw-str-iter str (+ x (* FONT-WIDTH scale)) y scale (1+ i) len))))

(defun draw-str (str x y scale) (draw-str-iter str x y scale 0 (length str)))

(draw-str "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" 5 120 1)
(draw-str "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" 5 100 2)
(draw-str "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" 5 70 3)