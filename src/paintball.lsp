;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer del controlador principal.
;; <Descripció de les funcions d'aquest fitxer>

;; Necessari per a l'optimització de crides recursives.
; (load 'common) ; https://almy.us/files/xl305req.zip
; (load 'tco)    ; https://github.com/antoni-oliver/defun-tco

;; Altres fitxers de la pràctica:
; (load 'grafics)
; (load 'agent-abc123)
; (load 'agent-xyz999)

; ******************** utilities ********************

(defun list-set (l n v)
    (append (subseq l 0 n) (list v) (subseq l (1+ n))))

(defun sum (l)
    "Calcula la suma de tots els elements de la llista"
    ; TODO: add recursive sum (lists of lists)
    (reduce '+ l))

(defun filter (f l)
    "Crea una nova llista amb els elements de l que compleixen la condició definida per la funció f"
    (cond ((null l) nil)
          ((funcall f (car l)) (cons (car l) (filter f (cdr l))))
          (t (filter f (cdr l)))))

(defun dist (x1 y1 x2 y2)
    "Calcula la distància Euclidiana al quadrat entre (x1, y1) i (x2, y2)"
    (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2))))

; **************************************************
; MAPA
; **************************************************

(defun map-load (name)
    (let* ((fp (open (format nil "maps/~a.map" name) :direction :input))
           (m (read fp nil nil)))
        (close fp)
        m))

(defun map-height (m) (length m))
(defun map-width (m) (length (car m)))

; **************************************************
; CELLS
; **************************************************

#|
    Cell structure
        Aigua: (AIGUA)
        Terra: (TERRA COLOR)
          Lab: (TERRA COLOR LAB   EQUIP)
         Base: (TERRA COLOR BASE  EQUIP COLORS-PINTAT)
        Bolla: (TERRA COLOR BOLLA EQUIP COLORS-PINTAT COLOR-PROPI TR-PINTAR TR-MOURE)
|#

; cell accessor functions
(defun cell-get (m x y) (and (>= x 0) (>= y 0) (nth x (nth y m))))
(defun cell-set (m x y cell) (list-set m y (list-set (nth y m) x cell)))
(defun cell-type (cell) (car cell))
(defun cell-type-water (cell) (eq (cell-type cell) 'aigua))
(defun cell-type-land (cell) (eq (cell-type cell) 'terra))
(defun cell-color (cell) (cadr cell))
(defun cell-element (cell) (caddr cell))
(defun cell-element-team (cell) (cadddr cell))
(defun cell-element-color (cell) (nth 4 cell))
(defun cell-ball-color (cell) (nth 5 cell))
(defun cell-ball-tr-paint (cell) (nth 6 cell))
(defun cell-ball-tr-move (cell) (nth 7 cell))