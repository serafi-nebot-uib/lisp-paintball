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

; **************************************************
; CONSTANTS
; **************************************************

(defconstant PAINT-INIT       200)
(defconstant PAINT-INC-TURN   2)
(defconstant PAINT-INC-LAB    1)
(defconstant TEAM-1           'e1)
(defconstant TEAM-2           'e2)
(defconstant WATER            'aigua)
(defconstant LAND             'terra)
(defconstant LAB              'lab)
(defconstant BASE             'base)
(defconstant BALL             'bolla)
(defconstant UNIT-TYPES       (list BASE BALL))
(defconstant VISION-BASE      64)
(defconstant VISION-BALL      20)
(defconstant R                'r)
(defconstant G                'g)
(defconstant B                'b)

; **************************************************
; UTILITIES
; **************************************************

(defun list-set (lst n val)
    (append (subseq lst 0 n) (list val) (subseq lst (1+ n))))

(defun flatten (lst)
    (cond ((null lst) nil)
          ((atom lst) (list lst))
          (t (append (flatten (car lst)) (flatten (cdr lst))))))

(defun sum (lst) (reduce '+ (flatten lst)))

(defun filter (fun lst)
    "Crea una nova llista amb els elements de lst que compleixen la condició definida per la funció fun.
    Si f=nil llavors s'eliminen tots els elements nil de la llista lst."
    (let* ((f (if (null fun) (lambda (x) (not (null x))) fun)))
          (cond ((null lst) nil)
                ((funcall f (car lst)) (cons (car lst) (filter f (cdr lst))))
                (t (filter f (cdr lst))))))

(defun dist (x1 y1 x2 y2)
    "Calcula la distància Euclidiana al quadrat entre (x1, y1) i (x2, y2)"
    (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2))))

(defun mapfun (x fns) (mapcar (lambda (f) (funcall f x)) fns))

(defun bool->int (lst) (mapcar (lambda (x) (if x 1 0)) lst))

; **************************************************
; MAP
; **************************************************

(defun map-load (name)
    "Carrega el mapa a partir del seu nom, especificat al paràmetre name."
    (let* ((fp (open (format nil "maps/~a.map" name) :direction :input))
           (m (read fp nil nil)))
        (close fp)
        m))

(defun map-height (m) (length m))
(defun map-width (m) (length (car m)))

(defun map-count (m fun)
    "Calcula el nombre de cel·les del mapa m que compleixen amb la condició retornada per fun."
    (sum (mapcar (lambda (row) (bool->int (mapcar fun row))) m)))

(defun map-apply (m fun)
    "Crida a la funció fun per a cada una de les cel·les del mapa m."
    (mapcar (lambda (row) (mapcar fun row)) m))

; *************************************************
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
(defun cell-get           (m x y) (and (>= x 0) (>= y 0) (nth x (nth y m))))
(defun cell-set           (m x y cell) (list-set m y (list-set (nth y m) x cell)))
(defun cell-type          (cell) (nth 0 cell))
(defun cell-type-water    (cell) (eq (cell-type cell) WATER))
(defun cell-type-land     (cell) (eq (cell-type cell) LAND))
(defun cell-color         (cell) (nth 1 cell))
(defun cell-element       (cell) (nth 2 cell))
(defun cell-element-team  (cell) (nth 3 cell))
(defun cell-element-color (cell) (nth 4 cell))
(defun cell-ball-color    (cell) (nth 5 cell))
(defun cell-ball-tr-paint (cell) (nth 6 cell))
(defun cell-ball-tr-move  (cell) (nth 7 cell))
(defun cell-has-base      (cell) (eq (cell-element cell) BASE))
(defun cell-has-lab       (cell) (eq (cell-element cell) LAB))
(defun cell-has-ball      (cell) (eq (cell-element cell) BALL))

(defun cell-ball-tr-decrease (cell team)
    "Decrementa el cooldown de la cel·la cell si hi ha una bolla de l'equip team"
    (cond ((and (cell-has-ball cell) (eq (cell-element-team cell) team))
           (append (mapfun cell '(cell-type cell-color cell-element cell-element-team cell-element-color cell-ball-color))
                   (list (max 0 (- (cell-ball-tr-paint cell) 1)) (max 0 (- (cell-ball-tr-move cell) 1)))))
          (t cell)))

; TODO: is there a way to generalize this? leaving it for the moment
;       also, this will generate a new anonymous function every time,
;       perhaps it would be better to have separate and more specific functions (one for every team)
(defun cell-check-lab-team (team)
    "Retorna una funció anònima que donada una cel·la comprova si conté un lab assignat a l'equip team"
    (lambda (cell) (and (cell-has-lab cell) (eq (cell-element-team cell) team))))

; **************************************************
; UNITS
; **************************************************

(defun unit-find (m team)
    "Retorna les posicions (x y) de les unitats de l'equip team."
    (unit-find-rows m team 0))

(defun unit-find-rows (m team y)
    "Cerca unitats de l'equip team per files."
    (cond ((null m) nil)
          (t (append (unit-find-cols (car m) team 0 y)
                     (unit-find-rows (cdr m) team (1+ y))))))

(defun unit-find-cols (row team x y)
    "Cerca unitats de l'equip team per columnes dins una fila."
    (cond ((null row) nil)
          ((and (member (cell-element (car row)) UNIT-TYPES)
                (eq (cell-element-team (car row)) team))
           (cons (list x y) (unit-find-cols (cdr row) team (1+ x) y)))
          (t (unit-find-cols (cdr row) team (1+ x) y))))

(defun unit-info (state team x y cell)
    "Construeix la llista d'informacio que s'envia a l'agent per una unitat.
     Format: (torn equip pintura tipus posicio colors-pintat color-bolla
              tr-pintar tr-moure visio memoria)."
    (let* ((turn (state-turn state))
           (m (state-map state))
           (paint (state-paint-get state team))
           (dx (state-dx state))
           (dy (state-dy state))
           (elem (cell-element cell))
           (pos (list (+ x dx) (+ y dy)))
           (elem-color (cell-element-color cell))
           (ball-color (cell-ball-color cell))
           (tr-paint (cell-ball-tr-paint cell))
           (tr-move (cell-ball-tr-move cell))
           (vision-range (if (eq elem BASE) VISION-BASE VISION-BALL))
           (vision (compute-vision m x y vision-range dx dy))
           (memory nil))
        (list turn team paint elem pos elem-color ball-color tr-paint tr-move vision memory)))

(defun vision (m cx cy range)
    (let* ((off (truncate (sqrt (float range))))
           (w (map-width m))
           (h (map-height m))
           (min-x (max 0 (- cx off)))
           (min-y (max 0 (- cy off)))
           (max-x (min (- w 1) (+ cx off)))
           (max-y (min (- h 1) (+ cy off)))
    )))

; **************************************************
; GAME STATE
; **************************************************

(defun state-new (turn m pt1 pt2 dx dy) (list turn m pt1 pt2 dx dy))
(defun state-turn (s) (nth 0 s))
(defun state-map  (s) (nth 1 s))
(defun state-dx   (s) (nth 4 s))
(defun state-dy   (s) (nth 5 s))
(defun state-paint-get (s team)
    (cond ((eq team TEAM-1) (nth 2 s))
          ((eq team TEAM-2) (nth 3 s))))
(defun state-paint-set (s team p)
    (cond ((eq team TEAM-1) (list-set s 2 p))
          ((eq team TEAM-2) (list-set s 3 p))))

; **************************************************
; CONTROLLER
; **************************************************

(defun game-paint-increase (s team)
    "Incrementa la quantitat de pintura que li pertoca per al torn actual a l'equip team"
    (let* ((m (state-map s))
           (labs (map-count m (cell-check-lab-team team)))
           (inc (+ PAINT-INC-TURN (* PAINT-INC-LAB labs)))
           (curr (state-paint-get s team)))
          (state-paint-set s team (+ curr inc))))

(defun game-tr-decrease (s team)
    "Decrementa el cooldown de totes les cel·les del mapa que contenen una bolla del l'equip team"
    (list-set s 1 (map-apply (state-map s) (lambda (cell) (cell-ball-tr-decrease cell team)))))

(defun game-turn (s)
    (let* ((turn (state-turn s))
           (team (if (evenp turn) TEAM-1 TEAM-2))
           ; 1. add paint increase
           (s1 (game-paint-increase s team))
           ; 2. decrement cooldowns
           (s2 (game-tr-decrease s1 team))
           ; 3. process all units
           (s3 (units-find (state-map s2) team))
        )
        ; 4. increase turn
        (list-set s3 0 (1+ turn))))

(defun game-loop (s)
    (let* ((n1 (game-paint-increase s TEAM-1))
            (n2 (game-paint-increase s TEAM-2)))
        (print (state-paint-get n1 TEAM-1))
        (print (state-paint-get n2 TEAM-2))))

(defun paintball (map-name)
    (let* ((m (map-load map-name))
           (dx (random 1001))
           (dy (random 1001))
           (state (state-new 0 m PAINT-INIT PAINT-INIT dx dy)))
        ; graphic window setup
        ; (color 0 0 0 255 255 255)
        ; (mode 0 0 640 375)
        ; enter game loop
        (game-loop state)))

; (let* ((m '(((AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA))
;             ((AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA))
;             ((AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA))
;             ((AIGUA) (AIGUA) (AIGUA) (TERRA G) (TERRA B) (TERRA R) (TERRA G) (TERRA B) (TERRA R) (TERRA G) (TERRA G) (TERRA R) (TERRA G) (TERRA G) (TERRA R) (TERRA B) (TERRA R) (AIGUA) (AIGUA) (AIGUA))
;             ((AIGUA) (AIGUA) (AIGUA) (TERRA G) (TERRA G BASE E1) (TERRA B) (TERRA B) (TERRA B) (TERRA G) (TERRA R) (TERRA G) (TERRA B) (TERRA B) (TERRA R) (TERRA G) (TERRA G) (TERRA G) (AIGUA) (AIGUA) (AIGUA))
;             ((AIGUA) (AIGUA) (AIGUA) (TERRA G) (TERRA G) (TERRA R) (TERRA G) (TERRA B) (TERRA G) (TERRA B) (TERRA G) (TERRA R) (TERRA G) (TERRA G) (TERRA B LAB) (TERRA G) (TERRA B) (AIGUA) (AIGUA) (AIGUA))
;             ((AIGUA) (AIGUA) (AIGUA) (TERRA B) (TERRA G) (TERRA G) (TERRA B) (TERRA G) (TERRA R) (TERRA B) (TERRA R) (TERRA R) (TERRA B) (TERRA B) (TERRA B) (TERRA B) (TERRA G) (AIGUA) (AIGUA) (AIGUA))
;             ((AIGUA) (AIGUA) (AIGUA) (TERRA B) (TERRA B) (TERRA B) (TERRA G) (TERRA R) (TERRA G) (TERRA G) (TERRA R) (TERRA R) (TERRA B) (TERRA R) (TERRA R) (TERRA G) (TERRA G) (AIGUA) (AIGUA) (AIGUA))
;             ((AIGUA) (AIGUA) (AIGUA) (TERRA R) (TERRA G) (TERRA B) (TERRA G) (TERRA G) (TERRA B) (TERRA R) (TERRA B) (TERRA G LAB e2) (TERRA R) (TERRA R) (TERRA B) (TERRA G) (TERRA G) (AIGUA) (AIGUA) (AIGUA))
;             ((AIGUA) (AIGUA) (AIGUA) (TERRA B) (TERRA G) (TERRA B) (TERRA R) (TERRA G) (TERRA B) (TERRA R) (TERRA G) (TERRA G) (TERRA G) (TERRA B) (TERRA B) (TERRA G) (TERRA R) (AIGUA) (AIGUA) (AIGUA))
;             ((AIGUA) (AIGUA) (AIGUA) (TERRA B) (TERRA R) (TERRA R) (TERRA R) (TERRA B) (TERRA G) (TERRA B) (TERRA G) (TERRA R) (TERRA R) (TERRA R) (TERRA R) (TERRA R) (TERRA R) (AIGUA) (AIGUA) (AIGUA))
;             ((AIGUA) (AIGUA) (AIGUA) (TERRA B) (TERRA G) (TERRA R) (TERRA G) (TERRA G) (TERRA G LAB e1) (TERRA B) (TERRA G) (TERRA B) (TERRA B) (TERRA B) (TERRA G) (TERRA B) (TERRA R) (AIGUA) (AIGUA) (AIGUA))
;             ((AIGUA) (AIGUA) (AIGUA) (TERRA B) (TERRA G) (TERRA R) (TERRA B) (TERRA G) (TERRA R) (TERRA R) (TERRA G) (TERRA G) (TERRA B) (TERRA G) (TERRA R) (TERRA G) (TERRA G) (AIGUA) (AIGUA) (AIGUA))
;             ((AIGUA) (AIGUA) (AIGUA) (TERRA G) (TERRA B) (TERRA G) (TERRA G) (TERRA G) (TERRA G) (TERRA G) (TERRA G) (TERRA B) (TERRA G) (TERRA R) (TERRA G) (TERRA B) (TERRA R) (AIGUA) (AIGUA) (AIGUA))
;             ((AIGUA) (AIGUA) (AIGUA) (TERRA G) (TERRA B) (TERRA G LAB e1) (TERRA G) (TERRA B) (TERRA B) (TERRA G) (TERRA G) (TERRA R) (TERRA G) (TERRA R) (TERRA G) (TERRA G) (TERRA G) (AIGUA) (AIGUA) (AIGUA))
;             ((AIGUA) (AIGUA) (AIGUA) (TERRA G) (TERRA R) (TERRA B) (TERRA B) (TERRA B) (TERRA G) (TERRA G) (TERRA G) (TERRA B) (TERRA G) (TERRA R) (TERRA R) (TERRA G BASE E2) (TERRA B) (AIGUA) (AIGUA) (AIGUA))
;             ((AIGUA) (AIGUA) (AIGUA) (TERRA B) (TERRA R) (TERRA G) (TERRA G) (TERRA R) (TERRA R) (TERRA B) (TERRA B) (TERRA B) (TERRA R) (TERRA B) (TERRA G) (TERRA R) (TERRA G) (AIGUA) (AIGUA) (AIGUA))
;             ((AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA))
;             ((AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA))
;             ((AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA) (AIGUA))))
;         (dx (random 1001))
;         (dy (random 1001))
;         (state (state-new 0 m PAINT-INIT PAINT-INIT dx dy)))
;     (game-loop state))