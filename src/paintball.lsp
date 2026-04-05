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
; (load "grafics.lsp")

; TODO: rename agents to author names
(load "agent-abc123.lsp")
(load "agent-xyz999.lsp")

; **************************************************
; CONSTANTS
; **************************************************

(defconstant PAINT-INIT                 200)
(defconstant PAINT-INC-TURN             2)
(defconstant PAINT-INC-LAB              1)
(defconstant TEAM-1                     'e1)
(defconstant TEAM-2                     'e2)
(defconstant WATER                      'aigua)
(defconstant LAND                       'terra)
(defconstant LAB                        'lab)
(defconstant BASE                       'base)
(defconstant BALL                       'bolla)
(defconstant UNIT-TYPES                 (list BASE BALL))
(defconstant VISION-BASE                64)
(defconstant VISION-BALL                20)
(defconstant RGB-R                      'r)
(defconstant RGB-G                      'g)
(defconstant RGB-B                      'b)
(defconstant RGB                        (list RGB-R RGB-G RGB-B))
(defconstant BALL-MOVE-TR               1)
(defconstant BALL-MOVE-TR-DIAG          1.4142)
(defconstant BALL-MOVE-TR-DIFF-COLOR    3)
(defconstant BALL-MOVE-RANGE            2)
(defconstant BALL-PAINT-TR              3)
(defconstant BALL-PAINT-RANGE           5)
(defconstant BALL-PAINT-TR-DIFF-COLOR   3)
(defconstant BASE-CREATE-COST           50)
(defconstant BASE-CREATE-RANGE          2)


; **************************************************
; UTILITIES
; **************************************************

(defun list-set (lst n val)
    (append (subseq lst 0 n) (list val) (subseq lst (1+ n))))

(defun flatten (lst)
    (cond ((null lst) nil)
          ((atom lst) (list lst))
          (t (append (flatten (car lst)) (flatten (cdr lst))))))

(defun add   (a b) (mapcar '+ a b))
(defun sub   (a b) (mapcar '- a b))
(defun div   (a b) (mapcar '/ a b))
(defun mul   (a b) (mapcar '* a b))
(defun equ   (a b) (mapcar '= a b))
(defun lt    (a b) (mapcar '< a b))
(defun gt    (a b) (mapcar '> a b))
(defun lte   (a b) (mapcar '<= a b))
(defun gte   (a b) (mapcar '>= a b))
(defun pow   (a e) (mapcar (lambda (x) (expt x e)) a))
(defun sum   (a)   (reduce '+ (flatten a)))
(defun prod  (a)   (reduce '* (flatten a)))
(defun dist  (a b) (sum (pow (sub a b) 2)))

(defun zip   (&rest l) (apply #'mapcar #'list l))

(defun range (s e) (if (> s e) nil (cons s (range (1+ s) e))))
(defun cartesian (l1 l2)
    (if (null l1)
        nil
        (append (mapcar (lambda (x) (list (car l1) x)) l2)
                (cartesian (cdr l1) l2))))

(defun mapfun (x fns) (mapcar (lambda (f) (funcall f x)) fns))

(defun bool->int (lst) (mapcar (lambda (x) (if x 1 0)) lst))

(defun unique (lst)
    (cond ((null lst) nil)
          ((member (car lst) (cdr lst)) (unique (cdr lst)))
          (t (cons (car lst) (unique (cdr lst))))))

; **************************************************
; MAP
; **************************************************

; TODO: add cell init (units need a unique id)
(defun map-load (name)
    "Carrega el mapa a partir del seu nom, especificat al paràmetre name."
    (let* ((fp (open (format nil "maps/~a.map" name) :direction :input))
           (m (read fp nil nil)))
        (close fp) (map-init-rows m 0)))

(defun map-height (m) (length m))
(defun map-width  (m) (length (car m)))
(defun map-bounds (m x y) (and (>= x 0) (>= y 0) (< x (map-width m)) (< y (map-height m))))
(defun map-cell (m x y &optional val)
    (if val (list-set m y (list-set (nth y m) x val))
            (nth x (nth y m))))

(defun map-update (m &rest upd)
    (reduce (lambda (m p) (apply #'map-cell (cons m p))) upd :initial-value m))

(defun map-count (m fun)
    "Calcula el nombre de cel·les del mapa m que compleixen amb la condició retornada per fun."
    (sum (mapcar (lambda (row) (bool->int (mapcar fun row))) m)))

(defun map-apply (m fun) (mapcar (lambda (row) (mapcar fun row)) m))

; *************************************************
; CELLS
; **************************************************

#|
    Cell structure
        Aigua: (AIGUA)
        Terra: (TERRA COLOR)
          Lab: (TERRA COLOR LAB   EQUIP)
         Base: (TERRA COLOR BASE  EQUIP ID COLORS-PINTAT)
        Bolla: (TERRA COLOR BOLLA EQUIP ID COLORS-PINTAT COLOR-PROPI TR-PINTAR TR-MOURE)
|#

(defun map-init-rows (m y)
    (if (< y (map-height m))
        (cons (map-init-row (nth y m) 0 y (map-width m)) (map-init-rows m (1+ y)))
        nil))

(defun map-init-row (row x y w)
    (if (< x w)
        (cons (cell-init (nth x row) (+ x (* y w))) (map-init-row row (1+ x) y w))
        nil))

(defun cell-init (cell next-id)
    (cond
        ((cell-type-water cell) (list WATER))
        ((cell-type-land cell)
            (cond   ((cell-has-lab cell)  (list LAND (cell-color cell) LAB (cell-unit-team cell)))
                    ((cell-has-base cell) (list LAND (cell-color cell) BASE (cell-unit-team cell) next-id '()))
                    ((cell-has-ball cell) (list LAND (cell-color cell) BALL (cell-unit-team cell) next-id '()
                                                     (cell-unit-color cell)
                                                     (cell-unit-tr-paint cell)
                                                     (cell-unit-tr-move cell)))
                    (t (list LAND (cell-color cell)))))))

; cell accessor functions
(defun cell-type          (cell &optional new) (if new (list-set cell 0 new) (nth 0 cell)))
(defun cell-color         (cell &optional new) (if new (list-set cell 1 new) (nth 1 cell)))
(defun cell-unit          (cell &optional new) (if new (list-set cell 2 new) (nth 2 cell)))
(defun cell-unit-team     (cell &optional new) (if new (list-set cell 3 new) (nth 3 cell)))
(defun cell-unit-id       (cell &optional new) (if new (list-set cell 4 new) (nth 4 cell)))
(defun cell-unit-paint    (cell &optional new) (if new (list-set cell 5 (unique new)) (nth 5 cell)))
(defun cell-unit-color    (cell &optional new) (if new (list-set cell 6 new) (nth 6 cell)))
(defun cell-unit-tr-paint (cell &optional new) (if new (list-set cell 7 new) (nth 7 cell)))
(defun cell-unit-tr-move  (cell &optional new) (if new (list-set cell 8 new) (nth 8 cell)))
(defun cell-type-water    (cell) (eq (cell-type cell) WATER))
(defun cell-type-land     (cell) (eq (cell-type cell) LAND))
(defun cell-has-base      (cell) (eq (cell-unit cell) BASE))
(defun cell-has-lab       (cell) (eq (cell-unit cell) LAB))
(defun cell-has-ball      (cell) (eq (cell-unit cell) BALL))

(defun paint-check-all (paint) (and (member RGB-R paint) (member RGB-G paint) (member RGB-B paint)))

(defun cell-owned-by (cell team) (eq (cell-unit-team cell) team))
(defun cell-empty (cell) (null (cell-unit cell)))
(defun cell-land-empty (cell) (and (cell-type-land cell) (cell-empty cell)))
(defun cell-lab-team (cell team) (and (cell-has-lab cell) (cell-owned-by cell team)))

(defun cell-ball-tr-decrease (cell)
    (list (cell-type cell)
          (cell-color cell)
          (cell-unit cell)
          (cell-unit-team cell)
          (cell-unit-id cell)
          (cell-unit-paint cell)
          (cell-unit-color cell)
          (max 0 (- (cell-unit-tr-paint cell) 1))
          (max 0 (- (cell-unit-tr-move cell) 1))))

(defun cell-apply-paint (team cell color)
    (let ((cell-new (cond
        ; cell unit is lab -> capture for the team
        ((cell-has-lab cell) (cell-unit-team cell team))
        ; cell unit is base or ball -> add color to painted list & check if it should be eliminated
        ((or (cell-has-base cell) (cell-has-ball cell))
         (let ((paint (cons color (cell-unit-paint cell))))
             (if (paint-check-all paint)
                 (subseq cell 0 2) ; full of paint -> eliminate unit
                 (cell-unit-paint cell paint)))) ; not full of paint -> update cell with new color
        (t cell))))
    (cell-color cell-new color)))

; **************************************************
; GAME STATE
; **************************************************

(defun state-new (turn m pt1 pt2 dx dy next-id) (list turn m pt1 pt2 dx dy next-id))
(defun state-turn      (s &optional new) (if new (list-set s 0 new) (nth 0 s)))
(defun state-map       (s &optional new) (if new (list-set s 1 new) (nth 1 s)))
(defun state-paint     (s team &optional new)
    (if new (if (eq team TEAM-1) (list-set s 2 new) (list-set s 3 new))
            (if (eq team TEAM-1) (nth 2 s) (nth 3 s))))
(defun state-dx        (s &optional new) (if new (list-set s 4 new) (nth 4 s)))
(defun state-dy        (s &optional new) (if new (list-set s 5 new) (nth 5 s)))
(defun state-next-id   (s &optional new) (if new (list-set s 6 new) (nth 6 s)))

; **************************************************
; UNITS
; **************************************************

(defun unit-find (m team unit-type)
    "Retorna les posicions (x y) de les unitats de l'equip team."
    (unit-find-rows m team 0 unit-type))

(defun unit-find-rows (m team y unit-type)
    "Cerca unitats de l'equip team per files."
    (cond ((null m) nil)
          (t (append (unit-find-cols (car m) team 0 y unit-type)
                     (unit-find-rows (cdr m) team (1+ y) unit-type)))))

(defun unit-find-cols (row team x y unit-type)
    "Cerca unitats de l'equip team per columnes dins una fila."
    (cond ((null row) nil)
          ((and (eq (cell-unit (car row)) unit-type)
                (cell-owned-by (car row) team))
           (cons (list x y) (unit-find-cols (cdr row) team (1+ x) y unit-type)))
          (t (unit-find-cols (cdr row) team (1+ x) y unit-type))))

(defun unit-info (state team x y)
    "Construeix la llista d'informació que s'envia a l'agent per una unitat.
     Format: (ronda equip pintura id-unitat tipus-unitat coordenada colors-pintat color-propi
        tr-pintar tr-moure visió memòria-compartida)"
    (let* ((turn (state-turn state))
           (m (state-map state))
           (cell (map-cell m x y))
           (paint (state-paint state team))
           (id (cell-unit-id cell))
           (unit (cell-unit cell))
           (coord (list (+ x (state-dx state)) (+ y (state-dy state))))
           (unit-color (cell-unit-paint cell))
           (ball-color (cell-unit-color cell))
           (tr-paint (cell-unit-tr-paint cell))
           (tr-move (cell-unit-tr-move cell))
           (vision-range (if (eq unit BASE) VISION-BASE VISION-BALL))
           (vis-coords (unit-vision m x y vision-range))
           (vis (mapcar (lambda (xy) (unit-vision-format m (car xy) (cadr xy))) vis-coords))
           (mem nil))
        (list turn team paint id unit coord unit-color ball-color tr-paint tr-move vis mem)))

(defun unit-agent (team info)
    (if (eq team TEAM-1)
        (agent-abc123 info)
        (agent-xyz999 info)))

(defun unit-action (state team))

(defun unit-ball-move (state team src dst)
    (let* ((m (state-map state))
           (ux (car src)) (uy (cadr src)) (tx (car dst)) (ty (cadr dst))
           (src-cell (map-cell m ux uy))
           (dst-cell (map-cell m tx ty))
           (d (dist src dst)))
      (or (when (and (cell-has-ball src-cell)
                     (cell-owned-by src-cell team)
                     (< (cell-unit-tr-move src-cell) BALL-MOVE-TR)
                     (<= d BALL-MOVE-RANGE)
                     (map-bounds m tx ty)
                     (cell-type-land dst-cell)
                     (cell-empty dst-cell)
                     (> d 0))
            (let* ((diag-penalty (if (= d 2) BALL-MOVE-TR-DIAG 1))
                   (ball-color (cell-unit-color src-cell))
                   (color-penalty (if (eq ball-color (cell-color dst-cell)) 1 BALL-MOVE-TR-DIFF-COLOR))
                   (tr-move-new (+ (cell-unit-tr-move src-cell)
                                   (* BALL-MOVE-TR diag-penalty color-penalty)))
                   (src-new (list LAND (cell-color src-cell)))
                   (dst-new (list LAND (cell-color dst-cell) BALL team
                                  (cell-unit-id src-cell)
                                  (cell-unit-paint src-cell)
                                  ball-color
                                  (cell-unit-tr-paint src-cell)
                                  tr-move-new)))
               (state-map state (map-update m
                   (list ux uy src-new)
                   (list tx ty dst-new))))
          state))))

(defun unit-ball-paint (state team src dst)
    (let* ((m (state-map state))
           (ux (car src)) (uy (cadr src)) (tx (car dst)) (ty (cadr dst))
           (src-cell (map-cell m ux uy))
           (dst-cell (map-cell m tx ty))
           (d (dist src dst)))
      (or (when (and (cell-has-ball src-cell)
                     (cell-owned-by src-cell team)
                     (< (cell-unit-tr-paint src-cell) BALL-PAINT-TR)
                     (<= d BALL-PAINT-RANGE)
                     (map-bounds m tx ty)
                     (cell-type-land dst-cell))
            (let* ((src-color (cell-color src-cell))
                   (ball-color (cell-unit-color src-cell))
                   (tr-paint-new (+ (cell-unit-tr-paint src-cell)
                                    (if (eq src-color ball-color)
                                      BALL-PAINT-TR
                                      (* BALL-PAINT-TR BALL-PAINT-TR-DIFF-COLOR))))
                   (src-cell-new (cell-unit-tr-paint src-cell tr-paint-new))
                   (dst-cell-new (cell-apply-paint team dst-cell ball-color)))
               (state-map state (map-update m
                   (list ux uy src-cell-new)
                   (list tx ty dst-cell-new))))
          state))))

(defun unit-base-create-ball (state team src dst color)
    (let* ((m (state-map state))
           (ux (car src)) (uy (cadr src)) (tx (car dst)) (ty (cadr dst))
           (src-cell (map-cell m ux uy))
           (dst-cell (map-cell m tx ty))
           (paint (state-paint state team))
           (d (dist src dst)))
      (or (when (and (cell-has-base src-cell)
                     (cell-owned-by src-cell team)
                     (>= paint BASE-CREATE-COST)
                     (<= d BASE-CREATE-RANGE)
                     (map-bounds m tx ty)
                     (cell-type-land dst-cell)
                     (cell-empty dst-cell)
                     (> d 0))
            (let* ((next-id (state-next-id state))
                   (dst-new (list LAND (cell-color dst-cell) BALL team next-id (list color) color 0 0))
                   (s1 (state-map state (map-cell m tx ty dst-new)))
                   (s2 (state-paint s1 team (- paint BASE-CREATE-COST)))
                   (s3 (state-next-id s2 (1+ next-id))))
              s3))
          state)))

; d^2 = (x1 - x2)^2 + (y1 - y2)^2 
; 
; for a 16u^2 range, only the cells (x2 y2) will be visible from (x1 x2) if
;    (x1 - x2)^2 + (y1 - y2)^2 <= 16
; therefore, it is only needed to check cells that are within the 4x4 (4 = sqrt(16)) square surrounding
; the (x1, y1) point, as the cells farther away will always exceed the limit
; 
;    .....................
;    ......#########......
;    ......#########......
;    ......#########......
;    ......#########......
;    ......####X####......
;    ......#########......
;    ......#########......
;    ......#########......
;    ......#########......
;    .....................

(defun unit-vision (m cx cy r)
    (let* ((off (truncate (sqrt (float r))))
           (w (map-width m))
           (h (map-height m))
           (src (list cx cy))
           ; calculate the bounds of the inner map & clip them to the outside of the total map
           (min-x (max 0 (- cx off)))
           (min-y (max 0 (- cy off)))
           (max-x (min (- w 1) (+ cx off)))
           (max-y (min (- h 1) (+ cy off)))
           ; generate all of the coordinates for the inner map (entire square)
           (coord-range (cartesian (range min-x max-x) (range min-y max-y))))
        ; filter all of the inner map coordinates that are visible with range r
        (remove-if (lambda (dst) (> (dist src dst) r)) coord-range)))

(defun unit-vision-format (m x y)
    (let ((cell (map-cell m x y)))
         (list (list x y)
          (cell-type cell)
          (cell-color cell)
          (cell-unit cell)
          (cell-unit-team cell)
          (cell-unit-paint cell)
          (cell-unit-color cell)
          (cell-unit-tr-paint cell)
          (cell-unit-tr-move cell))))

; **************************************************
; CONTROLLER
; **************************************************

(defun game-paint-increase (s team)
    "Incrementa la quantitat de pintura que li pertoca per al torn actual a l'equip team"
    (let* ((m (state-map s))
           (labs (map-count m (lambda (cell) (cell-lab-team cell team))))
           (inc (+ PAINT-INC-TURN (* PAINT-INC-LAB labs)))
           (curr (state-paint s team)))
          (state-paint s team (+ curr inc))))

(defun game-tr-decrease (s team)
    "Decrementa el cooldown de totes les cel·les del mapa que contenen una bolla del l'equip team"
    (state-map s (map-apply (state-map s)
                            (lambda (cell) (if (and (cell-has-ball cell) (cell-owned-by cell team))
                                               (cell-ball-tr-decrease cell)
                                               cell)))))

(defun game-turn (s)
    (let* ((turn (state-turn s))
           (team (if (evenp turn) TEAM-1 TEAM-2))
           ; 1. add paint increase
           (s1 (game-paint-increase s team))
           ; 2. decrement cooldowns
           (s2 (game-tr-decrease s1 team))
           ; 3. find all bases & call agents
           (bases (unit-find (state-map s2) team BASE))
           (info-list (mapcar (lambda (xy) (unit-info s2 team (car xy) (cadr xy))) bases))
           (actions (mapcar (lambda (info) (unit-agent team info)) info-list))
        )
        ; 4. increase turn
        (list-set s2 0 (1+ turn))))

(defun game-loop (s))

(defun paintball (map-name)
    (let* ((m (map-load map-name))
           (dx (random 1001))
           (dy (random 1001))
           (state (state-new 0 m PAINT-INIT PAINT-INIT dx dy (* (map-width m) (map-height m)))))
        ; graphic window setup
        ; (color 0 0 0 255 255 255)
        ; (mode 0 0 640 375)
        ; enter game loop
        (princ m)
        (game-loop state)))

(paintball "tiny")