;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: SNG656, JGR448.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer de l'agent intel·ligent JGR448.
;;
;; Estratègia general de l'agent (versió mínima):
;; - Cada unitat decideix només amb la seva visió actual: sense memòria compartida,
;;   sense exploració activa amb estat ni càlcul de fronteres.
;; - La base, si té pintura i un veí buit, crea una bolla amb color rotatori (mod torn 3).
;; - Les bolles pinten la primera cel·la útil dins rang (bolla/base enemiga o lab no propi
;;   que encara no té el seu color), i sempre exploren allunyant-se de la base pròpia.

; **************************************************
; CONSTANTS
; **************************************************

(defconstant AGENT-JGR448-LAND          'terra)
(defconstant AGENT-JGR448-LAB           'lab)
(defconstant AGENT-JGR448-BASE          'base)
(defconstant AGENT-JGR448-BALL          'bolla)
(defconstant AGENT-JGR448-RGB           '(r g b))
(defconstant AGENT-JGR448-BALL-COST     50)
(defconstant AGENT-JGR448-PAINT-RANGE   5)

; offsets (dx dy) dels 8 veïns adjacents
(defconstant AGENT-JGR448-NEIGH-OFFSETS
    '((-1 -1) ( 0 -1) ( 1 -1)
      (-1  0)         ( 1  0)
      (-1  1) ( 0  1) ( 1 1)))

; **************************************************
; UTILITATS
; **************************************************

(defun agent-jgr448-add (a b)
    "Suma element a element les llistes 'a' i 'b'."
    (mapcar '+ a b))

(defun agent-jgr448-dist (a b)
    "Distància euclidiana al quadrat entre les coordenades 2D 'a' i 'b'."
    (let ((dx (- (car a) (car b)))
          (dy (- (cadr a) (cadr b))))
         (+ (* dx dx) (* dy dy))))

; **************************************************
; INFORMACIÓ DE L'AGENT
; **************************************************

; format de info:
;   (ronda equip pintura id-unitat tipus-unitat coordenada
;    colors-pintat color-propi tr-pintar tr-moure visió memòria-compartida)
(defun agent-jgr448-info-turn      (info) (nth 0  info))
(defun agent-jgr448-info-team      (info) (nth 1  info))
(defun agent-jgr448-info-paint     (info) (nth 2  info))
(defun agent-jgr448-info-unit      (info) (nth 4  info))
(defun agent-jgr448-info-coord     (info) (nth 5  info))
(defun agent-jgr448-info-own-color (info) (nth 7  info))
(defun agent-jgr448-info-tr-paint  (info) (nth 8  info))
(defun agent-jgr448-info-tr-move   (info) (nth 9  info))
(defun agent-jgr448-info-vision    (info) (nth 10 info))

; **************************************************
; VISIÓ
; **************************************************

; format de longitud variable segons el contingut:
;        aigua: (coord AIGUA)
;  terra buida: (coord TERRA color)
;          lab: (coord TERRA color LAB equip)
;         base: (coord TERRA color BASE equip colors-pintat)
;        bolla: (coord TERRA color BOLLA equip colors-pintat color-propi tr-pintar tr-moure)
(defun agent-jgr448-cell-coord   (cell) (nth 0 cell))
(defun agent-jgr448-cell-type    (cell) (nth 1 cell))
(defun agent-jgr448-cell-element (cell) (nth 3 cell))
(defun agent-jgr448-cell-team    (cell) (nth 4 cell))
(defun agent-jgr448-cell-painted (cell) (nth 5 cell))

(defun agent-jgr448-is-land  (cell) (eq (agent-jgr448-cell-type    cell) AGENT-JGR448-LAND))
(defun agent-jgr448-is-lab   (cell) (eq (agent-jgr448-cell-element cell) AGENT-JGR448-LAB))
(defun agent-jgr448-is-base  (cell) (eq (agent-jgr448-cell-element cell) AGENT-JGR448-BASE))
(defun agent-jgr448-is-ball  (cell) (eq (agent-jgr448-cell-element cell) AGENT-JGR448-BALL))
(defun agent-jgr448-is-empty (cell)
    (and (agent-jgr448-is-land cell) (null (agent-jgr448-cell-element cell))))

(defun agent-jgr448-cell-at (cells coord)
    "Cerca dins 'cells' la cel·la amb coordenada 'coord', o nil si no hi és."
    (cond ((null cells) nil)
          ((equal (agent-jgr448-cell-coord (car cells)) coord) (car cells))
          (t (agent-jgr448-cell-at (cdr cells) coord))))

(defun agent-jgr448-friendly-base (cells team)
    "Retorna la cel·la de la base pròpia dins 'cells', o nil si no és visible."
    (cond ((null cells) nil)
          ((and (agent-jgr448-is-base (car cells))
                (eq (agent-jgr448-cell-team (car cells)) team))
           (car cells))
          (t (agent-jgr448-friendly-base (cdr cells) team))))

; **************************************************
; ESTRATÈGIA DE LA BASE
; **************************************************

(defun agent-jgr448-find-empty-neighbour (src cells offsets)
    "Primer veí de 'src' (segons 'offsets') que sigui terra buida dins 'cells'."
    (if (null offsets)
        nil
        (let* ((dst (agent-jgr448-add src (car offsets)))
               (cell (agent-jgr448-cell-at cells dst)))
              (if (and cell (agent-jgr448-is-empty cell))
                  dst
                  (agent-jgr448-find-empty-neighbour src cells (cdr offsets))))))

(defun agent-jgr448-base-pick-color (info)
    "Color de nova bolla amb rotació simple segons el torn."
    (nth (mod (agent-jgr448-info-turn info) (length AGENT-JGR448-RGB)) AGENT-JGR448-RGB))

(defun agent-jgr448-base (info cells)
    "Si hi ha pintura suficient i un veí buit, crea una bolla amb color rotatori."
    (if (< (agent-jgr448-info-paint info) AGENT-JGR448-BALL-COST)
        nil
        (let ((dst (agent-jgr448-find-empty-neighbour
                        (agent-jgr448-info-coord info) cells AGENT-JGR448-NEIGH-OFFSETS)))
             (if dst
                 (list (list 'CREA-BOLLA (list (agent-jgr448-base-pick-color info) dst)))
                 nil))))

; **************************************************
; ESTRATÈGIA DE LA BOLLA
; **************************************************

(defun agent-jgr448-paintable (cell src team own-color)
    "Cert si 'cell' és un objectiu vàlid de pintura per la bolla."
    (and (not (eq (agent-jgr448-cell-team cell) team))
         (or (agent-jgr448-is-ball cell)
             (agent-jgr448-is-base cell)
             (agent-jgr448-is-lab  cell))
         (<= (agent-jgr448-dist (agent-jgr448-cell-coord cell) src) AGENT-JGR448-PAINT-RANGE)
         (not (member own-color (agent-jgr448-cell-painted cell)))))

(defun agent-jgr448-find-paint-target (cells src team own-color)
    "Primer objectiu de pintura útil dins 'cells' (recorre la visió en ordre)."
    (cond ((null cells) nil)
          ((agent-jgr448-paintable (car cells) src team own-color) (car cells))
          (t (agent-jgr448-find-paint-target (cdr cells) src team own-color))))

(defun agent-jgr448-far-step-rec (src cells away offsets best best-d)
    "Recorre 'offsets' i guarda el veí buit més llunyà de 'away'."
    (if (null offsets)
        best
        (let* ((dst (agent-jgr448-add src (car offsets)))
               (cell (agent-jgr448-cell-at cells dst)))
              (if (and cell (agent-jgr448-is-empty cell))
                  (let ((d (agent-jgr448-dist dst away)))
                       (if (or (null best) (> d best-d))
                           (agent-jgr448-far-step-rec src cells away (cdr offsets) dst d)
                           (agent-jgr448-far-step-rec src cells away (cdr offsets) best best-d)))
                  (agent-jgr448-far-step-rec src cells away (cdr offsets) best best-d)))))

(defun agent-jgr448-explore-step (src cells team)
    "Pas d'exploració: el veí buit més llunyà de la base pròpia (o qualsevol si no la veu)."
    (let* ((fbase (agent-jgr448-friendly-base cells team))
           (away  (if fbase (agent-jgr448-cell-coord fbase) src)))
          (agent-jgr448-far-step-rec src cells away AGENT-JGR448-NEIGH-OFFSETS nil 0)))

(defun agent-jgr448-ball (info cells)
    "Pinta el primer objectiu útil dins rang i sempre fa un pas d'exploració."
    (let* ((src       (agent-jgr448-info-coord     info))
           (team      (agent-jgr448-info-team      info))
           (own-color (agent-jgr448-info-own-color info))
           (paint-dst (and (< (agent-jgr448-info-tr-paint info) 1)
                           (agent-jgr448-find-paint-target cells src team own-color)))
           (move-dst  (and (< (agent-jgr448-info-tr-move  info) 1)
                           (agent-jgr448-explore-step src cells team))))
          (append (if paint-dst (list (list 'PINTA (list (agent-jgr448-cell-coord paint-dst)))) nil)
                  (if move-dst  (list (list 'MOU   (list move-dst)))                            nil))))

; **************************************************
; ENTRADA DE L'AGENT
; **************************************************

(defun agent-jgr448 (info)
    "Punt d'entrada de l'agent JGR448; despatxa segons el tipus d'unitat."
    (let ((unit  (agent-jgr448-info-unit   info))
          (cells (agent-jgr448-info-vision info)))
         (cond ((eq unit AGENT-JGR448-BASE) (agent-jgr448-base info cells))
               ((eq unit AGENT-JGR448-BALL) (agent-jgr448-ball info cells))
               (t nil))))
