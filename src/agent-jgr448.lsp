;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: SNG656, JGR448.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer de l'agent intel·ligent JGR448.
;;
;; Estratègia general de l'agent:
;; - És una versió lleugera i defensiva. Cada unitat decideix només amb la seva visió
;;   actual: no escriu ni llegeix memòria compartida, no fa exploració activa i no
;;   calcula fronteres.
;; - La base crea bolles si té pintura i una casella adjacent buida. El color rota
;;   simplement amb (mod torn 3), sense comptar enemics ni composició de l'equip.
;; - Les bolles només pinten objectius útils dins rang: bases o bolles enemigues i
;;   laboratoris no propis. No repinten objectius que ja tenen el color propi de la
;;   bolla.
;; - Per moure's, les bolles prioritzen defensar: s'acosten a la bolla enemiga més
;;   propera a la nostra base si la poden veure. Si no hi ha enemics visibles, tornen
;;   cap a la base visible si són lluny i no cerquen mapa nou.

; **************************************************
; CONSTANTS
; **************************************************

; constants del joc
(defconstant AGENT-JGR448-LAND          'terra)
(defconstant AGENT-JGR448-LAB           'lab)
(defconstant AGENT-JGR448-BASE          'base)
(defconstant AGENT-JGR448-BALL          'bolla)
(defconstant AGENT-JGR448-RGB           '(r g b))
(defconstant AGENT-JGR448-BALL-COST     50)
(defconstant AGENT-JGR448-PAINT-RANGE   5)

; offsets (dx dy) dels 8 veïns adjacents (d² ≤ 2, excloent (0 0))
(defconstant AGENT-JGR448-NEIGH-OFFSETS
    '((-1 -1) ( 0 -1) ( 1 -1)
      (-1  0)         ( 1  0)
      (-1  1) ( 0  1) ( 1 1)))

; **************************************************
; UTILITATS
; **************************************************

; operacions aritmètiques bàsiques sobre llistes de N elements
(defun agent-jgr448-add (a b)
    "Suma element a element les llistes 'a' i 'b'."
    (mapcar '+ a b))

(defun agent-jgr448-sub (a b)
    "Resta element a element la llista 'b' de la llista 'a'."
    (mapcar '- a b))

(defun agent-jgr448-mul (a b)
    "Multiplica element a element les llistes 'a' i 'b'."
    (mapcar '* a b))

(defun agent-jgr448-sum (a)
    "Retorna la suma de tots els elements de la llista 'a'."
    (reduce '+ a :initial-value 0))

(defun agent-jgr448-dist (a b)
    "Calcula la distància euclidiana al quadrat entre les coordenades 'a' i 'b'."
    (agent-jgr448-sum (agent-jgr448-mul (agent-jgr448-sub a b)
                                        (agent-jgr448-sub a b))))

; **************************************************
; INFORMACIÓ DE L'AGENT
; **************************************************

; format de info:
;   (ronda equip pintura id-unitat tipus-unitat coordenada
;    colors-pintat color-propi tr-pintar tr-moure visió memòria-compartida)
(defun agent-jgr448-info-turn      (info)
    "Llegeix el torn de la informació d'agent 'info'."
    (nth 0 info))
(defun agent-jgr448-info-team      (info)
    "Llegeix l'equip de la informació d'agent 'info'."
    (nth 1 info))
(defun agent-jgr448-info-paint     (info)
    "Llegeix la reserva de pintura de la informació d'agent 'info'."
    (nth 2 info))
(defun agent-jgr448-info-unit      (info)
    "Llegeix el tipus d'unitat de la informació d'agent 'info'."
    (nth 4 info))
(defun agent-jgr448-info-coord     (info)
    "Llegeix la coordenada visible de la informació d'agent 'info'."
    (nth 5 info))
(defun agent-jgr448-info-own-color (info)
    "Llegeix el color propi de la bolla dins la informació d'agent 'info'."
    (nth 7 info))
(defun agent-jgr448-info-tr-paint  (info)
    "Llegeix el cooldown de pintar de la informació d'agent 'info'."
    (nth 8 info))
(defun agent-jgr448-info-tr-move   (info)
    "Llegeix el cooldown de moviment de la informació d'agent 'info'."
    (nth 9 info))
(defun agent-jgr448-info-vision    (info)
    "Llegeix la visió local de la informació d'agent 'info'."
    (nth 10 info))
; **************************************************
; VISIÓ
; **************************************************

; format de longitud variable segons el contingut:
;        aigua: (coord AIGUA)
;  terra buida: (coord TERRA color)
;          lab: (coord TERRA color LAB equip)
;         base: (coord TERRA color BASE equip colors-pintat)
;        bolla: (coord TERRA color BOLLA equip colors-pintat color-propi tr-pintar tr-moure)
(defun agent-jgr448-cell-coord   (cell)
    "Llegeix la coordenada de la cel·la visible 'cell'."
    (nth 0 cell))
(defun agent-jgr448-cell-type    (cell)
    "Llegeix el tipus de terreny de la cel·la visible 'cell'."
    (nth 1 cell))
(defun agent-jgr448-cell-element (cell)
    "Llegeix l'element que conté la cel·la visible 'cell'."
    (nth 3 cell))
(defun agent-jgr448-cell-team    (cell)
    "Llegeix l'equip associat a l'element de la cel·la visible 'cell'."
    (nth 4 cell))
(defun agent-jgr448-cell-painted (cell)
    "Llegeix els colors pintats de la unitat de la cel·la visible 'cell'."
    (nth 5 cell))

; predicats sobre cel·les del mapa
(defun agent-jgr448-is-land (cell)
    "Retorna cert si 'cell' és una cel·la de terra."
    (eq (agent-jgr448-cell-type cell) AGENT-JGR448-LAND))
(defun agent-jgr448-is-lab (cell)
    "Retorna cert si 'cell' conté un laboratori."
    (eq (agent-jgr448-cell-element cell) AGENT-JGR448-LAB))
(defun agent-jgr448-is-base (cell)
    "Retorna cert si 'cell' conté una base."
    (eq (agent-jgr448-cell-element cell) AGENT-JGR448-BASE))
(defun agent-jgr448-is-ball (cell)
    "Retorna cert si 'cell' conté una bolla."
    (eq (agent-jgr448-cell-element cell) AGENT-JGR448-BALL))
(defun agent-jgr448-is-empty (cell)
    "Retorna cert si 'cell' és terra i no conté cap element."
    (and (agent-jgr448-is-land cell) (null (agent-jgr448-cell-element cell))))

; **************************************************
; FILTRES DE VISIÓ
; **************************************************

(defun agent-jgr448-cell-at (cells coord)
    "Cerca dins 'cells' la cel·la amb coordenada 'coord', o retorna nil si no hi és."
    (cond ((null cells) nil)
          ((equal (agent-jgr448-cell-coord (car cells)) coord) (car cells))
          (t (agent-jgr448-cell-at (cdr cells) coord))))

(defun agent-jgr448-cells-where (cells fun)
    "Retorna les cel·les de 'cells' on el predicat 'fun' és cert."
    (remove-if (lambda (cell) (not (funcall fun cell))) cells))

(defun agent-jgr448-enemy-balls (cells team)
    "Retorna les cel·les de 'cells' que contenen bolles enemigues de l'equip 'team'."
    (agent-jgr448-cells-where cells
        (lambda (cell) (and (agent-jgr448-is-ball cell)
                            (not (eq (agent-jgr448-cell-team cell) team))))))

(defun agent-jgr448-friendly-base (cells team)
    "Retorna la primera cel·la de 'cells' que conté la base pròpia de l'equip 'team'."
    (car (agent-jgr448-cells-where cells
        (lambda (cell) (and (agent-jgr448-is-base cell)
                            (eq (agent-jgr448-cell-team cell) team))))))

; **************************************************
; ESTRATÈGIA DE LA BASE
; **************************************************

(defun agent-jgr448-find-empty-neighbour (src cells offsets)
    "Cerca el primer veí de 'src' indicat per 'offsets' que sigui terra buida dins 'cells'."
    (if (null offsets)
        nil
        (let* ((dst (agent-jgr448-add src (car offsets)))
               (cell (agent-jgr448-cell-at cells dst)))
              (if (and cell (agent-jgr448-is-empty cell))
                  dst
                  (agent-jgr448-find-empty-neighbour src cells (cdr offsets))))))

(defun agent-jgr448-base-pick-color (info)
    "Tria el color de nova bolla amb rotació simple segons el torn de 'info'."
    (nth (mod (agent-jgr448-info-turn info) (length AGENT-JGR448-RGB))
         AGENT-JGR448-RGB))

; estratègia de la base:
;   - si no hi ha pintura suficient, no fa res
;   - si hi ha un veí buit, crea una bolla amb color rotatori
(defun agent-jgr448-base (info cells)
    "Retorna accions de base: crea una bolla de color rotatori si hi ha pintura suficient i un veí buit."
    (if (< (agent-jgr448-info-paint info) AGENT-JGR448-BALL-COST)
        nil
        (let ((dst (agent-jgr448-find-empty-neighbour
                        (agent-jgr448-info-coord info)
                        cells
                        AGENT-JGR448-NEIGH-OFFSETS)))
             (if dst
                 (list (list 'CREA-BOLLA (list (agent-jgr448-base-pick-color info) dst)))
                 nil))))

; **************************************************
; ESTRATÈGIA DE LA BOLLA
; **************************************************

; puntuació defensiva d'una cel·la com a objectiu de pintura:
;   bolla enemiga: 200  base enemiga: 100  lab no nostre: 25  altre: 0
(defun agent-jgr448-score-paint-cell (cell team)
    "Retorna la puntuació defensiva de 'cell' com a objectiu de pintura per a 'team'."
    (cond ((eq (agent-jgr448-cell-team cell) team) 0)
          ((agent-jgr448-is-ball cell) 200)
          ((agent-jgr448-is-base cell) 100)
          ((agent-jgr448-is-lab cell) 25)
          (t 0)))

; tria objectius de pintura dins rang, evitant repintar amb el mateix color propi
(defun agent-jgr448-best-paint-cell-rec (cells src team own-color best)
    "Cerca recursivament el millor objectiu de pintura dins 'cells'."
    (if (null cells)
        best
        (let* ((cell (car cells))
               (score (agent-jgr448-score-paint-cell cell team))
               (dist (agent-jgr448-dist (agent-jgr448-cell-coord cell) src))
               (painted (agent-jgr448-cell-painted cell)))
              (agent-jgr448-best-paint-cell-rec
                  (cdr cells)
                  src
                  team
                  own-color
                  (cond ((> dist AGENT-JGR448-PAINT-RANGE) best)
                        ((zerop score) best)
                        ((member own-color painted) best)
                        ((null best) (list cell score dist))
                        ((> score (cadr best)) (list cell score dist))
                        ((and (= score (cadr best)) (< dist (caddr best))) (list cell score dist))
                        (t best))))))

(defun agent-jgr448-best-paint-target (info cells own-color)
    "Retorna el millor objectiu defensiu dins rang de pintar per la bolla descrita per 'info'."
    (let ((best (agent-jgr448-best-paint-cell-rec cells
                                                  (agent-jgr448-info-coord info)
                                                  (agent-jgr448-info-team info)
                                                  own-color
                                                  nil)))
         (if best (car best) nil)))

; tria l'enemic a defensar: la bolla enemiga més propera a la nostra base
(defun agent-jgr448-best-defense-target-rec (enemy-balls base-coord best)
    "Cerca la bolla enemiga més propera a 'base-coord'."
    (if (null enemy-balls)
        best
        (let* ((cell (car enemy-balls))
               (dist (agent-jgr448-dist (agent-jgr448-cell-coord cell) base-coord)))
              (agent-jgr448-best-defense-target-rec
                  (cdr enemy-balls)
                  base-coord
                  (cond ((null best) (list cell dist))
                        ((< dist (cadr best)) (list cell dist))
                        (t best))))))

(defun agent-jgr448-best-defense-target (info cells)
    "Retorna la bolla enemiga coneguda que més amenaça la base pròpia."
    (let* ((team (agent-jgr448-info-team info))
           (fbase (agent-jgr448-friendly-base cells team))
           (base-coord (if fbase (agent-jgr448-cell-coord fbase) (agent-jgr448-info-coord info)))
           (best (agent-jgr448-best-defense-target-rec
                      (agent-jgr448-enemy-balls cells team)
                      base-coord
                      nil)))
          (if best (car best) nil)))

(defun agent-jgr448-best-step-rec (src cells target offsets best)
    "Cerca recursivament el millor pas adjacent de terra buida des de 'src' cap a 'target'."
    (if (null offsets)
        best
        (let* ((dst (agent-jgr448-add src (car offsets)))
               (cell (agent-jgr448-cell-at cells dst)))
              (if (and cell (agent-jgr448-is-empty cell))
                  (let* ((dist (agent-jgr448-dist dst target))
                         (best-new (if (or (null best) (< dist (cadr best)))
                                        (list dst dist)
                                        best)))
                        (agent-jgr448-best-step-rec src cells target (cdr offsets) best-new))
                  (agent-jgr448-best-step-rec src cells target (cdr offsets) best)))))

(defun agent-jgr448-step-target (src cells target)
    "Retorna un pas cap a 'target' només si redueix realment la distància des de 'src'."
    (let ((best (agent-jgr448-best-step-rec src cells target AGENT-JGR448-NEIGH-OFFSETS nil)))
         (if (and best (< (cadr best) (agent-jgr448-dist src target)))
             (car best)
             nil)))

; moviment passiu:
;   - si hi ha una bolla enemiga coneguda, s'hi acosta per defensar
;   - si no hi ha enemics i és lluny de la base, torna a la base
;   - si ja és prop de la base, no es mou
(defun agent-jgr448-best-move-step (info cells)
    "Retorna el moviment defensiu de la bolla descrita per 'info'."
    (let* ((src (agent-jgr448-info-coord info))
           (team (agent-jgr448-info-team info))
           (enemy (agent-jgr448-best-defense-target info cells))
           (fbase (agent-jgr448-friendly-base cells team))
           (target (cond (enemy (agent-jgr448-cell-coord enemy))
                         ((and fbase (> (agent-jgr448-dist src (agent-jgr448-cell-coord fbase)) 8))
                          (agent-jgr448-cell-coord fbase))
                         (t nil))))
          (if target (agent-jgr448-step-target src cells target) nil)))

; estratègia de la bolla:
;   - pinta objectius defensius si pot
;   - es mou només per perseguir bolles enemigues conegudes o tornar cap a la base
(defun agent-jgr448-ball (info cells)
    "Retorna les accions de bolla defensiva per la unitat descrita per 'info'."
    (let* ((tr-paint (agent-jgr448-info-tr-paint info))
           (tr-move (agent-jgr448-info-tr-move info))
           (own-color (agent-jgr448-info-own-color info))
           (paint-dst (and (< tr-paint 1) (agent-jgr448-best-paint-target info cells own-color)))
           (move-step (and (< tr-move 1) (agent-jgr448-best-move-step info cells)))
           (paint-action (if paint-dst
                             (list (list 'PINTA (list (agent-jgr448-cell-coord paint-dst))))
                             nil))
           (move-action (if move-step
                            (list (list 'MOU (list move-step)))
                            nil)))
          (append paint-action move-action)))

; **************************************************
; ENTRADA DE L'AGENT
; **************************************************

; crea noves accions segons el tipus d'unitat:
;   1. llegeix només la visió actual
;   2. crida l'estratègia simple de base o bolla
(defun agent-jgr448 (info)
    "Punt d'entrada de l'agent JGR448; usa només la visió actual i retorna accions defensives per 'info'."
    (let* ((unit (agent-jgr448-info-unit info))
           (cells (agent-jgr448-info-vision info))
           (actions (cond ((eq unit AGENT-JGR448-BASE) (agent-jgr448-base info cells))
                          ((eq unit AGENT-JGR448-BALL) (agent-jgr448-ball info cells))
                          (t nil))))
          actions))
