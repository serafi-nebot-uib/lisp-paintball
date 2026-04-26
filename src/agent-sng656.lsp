;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer de l'agent intel·ligent SNG656.
;; <Descripció de les funcions d'aquest fitxer>

; **************************************************
; CONSTANTS
; **************************************************

; constants del joc
(defconstant AGENT-SNG656-WATER  'aigua)
(defconstant AGENT-SNG656-LAND   'terra)
(defconstant AGENT-SNG656-LAB    'lab)
(defconstant AGENT-SNG656-BASE   'base)
(defconstant AGENT-SNG656-BALL   'bolla)
(defconstant AGENT-SNG656-RGB    '(r g b))

; **************************************************
; INFORMACIÓ DE L'AGENT
; **************************************************

; format de info:
;   (ronda equip pintura id-unitat tipus-unitat coordenada
;    colors-pintat color-propi tr-pintar tr-moure visió memòria-compartida)
(defun agent-sng656-info-turn      (info) (nth 0  info))
(defun agent-sng656-info-team      (info) (nth 1  info))
(defun agent-sng656-info-paint     (info) (nth 2  info))
(defun agent-sng656-info-id        (info) (nth 3  info))
(defun agent-sng656-info-unit      (info) (nth 4  info))
(defun agent-sng656-info-coord     (info) (nth 5  info))
(defun agent-sng656-info-painted   (info) (nth 6  info))
(defun agent-sng656-info-src-color (info) (nth 7  info))
(defun agent-sng656-info-tr-paint  (info) (nth 8  info))
(defun agent-sng656-info-tr-move   (info) (nth 9  info))
(defun agent-sng656-info-vision    (info) (nth 10 info))
(defun agent-sng656-info-memory    (info) (nth 11 info))

; **************************************************
; VISIÓ
; **************************************************

; format de longitud variable segons el contingut:
;        aigua: (coord AIGUA)
;  terra buida: (coord TERRA color)
;          lab: (coord TERRA color LAB equip)
;         base: (coord TERRA color BASE equip colors-pintat)
;        bolla: (coord TERRA color BOLLA equip colors-pintat color-propi tr-pintar tr-moure)
(defun agent-sng656-cell-coord    (cell) (nth 0 cell))
(defun agent-sng656-cell-type     (cell) (nth 1 cell))
(defun agent-sng656-cell-color    (cell) (nth 2 cell))
(defun agent-sng656-cell-element  (cell) (nth 3 cell))
(defun agent-sng656-cell-team     (cell) (nth 4 cell))
(defun agent-sng656-cell-painted  (cell) (nth 5 cell))
(defun agent-sng656-cell-src-col  (cell) (nth 6 cell))
(defun agent-sng656-cell-tr-paint (cell) (nth 7 cell))
(defun agent-sng656-cell-tr-move  (cell) (nth 8 cell))

; predicats sobre cel·les del mapa
(defun agent-sng656-is-water (cell) (eq (agent-sng656-cell-type cell) AGENT-SNG656-WATER))
(defun agent-sng656-is-land  (cell) (eq (agent-sng656-cell-type cell) AGENT-SNG656-LAND))
(defun agent-sng656-is-lab   (cell) (eq (agent-sng656-cell-element cell) AGENT-SNG656-LAB))
(defun agent-sng656-is-base  (cell) (eq (agent-sng656-cell-element cell) AGENT-SNG656-BASE))
(defun agent-sng656-is-ball  (cell) (eq (agent-sng656-cell-element cell) AGENT-SNG656-BALL))
(defun agent-sng656-is-empty (cell) (and (agent-sng656-is-land cell) (null (agent-sng656-cell-element cell))))

; **************************************************
; UTILITATS
; **************************************************

; operacions aritmètiques i lògiques bàsiques sobre llistes de N elements 
(defun agent-sng656-add   (a b) (mapcar '+ a b))
(defun agent-sng656-sub   (a b) (mapcar '- a b))
(defun agent-sng656-pow   (a e) (mapcar (lambda (x) (expt x e)) a))
(defun agent-sng656-sum   (a)   (reduce '+ a :initial-value 0))

; distància euclidiana al quadrat entre dos coords (x y) — vàlida en espai desplaçat
(defun agent-sng656-dist (a b) (agent-sng656-sum (agent-sng656-pow (agent-sng656-sub a b) 2)))

; genera la llista d'enters en el rang [s e]
(defun agent-sng656-range (s e)
    (if (> s e) nil (cons s (agent-sng656-range (1+ s) e))))

; producte cartesià entre dues llistes (cada parell com a (a b))
(defun agent-sng656-cartesian (l1 l2)
    (if (null l1)
        nil
        (append (mapcar (lambda (x) (list (car l1) x)) l2)
                (agent-sng656-cartesian (cdr l1) l2))))

; tots els offsets (dx dy) amb d² ≤ r, excloent (0 0)
; truncate(sqrt(r)) acota la finestra mínima a explorar; després filtrem per d²
(defun agent-sng656-offsets-within (r)
    (let ((b (truncate (sqrt (float r)))))
        (remove-if (lambda (off)
                       (or (and (zerop (car off)) (zerop (cadr off)))
                           (> (agent-sng656-dist '(0 0) off) r)))
                   (agent-sng656-cartesian (agent-sng656-range (- b) b)
                                            (agent-sng656-range (- b) b)))))

; rang (d²) considerat per als veïns "adjacents" de moviment i creació de bolla
(defconstant AGENT-SNG656-NEIGH-RANGE   2)
(defconstant AGENT-SNG656-NEIGH-OFFSETS (agent-sng656-offsets-within AGENT-SNG656-NEIGH-RANGE))

; cerca a vision la cel·la amb la coord donada (o nil si no hi és)
(defun agent-sng656-cell-at (vision coord)
    (cond ((null vision) nil)
          ((equal (agent-sng656-cell-coord (car vision)) coord) (car vision))
          (t (agent-sng656-cell-at (cdr vision) coord))))

; **************************************************
; FILTRES SOBRE VISION
; **************************************************

; cel·les visibles que satisfan el predicat fun
(defun agent-sng656-cells-where (vision fun)
    (cond ((null vision) nil)
          ((funcall fun (car vision)) (cons (car vision) (agent-sng656-cells-where (cdr vision) fun)))
          (t (agent-sng656-cells-where (cdr vision) fun))))

; bolles enemigues visibles
(defun agent-sng656-enemy-balls (vision team)
    (agent-sng656-cells-where vision
        (lambda (c) (and (agent-sng656-is-ball c) (not (eq (agent-sng656-cell-team c) team))))))

; bases enemigues visibles
(defun agent-sng656-enemy-bases (vision team)
    (agent-sng656-cells-where vision
        (lambda (c) (and (agent-sng656-is-base c) (not (eq (agent-sng656-cell-team c) team))))))

; laboratoris no controlats per nosaltres
(defun agent-sng656-target-labs (vision team)
    (agent-sng656-cells-where vision
        (lambda (c) (and (agent-sng656-is-lab c) (not (eq (agent-sng656-cell-team c) team))))))

; cel·les terra buides
(defun agent-sng656-empty-lands (vision)
    (agent-sng656-cells-where vision #'agent-sng656-is-empty))

; **************************************************
; ENTRADA DE L'AGENT
; **************************************************

; despatxa segons el tipus d'unitat; les estratègies viuen a -base i -ball
(defun agent-sng656 (info)
    (let ((unit (agent-sng656-info-unit info)))
        (cond ((eq unit AGENT-SNG656-BASE) (agent-sng656-base info))
              ((eq unit AGENT-SNG656-BALL) (agent-sng656-ball info))
              (t nil))))

; **************************************************
; ESTRATÈGIA DE LA BASE
; **************************************************

; cost de crear una bolla nova
(defconstant AGENT-SNG656-BALL-COST 50)

; cerca el primer veinat que sigui terra buida dins de vision
; recorre la llista d'offsets recursivament i retorna la primera coord vàlida (o nil)
(defun agent-sng656-find-empty-neighbour (src vision offsets)
    (if (null offsets)
        nil
        (let* ((dst (agent-sng656-add src (car offsets)))
               (dst-cell (agent-sng656-cell-at vision dst)))
               (if (and dst-cell (agent-sng656-is-empty dst-cell))
                    dst
                    (agent-sng656-find-empty-neighbour src vision (cdr offsets))))))

; donada la llista painted d'una unitat enemiga, retorna un color RGB que no en formi part
; si la llista està buida, agafem 'r per defecte
(defun agent-sng656-pick-color-missing (painted)
    (let ((missing (remove-if (lambda (c) (member c painted)) AGENT-SNG656-RGB)))
         (if missing (car missing) 'r)))

; tria un color per crear la bolla:
;   - si veiem enemics, escollim un color del que encara no estiguin pintats
;   - si no, rotam entre r/g/b segons (mod ronda 3) per variar
(defun agent-sng656-base-pick-color (info)
    (let* ((vision (agent-sng656-info-vision info))
           (team (agent-sng656-info-team info))
           (enemies (append (agent-sng656-enemy-bases vision team) ; aquí prioritzam les bases perque ens atraca més a guanyar
                            (agent-sng656-enemy-balls vision team))))
        (cond (enemies (agent-sng656-pick-color-missing (agent-sng656-cell-painted (car enemies))))
              (t (nth (mod (agent-sng656-info-turn info) (length AGENT-SNG656-RGB)) AGENT-SNG656-RGB)))))

; estratègia de la base:
;   - si tenim menys pintura que el cost d'una bolla, no fem res
;   - si tenim un veí buit, hi creem una bolla amb un color útil
;   - altrament, no fem res
(defun agent-sng656-base (info)
    (cond ((< (agent-sng656-info-paint info) AGENT-SNG656-BALL-COST) nil)
          (t (let ((dst (agent-sng656-find-empty-neighbour
                             (agent-sng656-info-coord info)
                             (agent-sng656-info-vision info)
                             AGENT-SNG656-NEIGH-OFFSETS)))
                (if dst
                    (list (list 'CREA-BOLLA (list (agent-sng656-base-pick-color info) dst)))
                    nil)))))

; **************************************************
; ESTRATÈGIA DE LA BOLLA
; **************************************************

; rang de pintar (d²); copiat de l'enunciat per ser autocontingut
(defconstant AGENT-SNG656-PAINT-RANGE 5)

; puntuació d'una cel·la com a objectiu (per pintar o per orientar el moviment):
;   base enemiga: 1000  bolla enemiga: 100  lab no nostre: 50  altre: 0
(defun agent-sng656-score-cell (cell team)
    (cond ((eq (agent-sng656-cell-team cell) team) 0)
          ((and (agent-sng656-is-base cell)) 1000)
          ((and (agent-sng656-is-ball cell)) 100)
          ((and (agent-sng656-is-lab  cell)) 50)
          (t 0)))

; entre una llista de cel·les, retorna la millor com a (cell score d²)
; criteris: més puntuació primer, després més propera a src; nil si cap puntua > 0
(defun agent-sng656-best-cell (cells src team)
    (reduce (lambda (best c)
                (let* ((s (agent-sng656-score-cell c team))
                       (d (agent-sng656-dist (agent-sng656-cell-coord c) src)))
                    (cond ((zerop s) best)
                          ((null best) (list c s d))
                          ((> s (cadr best)) (list c s d))
                          ((and (= s (cadr best)) (< d (caddr best))) (list c s d))
                          (t best))))
            cells :initial-value nil))

; millor objectiu a pintar: cel·la de vision dins del rang de pintar amb puntuació > 0
(defun agent-sng656-best-paint-target (info)
    (let* ((src (agent-sng656-info-coord info))
           (team (agent-sng656-info-team info))
           (vision (agent-sng656-info-vision info))
           (in-range (remove-if (lambda (c)
                                    (> (agent-sng656-dist (agent-sng656-cell-coord c) src)
                                       AGENT-SNG656-PAINT-RANGE))
                                vision)))
        (agent-sng656-best-cell in-range src team)))

; millor objectiu llunyà (per orientar el moviment): qualsevol cel·la enemiga/lab visible
(defun agent-sng656-best-far-target (info)
    (agent-sng656-best-cell (agent-sng656-info-vision info)
                            (agent-sng656-info-coord info)
                            (agent-sng656-info-team info)))

; recorre els 8 offsets veïns recursivament i tria el que minimitza d² al target
; cada candidat ha de ser terra buida i visible (això garanteix que sigui dins del mapa)
; acumulador best té format (coord d²)
(defun agent-sng656-best-step-rec (src vision target offsets best)
    (cond ((null offsets) best)
          (t (let* ((dst (agent-sng656-add src (car offsets)))
                    (cell (agent-sng656-cell-at vision dst)))
                    ; només podem moure a la posició de destí si està buida
                   (if (and cell (agent-sng656-is-empty cell))
                       (let* ((d (if target (agent-sng656-dist dst target) 0))
                              (best-new (if (or (null best) (< d (cadr best))) (list dst d) best)))
                             (agent-sng656-best-step-rec src vision target (cdr offsets) best-new))
                       (agent-sng656-best-step-rec src vision target (cdr offsets) best))))))

; selecciona la coord del veí cap a on moure'ns; nil si no hi ha cap veí buit visible
(defun agent-sng656-best-move-step (info)
    (let* ((src (agent-sng656-info-coord info))
           (vision (agent-sng656-info-vision info))
           (target (agent-sng656-best-far-target info))
           (target-coord (if target (agent-sng656-cell-coord (car target)) nil))
           (best (agent-sng656-best-step-rec src vision target-coord
                                             AGENT-SNG656-NEIGH-OFFSETS nil)))
        (if best (car best) nil)))

; estratègia de la bolla:
;   - intenta pintar el millor objectiu en rang (si tr-paint < 1)
;   - intenta moure cap a l'objectiu visible de més puntuació (si tr-move < 1)
; ambdues accions es decideixen sobre l'estat actual; pintar primer i moure després és segur
; perquè el pintat no afecta la validesa del moviment posterior
(defun agent-sng656-ball (info)
    (let* ((tr-paint (agent-sng656-info-tr-paint info))
           (tr-move  (agent-sng656-info-tr-move  info))
           (paint-dst (and (< tr-paint 1) (agent-sng656-best-paint-target info)))
           (move-step (and (< tr-move  1) (agent-sng656-best-move-step    info)))
           (paint-action (if paint-dst
                             (list (list 'PINTA (list (agent-sng656-cell-coord (car paint-dst)))))
                             nil))
           (move-action  (if move-step
                             (list (list 'MOU (list move-step)))
                             nil)))
        (append paint-action move-action)))
