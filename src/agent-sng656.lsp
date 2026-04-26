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
(defconstant AGENT-SNG656-WATER         'aigua)
(defconstant AGENT-SNG656-LAND          'terra)
(defconstant AGENT-SNG656-LAB           'lab)
(defconstant AGENT-SNG656-BASE          'base)
(defconstant AGENT-SNG656-BALL          'bolla)
(defconstant AGENT-SNG656-RGB           '(r g b))
(defconstant AGENT-SNG656-BALL-COST     50)
(defconstant AGENT-SNG656-PAINT-RANGE   5)
(defconstant AGENT-SNG656-DEFENDER-MOD  4)
(defconstant AGENT-SNG656-DEFENSE-RANGE 100)

; antiguitat màxima (en torns) abans de descartar una entrada de la memòria
(defconstant AGENT-SNG656-AGE-MAX 100)

; offsets (dx dy) dels 8 veïns adjacents (d² ≤ 2, excloent (0 0))
; emprat tant per moviment com per creació de bolla
(defconstant AGENT-SNG656-NEIGH-OFFSETS
    '((-1 -1) ( 0 -1) ( 1 -1)
      (-1  0)         ( 1  0)
      (-1  1) ( 0  1) ( 1 1)))

; **************************************************
; UTILITATS
; **************************************************

; operacions aritmètiques i lògiques bàsiques sobre llistes de N elements 
(defun agent-sng656-add   (a b) (mapcar '+ a b))
(defun agent-sng656-mul   (a b) (mapcar '* a b))
(defun agent-sng656-sub   (a b) (mapcar '- a b))
(defun agent-sng656-pow   (a e) (mapcar (lambda (x) (expt x e)) a))
(defun agent-sng656-sum   (a)   (reduce '+ a :initial-value 0))

; converteix un booleà Lisp a enter
(defun agent-sng656-bool-int (x) (if x 1 0))

; distància euclidiana al quadrat entre dos coords (x y) — vàlida en espai desplaçat
(defun agent-sng656-dist (a b) (agent-sng656-sum (agent-sng656-pow (agent-sng656-sub a b) 2)))

; índex de l'element màxim d'una llista (primer en cas d'empat)
(defun agent-sng656-argmax-rec (a i best-i best-a)
    (cond ((null a) best-i)
          ((> (car a) best-a) (agent-sng656-argmax-rec (cdr a) (1+ i) i (car a)))
          (t (agent-sng656-argmax-rec (cdr a) (1+ i) best-i best-a))))
(defun agent-sng656-argmax (a) (agent-sng656-argmax-rec (cdr a) 1 0 (car a)))

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
; MEMÒRIA / VISIÓ COMPARTIDA
; **************************************************

; entrada de la memòria: (turn cell)
;   turn:  torn en què es va observar la cel·la per darrera vegada
;   cell:  cel·la (mateix format que info-vision)
(defun agent-sng656-entry-make  (turn cell) (list turn cell))
(defun agent-sng656-entry-turn  (e) (car  e))
(defun agent-sng656-entry-cell  (e) (cadr e))
(defun agent-sng656-entry-coord (e) (agent-sng656-cell-coord (agent-sng656-entry-cell e)))

; cerca dins entries l'entrada amb la coord donada (o nil si no hi és)
(defun agent-sng656-entry-at (entries coord)
    (cond ((null entries) nil)
          ((equal (agent-sng656-entry-coord (car entries)) coord) (car entries))
          (t (agent-sng656-entry-at (cdr entries) coord))))

; combina la visió actual (etiquetada amb turn) amb la memòria
; les entrades més recents sobreescriuen les antigues
(defun agent-sng656-merge-vision (vision turn mem)
    (let ((new (mapcar (lambda (c) (agent-sng656-entry-make turn c)) vision)))
        (append new
                ; filtra les cel·les de la memòria que tenen la mateixa coordenada que la visió actual
                (remove-if (lambda (e) (agent-sng656-entry-at new (agent-sng656-entry-coord e))) mem))))

; descarta entrades amb antiguitat superior a AGE-MAX
(defun agent-sng656-entries-drop-old (entries turn)
    (remove-if (lambda (e) (> (- turn (agent-sng656-entry-turn e)) AGENT-SNG656-AGE-MAX))
               entries))

; entries que satisfan el predicat fun aplicat a la cel·la interna
(defun agent-sng656-entries-where (entries fun)
    (remove-if (lambda (e) (not (funcall fun (agent-sng656-entry-cell e)))) entries))

; entrades que tenen una bolla enemiga
(defun agent-sng656-enemy-balls (entries team)
    (agent-sng656-entries-where entries
        (lambda (c) (and (agent-sng656-is-ball c) (not (eq (agent-sng656-cell-team c) team))))))

; entrades que tenen una base enemiga
(defun agent-sng656-enemy-bases (entries team)
    (agent-sng656-entries-where entries
        (lambda (c) (and (agent-sng656-is-base c) (not (eq (agent-sng656-cell-team c) team))))))

; entrada que conté la nostra base
(defun agent-sng656-friendly-base (entries team)
    (car (agent-sng656-entries-where entries
        (lambda (c) (and (agent-sng656-is-base c) (eq (agent-sng656-cell-team c) team))))))

; entrades que tenen la cel·la buida
(defun agent-sng656-empty-lands (entries)
    (agent-sng656-entries-where entries #'agent-sng656-is-empty))

; una bolla de cada DEFENDER-MOD es queda defensant la base
(defun agent-sng656-defender (info)
    (zerop (mod (agent-sng656-info-id info) AGENT-SNG656-DEFENDER-MOD)))

; **************************************************
; ESTRATÈGIA DE LA BASE
; **************************************************

; cerca el primer veïnat que sigui terra buida dins de entries
; recorre la llista d'offsets recursivament i retorna la primera coord vàlida (o nil)
(defun agent-sng656-find-empty-neighbour (src entries offsets)
    (if (null offsets)
        nil
        (let* ((dst (agent-sng656-add src (car offsets)))
               (dst-entry (agent-sng656-entry-at entries dst))
               (dst-cell (if dst-entry (agent-sng656-entry-cell dst-entry) nil)))
               (if (and dst-cell (agent-sng656-is-empty dst-cell))
                    dst
                    (agent-sng656-find-empty-neighbour src entries (cdr offsets))))))

; pes d'una cel·la enemiga segons el seu tipus
(defun agent-sng656-color-weight (cell)
    (cond ((agent-sng656-is-base cell) 10)
          ((agent-sng656-is-ball cell) 1)
          (t 0)))

; vector (r g b) on cada element val 1 si la unitat NO està pintada amb aquell color
(defun agent-sng656-missing-counts (cell)
    (let ((w (agent-sng656-color-weight cell))
          (painted (agent-sng656-cell-painted cell)))
         (mapcar (lambda (c) (if (member c painted) 0 w)) AGENT-SNG656-RGB)))

; suma vectorial (r g b): per cada enemic visible, agrega missing-counts ponderat
(defun agent-sng656-missing-total (entries team)
    (reduce #'agent-sng656-add
            (mapcar (lambda (e) (agent-sng656-missing-counts (agent-sng656-entry-cell e)))
                    (append (agent-sng656-enemy-bases entries team)
                            (agent-sng656-enemy-balls entries team)))
            :initial-value '(0 0 0)))

; tria un color per crear la bolla:
;   - si veiem enemics, agreguem els colors que els falten ponderats per tipus i triem el màxim
;   - si no, rotam entre r/g/b segons (mod ronda 3) per variar
(defun agent-sng656-base-pick-color (info entries)
    (let* ((team (agent-sng656-info-team info))
           (totals (agent-sng656-missing-total entries team))
           (idx (agent-sng656-argmax totals)))
        (if (> (nth idx totals) 0)
            (nth idx AGENT-SNG656-RGB)
            (nth (mod (agent-sng656-info-turn info) (length AGENT-SNG656-RGB)) AGENT-SNG656-RGB))))

; estratègia de la base:
;   - si tenim menys pintura que el cost d'una bolla, no fem res
;   - si tenim un veí buit, hi creem una bolla amb un color útil
;   - altrament, no fem res
(defun agent-sng656-base (info entries)
    (cond ((< (agent-sng656-info-paint info) AGENT-SNG656-BALL-COST) nil)
          (t (let ((dst (agent-sng656-find-empty-neighbour
                             (agent-sng656-info-coord info)
                             entries
                             AGENT-SNG656-NEIGH-OFFSETS)))
                (if dst
                    (list (list 'CREA-BOLLA (list (agent-sng656-base-pick-color info entries) dst)))
                    nil)))))

; **************************************************
; ESTRATÈGIA DE LA BOLLA
; **************************************************

; puntuació base d'una cel·la com a objectiu (sense tenir en compte l'antiguitat):
;   base enemiga: 1000  bolla enemiga: 100  lab no nostre: 50
;   terra buida fora del nostre color: 5 (només si es passa src-color, per pintar el camí)
;   altre: 0
(defun agent-sng656-score-cell (cell team &optional src-color)
    (cond ((eq (agent-sng656-cell-team cell) team) 0)
          ((agent-sng656-is-base cell) 1000)
          ((agent-sng656-is-ball cell) 100)
          ((agent-sng656-is-lab  cell) 50)
          ((and src-color
                (agent-sng656-is-empty cell)
                (not (eq (agent-sng656-cell-color cell) src-color))) 5)
          (t 0)))

; puntuació d'una entrada: la base menys l'antiguitat (cap a 0 mai)
; així informació recent és preferida sense descartar del tot la antiga
(defun agent-sng656-score-entry (entry team turn &optional src-color)
    (let ((age (- turn (agent-sng656-entry-turn entry)))
          (raw (agent-sng656-score-cell (agent-sng656-entry-cell entry) team src-color)))
         (max 0 (- raw age))))

; entre una llista d'entries, retorna la millor com a (entry score d²)
; criteris: més puntuació primer, després més propera a src; nil si cap puntua > 0
(defun agent-sng656-best-entry (entries src team turn &optional src-color)
    (reduce (lambda (best e)
                (let* ((s (agent-sng656-score-entry e team turn src-color))
                       (d (agent-sng656-dist (agent-sng656-entry-coord e) src)))
                    (cond ((zerop s) best)
                          ((null best) (list e s d))
                          ((> s (cadr best)) (list e s d))
                          ((and (= s (cadr best)) (< d (caddr best))) (list e s d))
                          (t best))))
            entries :initial-value nil))

; millor objectiu a pintar: entry dins del rang de pintar amb puntuació > 0
(defun agent-sng656-best-paint-target (info entries)
    (let* ((src (agent-sng656-info-coord info))
           (team (agent-sng656-info-team info))
           (turn (agent-sng656-info-turn info))
           (src-color (agent-sng656-info-src-color info))
           (in-range (remove-if (lambda (e)
                                    (> (agent-sng656-dist (agent-sng656-entry-coord e) src)
                                       AGENT-SNG656-PAINT-RANGE))
                                entries)))
        (agent-sng656-best-entry in-range src team turn src-color)))

; millor objectiu visible (per orientar el moviment): qualsevol entry enemiga/lab
(defun agent-sng656-best-tactical-target (info entries)
    (agent-sng656-best-entry entries
                             (agent-sng656-info-coord info)
                             (agent-sng656-info-team info)
                             (agent-sng656-info-turn info)))

; compta quants veïns d'una coord encara no són a la memòria/visió compartida
(defun agent-sng656-unknown-neighbours (coord entries)
    (agent-sng656-sum (mapcar
        (lambda (off) (agent-sng656-bool-int
            (null (agent-sng656-entry-at entries (agent-sng656-add coord off)))))
        AGENT-SNG656-NEIGH-OFFSETS)))

; millor objectiu d'exploració quan no hi ha enemics/labs a la memòria.
; Només considera terra buida amb almenys un veí desconegut. El valor best té format
; (entry unknown base-dist ball-dist), on unknown és el nombre de veïns desconeguts.
; Criteri: maximitzar unknown, després maximitzar base-dist i finalment minimitzar ball-dist.
(defun agent-sng656-best-frontier-target (info entries)
    (let* ((src (agent-sng656-info-coord info))
           (team (agent-sng656-info-team info))
           (own-base-entry (agent-sng656-friendly-base entries team))
           (origin (if own-base-entry (agent-sng656-entry-coord own-base-entry) src)))
          (reduce (lambda (best entry)
                    (let* ((coord (agent-sng656-entry-coord entry))
                           (unknown (if (agent-sng656-is-empty (agent-sng656-entry-cell entry))
                                        (agent-sng656-unknown-neighbours coord entries)
                                        0))
                           (base-dist (agent-sng656-dist coord origin))
                           (ball-dist (agent-sng656-dist coord src)))
                          (cond ((zerop unknown) best)
                                ((null best) (list entry unknown base-dist ball-dist))
                                ((> unknown (cadr best)) (list entry unknown base-dist ball-dist)) ; maximitzar nombre de cel·les adjacents no explorades
                                ((and (= unknown (cadr best)) (> base-dist (caddr best))) (list entry unknown base-dist ball-dist))
                                ((and (= unknown (cadr best))
                                      (= base-dist (caddr best))
                                      (< ball-dist (nth 3 best))) (list entry unknown base-dist ball-dist))
                                (t best))))
                  entries
                  :initial-value nil)))

; objectiu defensiu per bolles marcades com a defensores.
; Cerca bolles enemigues dins DEFENSE-RANGE de la nostra base coneguda. Si encara no
; coneixem la base pròpia dins la memòria, usa la posició actual com a origen.
; Retorna nil si no hi ha amenaça propera, així el defensor no s'allunya explorant.
(defun agent-sng656-best-defense-target (info entries)
    (let* ((team (agent-sng656-info-team info))
           (own-base-entry (agent-sng656-friendly-base entries team))
           (origin (if own-base-entry
                       (agent-sng656-entry-coord own-base-entry)
                       (agent-sng656-info-coord info)))
           (near-enemies (remove-if
                            (lambda (e) (> (agent-sng656-dist (agent-sng656-entry-coord e) origin)
                                           AGENT-SNG656-DEFENSE-RANGE))
                            (agent-sng656-enemy-balls entries team))))
          (agent-sng656-best-entry near-enemies
                                   (agent-sng656-info-coord info)
                                   team
                                   (agent-sng656-info-turn info))))

; millor objectiu visible (per orientar el moviment), amb exploració simple:
;   - defensors: només persegueixen bolles enemigues properes a la base
;   - exploradors: enemics/labs primer; si no, frontera de mapa conegut
(defun agent-sng656-best-target (info entries)
    (cond ((agent-sng656-defender info)
           (agent-sng656-best-defense-target info entries))
          (t (let ((target (agent-sng656-best-tactical-target info entries)))
                (if target
                    target
                    (agent-sng656-best-frontier-target info entries))))))

; recorre els 8 offsets veïns recursivament i tria el que minimitza d² al target
; cada candidat ha de ser terra buida i present a entries (això garanteix que sigui dins del mapa)
; acumulador best té format (coord d²)
(defun agent-sng656-best-step-rec (src entries target offsets best)
    (cond ((null offsets) best)
          (t (let* ((dst (agent-sng656-add src (car offsets)))
                    (entry (agent-sng656-entry-at entries dst))
                    (cell (if entry (agent-sng656-entry-cell entry) nil)))
                    ; només podem moure a la posició de destí si està buida
                   (if (and cell (agent-sng656-is-empty cell))
                       (let* ((d (if target (agent-sng656-dist dst target) 0))
                              (best-new (if (or (null best) (< d (cadr best))) (list dst d) best)))
                             (agent-sng656-best-step-rec src entries target (cdr offsets) best-new))
                       (agent-sng656-best-step-rec src entries target (cdr offsets) best))))))

; selecciona la coord del veí cap a on moure'ns; nil si no hi ha cap veí buit visible
(defun agent-sng656-best-move-step (info entries)
    (let* ((src (agent-sng656-info-coord info))
           (target (agent-sng656-best-target info entries))
           (target-coord (if target (agent-sng656-entry-coord (car target)) nil)))
        (if target-coord
            (let ((best (agent-sng656-best-step-rec src entries target-coord
                                                    AGENT-SNG656-NEIGH-OFFSETS nil)))
                (if best (car best) nil))
            nil)))

; estratègia de la bolla:
;   - intenta pintar el millor objectiu en rang (si tr-paint < 1)
;   - intenta moure cap a l'objectiu de més puntuació (si tr-move < 1)
(defun agent-sng656-ball (info entries)
    (let* ((tr-paint (agent-sng656-info-tr-paint info))
           (tr-move  (agent-sng656-info-tr-move  info))
           (paint-dst (and (< tr-paint 1) (agent-sng656-best-paint-target info entries)))
           (move-step (and (< tr-move  1) (agent-sng656-best-move-step    info entries)))
           (paint-action (if paint-dst
                             (list (list 'PINTA (list (agent-sng656-entry-coord (car paint-dst)))))
                             nil))
           (move-action  (if move-step
                             (list (list 'MOU (list move-step)))
                             nil)))
        (append paint-action move-action)))

; **************************************************
; ENTRADA DE L'AGENT
; **************************************************

; despatxa segons el tipus d'unitat:
;   1. fusiona la visió actual amb la memòria, descartant entrades antigues
;   2. emet ESCRIU-MEMORIA amb la nova memòria fusionada perquè els companys
;      la vegin a partir del proper torn
;   3. crida l'estratègia corresponent passant les entries fusionades
(defun agent-sng656 (info)
    (let* ((unit (agent-sng656-info-unit info))
           (turn (agent-sng656-info-turn info))
           (entries (agent-sng656-merge-vision (agent-sng656-info-vision info)
                    turn
                    (agent-sng656-entries-drop-old (agent-sng656-info-memory info) turn)))
           (mem-new (list (list 'ESCRIU-MEMORIA (list entries))))
           (actions (cond ((eq unit AGENT-SNG656-BASE) (agent-sng656-base info entries))
                          ((eq unit AGENT-SNG656-BALL) (agent-sng656-ball info entries))
                          (t nil))))
        (append mem-new actions)))
