;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer de l'agent intel·ligent SNG656.
;;
;; Estratègia general de l'agent:
;; - Cada unitat combina la seva visió actual amb la memòria compartida de l'equip.
;;   La memòria desa entrades (torn cel·la), de manera que les observacions recents
;;   sobreescriuen les antigues. Les cel·les que falten dins el rang de visió es
;;   guarden com a (coord nil) per no explorar més enllà de les vores. Les entrades
;;   antigues només es descarten si contenien bolles, perquè són l'únic element que es
;;   mou.
;; - La base crea bolles només si té pintura suficient i hi ha una casella adjacent
;;   buida, tal com exigeix la lògica del joc. El color de la nova bolla es tria per
;;   equilibrar els colors de les bolles pròpies conegudes.
;; - Les bolles primer intenten pintar el millor objectiu dins rang: base enemiga,
;;   bolla enemiga o laboratori.
;; - Per moure's, les bolles avancen en una direcció fixa segons el seu id-unitat
;;   mentre no hi hagi cap objectiu enemic a la memòria compartida. Quan es coneix
;;   una base o bolla enemiga, passen a mode atacant i s'hi acosten.

; **************************************************
; CONSTANTS
; **************************************************

; constants del joc
(defconstant AGENT-SNG656-LAND          'terra)
(defconstant AGENT-SNG656-LAB           'lab)
(defconstant AGENT-SNG656-BASE          'base)
(defconstant AGENT-SNG656-BALL          'bolla)
(defconstant AGENT-SNG656-RGB           '(r g b))
(defconstant AGENT-SNG656-BALL-COST     50)
(defconstant AGENT-SNG656-PAINT-RANGE   5)

; antiguitat màxima (en torns) abans de descartar una entrada de la memòria
(defconstant AGENT-SNG656-AGE-MAX 100)

; offsets (dx dy) dels 8 veïns adjacents (d² ≤ 2, excloent (0 0))
; emprat tant per moviment com per creació de bolla
(defconstant AGENT-SNG656-NEIGH-OFFSETS
    '((-1 -1) ( 0 -1) ( 1 -1)
      (-1  0)         ( 1  0)
      (-1  1) ( 0  1) ( 1 1)))

; offsets (dx dy) dins la visió d'una bolla (d² ≤ 20)
(defconstant AGENT-SNG656-BALL-VISION-OFFSETS
    '((-4 -2) (-4 -1) (-4  0) (-4  1) (-4  2)
      (-3 -3) (-3 -2) (-3 -1) (-3  0) (-3  1) (-3  2) (-3  3)
      (-2 -4) (-2 -3) (-2 -2) (-2 -1) (-2  0) (-2  1) (-2  2) (-2  3) (-2  4)
      (-1 -4) (-1 -3) (-1 -2) (-1 -1) (-1  0) (-1  1) (-1  2) (-1  3) (-1  4)
      ( 0 -4) ( 0 -3) ( 0 -2) ( 0 -1) ( 0  0) ( 0  1) ( 0  2) ( 0  3) ( 0  4)
      ( 1 -4) ( 1 -3) ( 1 -2) ( 1 -1) ( 1  0) ( 1  1) ( 1  2) ( 1  3) ( 1  4)
      ( 2 -4) ( 2 -3) ( 2 -2) ( 2 -1) ( 2  0) ( 2  1) ( 2  2) ( 2  3) ( 2  4)
      ( 3 -3) ( 3 -2) ( 3 -1) ( 3  0) ( 3  1) ( 3  2) ( 3  3)
      ( 4 -2) ( 4 -1) ( 4  0) ( 4  1) ( 4  2)))

; **************************************************
; UTILITATS
; **************************************************

; operacions aritmètiques i lògiques bàsiques sobre llistes de N elements 
(defun agent-sng656-add   (a b) (mapcar '+ a b))
(defun agent-sng656-mul   (a b) (mapcar '* a b))
(defun agent-sng656-sub   (a b) (mapcar '- a b))
(defun agent-sng656-sum   (a)   (reduce '+ a :initial-value 0))

; converteix un booleà Lisp a enter
(defun agent-sng656-bool-int (x) (if x 1 0))

; distància euclidiana al quadrat entre dos coords (x y) — vàlida en espai desplaçat
(defun agent-sng656-dist (a b)
    (let ((d (agent-sng656-sub a b)))
         (agent-sng656-sum (agent-sng656-mul d d))))

; índex de l'element màxim d'una llista (primer en cas d'empat)
(defun agent-sng656-argmax-rec (a i best-i best-a)
    (cond ; ja hem recorregut tota la llista: retornam el millor índex trobat
          ((null a) best-i)
          ; l'element actual supera el millor valor: actualitzam índex i valor
          ((> (car a) best-a) (agent-sng656-argmax-rec (cdr a) (1+ i) i (car a)))
          ; l'element actual no millora el resultat: continuam amb el millor existent
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
(defun agent-sng656-info-own-color (info) (nth 7  info))
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
(defun agent-sng656-cell-own-color (cell) (nth 6 cell))

; predicats sobre cel·les del mapa
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

; cerca dins una llista de cel·les la que té la coord donada (o nil si no hi és)
(defun agent-sng656-cell-at (cells coord)
    (cond ((null cells) nil)
          ((equal (agent-sng656-cell-coord (car cells)) coord) (car cells))
          (t (agent-sng656-cell-at (cdr cells) coord))))

; crea cel·les sintètiques (coord nil) per coords visibles que no existeixen al mapa
(defun agent-sng656-edge-cells (src vision offsets)
    (if (null offsets)
        nil
        (let ((coord (agent-sng656-add src (car offsets))))
             (if (agent-sng656-cell-at vision coord)
                 (agent-sng656-edge-cells src vision (cdr offsets))
                 (cons (list coord nil)
                       (agent-sng656-edge-cells src vision (cdr offsets)))))))

; visió actual ampliada amb vores del mapa inferides
(defun agent-sng656-known-cells (info)
    (let ((vision (agent-sng656-info-vision info)))
         (append vision
                 (agent-sng656-edge-cells
                     (agent-sng656-info-coord info)
                     vision
                     AGENT-SNG656-BALL-VISION-OFFSETS))))

; combina la visió actual (etiquetada amb turn) amb la memòria
; les entrades més recents sobreescriuen les antigues
(defun agent-sng656-merge-vision (vision turn mem)
    (let ((new (mapcar (lambda (c) (agent-sng656-entry-make turn c)) vision)))
        (append new (remove-if (lambda (e) (agent-sng656-entry-at new (agent-sng656-entry-coord e))) mem))))

; descarta només bolles antigues: bases, labs, terreny i vores no es mouen
(defun agent-sng656-entries-drop-old (entries turn)
    (remove-if (lambda (e)
                   (and (agent-sng656-is-ball (agent-sng656-entry-cell e))
                        (> (- turn (agent-sng656-entry-turn e)) AGENT-SNG656-AGE-MAX)))
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
               (dst-cell (agent-sng656-entry-cell dst-entry)))
               (if (and dst-cell (agent-sng656-is-empty dst-cell))
                    ; veí vàlid: la base hi pot crear una bolla
                    dst
                    ; veí no serveix: provam el següent offset
                    (agent-sng656-find-empty-neighbour src entries (cdr offsets))))))

; compta quantes bolles pròpies conegudes hi ha d'un color concret
(defun agent-sng656-own-balls-color-count (entries team color)
    (agent-sng656-sum
        (mapcar (lambda (e)
                    (let ((cell (agent-sng656-entry-cell e)))
                         (agent-sng656-bool-int
                             (and (agent-sng656-is-ball cell)
                                  (eq (agent-sng656-cell-team cell) team)
                                  (eq (agent-sng656-cell-own-color cell) color)))))
                entries)))

; vector (r g b) amb el total de bolles pròpies conegudes de cada color
(defun agent-sng656-own-balls-color-counts (entries team)
    (mapcar (lambda (color) (agent-sng656-own-balls-color-count entries team color))
            AGENT-SNG656-RGB))

; tria un color per crear la bolla: el color menys present entre les bolles pròpies conegudes
(defun agent-sng656-base-pick-color (info entries)
    (let* ((team (agent-sng656-info-team info))
           (counts (agent-sng656-own-balls-color-counts entries team))
           (min-count (reduce 'min counts)))
          (nth (mod (agent-sng656-info-turn info)
                    (length (remove-if (lambda (color)
                                           (/= (agent-sng656-own-balls-color-count entries team color)
                                               min-count))
                                       AGENT-SNG656-RGB)))
               (remove-if (lambda (color)
                              (/= (agent-sng656-own-balls-color-count entries team color)
                                  min-count))
                          AGENT-SNG656-RGB))))

; estratègia de la base:
;   - si tenim menys pintura que el cost d'una bolla, no fem res
;   - si tenim un veí buit, hi creem una bolla amb un color útil
;   - altrament, no fem res
(defun agent-sng656-base (info entries)
    (cond ; sense pintura suficient no intentam crear cap bolla
          ((< (agent-sng656-info-paint info) AGENT-SNG656-BALL-COST) nil)
          ; tenim pintura: cercam una casella adjacent buida on la creació sigui vàlida
          (t (let ((dst (agent-sng656-find-empty-neighbour
                             (agent-sng656-info-coord info)
                             entries
                             AGENT-SNG656-NEIGH-OFFSETS)))
                (if dst
                    ; hi ha lloc lliure: cream una bolla amb el color més útil
                    (list (list 'CREA-BOLLA (list (agent-sng656-base-pick-color info entries) dst)))
                    ; l'anell de creació està ple
                    nil)))))

; **************************************************
; ESTRATÈGIA DE LA BOLLA
; **************************************************

; puntuació base d'una cel·la com a objectiu (sense tenir en compte l'antiguitat):
;   base enemiga: 1000  bolla enemiga: 100  lab no nostre: 50
;   altre: 0
(defun agent-sng656-score-cell (cell team)
    (cond ((eq (agent-sng656-cell-team cell) team) 0)
          ((agent-sng656-is-base cell) 1000)
          ((agent-sng656-is-ball cell) 100)
          ((agent-sng656-is-lab  cell) 50)
          (t 0)))

; puntuació d'una entrada: només depèn del valor tàctic de la cel·la.
; Les entrades velles ja es filtren en actualitzar la memòria.
(defun agent-sng656-score-entry (entry team)
    (agent-sng656-score-cell (agent-sng656-entry-cell entry) team))

; entre una llista d'entries, retorna la millor entry
; criteris: més puntuació primer, després més propera a src; nil si cap puntua > 0
(defun agent-sng656-best-entry (entries src team)
    (car (reduce (lambda (best e)
                    (let* ((s (agent-sng656-score-entry e team))
                            (d (agent-sng656-dist (agent-sng656-entry-coord e) src)))
                        (cond ; puntuació zero significa que aquesta entrada no és objectiu
                              ((zerop s) best)
                              ; primera entrada útil trobada: inicialitzam el millor candidat
                              ((null best) (list e s d))
                              ; més puntuació estratègica: prioritat principal
                              ((> s (cadr best)) (list e s d))
                              ; en empat de puntuació, triam l'objectiu més proper
                              ((and (= s (cadr best)) (< d (caddr best))) (list e s d))
                              ; el candidat actual no millora el millor conegut
                              (t best))))
                entries :initial-value nil)))

; millor objectiu a pintar: entry dins del rang de pintar amb puntuació > 0
(defun agent-sng656-best-paint-target (info entries)
    (let* ((src (agent-sng656-info-coord info))
            (team (agent-sng656-info-team info))
            (own-color (agent-sng656-info-own-color info))
            (in-range (remove-if (lambda (e)
                                      (let ((cell (agent-sng656-entry-cell e)))
                                           (or (> (agent-sng656-dist (agent-sng656-entry-coord e) src)
                                                  AGENT-SNG656-PAINT-RANGE)
                                               (and (agent-sng656-is-ball cell)
                                                    (member own-color (agent-sng656-cell-painted cell))))))
                                  entries)))
        (agent-sng656-best-entry in-range src team)))

; compta quants veïns d'una coord encara no són a la memòria/visió compartida
(defun agent-sng656-unknown-neighbours (coord entries)
    (agent-sng656-sum (mapcar
        (lambda (off) (agent-sng656-bool-int
            (null (agent-sng656-entry-at entries (agent-sng656-add coord off)))))
        AGENT-SNG656-NEIGH-OFFSETS)))

; construeix el candidat de frontera (entry unknown base-dist ball-dist) per una entry
(defun agent-sng656-frontier-candidate (entry entries origin src)
    (let* ((coord (agent-sng656-entry-coord entry))
           ; només les caselles buides poden ser fronteres explorables
           (unknown (if (agent-sng656-is-empty (agent-sng656-entry-cell entry))
                        (agent-sng656-unknown-neighbours coord entries)
                        0))
           (base-dist (agent-sng656-dist coord origin))
           (ball-dist (agent-sng656-dist coord src)))
          (if (zerop unknown)
              ; sense veïns desconeguts, aquesta casella no obre mapa nou
              nil
              ; candidat vàlid per comparar dins best-frontier-target
              (list entry unknown base-dist ball-dist))))

; compara dos candidats de frontera i retorna el millor segons la prioritat definida
(defun agent-sng656-better-frontier (best candidate)
    (cond ; si entry no era frontera, no pot millorar el millor candidat
          ((null candidate) best)
          ; primer candidat de frontera vàlid
          ((null best) candidate)
          ; prioritat principal: màxim nombre de cel·les per descobrir
          ((> (cadr candidate) (cadr best)) candidate)
          ; en empat de cel·les per descobrir, preferim avançar cap enfora de la base
          ((and (= (cadr candidate) (cadr best))
                (> (caddr candidate) (caddr best))) candidate)
          ; si també empata la distància a la base, preferim desplaçaments propers a la cel·la actual
          ((and (= (cadr candidate) (cadr best))
                (= (caddr candidate) (caddr best))
                (< (nth 3 candidate) (nth 3 best))) candidate)
          ; mantenim el millor candidat anterior
          (t best)))

; millor entry d'exploració quan no hi ha enemics/labs a la memòria.
; només considera terra buida amb almenys un veí desconegut. internament, el valor best
; té format (entry unknown base-dist ball-dist), on unknown és el nombre de veïns desconeguts.
; criteri: maximitzar unknown, després maximitzar base-dist i finalment minimitzar ball-dist.
(defun agent-sng656-best-frontier-target (info entries)
    (let* ((src (agent-sng656-info-coord info))
           (team (agent-sng656-info-team info))
           (own-base-entry (agent-sng656-friendly-base entries team))
           (origin (if own-base-entry (agent-sng656-entry-coord own-base-entry) src)))
          (car (reduce (lambda (best entry) (agent-sng656-better-frontier best (agent-sng656-frontier-candidate entry entries origin src)))
                       entries
                       :initial-value nil))))

; millor objectiu tàctic conegut: enemic o laboratori no propi
(defun agent-sng656-best-tactical-target (info entries)
    (agent-sng656-best-entry entries
                              (agent-sng656-info-coord info)
                              (agent-sng656-info-team info)))

; objectiu enemic conegut per activar el mode atacant: base o bolla rival, no labs
(defun agent-sng656-best-enemy-target (info entries)
    (let ((team (agent-sng656-info-team info)))
         (agent-sng656-best-entry (append (agent-sng656-enemy-bases entries team)
                                          (agent-sng656-enemy-balls entries team))
                                  (agent-sng656-info-coord info)
                                  team)))

; recorre els 8 offsets veïns recursivament i tria el que minimitza d² al target
; cada candidat ha de ser terra buida i present a entries (això garanteix que sigui dins del mapa)
; acumulador best té format (coord d²)
(defun agent-sng656-best-step-rec (src entries target offsets best)
    (cond ; ja hem avaluat tots els veïns possibles: retornam el millor pas
          ((null offsets) best)
          ; avaluam el veí indicat pel primer offset pendent
          (t (let* ((dst (agent-sng656-add src (car offsets)))
                    (entry (agent-sng656-entry-at entries dst))
                    (cell (agent-sng656-entry-cell entry)))
                    ; només podem moure a la posició de destí si està buida
                   (if (and cell (agent-sng656-is-empty cell))
                       (let* ((d (if target (agent-sng656-dist dst target) 0))
                              (best-new (if (or (null best) (< d (cadr best))) (list dst d) best)))
                             ; veí vàlid: el guardam si acosta més al target
                             (agent-sng656-best-step-rec src entries target (cdr offsets) best-new))
                        ; veí invalid o desconegut: l'ignoram
                        (agent-sng656-best-step-rec src entries target (cdr offsets) best))))))

; versió simple d'un pas greedy cap a qualsevol objectiu de moviment
(defun agent-sng656-step-towards (src entries target)
    (let ((best (agent-sng656-best-step-rec src entries target AGENT-SNG656-NEIGH-OFFSETS nil)))
         (if best (car best) nil)))

; direcció estable d'exploració segons id-unitat mod 8
(defun agent-sng656-explore-offset (info)
    (nth (mod (agent-sng656-info-id info) (length AGENT-SNG656-NEIGH-OFFSETS))
         AGENT-SNG656-NEIGH-OFFSETS))

; tria el millor veí exploratori: terra buida amb més veïns desconeguts
(defun agent-sng656-best-unknown-neighbour-rec (src entries offsets best)
    (cond ((null offsets) best)
          (t (let* ((dst (agent-sng656-add src (car offsets)))
                    (entry (agent-sng656-entry-at entries dst))
                    (cell (agent-sng656-entry-cell entry)))
                   (if (and cell (agent-sng656-is-empty cell))
                       (let* ((unknown (agent-sng656-unknown-neighbours dst entries))
                              (best-new (if (or (null best) (> unknown (cadr best)))
                                            (list dst unknown)
                                            best)))
                             (agent-sng656-best-unknown-neighbour-rec src entries (cdr offsets) best-new))
                       (agent-sng656-best-unknown-neighbour-rec src entries (cdr offsets) best))))))

(defun agent-sng656-best-unknown-neighbour (src entries)
    (let ((best (agent-sng656-best-unknown-neighbour-rec src entries AGENT-SNG656-NEIGH-OFFSETS nil)))
         (if best (car best) nil)))

; pas exploratori: prova la direcció preferida; si està bloquejada, tria el veí que obre més mapa
(defun agent-sng656-directional-step (info entries)
    (let* ((dst (agent-sng656-add (agent-sng656-info-coord info)
                                  (agent-sng656-explore-offset info)))
            (entry (agent-sng656-entry-at entries dst))
            (cell (agent-sng656-entry-cell entry)))
          (if (and cell (agent-sng656-is-empty cell))
              dst
              (agent-sng656-best-unknown-neighbour (agent-sng656-info-coord info) entries))))

; pas greedy que només accepta acostar-se realment al target
(defun agent-sng656-step-closer (src entries target)
    (let ((step (agent-sng656-step-towards src entries target)))
         (if (and step (< (agent-sng656-dist step target) (agent-sng656-dist src target)))
             step
             nil)))

; moviment d'exploració cap a la millor frontera coneguda
(defun agent-sng656-frontier-step (info entries)
    (let* ((src (agent-sng656-info-coord info))
           (target (agent-sng656-best-frontier-target info entries))
           (target-coord (if target (agent-sng656-entry-coord target) nil)))
          (if target-coord
              (agent-sng656-step-towards src entries target-coord)
              nil)))

; moviment greedy: explora en direcció fixa fins que hi ha objectiu enemic; llavors ataca
(defun agent-sng656-best-move-step (info entries)
    (let* ((src (agent-sng656-info-coord info))
            (target (agent-sng656-best-enemy-target info entries))
            (target-coord (if target (agent-sng656-entry-coord target) nil)))
           (cond ((null target-coord)
                  (agent-sng656-directional-step info entries))
                 ; si ja podem pintar l'objectiu, no ens n'allunyam
                 ((<= (agent-sng656-dist src target-coord) AGENT-SNG656-PAINT-RANGE)
                  nil)
                 ; si l'atac greedy queda bloquejat, no tornam a explorar mentre hi ha enemic conegut
                 (t (let ((step (agent-sng656-step-closer src entries target-coord)))
                        step)))))

; estratègia de la bolla:
;   - intenta pintar el millor objectiu en rang (si tr-paint < 1)
;   - intenta moure cap a l'objectiu de més puntuació (si tr-move < 1)
(defun agent-sng656-ball (info entries)
    (let* ((tr-paint (agent-sng656-info-tr-paint info))
           (tr-move  (agent-sng656-info-tr-move  info))
           ; només cercam tret si el temps de recuperació permet pintar
           (paint-dst (and (< tr-paint 1) (agent-sng656-best-paint-target info entries)))
           ; només cercam moviment si el temps de recuperació permet moure
           (move-step (and (< tr-move 1) (agent-sng656-best-move-step info entries)))
           (paint-action (if paint-dst
                              (list (list 'PINTA (list (agent-sng656-entry-coord paint-dst))))
                              nil))
           (move-action  (if move-step
                              (list (list 'MOU (list move-step)))
                              nil)))
        (append paint-action move-action)))

; **************************************************
; ENTRADA DE L'AGENT
; **************************************************

; crea noves accions segons el tipus d'unitat:
;   1. fusiona la visió actual ampliada amb vores i la memòria
;   2. emet ESCRIU-MEMORIA amb la nova memòria fusionada perquè la resta de l'equip
;      tenguin la mateixa visió en el pròxim torn
;   3. crida l'estratègia corresponent passant les entries fusionades
(defun agent-sng656 (info)
    (let* ((unit (agent-sng656-info-unit info))
           (turn (agent-sng656-info-turn info))
           ; fusionam visió i memòria abans de decidir, així cada unitat usa el millor mapa conegut
           (entries (agent-sng656-merge-vision
                         (agent-sng656-known-cells info)
                         turn
                         (agent-sng656-entries-drop-old (agent-sng656-info-memory info) turn)))
           ; sempre reescrivim la memòria amb la versió fusionada
           (mem-new (list (list 'ESCRIU-MEMORIA (list entries))))
           (actions (cond ; la base només decideix creació de bolles
                         ((eq unit AGENT-SNG656-BASE) (agent-sng656-base info entries))
                         ; les bolles poden pintar i moure's
                         ((eq unit AGENT-SNG656-BALL) (agent-sng656-ball info entries))
                         ; tipus desconegut: no feim cap acció específica
                         (t nil))))
        (append mem-new actions)))
