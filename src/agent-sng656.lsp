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
;;   sobreescriuen les antigues i les entrades massa velles es descarten. Això permet
;;   que les bolles aprofitin el mapa descobert per altres unitats sense assumir que
;;   la informació antiga encara és completament fiable.
;; - La base crea bolles només si té pintura suficient i hi ha una casella adjacent
;;   buida, tal com exigeix la lògica del joc. El color de la nova bolla es tria segons
;;   els colors que falten als enemics coneguts: les bases enemigues pesen més que
;;   les bolles perquè destruir la base és l'objectiu principal. Si no hi ha enemics
;;   coneguts, la base alterna colors per mantenir varietat.
;; - Les bolles primer intenten pintar el millor objectiu dins rang: base enemiga,
;;   bolla enemiga, laboratori o casella útil per pintar camí. La puntuació baixa
;;   amb l'antiguitat de la informació perquè una observació recent és més valuosa
;;   que una posició antiga de memòria.
;; - Per moure's, les bolles ataquen primer objectius tàctics coneguts. Si no hi ha
;;   enemics ni laboratoris, les bolles exploradores cerquen fronteres: caselles de
;;   terra buida conegudes amb veïns desconeguts. La millor frontera és la que obre
;;   més cel·les desconegudes; en empat, es prefereix anar més lluny de la nostra
;;   base i, finalment, triar l'opció més propera a la bolla actual. Això evita que
;;   les bolles oscil·lin prop de la base quan no hi ha objectius.
;; - Una fracció simple de bolles es marca com a defensora amb el residu de l'id.
;;   És una heurística barata, no un recompte exacte per equip, perquè els id són
;;   globals i també avancen quan l'enemic crea bolles. Les defensores només surten
;;   cap a bolles enemigues properes a la nostra base; si no hi ha amenaça, no fan
;;   exploració ofensiva.
;; - Abans de qualsevol altre moviment, una bolla adjacent a la base pròpia intenta
;;   allunyar-se de la base. Aquesta evacuació de l'anell de creació evita bloquejar
;;   les 8 caselles on la base pot crear noves bolles i manté la producció activa.

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
(defconstant AGENT-SNG656-DEFENDER-MOD  8)
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
        (append new (remove-if (lambda (e) (agent-sng656-entry-at new (agent-sng656-entry-coord e))) mem))))

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
               (dst-cell (agent-sng656-entry-cell dst-entry)))
               (if (and dst-cell (agent-sng656-is-empty dst-cell))
                    ; veí vàlid: la base hi pot crear una bolla
                    dst
                    ; veí no serveix: provam el següent offset
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
            ; hi ha enemics coneguts: triem el color que més falta als objectius
            (nth idx AGENT-SNG656-RGB)
            ; no tenim informació ofensiva: alternam colors per no produir sempre igual
            (nth (mod (agent-sng656-info-turn info) (length AGENT-SNG656-RGB)) AGENT-SNG656-RGB))))

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

; millor objectiu d'exploració quan no hi ha enemics/labs a la memòria.
; només considera terra buida amb almenys un veí desconegut. el valor best té format
; (entry unknown base-dist ball-dist), on unknown és el nombre de veïns desconeguts.
; criteri: maximitzar unknown, després maximitzar base-dist i finalment minimitzar ball-dist.
(defun agent-sng656-best-frontier-target (info entries)
    (let* ((src (agent-sng656-info-coord info))
           (team (agent-sng656-info-team info))
           (own-base-entry (agent-sng656-friendly-base entries team))
           (origin (if own-base-entry (agent-sng656-entry-coord own-base-entry) src)))
          (reduce (lambda (best entry) (agent-sng656-better-frontier best (agent-sng656-frontier-candidate entry entries origin src)))
                  entries
                  :initial-value nil)))

; objectiu defensiu per bolles marcades com a defensores.
; cerca bolles enemigues dins defense-range de la nostra base coneguda. si encara no
; coneixem la base pròpia dins la memòria, usa la posició actual com a origen.
; retorna nil si no hi ha amenaça propera, així el defensor no s'allunya explorant.
(defun agent-sng656-best-defense-target (info entries)
    (let* ((team (agent-sng656-info-team info))
           (own-base-entry (agent-sng656-friendly-base entries team))
           ; normalment defensam al voltant de la base; si no la coneixem, defensam la zona actual
           (origin (if own-base-entry
                       (agent-sng656-entry-coord own-base-entry)
                       (agent-sng656-info-coord info)))
           ; filtram només les bolles enemigues prou properes per ser una amenaça real
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
    (cond
          ; bolles defensores
          ((agent-sng656-defender info) (agent-sng656-best-defense-target info entries))
          ; bolles atacants/exploradores
          (t (let ((target (agent-sng656-best-tactical-target info entries)))
                (if target
                    ; si hi ha enemic/lab/casella útil, aquest objectiu domina l'exploració
                    target
                    ; sense objectiu tàctic, cercam una frontera per descobrir mapa
                    (agent-sng656-best-frontier-target info entries))))))

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

; si una bolla ocupa l'anell adjacent de la base, tria un pas que l'allunyi de la base
; per deixar espai lliure a noves creacions. best té format (coord base-dist).
(defun agent-sng656-base-escape-step (src entries origin)
    (let ((best (reduce (lambda (best offset)
                    (let* ((dst (agent-sng656-add src offset))
                           (entry (agent-sng656-entry-at entries dst))
                           (cell (agent-sng656-entry-cell entry))
                           (base-dist (agent-sng656-dist dst origin)))
                          (cond ; només ens podem moure cap a caselles buides
                                ((or (null cell) (not (agent-sng656-is-empty cell))) best)
                                ; primera acció vàlida trobada
                                ((null best) (list dst base-dist))
                                ; preferim l'acció que deixa la bolla més lluny de la base
                                ((> base-dist (cadr best)) (list dst base-dist))
                                ; l'acció actual no millora l'anterior
                                (t best))))
                  AGENT-SNG656-NEIGH-OFFSETS
                  :initial-value nil)))
        (if best
            ; retornam només la coordenada la millor acció
            (car best)
            ; no hi ha cap veí valid per sortir de l'anell
            nil)))

; moviment prioritari per buidar l'anell de creació de la base pròpia
(defun agent-sng656-spawn-escape-step (info entries)
    (let* ((src (agent-sng656-info-coord info))
           (team (agent-sng656-info-team info))
           (own-base-entry (agent-sng656-friendly-base entries team))
           (origin (if own-base-entry (agent-sng656-entry-coord own-base-entry) nil)))
          (if (and origin (<= (agent-sng656-dist src origin) 2))
              ; som dins l'anell de creació: prioritzam alliberar una casella adjacent
              (agent-sng656-base-escape-step src entries origin)
              ; no bloquejam la base o no coneixem la base: cap moviment especial
              nil)))

; selecciona la coord del veí cap a on moure'ns; nil si no hi ha cap veí buit visible
(defun agent-sng656-best-move-step (info entries)
    (let ((escape (agent-sng656-spawn-escape-step info entries)))
        (if escape
            ; l'evacuació de l'anell de creació té prioritat sobre atacar o explorar
            escape
            (let* ((src (agent-sng656-info-coord info))
                   (target (agent-sng656-best-target info entries))
                   (target-coord (if target (agent-sng656-entry-coord (car target)) nil)))
                (if target-coord
                    ; hi ha objectiu: triam el millor veí que s'hi acosta
                    (let ((best (agent-sng656-best-step-rec src entries target-coord
                                                            AGENT-SNG656-NEIGH-OFFSETS nil)))
                        (if best
                            ; retornam només la coordenada del pas triat
                            (car best)
                            ; cap veí valid permet avançar cap a l'objectiu
                            nil))
                    ; no hi ha objectiu defensiu/ofensiu/exploratori
                    nil)))))


; estratègia de la bolla:
;   - intenta pintar el millor objectiu en rang (si tr-paint < 1)
;   - intenta moure cap a l'objectiu de més puntuació (si tr-move < 1)
(defun agent-sng656-ball (info entries)
    (let* ((tr-paint (agent-sng656-info-tr-paint info))
           (tr-move  (agent-sng656-info-tr-move  info))
           ; només cercam tret si el temps de recuperació permet pintar
           (paint-dst (and (< tr-paint 1) (agent-sng656-best-paint-target info entries)))
           ; només cercam moviment si el temps de recuperació permet moure
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

; crea noves accions segons el tipus d'unitat:
;   1. fusiona la visió actual amb la memòria, descartant entrades antigues
;   2. emet ESCRIU-MEMORIA amb la nova memòria fusionada perquè la resta de l'equip
;      tenguin la mateixa visió en el pròxim torn
;   3. crida l'estratègia corresponent passant les entries fusionades
(defun agent-sng656 (info)
    (let* ((unit (agent-sng656-info-unit info))
           (turn (agent-sng656-info-turn info))
           ; fusionam visió i memòria abans de decidir, així cada unitat usa el millor mapa conegut
           (entries (agent-sng656-merge-vision
                        (agent-sng656-info-vision info)
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
