;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer de l'agent intel·ligent JGR448.
;;
;; Estratègia general de l'agent:
;; - Cada unitat combina la seva visió actual amb la memòria compartida de l'equip.
;;   La memòria desa entrades (torn cel·la), de manera que les observacions recents
;;   sobreescriuen les antigues. Les cel·les que falten dins el rang de visió es
;;   guarden com a (coord nil) per no explorar més enllà de les vores. Les entrades
;;   antigues només es descarten si contenien bolles, perquè són l'únic element que es
;;   mou.
;; - La base crea bolles només si té pintura suficient i hi ha una casella adjacent
;;   buida, tal com exigeix la lògica del joc. El color de la nova bolla rota entre
;;   r/g/b segons el torn, sense analitzar la memòria, per mantenir l'agent lleuger.
;; - Les bolles primer intenten pintar el millor objectiu dins rang: base enemiga,
;;   laboratori o bolla enemiga. La puntuació baixa amb l'antiguitat de la informació
;;   perquè una observació recent és més valuosa que una posició antiga de memòria.
;; - Per moure's, les bolles ataquen primer objectius tàctics coneguts. Si no hi ha
;;   enemics ni laboratoris, només miren les 8 caselles adjacents i trien la buida que
;;   obre més veïns desconeguts. Si poden pintar, no calculen moviment aquell torn.

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

; antiguitat màxima (en torns) abans de descartar una entrada de la memòria
(defconstant AGENT-JGR448-AGE-MAX 50)

; offsets (dx dy) dels 8 veïns adjacents (d² ≤ 2, excloent (0 0))
; emprat tant per moviment com per creació de bolla
(defconstant AGENT-JGR448-NEIGH-OFFSETS
    '((-1 -1) ( 0 -1) ( 1 -1)
      (-1  0)         ( 1  0)
      (-1  1) ( 0  1) ( 1 1)))

; offsets (dx dy) dins la visió d'una bolla (d² ≤ 20)
(defconstant AGENT-JGR448-BALL-VISION-OFFSETS
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
(defun agent-jgr448-add   (a b)
    "Suma element a element les llistes 'a' i 'b'."
    (mapcar '+ a b))
(defun agent-jgr448-mul   (a b)
    "Multiplica element a element les llistes 'a' i 'b'."
    (mapcar '* a b))
(defun agent-jgr448-sub   (a b)
    "Resta element a element la llista 'b' de la llista 'a'."
    (mapcar '- a b))
(defun agent-jgr448-sum   (a)
    "Retorna la suma dels elements de la llista 'a'."
    (reduce '+ a :initial-value 0))

(defun agent-jgr448-bool-int (x)
    "Converteix el booleà Lisp 'x' en 0 o 1."
    (if x 1 0))

(defun agent-jgr448-dist (a b)
    "Calcula la distància euclidiana al quadrat entre les coordenades desplaçades 'a' i 'b'."
    (let ((d (agent-jgr448-sub a b)))
         (agent-jgr448-sum (agent-jgr448-mul d d))))

; **************************************************
; INFORMACIÓ DE L'AGENT
; **************************************************

; format de info:
;   (ronda equip pintura id-unitat tipus-unitat coordenada
;    colors-pintat color-propi tr-pintar tr-moure visió memòria-compartida)
(defun agent-jgr448-info-turn      (info)
    "Retorna el torn de la llista d'informació 'info'."
    (nth 0  info))
(defun agent-jgr448-info-team      (info)
    "Retorna l'equip de la llista d'informació 'info'."
    (nth 1  info))
(defun agent-jgr448-info-paint     (info)
    "Retorna la pintura disponible de la llista d'informació 'info'."
    (nth 2  info))
(defun agent-jgr448-info-unit      (info)
    "Retorna el tipus d'unitat de la llista d'informació 'info'."
    (nth 4  info))
(defun agent-jgr448-info-coord     (info)
    "Retorna la coordenada visible de la llista d'informació 'info'."
    (nth 5  info))
(defun agent-jgr448-info-tr-paint  (info)
    "Retorna el cooldown de pintar de la llista d'informació 'info'."
    (nth 8  info))
(defun agent-jgr448-info-tr-move   (info)
    "Retorna el cooldown de moviment de la llista d'informació 'info'."
    (nth 9  info))
(defun agent-jgr448-info-vision    (info)
    "Retorna la visió actual de la llista d'informació 'info'."
    (nth 10 info))
(defun agent-jgr448-info-memory    (info)
    "Retorna la memòria compartida de la llista d'informació 'info'."
    (nth 11 info))

; **************************************************
; VISIÓ
; **************************************************

; format de longitud variable segons el contingut:
;        aigua: (coord AIGUA)
;  terra buida: (coord TERRA color)
;          lab: (coord TERRA color LAB equip)
;         base: (coord TERRA color BASE equip colors-pintat)
;        bolla: (coord TERRA color BOLLA equip colors-pintat color-propi tr-pintar tr-moure)
(defun agent-jgr448-cell-coord    (cell)
    "Retorna la coordenada de la cel·la visible 'cell'."
    (nth 0 cell))
(defun agent-jgr448-cell-type     (cell)
    "Retorna el tipus de terreny de la cel·la visible 'cell'."
    (nth 1 cell))
(defun agent-jgr448-cell-element  (cell)
    "Retorna l'element contingut a la cel·la visible 'cell'."
    (nth 3 cell))
(defun agent-jgr448-cell-team     (cell)
    "Retorna l'equip de l'element de la cel·la visible 'cell'."
    (nth 4 cell))

; predicats sobre cel·les del mapa
(defun agent-jgr448-is-land  (cell)
    "Retorna cert si 'cell' és una cel·la de terra."
    (eq (agent-jgr448-cell-type cell) AGENT-JGR448-LAND))
(defun agent-jgr448-is-lab   (cell)
    "Retorna cert si 'cell' conté un laboratori."
    (eq (agent-jgr448-cell-element cell) AGENT-JGR448-LAB))
(defun agent-jgr448-is-base  (cell)
    "Retorna cert si 'cell' conté una base."
    (eq (agent-jgr448-cell-element cell) AGENT-JGR448-BASE))
(defun agent-jgr448-is-ball  (cell)
    "Retorna cert si 'cell' conté una bolla."
    (eq (agent-jgr448-cell-element cell) AGENT-JGR448-BALL))
(defun agent-jgr448-is-empty (cell)
    "Retorna cert si 'cell' és terra i no conté cap element."
    (and (agent-jgr448-is-land cell) (null (agent-jgr448-cell-element cell))))

; **************************************************
; MEMÒRIA / VISIÓ COMPARTIDA
; **************************************************

; entrada de la memòria: (turn cell)
;   turn:  torn en què es va observar la cel·la per darrera vegada
;   cell:  cel·la (mateix format que info-vision)
(defun agent-jgr448-entry-make  (turn cell)
    "Crea una entrada de memòria amb el torn 'turn' i la cel·la visible 'cell'."
    (list turn cell))
(defun agent-jgr448-entry-turn  (e)
    "Retorna el torn de l'entrada de memòria 'e'."
    (car  e))
(defun agent-jgr448-entry-cell  (e)
    "Retorna la cel·la visible de l'entrada de memòria 'e'."
    (cadr e))
(defun agent-jgr448-entry-coord (e)
    "Retorna la coordenada de la cel·la de l'entrada de memòria 'e'."
    (agent-jgr448-cell-coord (agent-jgr448-entry-cell e)))

(defun agent-jgr448-entry-at (entries coord)
    "Cerca dins 'entries' l'entrada amb coordenada 'coord', o nil si no hi és."
    (cond ((null entries) nil)
          ((equal (agent-jgr448-entry-coord (car entries)) coord) (car entries))
          (t (agent-jgr448-entry-at (cdr entries) coord))))

(defun agent-jgr448-cell-at (cells coord)
    "Cerca dins 'cells' la cel·la amb coordenada 'coord', o nil si no hi és."
    (cond ((null cells) nil)
          ((equal (agent-jgr448-cell-coord (car cells)) coord) (car cells))
          (t (agent-jgr448-cell-at (cdr cells) coord))))

(defun agent-jgr448-edge-cells (src vision offsets)
    "Crea cel·les sintètiques (coord nil) per offsets visibles des de 'src' que no apareixen a 'vision'."
    (if (null offsets)
        nil
        (let ((coord (agent-jgr448-add src (car offsets))))
             (if (agent-jgr448-cell-at vision coord)
                 (agent-jgr448-edge-cells src vision (cdr offsets))
                 (cons (list coord nil)
                       (agent-jgr448-edge-cells src vision (cdr offsets)))))))

(defun agent-jgr448-known-cells (info)
    "Retorna la visió actual d''info' ampliada amb vores del mapa inferides."
    (let ((vision (agent-jgr448-info-vision info)))
         (append vision
                 (agent-jgr448-edge-cells
                     (agent-jgr448-info-coord info)
                     vision
                     AGENT-JGR448-BALL-VISION-OFFSETS))))

(defun agent-jgr448-merge-vision (vision turn mem)
    "Combina 'vision', etiquetada amb 'turn', amb 'mem'; les entrades més recents sobreescriuen les antigues."
    (let ((new (mapcar (lambda (c) (agent-jgr448-entry-make turn c)) vision)))
        (append new (remove-if (lambda (e) (agent-jgr448-entry-at new (agent-jgr448-entry-coord e))) mem))))

(defun agent-jgr448-entries-drop-old (entries turn)
    "Descarta de 'entries' les bolles més antigues que AGENT-JGR448-AGE-MAX respecte 'turn'."
    (remove-if (lambda (e)
                   (and (agent-jgr448-is-ball (agent-jgr448-entry-cell e))
                        (> (- turn (agent-jgr448-entry-turn e)) AGENT-JGR448-AGE-MAX)))
               entries))

; **************************************************
; ESTRATÈGIA DE LA BASE
; **************************************************

(defun agent-jgr448-find-empty-neighbour (src entries offsets)
    "Recorre 'offsets' i retorna la primera coordenada veïna de 'src' que és terra buida dins 'entries'."
    (if (null offsets)
        nil
        (let* ((dst (agent-jgr448-add src (car offsets)))
               (dst-entry (agent-jgr448-entry-at entries dst))
               (dst-cell (agent-jgr448-entry-cell dst-entry)))
               (if (and dst-cell (agent-jgr448-is-empty dst-cell))
                    ; veí vàlid: la base hi pot crear una bolla
                    dst
                    ; veí no serveix: provam el següent offset
                    (agent-jgr448-find-empty-neighbour src entries (cdr offsets))))))

; tria un color per crear la bolla rotant entre r/g/b segons el torn
(defun agent-jgr448-base-pick-color (info)
    "Tria el color d'una bolla nova rotant uniformement segons el torn d''info'."
    (nth (mod (agent-jgr448-info-turn info) (length AGENT-JGR448-RGB)) AGENT-JGR448-RGB))

; estratègia de la base:
;   - si tenim menys pintura que el cost d'una bolla, no fem res
;   - si tenim un veí buit, hi creem una bolla amb el color que toca per rotació
;   - altrament, no fem res
(defun agent-jgr448-base (info entries)
    "Decideix les accions de base per la unitat descrita per 'info' usant les entrades 'entries'."
    (cond ; sense pintura suficient no intentam crear cap bolla
          ((< (agent-jgr448-info-paint info) AGENT-JGR448-BALL-COST) nil)
          ; tenim pintura: cercam una casella adjacent buida on la creació sigui vàlida
          (t (let ((dst (agent-jgr448-find-empty-neighbour
                             (agent-jgr448-info-coord info)
                             entries
                             AGENT-JGR448-NEIGH-OFFSETS)))
                (if dst
                    ; hi ha lloc lliure: cream una bolla amb el color de la rotació
                    (list (list 'CREA-BOLLA (list (agent-jgr448-base-pick-color info) dst)))
                    ; l'anell de creació està ple
                    nil)))))

; **************************************************
; ESTRATÈGIA DE LA BOLLA
; **************************************************

; puntuació base d'una cel·la com a objectiu (sense tenir en compte l'antiguitat):
;   base enemiga: 800  lab no nostre: 300  bolla enemiga: 120
;   altre: 0
(defun agent-jgr448-score-cell (cell team)
    "Retorna la puntuació tàctica base de 'cell' com a objectiu per l'equip 'team'."
    (cond ((eq (agent-jgr448-cell-team cell) team) 0)
          ((agent-jgr448-is-base cell) 800)
          ((agent-jgr448-is-lab  cell) 300)
          ((agent-jgr448-is-ball cell) 120)
          (t 0)))

; puntuació d'una entrada: la base menys l'antiguitat (cap a 0 mai)
; així informació recent és preferida sense descartar del tot la antiga
(defun agent-jgr448-score-entry (entry team turn)
    "Retorna la puntuació de l'entrada 'entry' per 'team' al torn 'turn', penalitzada per antiguitat."
    (let ((age (- turn (agent-jgr448-entry-turn entry)))
          (raw (agent-jgr448-score-cell (agent-jgr448-entry-cell entry) team)))
         (max 0 (- raw age))))

(defun agent-jgr448-best-entry (entries src team turn)
    "Retorna la millor entrada de 'entries' per 'team' des de 'src': més puntuació i després més proximitat."
    (car (reduce (lambda (best e)
                    (let* ((s (agent-jgr448-score-entry e team turn))
                           (d (agent-jgr448-dist (agent-jgr448-entry-coord e) src)))
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

(defun agent-jgr448-best-paint-target (info entries)
    "Retorna el millor objectiu dins rang de pintar per la unitat descrita per 'info'."
    (let* ((src (agent-jgr448-info-coord info))
           (team (agent-jgr448-info-team info))
           (turn (agent-jgr448-info-turn info))
           (in-range (remove-if (lambda (e)
                                     (> (agent-jgr448-dist (agent-jgr448-entry-coord e) src)
                                        AGENT-JGR448-PAINT-RANGE))
                                 entries)))
        (agent-jgr448-best-entry in-range src team turn)))

(defun agent-jgr448-unknown-neighbours (coord entries)
    "Compta quants veïns de 'coord' encara no apareixen dins la memòria compartida 'entries'."
    (agent-jgr448-sum (mapcar
        (lambda (off) (agent-jgr448-bool-int
            (null (agent-jgr448-entry-at entries (agent-jgr448-add coord off)))))
        AGENT-JGR448-NEIGH-OFFSETS)))

(defun agent-jgr448-best-tactical-target (info entries)
    "Retorna el millor objectiu tàctic conegut dins 'entries' per la unitat descrita per 'info'."
    (agent-jgr448-best-entry entries
                             (agent-jgr448-info-coord info)
                             (agent-jgr448-info-team info)
                             (agent-jgr448-info-turn info)))

; recorre els 8 offsets veïns recursivament i tria el que minimitza d² al target
; cada candidat ha de ser terra buida i present a entries (això garanteix que sigui dins del mapa)
; acumulador best té format (coord d²)
(defun agent-jgr448-best-step-rec (src entries target offsets best)
    "Avalua recursivament 'offsets' i retorna el millor pas des de 'src' cap a 'target'."
    (cond ; ja hem avaluat tots els veïns possibles: retornam el millor pas
          ((null offsets) best)
          ; avaluam el veí indicat pel primer offset pendent
          (t (let* ((dst (agent-jgr448-add src (car offsets)))
                    (entry (agent-jgr448-entry-at entries dst))
                    (cell (agent-jgr448-entry-cell entry)))
                    ; només podem moure a la posició de destí si està buida
                   (if (and cell (agent-jgr448-is-empty cell))
                       (let* ((d (if target (agent-jgr448-dist dst target) 0))
                              (best-new (if (or (null best) (< d (cadr best))) (list dst d) best)))
                             ; veí vàlid: el guardam si acosta més al target
                             (agent-jgr448-best-step-rec src entries target (cdr offsets) best-new))
                        ; veí invalid o desconegut: l'ignoram
                        (agent-jgr448-best-step-rec src entries target (cdr offsets) best))))))

(defun agent-jgr448-step-towards (src entries target)
    "Retorna un pas greedy des de 'src' cap a 'target' usant les cel·les conegudes 'entries'."
    (let ((best (agent-jgr448-best-step-rec src entries target AGENT-JGR448-NEIGH-OFFSETS nil)))
         (if best (car best) nil)))

(defun agent-jgr448-step-closer (src entries target)
    "Retorna un pas greedy des de 'src' només si redueix la distància a 'target'."
    (let ((step (agent-jgr448-step-towards src entries target)))
         (if (and step (< (agent-jgr448-dist step target) (agent-jgr448-dist src target)))
             step
             nil)))

; exploració local lleugera: només mira les 8 caselles adjacents i tria la que obre més desconegut.
; best té format (coord unknown); en empat es manté el primer veí segons AGENT-JGR448-NEIGH-OFFSETS.
(defun agent-jgr448-local-explore-rec (src entries offsets best)
    "Avalua els veïns de 'src' dins 'offsets' i retorna el pas buit que obre més veïns desconeguts."
    (if (null offsets)
        best
        (let* ((dst (agent-jgr448-add src (car offsets)))
               (entry (agent-jgr448-entry-at entries dst))
               (cell (agent-jgr448-entry-cell entry))
               (unknown (if (and cell (agent-jgr448-is-empty cell))
                            (agent-jgr448-unknown-neighbours dst entries)
                            0))
               (best-new (if (and (> unknown 0)
                                  (or (null best) (> unknown (cadr best))))
                             (list dst unknown)
                             best)))
              (agent-jgr448-local-explore-rec src entries (cdr offsets) best-new))))

(defun agent-jgr448-local-explore-step (info entries)
    "Retorna un pas d'exploració local per la unitat descrita per 'info', o nil si no n'hi ha."
    (let ((best (agent-jgr448-local-explore-rec
                    (agent-jgr448-info-coord info)
                    entries
                    AGENT-JGR448-NEIGH-OFFSETS
                    nil)))
         (if best (car best) nil)))

(defun agent-jgr448-best-move-step (info entries)
    "Retorna el millor pas de moviment tàctic o exploratori per la unitat descrita per 'info'."
    (let* ((src (agent-jgr448-info-coord info))
           (target (agent-jgr448-best-tactical-target info entries))
           (target-coord (if target (agent-jgr448-entry-coord target) nil)))
          (cond ((null target-coord)
                 (agent-jgr448-local-explore-step info entries))
                ; si ja podem pintar l'objectiu, no ens n'allunyam
                ((<= (agent-jgr448-dist src target-coord) AGENT-JGR448-PAINT-RANGE)
                 nil)
                ; si l'atac greedy queda bloquejat, feim una exploració local barata
                (t (let ((step (agent-jgr448-step-closer src entries target-coord)))
                       (if step step (agent-jgr448-local-explore-step info entries)))))))

(defun agent-jgr448-ball (info entries)
    "Decideix accions de bolla per 'info': pinta si pot; només calcula moviment si no pinta."
    (let* ((tr-paint (agent-jgr448-info-tr-paint info))
           (tr-move  (agent-jgr448-info-tr-move  info))
           ; només cercam tret si el temps de recuperació permet pintar
           (paint-dst (and (< tr-paint 1) (agent-jgr448-best-paint-target info entries)))
           (paint-action (if paint-dst
                              (list (list 'PINTA (list (agent-jgr448-entry-coord paint-dst))))
                              nil)))
        (if paint-action
            paint-action
            (let ((move-step (and (< tr-move 1) (agent-jgr448-best-move-step info entries))))
                 (if move-step
                     (list (list 'MOU (list move-step)))
                     nil)))))

; **************************************************
; ENTRADA DE L'AGENT
; **************************************************

; crea noves accions segons el tipus d'unitat:
;   1. fusiona la visió actual ampliada amb vores i la memòria
;   2. emet ESCRIU-MEMORIA amb la nova memòria fusionada perquè la resta de l'equip
;      tenguin la mateixa visió en el pròxim torn
;   3. crida l'estratègia corresponent passant les entries fusionades
(defun agent-jgr448 (info)
    "Retorna les accions de l'agent JGR448 per la unitat descrita per 'info'."
    (let* ((unit (agent-jgr448-info-unit info))
           (turn (agent-jgr448-info-turn info))
           ; fusionam visió i memòria abans de decidir, així cada unitat usa el millor mapa conegut
           (entries (agent-jgr448-merge-vision
                         (agent-jgr448-known-cells info)
                         turn
                         (agent-jgr448-entries-drop-old (agent-jgr448-info-memory info) turn)))
           ; sempre reescrivim la memòria amb la versió fusionada
           (mem-new (list (list 'ESCRIU-MEMORIA (list entries))))
           (actions (cond ; la base només decideix creació de bolles
                         ((eq unit AGENT-JGR448-BASE) (agent-jgr448-base info entries))
                         ; les bolles poden pintar i moure's
                         ((eq unit AGENT-JGR448-BALL) (agent-jgr448-ball info entries))
                         ; tipus desconegut: no feim cap acció específica
                         (t nil))))
        (append mem-new actions)))
