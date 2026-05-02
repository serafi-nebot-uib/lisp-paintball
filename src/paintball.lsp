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
(load "grafics.lsp")
(load "tco.lsp")

; TODO: canviar els noms dels fitxers d'agents pel codi d'aula dels autors
(load "agent-sng656.lsp")
(load "agent-jgr448.lsp")

; **************************************************
; CONSTANTS
; **************************************************

; pintura: quantitat inicial de cada equip i increments per torn (base i per laboratori capturat)
(defconstant PAINT-INIT                 200)
(defconstant PAINT-INC-TURN             2)
(defconstant PAINT-INC-LAB              1)

; identificadors dels dos equips
(defconstant TEAM-1                     'e1)
(defconstant TEAM-2                     'e2)

; tipus de casella i d'element del mapa
(defconstant WATER                      'aigua)
(defconstant LAND                       'terra)
(defconstant LAB                        'lab)
(defconstant BASE                       'base)
(defconstant BALL                       'bolla)
(defconstant UNIT-TYPES                 (list BASE BALL))

; rangs de visió (d²) per a cada tipus d'unitat
(defconstant VISION-BASE                64)
(defconstant VISION-BALL                20)

; colors primaris de la pintura (R, G, B)
(defconstant RGB-R                      'r)
(defconstant RGB-G                      'g)
(defconstant RGB-B                      'b)
(defconstant RGB                        (list RGB-R RGB-G RGB-B))

; paràmetres de l'acció moure d'una bolla:
;   TR:              cost base de cooldown afegit per cada moviment
;   TR-DIAG:         multiplicador de cost si el moviment és diagonal (~√2)
;   TR-DIFF-COLOR:   multiplicador de cost si la cel·la destí no és del color de la bolla
;   RANGE:           rang màxim del moviment (d²) — 8 cel·les adjacents
(defconstant BALL-MOVE-TR               1)
(defconstant BALL-MOVE-TR-DIAG          1.4142)
(defconstant BALL-MOVE-TR-DIFF-COLOR    3)
(defconstant BALL-MOVE-RANGE            2)

; paràmetres de l'acció pintar d'una bolla:
;   TR:              cost base de cooldown afegit per cada acció de pintar
;   RANGE:           rang màxim per a l'acció (d²)
;   TR-DIFF-COLOR:   multiplicador de cost si la cel·la origen no és del color de la bolla
(defconstant BALL-PAINT-TR              3)
(defconstant BALL-PAINT-RANGE           5)
(defconstant BALL-PAINT-TR-DIFF-COLOR   3)

; paràmetres de l'acció crear bolla d'una base:
;   COST:    pintura consumida per crear una bolla nova
;   RANGE:   rang màxim de creació respecte la base (d²) — 8 cel·les adjacents
(defconstant BASE-CREATE-COST           50)
(defconstant BASE-CREATE-RANGE          2)

; identificadors de les accions que retornen els agents (símbols emprats per l'enunciat)
(defconstant ACTION-CREATE-BALL         'CREA-BOLLA)
(defconstant ACTION-MOVE                'MOU)
(defconstant ACTION-PAINT               'PINTA)
(defconstant ACTION-MEM-WRITE           'ESCRIU-MEMORIA)

; nombre màxim de torns abans d'aplicar el desempat (evita partides infinites)
(defconstant TURN-LIMIT                 1500)

; **************************************************
; UTILITATS
; **************************************************

; canvia el valor a la posició n de la llista lst pel valor val
(defun list-set (lst n val)
    (if (zerop n)
        (cons val (cdr lst))
        (cons (car lst) (list-set (cdr lst) (1- n) val))))

; transforma la llista lst de 2D en una de 1D
; (defun flatten (lst)
;     (cond ((null lst) nil)
;           ((atom lst) (list lst))
;           (t (append (flatten (car lst)) (flatten (cdr lst))))))

; operacions aritmètiques i lògiques bàsiques sobre llistes de N elements 
; les funcions només són un "mapping" d'una operació bàsica:
;     es podria escriure manualment cada una d'aquestes operacions a totes les parts del codi
;     però d'aquesta manera es poden expressar operacions de forma curta i clara
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
(defun sum   (a)   (reduce '+ a :initial-value 0))
(defun prod  (a)   (reduce '* a :initial-value 1))

; rèplica de la funció zip() de python: agrupa els elements de múltiples llistes per posició
; (zip '(1 2 3) '(a b c)) => ((1 a) (2 b) (3 c))
; l és una llista de llistes; apply l'expandeix perquè mapcar rebi cada llista com a argument separat
; mapcar itera totes les llistes en paral·lel i aplica #'list a cada grup d'elements de la mateixa posició
; (defun zip   (&rest l) (apply #'mapcar #'list l))

; distància euclidiana al quadrat (d²) entre a i b
; tots els rangs i llindars del joc s'expressen com a d² per evitar arrels quadrades
(defun dist  (a b) (sum (pow (sub a b) 2)))

; genera una llista en el rang [s e]
(defun range (s e) (if (> s e) nil (cons s (range (1+ s) e))))

; genera el producte cartesià entre les llistes l1 i l2
(defun cartesian (l1 l2)
    (if (null l1)
        nil
        (append (mapcar (lambda (x) (list (car l1) x)) l2)
                (cartesian (cdr l1) l2))))

; (defun mapfun (x fns) (mapcar (lambda (f) (funcall f x)) fns))

; converteix un booleà a un enter:
;    nil -> 0
;      t -> 1
(defun bool->int (lst) (mapcar (lambda (x) (if x 1 0)) lst))

; elimina els duplicats de la llista lst
(defun unique (lst)
    (cond ((null lst) nil)
          ((member (car lst) (cdr lst)) (unique (cdr lst)))
          (t (cons (car lst) (unique (cdr lst))))))

(defun neq (a b) (not (eq 'a 'b)))

; **************************************************
; MAP
; **************************************************

; carrega el fitxer maps/<name>.map i construeix l'estructura interna del mapa
(defun map-load (name)
    (let* ((fp (open (format nil "maps/~a.map" name) :direction :input))
           (m (read fp nil nil)))
        (close fp) (map-init-rows m 0)))

; inicialitza recursivament totes les files del mapa, construint l'estructura interna de cada cel·la
(defun map-init-rows (m y)
    (if (< y (map-height m))
        (cons (map-init-row (nth y m) 0 y (map-width m)) (map-init-rows m (1+ y)))
        nil))

; Cada cel·la té com a id la posició lineal (fila*amplada + columna) (y*board_width + x)
(defun map-init-row (row x y w)
    (if (< x w)
        (cons (map-init-cell (nth x row) (+ x (* y w))) (map-init-row row (1+ x) y w))
        nil))

; construeix l'estructura interna d'una cel·la a partir del seu contingut al fitxer de mapa
(defun map-init-cell (cell next-id)
    (cond
        ((cell-type-water cell) (list WATER))
        ((cell-type-land cell)
            (cond   ((cell-has-lab cell)  (list LAND (cell-color cell) LAB  (cell-unit-team cell)))
                    ((cell-has-base cell) (list LAND (cell-color cell) BASE (cell-unit-team cell) next-id '()))
                    ((cell-has-ball cell) (list LAND (cell-color cell) BALL (cell-unit-team cell) next-id '()
                                                     (cell-unit-color cell)
                                                     (cell-unit-tr-paint cell)
                                                     (cell-unit-tr-move cell)))
                    (t (list LAND (cell-color cell)))))))

; retorna l'alçada i l'amplada del mapa, i comprova si la coordenada xy és dins dels seus límits
(defun map-height (m) (length m))
(defun map-width  (m) (length (car m)))
(defun map-bounds (m xy)
    (let ((x (car xy)) (y (cadr xy)))
        (and (>= x 0) (>= y 0) (< x (map-width m)) (< y (map-height m)))))

; llegeix o escriu la cel·la a la posició xy del mapa m
; si val és nil, retorna la cel·la actual; si no, retorna el mapa actualitzat amb el nou valor
(defun map-cell (m xy &optional val)
    (let ((x (car xy)) (y (cadr xy)))
        (if val (list-set m y (list-set (nth y m) x val))
                (nth x (nth y m)))))

; aplica múltiples actualitzacions al mapa m en seqüència; cada upd té format (x y nou-valor)
(defun map-update (m &rest upd)
    (reduce (lambda (m p) (map-cell m (list (car p) (cadr p)) (caddr p))) upd :initial-value m))

; calcula el nombre de cel·les del mapa m que compleixen la condició retornada per fun
(defun map-count (m fun) (sum (mapcar (lambda (row) (count-if fun row)) m)))

; aplica una funció a cada cel·la del mapa m, retornant un mapa nou amb els resultats
(defun map-apply (m fun) (mapcar (lambda (row) (mapcar fun row)) m))

; *************************************************
; CELLS
; **************************************************

#|
    Estructura d'una cel·la
        Aigua: (AIGUA)
        Terra: (TERRA COLOR)
          Lab: (TERRA COLOR LAB   EQUIP)
         Base: (TERRA COLOR BASE  EQUIP ID COLORS-PINTAT)
        Bolla: (TERRA COLOR BOLLA EQUIP ID COLORS-PINTAT COLOR-PROPI TR-PINTAR TR-MOURE)
|#

; accessors de cel·la: segueixen el patró (camp cell) per llegir i (camp cell nou-valor) per escriure
; retornen sempre una nova cel·la (sense mutar)
(defun cell-type          (cell &optional new) (if new (list-set cell 0 new) (nth 0 cell)))
(defun cell-color         (cell &optional new) (if new (list-set cell 1 new) (nth 1 cell)))
(defun cell-unit          (cell &optional new) (if new (list-set cell 2 new) (nth 2 cell)))
(defun cell-unit-team     (cell &optional new) (if new (list-set cell 3 new) (nth 3 cell)))
(defun cell-unit-id       (cell &optional new) (if new (list-set cell 4 new) (nth 4 cell)))
(defun cell-unit-paint    (cell &optional new) (if new (list-set cell 5 (unique new)) (nth 5 cell)))
(defun cell-unit-color    (cell &optional new) (if new (list-set cell 6 new) (nth 6 cell)))
(defun cell-unit-tr-paint (cell &optional new) (if new (list-set cell 7 new) (nth 7 cell)))
(defun cell-unit-tr-move  (cell &optional new) (if new (list-set cell 8 new) (nth 8 cell)))

; predicats sobre el tipus de cel·la i el tipus d'element que conté
(defun cell-type-water    (cell) (eq (cell-type cell) WATER))
(defun cell-type-land     (cell) (eq (cell-type cell) LAND))
(defun cell-has-base      (cell) (eq (cell-unit cell) BASE))
(defun cell-has-lab       (cell) (eq (cell-unit cell) LAB))
(defun cell-has-ball      (cell) (eq (cell-unit cell) BALL))

; comprova si la unitat ha estat pintada dels tres colors (r, g, b) i per tant ha d'explotar
(defun paint-check-all (paint) (and (member RGB-R paint) (member RGB-G paint) (member RGB-B paint)))

; predicats d'estat de la cel·la
(defun cell-owned-by (cell team) (eq (cell-unit-team cell) team))
(defun cell-empty (cell) (null (cell-unit cell)))
(defun cell-land-empty (cell) (and (cell-type-land cell) (cell-empty cell)))
(defun cell-lab-team (cell team) (and (cell-has-lab cell) (cell-owned-by cell team)))

; decrementa en 1 els temps de recuperació de la bolla (tr-pintar i tr-moure), fins a un mínim de 0
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

; aplica pintura del color color sobre la cel·la:
;   - si és un lab, el captura per a l'equip team
;   - si és una base o bolla, afegeix el color a la llista de colors pintats i l'elimina si en té els tres
;   - en qualsevol cas, actualitza el color de la casella
; pre: cell ha de ser de tipus terra; les cel·les d'aigua no es poden pintar
(defun cell-apply-paint (team cell color)
    (if (cell-type-water cell)
        cell
        (let ((cell-new
                (cond
                    ; unitat és un lab -> captura per a l'equip atacant
                    ((cell-has-lab cell) (cell-unit-team cell team))
                    ; unitat és una base o una bolla -> afageix color a pintat i comprova si s'ha d'eliminar
                    ((or (cell-has-base cell) (cell-has-ball cell))
                        (let ((paint (cons color (cell-unit-paint cell))))
                            (if (paint-check-all paint)
                                (subseq cell 0 2) ; conté tota la pintura -> eliminar unitat
                                (cell-unit-paint cell paint)))) ; no conté tota la pintura -> actualitza la cel·la amb el color nou
                    (t cell))))
        (cell-color cell-new color))))

; **************************************************
; GAME STATE
; **************************************************

; constructor i accessors de l'estat global del joc
; estructura: (torn mapa pintura-e1 pintura-e2 dx dy next-id mem-e1 mem-e2)
;   torn:     número de torn actual
;   mapa:     estat actual del mapa
;   pintura:  quantitat de pintura de cada equip
;   dx, dy:   desplaçament de coordenades aplicat a les unitats (aleatori, per ocultar els límits del mapa)
;   next-id:  identificador que s'assignarà a la propera bolla creada
;   mem:      memòria compartida de cada equip (valor arbitrari, escrivible amb ESCRIU-MEMORIA)
(defun state-new (turn m pt1 pt2 dx dy next-id) (list turn m pt1 pt2 dx dy next-id nil nil))
(defun state-turn      (s &optional new) (if new (list-set s 0 new) (nth 0 s)))
(defun state-map       (s &optional new) (if new (list-set s 1 new) (nth 1 s)))
(defun state-paint     (s team &optional new)
    (if new (if (eq team TEAM-1) (list-set s 2 new) (list-set s 3 new))
            (if (eq team TEAM-1) (nth 2 s) (nth 3 s))))
(defun state-dx        (s &optional new) (if new (list-set s 4 new) (nth 4 s)))
(defun state-dy        (s &optional new) (if new (list-set s 5 new) (nth 5 s)))
(defun state-next-id   (s &optional new) (if new (list-set s 6 new) (nth 6 s)))
; memòria compartida per equip; new=nil llegeix, no permet esborrar (cal escriure 'nil explícit
; via list-set a un valor sentinella si es vol distingir, però no fa falta per l'enunciat)
(defun state-mem       (s team &optional new)
    (if new (if (eq team TEAM-1) (list-set s 7 new) (list-set s 8 new))
            (if (eq team TEAM-1) (nth 7 s) (nth 8 s))))

; converteix entre coordenades internes del mapa i coordenades desplaçades visibles pels agents
(defun coord-shift (state xy) (list (+ (car xy) (state-dx state)) (+ (cadr xy) (state-dy state))))
(defun coord-unshift (state xy) (list (- (car xy) (state-dx state)) (- (cadr xy) (state-dy state))))

; **************************************************
; UNITS
; **************************************************

; retorna les posicions internes (x y) de totes les unitats de tipus unit-type de l'equip team
(defun unit-find (m team unit-type)
    (unit-find-rows m team 0 unit-type))

; cerca unitats fila per fila, acumulant les posicions trobades
(defun unit-find-rows (m team y unit-type)
    (cond ((null m) nil)
          (t (append (unit-find-cols (car m) team 0 y unit-type)
                     (unit-find-rows (cdr m) team (1+ y) unit-type)))))

; cerca unitats columna per columna dins d'una fila
(defun unit-find-cols (row team x y unit-type)
    (cond ((null row) nil)
          ((and (eq (cell-unit (car row)) unit-type)
                (cell-owned-by (car row) team))
           (cons (list x y) (unit-find-cols (cdr row) team (1+ x) y unit-type)))
          (t (unit-find-cols (cdr row) team (1+ x) y unit-type))))

; construeix la llista d'informació que s'envia a l'agent per a una unitat
; format: (ronda equip pintura id-unitat tipus-unitat coordenada colors-pintat color-propi
;          tr-pintar tr-moure visió memòria-compartida)
; les coordenades s'envien desplaçades (x+dx, y+dy) per ocultar els límits reals del mapa
(defun unit-info (state team xy)
    (let* ((turn (state-turn state))
           (m (state-map state))
           (cell (map-cell m xy))
           (paint (state-paint state team))
           (id (cell-unit-id cell))
           (unit (cell-unit cell))
           (coord (coord-shift state xy))
           (paint-list (cell-unit-paint cell))
           (own-color (cell-unit-color cell))
           (tr-paint (cell-unit-tr-paint cell))
           (tr-move (cell-unit-tr-move cell))
           (vision-range (if (eq unit BASE) VISION-BASE VISION-BALL))
           (vis-coords (unit-vision m xy vision-range))
           (vis (mapcar (lambda (vxy) (unit-vision-format state vxy)) vis-coords))
           (mem (state-mem state team)))
        (list turn team paint id unit coord paint-list own-color tr-paint tr-move vis mem)))

; crida l'agent de l'equip team amb la informació de la unitat i retorna la llista d'accions decidides
(defun unit-agent (team info)
    (if (eq team TEAM-1)
        (agent-sng656 info)
        (agent-jgr448 info)))

; ESTRUCTURA D'UNA ACCIÓ
; cada acció és una llista de dos elements: (nom arguments)
;    - nom:        el símbol identificador de l'acció
;    - arguments:  llista d'arguments de l'acció
; cada unitat pot retornar múltiples accions per torn (llista d'accions)
;
; TIPUS D'ACCIONS
;    (crea-bolla (color (x y)))
;    (pinta ((x y)))
;    (mou ((x y)))
;    (escriu-memoria (nou-valor-memoria))
;
; RESULTAT D'UNA ACCIÓ (valor de retorn de la funció d'acció)
;   1r: l'estat resultant d'aplicar l'acció
;   2n: llista d'actualitzacions aplicades al mapa; cada actualització té format (xy nova-cel·la)

; processa la llista d'accions d'una unitat aplicant-les en seqüència a l'estat
; retorna (estat-final llista-actualitzacions-del-mapa)
; el flag created limita una base a una sola creació de bolla per torn (t_r^create = 1)
(defun unit-actions (state team xy actions &optional updates created)
    (if actions
        (let* ((action (car actions))
               (name (car action))
               (args (cadr action))
               (skip-create (and created (eq name ACTION-CREATE-BALL)))
               (result (cond
                    (skip-create (list state nil))
                    ((eq name ACTION-MOVE) (unit-act-move state team xy (car args)))
                    ((eq name ACTION-PAINT) (unit-act-paint state team xy (car args)))
                    ((eq name ACTION-CREATE-BALL) (unit-act-create-ball state team xy (cadr args) (car args)))
                    ((eq name ACTION-MEM-WRITE) (unit-act-write-mem state team (car args)))
                    (t (list state nil))))
                (next-state (car result))
                (act-update (cadr result))
                (next-updates (append updates (if act-update (list act-update) nil))))
            (unit-actions next-state team xy (cdr actions) next-updates))
        (list state updates)))

; substitueix la memòria compartida de l'equip pel nou valor; no genera cap actualització de mapa
(defun unit-act-write-mem (state team value) (list (state-mem state team value) nil))

; mou la bolla de src a dst si l'acció és vàlida (bolla pròpia, cooldown 0, dst lliure i dins del rang)
; el cost de moviment augmenta si el moviment és diagonal o si dst no és del color de la bolla
(defun unit-act-move (state team src dst)
    (let* ((m (state-map state))
           (target (coord-unshift state dst))
           (src-cell (map-cell m src)))
        (if (map-bounds m target)
            (let* ((dst-cell (map-cell m target))
                   (d (dist src target)))
                (if (and (cell-has-ball src-cell)
                         (cell-owned-by src-cell team)
                         (< (cell-unit-tr-move src-cell) BALL-MOVE-TR)
                         (<= d BALL-MOVE-RANGE)
                         (cell-type-land dst-cell)
                         (cell-empty dst-cell)
                         (> d 0))
                    ; l'acció es vàlida, aplica els canvis al mapa
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
                                          tr-move-new))
                            (src-upd (list (car src) (cadr src) src-new))
                            (dst-upd (list (car target) (cadr target) dst-new))
                            (state-next (state-map state (map-update m src-upd dst-upd))))
                        (list state-next (list ACTION-MOVE src-upd dst-upd)))
                    ; l'acció és invàlida, no s'aplica cap canvi al mapa
                    (list state nil)))
            (list state nil))))
            
; pinta la cel·la dst amb el color de la bolla a src si l'acció és vàlida (cooldown 0, dins del rang)
; el cost de pintar es triplica si src no és del color de la bolla
(defun unit-act-paint (state team src dst)
    (let* ((m (state-map state))
           (target (coord-unshift state dst))
           (src-cell (map-cell m src)))
        (if (map-bounds m target)
            (let* ((dst-cell (map-cell m target))
                   (d (dist src target)))
                (if (and (cell-has-ball src-cell)
                         (cell-owned-by src-cell team)
                         (< (cell-unit-tr-paint src-cell) 1)
                         (<= d BALL-PAINT-RANGE)
                         (cell-type-land dst-cell))
                    ; l'acció és vàlida, s'apliquen els canvis al mapa
                    (let* ((src-color (cell-color src-cell))
                           (ball-color (cell-unit-color src-cell))
                           (tr-paint-new (+ (cell-unit-tr-paint src-cell)
                                            (if (eq src-color ball-color)
                                              BALL-PAINT-TR
                                              (* BALL-PAINT-TR BALL-PAINT-TR-DIFF-COLOR)))))
                        (if (equal src target)
                            ; src i dst son la mateixa cel·la: combinar les dues transformacions
                            ; (cooldown + pintura) en una sola actualització, perquè dues
                            ; actualitzacions sobre la mateixa posició es sobreescriurien
                            (let* ((cell-painted (cell-apply-paint team src-cell ball-color))
                                   ; només actualitza el cooldown si la bolla no s'ha destruit
                                   (cell-final (if (cell-has-ball cell-painted)
                                                   (cell-unit-tr-paint cell-painted tr-paint-new)
                                                   cell-painted))
                                   (upd (list (car src) (cadr src) cell-final))
                                   (state-next (state-map state (map-update m upd))))
                                (list state-next (list ACTION-PAINT upd upd)))
                            ; src i dst diferents: dues actualitzacions independents
                            (let* ((src-cell-new (cell-unit-tr-paint src-cell tr-paint-new))
                                   (dst-cell-new (cell-apply-paint team dst-cell ball-color))
                                   (src-upd (list (car src) (cadr src) src-cell-new))
                                   (dst-upd (list (car target) (cadr target) dst-cell-new))
                                   (state-next (state-map state (map-update m src-upd dst-upd))))
                                (list state-next (list ACTION-PAINT src-upd dst-upd)))))
                    ; l'acció és invàlida, no s'aplica cap canvi al mapa
                    (list state nil)))
            (list state nil))))

; crea una nova bolla de color color a dst a partir de la base a src, descomptant el cost de pintura
(defun unit-act-create-ball (state team src dst color)
    (let* ((m (state-map state))
           (target (coord-unshift state dst))
           (src-cell (map-cell m src))
           (paint (state-paint state team)))
        (if (map-bounds m target)
            (let* ((dst-cell (map-cell m target))
                   (d (dist src target)))
                (if (and (cell-has-base src-cell)
                         (cell-owned-by src-cell team)
                         (>= paint BASE-CREATE-COST)
                         (<= d BASE-CREATE-RANGE)
                         (cell-type-land dst-cell)
                         (cell-empty dst-cell)
                         (> d 0))
                    ; l'acció es vàlida, aplica els canvis al mapa
                    (let* ((next-id (state-next-id state))
                           (dst-new (list LAND (cell-color dst-cell) BALL team next-id (list color) color 0 0))
                           (upd (list (car target) (cadr target) dst-new))
                           (s1 (state-map state (map-update m upd)))
                           (s2 (state-paint s1 team (- paint BASE-CREATE-COST)))
                           (s3 (state-next-id s2 (1+ next-id))))
                      (list s3 (list ACTION-CREATE-BALL upd)))
                    ; l'acció és invàlida, no s'aplica cap canvi al mapa
                    (list state nil)))
            (list state nil))))

; retorna les coordenades de totes les cel·les visibles des de (cx, cy) amb rang r (distància euclidiana al quadrat)
; optimització: en lloc de recórrer tot el mapa, només es comproven les cel·les del quadrat de costat sqrt(r)
; centrat a (cx, cy), ja que les cel·les més llunyanes sempre superen el límit de distància
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

(defun unit-vision (m c r)
    (let* ((cx (car c))
           (cy (cadr c))
           (off (truncate (sqrt (float r))))
           (w (map-width m))
           (h (map-height m))
           ; calcula els límits del quadrat interior i els retalla als límits del mapa
           (min-x (max 0 (- cx off)))
           (min-y (max 0 (- cy off)))
           (max-x (min (- w 1) (+ cx off)))
           (max-y (min (- h 1) (+ cy off)))
           ; genera totes les coordenades del quadrat interior
           (coord-range (cartesian (range min-x max-x) (range min-y max-y))))
        ; filtra les coordenades visibles dins del rang r (distància al quadrat)
        (remove-if (lambda (dst) (> (dist c dst) r)) coord-range)))

; formata la informació d'una cel·la visible per enviar-la a l'agent
; el format és de longitud variable segons el contingut de la cel·la (veure enunciat):
;        aigua: (coord AIGUA)
;   terra buida: (coord TERRA color)
;          lab: (coord TERRA color LAB equip)
;         base: (coord TERRA color BASE equip colors-pintat)
;        bolla: (coord TERRA color BOLLA equip colors-pintat color-propi tr-pintar tr-moure)
; les coordenades s'envien desplaçades (x+dx, y+dy) per ocultar els límits reals del mapa
(defun unit-vision-format (state xy)
    (let* ((m (state-map state))
           (cell (map-cell m xy))
           (coord (coord-shift state xy)))
        (cond ((cell-type-water cell) (list coord WATER))
              ((cell-has-lab cell)
                  (list coord LAND (cell-color cell) LAB (cell-unit-team cell)))
              ((cell-has-base cell)
                  (list coord LAND (cell-color cell) BASE (cell-unit-team cell)
                        (cell-unit-paint cell)))
              ((cell-has-ball cell)
                  (list coord LAND (cell-color cell) BALL (cell-unit-team cell)
                        (cell-unit-paint cell)
                        (cell-unit-color cell)
                        (cell-unit-tr-paint cell)
                        (cell-unit-tr-move cell)))
              (t (list coord LAND (cell-color cell))))))

; **************************************************
; CONTROLLER
; **************************************************

; incrementa la pintura de l'equip team: 2 per torn més 1 per cada laboratori capturat
(defun game-paint-increase (s team)
    (let* ((m (state-map s))
           (labs (map-count m (lambda (cell) (cell-lab-team cell team))))
           (inc (+ PAINT-INC-TURN (* PAINT-INC-LAB labs)))
           (curr (state-paint s team)))
          (state-paint s team (+ curr inc))))

; decrementa en 1 els cooldowns de totes les bolles de l'equip team (tr-pintar i tr-moure)
(defun game-tr-decrease (s team)
    (state-map s (map-apply (state-map s)
                            (lambda (cell) (if (and (cell-has-ball cell) (cell-owned-by cell team))
                                               (cell-ball-tr-decrease cell)
                                               cell)))))

; aplica les accions de cada unitat en seqüència i retorna (estat-final llista-actualitzacions)
(defun game-actions (state team units actions &optional updates)
    (if (and units actions)
        (let* ((result (unit-actions state team (car units) (car actions)))
               (next-state (car result))
               (next-updates (append updates (cadr result))))
            (game-actions next-state team (cdr units) (cdr actions) next-updates))
        (list state updates)))

; executa un torn complet de l'equip actiu: incrementa pintura, decrementa cooldowns,
; processa les accions de les bases i les bolles, i avança el comptador de torn
(defun game-turn (state)
    (let* ((turn (state-turn state))
           (team (if (oddp turn) TEAM-1 TEAM-2))
           ; 1. incrementa la pintura de l'equip actiu
           (s1 (game-paint-increase state team))
           ; 2. decrementa els cooldowns de les bolles de l'equip actiu
           (s2 (game-tr-decrease s1 team))
           ; 3. cerca totes les bases i crida els agents per a cada una
           (bases (unit-find (state-map s2) team BASE))
           (base-info-list (mapcar (lambda (xy) (unit-info s2 team xy)) bases))
           (base-actions (mapcar (lambda (info) (unit-agent team info)) base-info-list))
           (s3-result (game-actions s2 team bases base-actions))
           (s3 (car s3-result))
           (base-updates (cadr s3-result))
           ; 4. cerca totes les bolles i crida els agents per a cada una
           (balls (unit-find (state-map s3) team BALL))
           (ball-info-list (mapcar (lambda (xy) (unit-info s3 team xy)) balls))
           (ball-actions (mapcar (lambda (info) (unit-agent team info)) ball-info-list))
           (s4-result (game-actions s3 team balls ball-actions))
           (s4 (car s4-result))
           (ball-updates (cadr s4-result)))
        (graphics-upd s4 (append base-updates ball-updates))
        ; 5. increase turn
        (state-turn s4 (1+ turn))))

; retorna t si la partida ha acabat: algun equip ha perdut la base o s'arriba al límit de torns
(defun game-check-end (state)
    (let* ((m (state-map state))
           (t1-alive (unit-find m TEAM-1 BASE))
           (t2-alive (unit-find m TEAM-2 BASE)))
        (or (not t1-alive) (not t2-alive) (>= (state-turn state) TURN-LIMIT))))

; retorna l'equip guanyador; si ambdós equips comparteixen estat (tots dos vius per límit de torns
; o tots dos morts simultàniament) aplica el desempat per: més bolles vives, més pintura, aleatori
(defun game-winner (state)
    (let* ((m (state-map state))
           (t1-alive (unit-find m TEAM-1 BASE))
           (t2-alive (unit-find m TEAM-2 BASE)))
        (cond ((and t1-alive (not t2-alive)) TEAM-1) ; equip 1 viu i equip 2 mort -> guanya equip 1
              ((and (not t1-alive) t2-alive) TEAM-2) ; equip 1 mort i equip 2 viu -> guanya equip 2
            ; ambdós equips comparteixen estat -> aplica desempat
            (t (let ((t1-balls (length (unit-find m TEAM-1 BALL)))
                      (t2-balls (length (unit-find m TEAM-2 BALL)))
                      (t1-paint (state-paint state TEAM-1))
                      (t2-paint (state-paint state TEAM-2)))
                (cond
                    ; desempat per nombre de bolles vives
                    ((> t1-balls t2-balls) TEAM-1)
                    ((< t1-balls t2-balls) TEAM-2)
                    ; desempat per quantitat de pintura
                    ((> t1-paint t2-paint) TEAM-1)
                    ((< t1-paint t2-paint) TEAM-2)
                    ; desempat aleatori
                    (t (if (zerop (random 2)) TEAM-1 TEAM-2))))))))

; refresca la finestra gràfica al final de cada torn
; updates conté les cel·les que han canviat (format (xy nova-cel·la)); s'ignora i es redibuixa
; el mapa sencer perquè grafics.lsp no ofereix una primitiva de repintat per cel·la independent
(defun graphics-update (state updates) (graphics-init (state-map state)))

; bucle principal del joc; executa torns fins que la partida acabi i retorna l'equip guanyador
; usa defun-tco per evitar desbordament de pila en partides llargues
(defun-tco game-loop (state)
    (if (game-check-end state)
        (game-winner state)
        (game-loop (game-turn state))))

; punt d'entrada: carrega el mapa, genera el desplaçament aleatori de coordenades i inicia la partida
(defun paintball (map-name)
    (let* ((m (map-load map-name))
           (dx (random 1001))
           (dy (random 1001))
           (state (state-new 0 m PAINT-INIT PAINT-INIT dx dy (* (map-width m) (map-height m)))))
        ; configuració inicial de la finestra gràfica
        ; (color 0 0 0 255 255 255)
        ; (mode 0 0 640 375)
        ; enter game loop
        (graphics-upd state)
        (game-loop (state-turn state 1))))

(paintball "tiny") ; descomentar per executar la partida automàticament en carregar el fitxer