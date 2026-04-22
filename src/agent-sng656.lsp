;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer de l'agent intel·ligent ABC123.
;; <Descripció de les funcions d'aquest fitxer>

(defun agent-sng656 (dades)
    ; (car dades) = ronda
    ; (cadr dades) = equip
    ; etc.
    (let* ((unitat (nth 4 dades))
           (coord (nth 5 dades))
           (x (car coord))
           (y (cadr coord)))
        (cond ((eq unitat 'base)
               (list (list 'crea-bolla (list 'r (list (1+ x) y)))))
              ((eq unitat 'bolla)
               (list (list 'pinta (list (list (1+ x) y)))
                     (list 'mou (list (list (1+ x) y)))))
              (t nil))))
