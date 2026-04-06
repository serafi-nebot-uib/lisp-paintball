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
    ; Exemple: retornam les següents accions:
    ; 1. Crear una bolla a la posició (2, 3).
    ; 2. Moure a la posició (4, 5).
    ; 3. Pintar a la posició (6, 7).
    ; 4. Escriure a la posició 1 de la memòria compartida el valor 42.
    ; 5. Escriure a la posició 2 de la memòria compartida el valor (2 3).
    '((crea-bolla (r (2 3)))
      (mou ((4 5)))
      (pinta ((6 7)))
      (escriu-memoria (1 42))
      (escriu-memoria (2 (2 3)))))