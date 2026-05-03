## Participants

Serafí Nebot Ginard -- SNG656
Jaume Galmés Ramis  -- JGR448

## Execució

Carregar `paintball.lsp` amb XLISP-PLUS i començar el joc amb `(paintball "<nom-mapa>")`,
que carrega el mapa indicat i inicia la partida. Exemple:  `(paintball "tiny")`.

## Estratègia dels agents

### SNG656 — agent amb memòria

Versió completa, fa servir la memòria compartida de l'equip per acumular
observacions entre torns:

- **Memòria.** Cada unitat fusiona la seva visió actual amb la memòria
  compartida; les observacions noves sobreescriuen les antigues, i les cel·les
  fora de visió però conegudes es mantenen. Les caselles que falten dins el
  rang es desen com a `(coord nil)` per delimitar la frontera del que s'ha
  explorat. Les entrades antigues amb bolles es descarten (les bolles són
  l'únic element que es mou).
- **Base.** Crea una bolla quan té prou pintura i un veí buit; tria el color
  que més falta a les seves bolles conegudes, alternant `r/g/b` segons el torn si
  no té cap referència.
- **Bolla — pintar.** Tria el millor objectiu dins rang amb prioritat
  base enemiga > bolla enemiga > laboratori, desempatant per proximitat. No
  repinta cel·les o unitats que ja tenen el seu color propi.
- **Bolla — moure.** Primer ataca objectius tàctics coneguts (enemics o
  laboratoris no propis). Si no n'hi ha, cerca fronteres: cel·les de
  terra buida amb veïns desconeguts. La millor frontera obre més cel·les
  noves; en empat es prefereix allunyar-se de la base i, finalment, la més
  propera. Si no hi ha cap pas adjacent que acosti l'objectiu tàctic, també
  cau a moviment de frontera per evitar bloquejos.

### JGR448 — agent mínim

Versió extremament lleugera. No usa memòria compartida, no fa exploració
amb estat ni càlcul de fronteres: cada decisió surt de la visió actual.

- **Base.** Si té pintura suficient i un veí buit, crea una bolla amb color
  rotatori `(mod torn 3)`.
- **Bolla — pintar.** Recorre la visió i pinta el primer objectiu vàlid dins
  rang (bolla, base o laboratori no propis) que encara no tingui el color
  propi.
- **Bolla — moure.** Sempre fa un pas. Si veu la base pròpia, tria el veí
  buit més llunyà d'ella per dispersar-se; si la perd de vista, qualsevol
  veí buit serveix. La defensa la fa la pintura de rang, no el moviment.

## Mòdul gràfic i font de bitmaps

`grafics.lsp` dibuixa el tauler i la barra lateral. La interfície es
divideix en una zona de mapa (cel·les de mida adaptativa entre `CELL-MIN-SIZE`
i `CELL-MAX-SIZE`) i un panell lateral amb torn, equips, pintura i un
registre d'esdeveniments.

Per al text de la barra lateral no es fa servir cap font del sistema sinó una
font pròpia 5x6 codificada com a bitmaps a `font.lsp`. Cada caràcter és una
entrada `(cons #\X '(width row0 row1 … row7))`, on cada fila és una llista
de 0/1 (8 files de 6 columnes, deixant un píxel d'aire dret/inferior per
separar caràcters).

### `tool/font.py` — generador de la font

`font.lsp` es genera automàticament: es regenera a partir de l'OTF original
(`res/5x6-font.otf`) amb el script `tool/font.py`, que fa servir
`freetype-py` per a processar la font. Tot i així, s'han editat manualment
alguns del bitmaps generats per acabar d'afinar-los.