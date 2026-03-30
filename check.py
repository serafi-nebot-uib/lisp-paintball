#!/usr/bin/env python3 

import re
from pathlib import Path

RESET = "\033[0m"
RED = "\033[31m"
GREEN = "\033[32m"
YELLOW = "\033[33m"
BLUE = "\033[34m"

FORBIDDEN = {
  # Reassignació
  "setq", "setf", r"set\b", "psetq", "psetf",
  "incf", "decf", "push", "pop", "pushnew",
  "defparameter", "defvar",
  # Mutació destructiva
  "rplaca", "rplacd",
  "nconc", "nreverse",
  "delete", "delete-if", "delete-if-not",
  "nsubst", "nsubst-if", "nsubst-if-not",
  r"sort\b", "stable-sort",
  r"fill\b", r"replace\b",
  "putprop", "remprop",
  "mapcan", "mapcon",
  # Iteració
  r"loop\b", r"do\b", r"do\*", "dotimes", "dolist",
  "prog", r"prog\*", "tagbody", r"\bgo\b",
  "mapc", "mapl",
}

pattern = re.compile(r"\((" + "|".join(FORBIDDEN) + r")\b", re.IGNORECASE)
comment = re.compile(r";.*$")
string  = re.compile(r"\"[^\"]*\"")

errors = []
for path in Path(".").glob("src/**/*.lsp"):
  for i, raw in enumerate(path.read_text().splitlines(), 1):
    line = string.sub("\"\"", comment.sub("", raw))  # strip comments & strings
    for m in pattern.finditer(line): errors.append(f"{path}:{i}: '{m.group()}' → {raw.strip()}")

if errors:
  print(f"{RED}█{RESET} invalid patterns found:")
  for e in errors: print(" ", e)
else:
  print(f"{GREEN}█{RESET} OK")