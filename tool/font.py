#!/usr/bin/env python3

import freetype
import numpy as np

# Render at 24px (15x18 glyph) then downsample 3x to get clean pixel grid
# Canvas is 6x7 (width x height) to allow 1px spacing and correct vertical placement
SCALE = 3
FONT_W = 6
FONT_H = 8
GLYPH_W = 5
GLYPH_H = 6

face = freetype.Face("../res/5x6-font.otf/5x6-font.otf")
# size=24 renders exactly 15x18 (3x scale of 5x6)
face.set_pixel_sizes(0, 24)

def char_matrix(char):
    face.load_char(char, freetype.FT_LOAD_TARGET_MONO | freetype.FT_LOAD_RENDER)
    bm = face.glyph.bitmap
    canvas_h = FONT_H * SCALE
    canvas_w = FONT_W * SCALE
    canvas = np.zeros((canvas_h, canvas_w), dtype=np.uint8)
    if bm.rows > 0 and bm.width > 0:
        # baseline at row GLYPH_H (font pixels), so glyphs are placed by bearing
        baseline = GLYPH_H * SCALE
        top = baseline - face.glyph.bitmap_top
        left = face.glyph.bitmap_left
        for r in range(bm.rows):
            for c in range(bm.width):
                byte = bm.buffer[r * bm.pitch + c // 8]
                bit = (byte >> (7 - (c % 8))) & 1
                cy, cx = top + r, left + c
                if 0 <= cy < canvas_h and 0 <= cx < canvas_w:
                    canvas[cy, cx] = bit
    return canvas.reshape(FONT_H, SCALE, FONT_W, SCALE).max(axis=(1, 3))

CHARS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz .,'?!_*%&$#()+-/:;<>=^|~"

ESCAPE = {' ': 'space'}

print("(defconstant FONT (list")
for c in sorted(set(CHARS)):
    m = char_matrix(c)
    # rightmost column with any set pixel + 1, minimum 1 for spacing
    width = max((col + 1 for row in m for col, v in enumerate(row) if v), default=GLYPH_W)
    rows = "".join(f"({' '.join(map(str, r))})" for r in m)
    name = ESCAPE.get(c, c)
    print(f"    (cons #\\{name} '({width} {rows}))")
print("))")
