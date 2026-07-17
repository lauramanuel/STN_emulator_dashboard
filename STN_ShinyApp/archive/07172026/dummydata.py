# -*- coding: utf-8 -*-
"""
Created on Thu Jul 16 09:48:50 2026

@author: lamanuel
"""

import csv
import itertools
import random

# -----------------------------
# Input values
# -----------------------------
VER = list(range(400, 30001, 800))
EXP = list(range(0, 20001, 1000))
EAST = list(range(1000, 20001, 4000))
XGEO = list(range(1000, 20001, 4000))
RISK = list(range(15, 81, 5))

outfile = "dummy_model_results.csv"

# Optional: make results reproducible
random.seed(42)

# -----------------------------
# Generate CSV
# -----------------------------
count = 0

with open(outfile, "w", newline="") as f:

    writer = csv.writer(f)

    writer.writerow([
        "VER",
        "EXP",
        "EAST",
        "XGEO",
        "RISK",
        "event_horizon_distance"
    ])

    for ver, exp, east, xgeo, risk in itertools.product(
        VER, EXP, EAST, XGEO, RISK
    ):

        # Dummy model
        ehd = (
            0.0025 * ver
            + 0.00035 * exp
            + 0.00020 * east
            + 0.00015 * xgeo
            + 0.75 * risk
            + random.uniform(-5, 5)
        )

        writer.writerow([
            ver,
            exp,
            east,
            xgeo,
            risk,
            round(ehd, 2)
        ])

        count += 1

        if count % 100000 == 0:
            print(f"{count:,} rows written...")

print()
print(f"Finished!")
print(f"Rows written: {count:,}")
print(f"Output file: {outfile}")