#!/usr/bin/env python3
"""Generate differential-test fixtures from the author's reference Python program.

The reference is read (read-only) from REF_PATH at run time, patched in memory
and executed once per fixture with TargetGearRatio=0 and a single pass.
Nothing is written under X:; the reference source is never copied into the repo.
"""
import contextlib
import io
import os
import re
import sys

REF_PATH = os.environ.get(
    "CPGC_REFERENCE_PY",
    r"X:\documents\Compound Planetary Gear Calculator - Gear Down For What"
    r"\2019-01-20-Python Compound Planetary Calculator Program"
    r"\Compound Planetary Caluclator CDCP.py")

OUT_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                       "..", "..", "core", "src", "test", "resources", "reference")

# Reference settings as the reference itself understands them (exclusive range()
# upper bounds, strict ring bounds). The Scala spec translates them.
BOUNDS = {
    "A": dict(MinTeeth=8, MaxTeeth=14, MaxSunTeeth=20, MinRingTeeth=30,
              MaxRingTeeth=50, MinModule=1.0, SunPlanetFactor=3, SetOD=150),
    "B": dict(MinTeeth=8, MaxTeeth=14, MaxSunTeeth=22, MinRingTeeth=36,
              MaxRingTeeth=56, MinModule=2.0, SunPlanetFactor=1.2, SetOD=150),
}
# NSSFIX: NSS with the reference's Planet1/Planet2 scoping slip repaired (see README).
MODES = ["Standard", "NSS", "NSSFIX", "CDCP"]
PLANETS = [3, 4]
KEYS = ["MinTeeth", "MaxTeeth", "MaxSunTeeth", "MinRingTeeth", "MaxRingTeeth",
        "MinModule", "SunPlanetFactor", "SetOD"]


def load_source():
    with open(REF_PATH, "r", encoding="utf-8") as f:
        return f.read()


def run_reference(src, mode, n, b):
    ov = dict(b, MinPlanets=n, MaxPlanets=n, TargetGearRatio=0, MaxTime=3600,
              CDCP=1 if mode == "CDCP" else 0,
              NSS=1 if mode in ("NSS", "NSSFIX") else 0,
              NumCombinations=200000)
    for k, v in ov.items():
        src, cnt = re.subn(r"(?m)^%s\s*=.*$" % k, "%s = %r" % (k, v), src, count=1)
        if cnt != 1:
            raise SystemExit("setting not found in reference: " + k)
    src = src.replace("import numpy as np", "")
    if mode == "NSSFIX":
        src = src.replace("def MatchSets():\n",
                          "def MatchSets():\n  global Planet1\n  global Planet2\n", 1)
    # Only the definitions: cut before the main loop, then run one pass by hand.
    head = src.split("start = time.time()")[0]
    g = {"__name__": "ref"}
    rows = []
    with contextlib.redirect_stdout(io.StringIO()):
        exec(compile(head, "reference", "exec"), g)

        def collect():
            rows.append((g["FinalRatio"], g["NumPlanets"], g["ModPG1"],
                         g["TS1"], g["TP1"], g["TR1"],
                         g["ModPG2"], g["TS2"], g["TP2"], g["TR2"]))
        g["PrintGearInfo"] = collect
        g["Reset"]()
        g["FindPlanetarys"]()
        g["MatchSets"]()
    return rows


def main():
    os.makedirs(OUT_DIR, exist_ok=True)
    src = load_source()
    manifest = ["file,mode,bounds,N," + ",".join(KEYS)]
    for mode in MODES:
        for bn, b in BOUNDS.items():
            for n in PLANETS:
                rows = run_reference(src, mode, n, b)
                name = "%s_%s_N%d.csv" % (mode.lower(), bn, n)
                with open(os.path.join(OUT_DIR, name), "w", newline="\n") as f:
                    f.write("ratio,N,m1,s1,p1,r1,m2,s2,p2,r2\n")
                    for r in rows:
                        f.write("%r,%d,%r,%d,%d,%d,%r,%d,%d,%d\n" % (
                            r[0], r[1], r[2], int(r[3]), int(r[4]), int(r[5]),
                            r[6], int(r[7]), int(r[8]), int(r[9])))
                manifest.append("%s,%s,%s,%d,%s" % (
                    name, mode, bn, n, ",".join(repr(b[k]) for k in KEYS)))
                print("%-24s %6d rows" % (name, len(rows)))
    with open(os.path.join(OUT_DIR, "manifest.csv"), "w", newline="\n") as f:
        f.write("\n".join(manifest) + "\n")


if __name__ == "__main__":
    sys.exit(main())
