# Reference fixtures

`gen_fixtures.py` produces the CSVs in `core/src/test/resources/reference/` that
`ReferenceDifferentialSpec` compares against `Calculator.search`.

Provenance: the author's 2019 Python calculator ("Compound Planetary Caluclator
CDCP.py", X: drive). It is third-party code and is NOT copied here. The script reads
it at run time (read-only), overrides the settings block by text substitution
(TargetGearRatio=0 for the full design set, MinPlanets=MaxPlanets=N, CDCP/NSS flags,
MinModule etc.), cuts the source before its `while` loop and runs one pass in-process,
collecting rows instead of printing.

Regenerate (needs the X: drive; override the path with `CPGC_REFERENCE_PY`):

    python tools/reference/gen_fixtures.py

Output is deterministic. Columns: ratio,N,m1,s1,p1,r1,m2,s2,p2,r2 (raw tooth counts
even where the reference omits them from its printout). `manifest.csv` records the
reference settings per fixture; the Scala spec translates them (exclusive `range()`
bounds, strict ring/module bounds) into a `SearchConfig`.

Modes: Standard, NSS, CDCP, and NSSFIX = NSS with the reference's `Planet1`/`Planet2`
scoping slip repaired in the scratch copy (`MatchSets` never declares them global, so
`DecideIfPrint` always reads `PlanetSpacingOK[0]`; literal NSS therefore applies no
spacing filter, or rejects everything when gearset 0 is unspaced).
