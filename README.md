# Chapter III — Use and Non-Use Value of Water Quality

Working folder for the Chapter III manuscript. Reorganised from the original
`Chapter III-UseNonUseValue/` tree, which is kept intact alongside this folder
until this one is confirmed good.

---

## Layout

```
Chapter3-WQ-Manuscript/
├── Chapter3-WQ-Manuscript.Rproj   open this first — sets the working directory
├── manuscript/                    LaTeX source
│   ├── main.tex                   the manuscript          (was main1.tex)
│   ├── appendix.tex               the appendix            (was Appendix_short.tex)
│   ├── appendix-long.tex          longer appendix variant (was Appendix.tex)
│   ├── refs.bib                   bibliography            (was mybibfile.bib)
│   ├── figures/                   the 12 figures main.tex actually uses
│   ├── tables/                    the 16 .tex tables
│   └── _build/                    compile output — safe to delete, gitignored
├── analysis/
│   ├── R/
│   │   ├── 00-run-all.R           master script, runs the pipeline in order
│   │   ├── 01-dataprep/           survey data → Apollo choice dataset
│   │   ├── 02-models/             Apollo model estimation
│   │   ├── 03-figures/            manuscript figures
│   │   └── 04-tables/             manuscript tables
│   └── outputs/
│       ├── models/                model .rds / _estimates.csv / _output.txt
│       ├── figures/               final figures  (+ diagnostics/ for weighting checks)
│       └── tables/                final tables
├── data/
│   ├── raw/                       read-only inputs — never write here
│   ├── derived/                   built by 01-dataprep
│   └── gis/                       shapefiles used by the mapping code
├── survey/                        instrument, choice designs, REB, CHASR notes
├── refs/                          Vossler replication, Apollo examples, background
└── archive/                       superseded material, kept deliberately
```

## How to rebuild

**Manuscript** (TeX Live, uses biber):

```bash
cd manuscript
latexmk -pdf -outdir=_build main.tex
latexmk -pdf -outdir=_build appendix.tex
```

**Analysis** — open `Chapter3-WQ-Manuscript.Rproj`, then:

```r
source("analysis/R/00-run-all.R")
```

All paths in the R code are relative to the project root, so the working
directory must be the folder containing the `.Rproj` file.

---

## What changed from the original

| Original | Now | Why |
|---|---|---|
| `Chapter III - Manuscript/Manuscript-Chapter III/main1.tex` | `manuscript/main.tex` | live manuscript |
| `Manuscript/` (336 MB) | left in place | dissertation-era, superseded |
| `R Project/Chapter-III/Codes_FinalData_Working/` | `analysis/R/01-04*/` | live code, sorted by pipeline stage |
| `R Project/Chapter-III/Codes/`, `Codes_PilotData/` | left in place | earlier/pilot stage |
| `Figures3/`, `Figures3new/` | `manuscript/figures/` | only the 12 referenced files |
| `Tables3new/` | `manuscript/tables/` | |
| `Figure/`, `Figures2/`, `Tables/`, `Tables2/` | left in place | 271 files, referenced by nothing |
| `Finaloutput/` current models | `analysis/outputs/models/` | 93 files |
| `Finaloutput/` `_OLD*` backups | `archive/Finaloutput_OLD_backups/` | 221 Apollo auto-backups |

Path prefixes were rewritten throughout the R code to match this layout:
`Rawdata/`→`data/raw/`, `Deriveddata/`→`data/derived/`,
`Finaloutput/`→`analysis/outputs/models/`, `Figures/Final/`→`analysis/outputs/figures/`,
`Tables/`→`analysis/outputs/tables/`, and the hardcoded absolute shapefile
paths →`data/gis/`.

## Verification done

- `main.tex` and `appendix.tex` recompile to PDFs **byte-identical** to the
  originals (6,702,015 and 130,576 bytes; 36 pages; 0 undefined citations or
  cross-references).
- All 24 `source()` targets in `00-run-all.R` resolve.
- 47 of 48 data/model/shapefile read paths resolve. The one that does not is a
  commented-out dead line (`01-dataprep/Survey Data Preparation Part 2.R:296`)
  that pointed at a directory which never existed.
- The R pipeline was **not executed** — Apollo estimation is long-running, so
  verification is static (path resolution), not a full reproduction run.

---

## Model pipeline repair (2026-09-05)

The model stage did not close: the figures and tables loaded nine model objects,
but `02-models/` could only produce six of them, and two scripts collided.
A clean-room run would have failed at the tables stage. All of it is now fixed.

**Script filenames did not match the `modelName` each one writes.** Files were
renamed so the two always agree (outputs are named by `modelName`, so no
existing output changed):

| Was | Now | Writes |
|---|---|---|
| `Model 3.R` | `Model 4.4.R` | `Model 4.4` |
| `Model 4 .R` | `Model 5.R` | `Model 5` |
| `Model 5.R` | `Model 6.R` | `Model 6` |
| `Model 6.R` | `Model 7_rectrip_alt.R` | `Model 7_rectrip_alt` |

**Collision removed.** `Model 6.R` and `Model 7.R` both wrote `modelName =
"Model 7"`, different specifications, so whichever ran second destroyed the
other. This had already happened — the loser survives only as
`archive/Finaloutput_OLD_backups/Model 7_OLD1`/`OLD4`. The superseded variant
(it adds `b_asc_rectrip_choice * REC_IN_CHOICE_BASIN`) is now
`Model 7_rectrip_alt.R`, writes its own `modelName`, and is commented out of
`00-run-all.R`. The manuscript uses `Model 7.R`'s specification.

**Three models had no producing script.** `Model 3`, `Model 4` and `Model 8`
existed only as leftover `.rds` files, yet feed `Table_1.tex`, `Table_2.tex`,
`figure_7_new.png` and the appendix perception table:

- `Model 3.R` and `Model 4.R` were recovered from
  `Codes_FinalData_Working/OldCodes/` in the original OneDrive tree
  (`Model 3 Final new.R`, `Model 4 New Random all obs and exp var.R`),
  identified by matching parameter signatures, and confirmed against the saved
  outputs' recorded settings (nCores 8; 4000 and 2000 sobol draws).
  Paths were rewritten to this layout.
- `Model 8.R` did not survive anywhere. It was reconstructed from `Model 4.4.R`
  plus the two perceived-SQ terms, using the model code Apollo embedded in
  `Model 8_output.txt`. **Verified by re-estimation: the regenerated
  `Model 8_estimates.csv` is byte-identical to the archived one across every
  column, with matching LL(final), AIC, BIC and N.**

**`nCores` is now portable.** It was hardcoded (8, 6 or 4) across 13 scripts.
Each is now `min(<original>, max(1, parallel::detectCores() - 1))`, which keeps
the original value on a machine with enough cores — so results still reproduce —
while degrading gracefully on a smaller one.

Every model loaded by `03-figures/` and `04-tables/` now has a producer in
`02-models/`, and every `source()` target in `00-run-all.R` resolves.

## Known issues, carried over from the original

1. **`Table_4.tex`** is used by `main.tex` but no script in the pipeline
   produces it. It appears to be hand-written — treat it as a source file.
2. **Appendix table name mismatch.** The pipeline writes
   `Table_appendix_perception_SQ.tex`, but the manuscript inputs
   `Table_appendix_percieved_SQ copy.tex`. The manuscript's copy was renamed by
   hand, so regenerating the table does not update the manuscript.
3. **`Figure 7.R` vs `Figure 7 new.R`** both write `figure_7_new.png`. The older
   `Figure 7.R` would silently overwrite the newer output, so `00-run-all.R`
   sources only `Figure 7 new.R`.
4. **Fixed here, was broken before:** `Survey Data Preparation Part 4.R` wrote
   `DerivedData/test/final_weights.csv` while Part 5 read `Deriveddata/...`.
   That only worked because macOS is case-insensitive; it would break on Linux.
   Both now use `data/derived/test/`.
5. The original `00.Run All.R` (kept as `00-run-all.R.orig`) was stale: dated
   September, it pointed at the `Codes/` folder rather than
   `Codes_FinalData_Working/`, and omitted six scripts the manuscript depends on.

## Left in the original folder, on purpose

These were not copied because nothing references them and they are large.
They remain safe in OneDrive at `../`:

- `R Project/Chapter-III/Rawdata/` — **13.4 GB**. Only `ChoiceSetDesigns.xlsx`
  and `Water_Quality_Final.xlsx` are read by the code; both were copied.
  `Rawdata/Old/Water_Quality_full.sav` alone is 7.9 GB.
- `CanMapPostalCodeSuitev2022.3/` — **6.6 GB** of licensed postal-code data.
  Referenced by no live code.
- `Survey/Shapefile/map_images/` and `saskatchewan-latest-free/` — **2.9 GB** of
  raw OpenStreetMap extracts (Geofabrik "latest-free" downloads for AB/MB/SK).
  Referenced only by `Codes_FinalData_Working/OldCodes/workinf3.R`, which is
  superseded and not part of this pipeline. Freely re-downloadable from
  Geofabrik if ever needed.
- `Survey/WQ Preliminary works/` — 2.2 GB of early exploratory work.
- `R Project/Chapter-III/Output/` — 1,072 files from the earlier pilot pipeline.
- `Other/PilotDataAnalysis/` — 1.4 GB, pilot stage.
- `Manuscript/` — 336 MB of dissertation-era LaTeX.

**The GIS the analysis actually needs is here and verified:** the five shapefiles
read by the code (`AB`, `MB`, `SK`, `study_area`, `study_area_map_with_WQ`) are in
`data/gis/` at 9.3 MB, and the research shapefiles from the survey (river basins,
cities, watersheds) are in `survey/Shapefile/` at 24 MB — 97 files, each verified
byte-for-byte against the original.
