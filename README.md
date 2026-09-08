# Welfare Estimation from a Discrete Choice Experiment

Mixed logit models on a 3,825-respondent stated-preference survey, 26,848
choice observations across Alberta, Manitoba and Saskatchewan, producing
willingness-to-pay estimates that vary by spatial scale and by whether the
affected water body is local to the respondent.

Full analysis pipeline: raw survey export to census-weighted estimation dataset,
fourteen estimated specifications, and the figures and tables of the resulting
manuscript. Reproducible end to end from `analysis/R/00-run-all.R`.

### Methods

| | |
|---|---|
| **Choice econometrics** | Mixed logit with independent normal random parameters, estimated in both preference space and WTP space; lognormal and truncated-normal mixing variants; sub-basin fixed effects. Simulated maximum likelihood over Sobol draws (`apollo`). |
| **Welfare measures** | Marginal and total WTP via the delta method, with robust standard errors; welfare surfaces mapped to 18 sub-basins. |
| **Survey weighting** | Iterative proportional fitting (raking) to 2021 Census margins on age, gender, income and education, per province, against live Statistics Canada tables (`anesrake`, `cansim`). |
| **Spatial analysis** | Polygon neighbour matrices, spatial joins, address geocoding, choropleth mapping (`sf`, `spdep`, `tmap`). |
| **Reproducibility** | Byte-level output verification, cached external data, session setup with dependency and working-directory checks. |

## Layout

```
Chapter3-WQ-Manuscript/
├── Chapter3-WQ-Manuscript.Rproj   open this first — sets the working directory
├── manuscript/                    LaTeX source
│   ├── Water-Quality-Spatial-WTP-manuscript.pdf   compiled manuscript (see Status)
│   ├── main.tex                   the manuscript          
│   ├── appendix.tex               the appendix           
│   ├── appendix-long.tex          longer appendix variant 
│   ├── refs.bib                   bibliography           
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
latexmk -pdf -jobname=Water-Quality-Spatial-WTP-manuscript main.tex
latexmk -pdf -outdir=_build appendix.tex
```

**Analysis** — open `Chapter3-WQ-Manuscript.Rproj`, then:

```r
source("analysis/R/00-run-all.R")
```

All paths in the R code are relative to the project root, so the working
directory must be the folder containing the `.Rproj` file.

---


