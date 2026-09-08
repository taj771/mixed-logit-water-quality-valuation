# =============================================================================
# Chapter III - Use and Non-Use Value of Water Quality
# Master run script
#
# Working directory must be the project root (Chapter3-WQ-Manuscript/),
# i.e. open Chapter3-WQ-Manuscript.Rproj before sourcing this file.
#
# All paths below are relative to that root:
#   data/raw/                  read-only survey + design inputs
#   data/derived/              intermediate datasets built by 01-dataprep
#   analysis/outputs/models/   Apollo model objects, estimates, logs
#   analysis/outputs/figures/  manuscript figures
#   analysis/outputs/tables/   manuscript tables (.tex)
# =============================================================================

# Loads every package the pipeline uses and checks the working directory.
# Source this on its own if you want to run individual scripts by hand.
source("analysis/R/00-setup.R")

# ---- 1. Data preparation ----------------------------------------------------
# Builds the Apollo-ready choice dataset in data/derived/
source("analysis/R/01-dataprep/Survey Data Preparation Part 1.R")
source("analysis/R/01-dataprep/Survey Data Preparation Part 2.R")
source("analysis/R/01-dataprep/Survey Data Preparation Part 3.R")
source("analysis/R/01-dataprep/Survey Data Preparation Part 4.R")
source("analysis/R/01-dataprep/Survey Data Preparation Part 5.R")
# Part 4_1 is an alternative variant of Part 4 - not in the main pipeline.
# source("analysis/R/01-dataprep/Survey Data Preparation Part 4_1.R")

# ---- 2. Model estimation ----------------------------------------------------
# Each writes <name>_model.rds / _estimates.csv / _output.txt to outputs/models/
# Script filenames match the modelName each one writes (verified).
source("analysis/R/02-models/Model 1.R")     # -> Model 1
source("analysis/R/02-models/Model 2.R")     # -> Model 2
source("analysis/R/02-models/Model 3.R")     # -> Model 3    (Table_1/2/3)
source("analysis/R/02-models/Model 4.R")     # -> Model 4    (Table Codes, figure_7_new)
source("analysis/R/02-models/Model 4.4.R")   # -> Model 4.4  (was misnamed "Model 3.R")
source("analysis/R/02-models/Model 5.R")     # -> Model 5    (was misnamed "Model 4 .R")
source("analysis/R/02-models/Model 6.R")     # -> Model 6    (was misnamed "Model 5.R")
source("analysis/R/02-models/Model 7.R")     # -> Model 7
source("analysis/R/02-models/Model 8.R")     # -> Model 8    (appendix perception table)

# WTP-space specifications -> Appendix Tables A1-A2
source("analysis/R/02-models/Model 2 - WTP Space.R")
source("analysis/R/02-models/Model 2 WTP space log normal distribution.R")
source("analysis/R/02-models/Model 2 WTP Space Normal_truncate.R")

# Sub-basin fixed-effects total WTP -> value_map.png
source("analysis/R/02-models/TWTP_based_model3_FE_subbasin_specific.R")

# Superseded alternative spec, deliberately NOT sourced. It previously wrote
# modelName "Model 7", silently overwriting the output of "Model 7.R".
# source("analysis/R/02-models/Model 7_rectrip_alt.R")

# ---- 3. Figures -------------------------------------------------------------
source("analysis/R/03-figures/Figure 1.R")
source("analysis/R/03-figures/Figure 3.R")
source("analysis/R/03-figures/Figure 5.R")        # -> marginal_WTP.png
source("analysis/R/03-figures/Figure 6.R")
source("analysis/R/03-figures/Figure 7.R")        # -> figure_7_new.png
# SWAPPED (verified 2026-09-07). "Figure 7 new.R" loads Model 4 and then calls
# deltaMethod with b_basewq_nonlocal / b_wq_x_bl_nonlocal - parameters that exist
# in NO model in analysis/outputs/models/. It errors immediately. "Figure 7.R"
# loads Model 4.4, runs clean, and its output is md5-identical to the figure the
# manuscript ships (manuscript/figures/figure_7_new.png, 93fef773...).
source("analysis/R/03-figures/Figure 8.R")        # -> sensitivity_WTP_marginal_checks.png

# Large-text variants for slides/print - not used by the manuscript:
# source("analysis/R/03-figures/Figure 5 largetext.R")
# source("analysis/R/03-figures/Figure 7 new largetext.R")

# NOTE: the note that used to sit here had this backwards - it claimed
# "Figure 7 new.R" superseded "Figure 7.R". Verified false: the former is broken
# and the latter reproduces the manuscript figure exactly. See the swap above.

# ---- 4. Tables --------------------------------------------------------------
source("analysis/R/04-tables/Table Codes.R")                  # -> Table_1/2/3.tex
source("analysis/R/04-tables/Percieved_SQ_effect_table.R")    # -> Table_appendix_perception_SQ.tex
# "Tabel 2 .R" is a superseded standalone version of Table 2 (in Table Codes.R).
# source("analysis/R/04-tables/Tabel 2 .R")

# =============================================================================
# Manuscript artifacts NOT reproduced by this pipeline (see README):
#   manuscript/tables/Table_4.tex  - no producing script found
#   manuscript/figures/quality_*_lwq.jpg, ChoiceImage.png - survey graphics
# =============================================================================
