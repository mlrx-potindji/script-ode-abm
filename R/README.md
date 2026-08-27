# R workflow

The R workflow contains the thesis ODE models implemented with R6 classes.

## Structure

- `R/` contains the R6 data classes, model classes, factories, helpers, and visualizers.
- `scripts/` contains executable baseline and corrected HGT model runs.
- `analyses/` contains grid exploration and sensitivity analyses.
- `config/` is reserved for external parameter files.
- Generated tables are written to `../data/processed/` and R figures to `../results/figures/R/`.

## Run from the repository root

```r
source("R/scripts/run_baseline.R")
source("R/scripts/run_hgt.R")
source("R/analyses/grid_exploration.R")
source("R/analyses/sensitivity.R")
```

The analyses create a new R6 model for every parameter combination and use the corrected `HGTModel` for HGT simulations.
