### Load R6 model components ###

source(file.path("R", "R", "packages.R"))
source(file.path("R", "R", "data_classes.R"))
source(file.path("R", "R", "parameters.R"))
source(file.path("R", "R", "baseline_model.R"))
source(file.path("R", "R", "hgt_model.R"))
source(file.path("R", "R", "visualization.R"))
source(file.path("R", "R", "model_helpers.R"))
load_model_packages()

### Run corrected HGT model ###

model <- new_hgt_model()
output <- model$run(seq(0, 150, by = 1))

### Save HGT outputs ###

output_dir <- file.path("data", "processed")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(output, file.path(output_dir, "hgt_ode_results.csv"))

print(SimulationVisualizer$new(output)$summary())
