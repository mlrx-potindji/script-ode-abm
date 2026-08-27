### Packages and model definitions ###

source(file.path("R", "R", "packages.R"))
source(file.path("R", "R", "data_classes.R"))
source(file.path("R", "R", "parameters.R"))
source(file.path("R", "R", "baseline_model.R"))
source(file.path("R", "R", "hgt_model.R"))
source(file.path("R", "R", "model_helpers.R"))
load_model_packages(include_analysis = TRUE)

dir.create(file.path("data", "processed"), recursive = TRUE, showWarnings = FALSE)
results_dir <- file.path("results", "figures", "R")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

### Sensitivity parameter ranges ###

baseline_ranges <- list(
  a = c(8, 15), x = c(0.15, 0.35), n = c(0.5, 0.65),
  s_b = c(0.03, 0.20), theta = c(0.35, 0.85), s_p = c(0.85, 0.98),
  low = c(2, 8), high = c(8, 24), psi_s = c(1 / 12, 1 / 7),
  psi_r = c(1 / 32, 1 / 19), iota = c(0.5, 0.8)
)
hgt_ranges <- c(baseline_ranges, list(eta = c(0.01, 0.05), phi = c(0.02, 0.1), mu_HGT = c(0.005, 0.015)))

### Generate sensitivity samples ###

set.seed(90725)
n_simulations <- 1000
sample_parameters <- function(ranges, n) {
  as.data.frame(lapply(ranges, function(range) runif(n, range[1], range[2])))
}
baseline_sample_1 <- sample_parameters(baseline_ranges, n_simulations)
baseline_sample_2 <- sample_parameters(baseline_ranges, n_simulations)
hgt_sample_1 <- sample_parameters(hgt_ranges, n_simulations)
hgt_sample_2 <- sample_parameters(hgt_ranges, n_simulations)

times <- seq(0, 150, by = 1)

### Sensitivity model functions ###

run_sensitivity <- function(parameter_row, model_type, outcome) {
  parameters <- as.list(parameter_row)
  if (model_type == "baseline") {
    model <- new_baseline_model(parameters)
    result <- model$run(times)
  } else {
    model <- new_hgt_model(parameters)
    result <- model$run(times)
  }

  compartments <- switch(
    outcome,
    resistant = c("Cp_r", "Ip_r"),
    sensitive = c("Cp_s", "Ip_s"),
    total = c("Cp_s", "Ip_s", "Cp_r", "Ip_r")
  )
  calculate_auc(result, compartments)
}

make_sobol_model <- function(model_type, outcome) {
  function(sample_matrix) {
    apply(sample_matrix, 1, run_sensitivity, model_type = model_type, outcome = outcome)
  }
}

### Baseline Sobol analyses ###

sobol_baseline_resistant <- sensitivity::sobolmartinez(
  model = make_sobol_model("baseline", "resistant"),
  X1 = baseline_sample_1, X2 = baseline_sample_2, nboot = 100, conf = 0.95
)
sobol_baseline_sensitive <- sensitivity::sobolmartinez(
  model = make_sobol_model("baseline", "sensitive"),
  X1 = baseline_sample_1, X2 = baseline_sample_2, nboot = 100, conf = 0.95
)
sobol_baseline_total <- sensitivity::sobolmartinez(
  model = make_sobol_model("baseline", "total"),
  X1 = baseline_sample_1, X2 = baseline_sample_2, nboot = 100, conf = 0.95
)

### Save baseline Sobol plots ###

save_sobol_plot <- function(result, filename) {
  grDevices::pdf(file.path(results_dir, filename), width = 8, height = 6)
  plot(result)
  abline(h = 0, col = "red", lty = 3)
  grDevices::dev.off()
}
save_sobol_plot(sobol_baseline_resistant, "baseline_ode_sobol_resistant.pdf")
save_sobol_plot(sobol_baseline_sensitive, "baseline_ode_sobol_sensitive.pdf")
save_sobol_plot(sobol_baseline_total, "baseline_ode_sobol_total.pdf")

### HGT Sobol analyses ###

sobol_hgt_resistant <- sensitivity::sobolmartinez(
  model = make_sobol_model("hgt", "resistant"),
  X1 = hgt_sample_1, X2 = hgt_sample_2, nboot = 100, conf = 0.95
)
sobol_hgt_sensitive <- sensitivity::sobolmartinez(
  model = make_sobol_model("hgt", "sensitive"),
  X1 = hgt_sample_1, X2 = hgt_sample_2, nboot = 100, conf = 0.95
)
sobol_hgt_total <- sensitivity::sobolmartinez(
  model = make_sobol_model("hgt", "total"),
  X1 = hgt_sample_1, X2 = hgt_sample_2, nboot = 100, conf = 0.95
)

### Save HGT Sobol plots ###

save_sobol_plot(sobol_hgt_resistant, "hgt_ode_sobol_resistant.pdf")
save_sobol_plot(sobol_hgt_sensitive, "hgt_ode_sobol_sensitive.pdf")
save_sobol_plot(sobol_hgt_total, "hgt_ode_sobol_total.pdf")

### Sobol summary table ###

sobol_table <- function(result, model, outcome) {
  dplyr::bind_rows(
    tibble::tibble(parameter = rownames(result$S), index = "First order", estimate = result$S$original, lower = result$S$`min. c.i.`, upper = result$S$`max. c.i.`),
    tibble::tibble(parameter = rownames(result$T), index = "Total order", estimate = result$T$original, lower = result$T$`min. c.i.`, upper = result$T$`max. c.i.`)
  ) |>
    dplyr::mutate(model = model, outcome = outcome)
}
sobol_summary <- dplyr::bind_rows(
  sobol_table(sobol_baseline_resistant, "baseline", "resistant"),
  sobol_table(sobol_baseline_sensitive, "baseline", "sensitive"),
  sobol_table(sobol_baseline_total, "baseline", "total"),
  sobol_table(sobol_hgt_resistant, "hgt", "resistant"),
  sobol_table(sobol_hgt_sensitive, "hgt", "sensitive"),
  sobol_table(sobol_hgt_total, "hgt", "total")
)
readr::write_csv(sobol_summary, file.path("data", "processed", "sobol_summary.csv"))

### Fitness cost and treatment loss grid ###

run_core <- function(fit_cost, trt_loss) {
  model <- new_baseline_model(list(s_b = fit_cost, s_p = trt_loss))
  result <- model$run(seq(0, 350, by = 1))
  resistant <- calculate_auc(result, c("Cp_r", "Ip_r"))
  sensitive <- calculate_auc(result, c("Cp_s", "Ip_s"))
  tibble::tibble(auc_sensitive = sensitive, auc_resistant = resistant, prevalence = resistant / (resistant + sensitive))
}
core_grid <- expand.grid(fit_cost = seq(0.05, 0.20, length.out = 30), trt_loss = seq(0.85, 0.98, length.out = 30))
core_grid_results <- purrr::map2_dfr(core_grid$fit_cost, core_grid$trt_loss, run_core)
core_grid <- dplyr::bind_cols(core_grid, core_grid_results)
readr::write_csv(core_grid, file.path("data", "processed", "core_treatment_grid.csv"))

### Plot fitness and treatment loss grid ###

png(file.path(results_dir, "core_treatment_grid.png"), width = 2400, height = 1800, res = 300)
print(ggplot2::ggplot(core_grid, ggplot2::aes(fit_cost, trt_loss, fill = prevalence)) +
  ggplot2::geom_tile() + ggplot2::scale_fill_viridis_c(name = "MRSA share") + ggplot2::theme_classic())
dev.off()

### Treatment effectiveness grid ###

run_treatment_effectiveness <- function(fit_cost, trt_loss, treatment_success, resistant_treatment) {
  model <- new_baseline_model(list(s_b = fit_cost, s_p = trt_loss, iota = treatment_success, psi_r = resistant_treatment))
  result <- model$run(seq(0, 350, by = 1))
  auc_sensitive <- calculate_auc(result, c("Cp_s", "Ip_s"))
  auc_resistant <- calculate_auc(result, c("Cp_r", "Ip_r"))
  tibble::tibble(auc_sensitive = auc_sensitive, auc_resistant = auc_resistant, auc_total = auc_sensitive + auc_resistant)
}
treatment_grid <- expand.grid(
  fit_cost = seq(0.05, 0.20, length.out = 30),
  trt_loss = seq(0.85, 0.98, length.out = 30),
  treatment_success = seq(0.70, 0.95, length.out = 5),
  resistant_treatment = seq(1 / 22, 1 / 10, length.out = 3)
)
treatment_results <- purrr::pmap_dfr(treatment_grid, run_treatment_effectiveness)
treatment_grid <- dplyr::bind_cols(treatment_grid, treatment_results)
readr::write_csv(treatment_grid, file.path("data", "processed", "treatment_effectiveness_grid.csv"))

### Plot treatment effectiveness distributions ###

treatment_long <- tidyr::pivot_longer(treatment_grid, c(auc_sensitive, auc_resistant, auc_total), names_to = "measure", values_to = "auc")
png(file.path(results_dir, "treatment_effectiveness_boxplot.png"), width = 2400, height = 1800, res = 300)
print(ggplot2::ggplot(treatment_long, ggplot2::aes(measure, auc, colour = measure)) +
  ggplot2::geom_boxplot() + ggplot2::theme_classic())
dev.off()
