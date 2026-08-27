### Packages and model definitions ###

source(file.path("R", "R", "packages.R"))
source(file.path("R", "R", "data_classes.R"))
source(file.path("R", "R", "parameters.R"))
source(file.path("R", "R", "baseline_model.R"))
source(file.path("R", "R", "hgt_model.R"))
source(file.path("R", "R", "model_helpers.R"))

load_model_packages(include_analysis = TRUE)

results_dir <- file.path("results", "figures", "R")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

### Grid simulation functions ###

run_grid <- function(fit_cost, trt_res, model_type = c("baseline", "hgt"), times = seq(0, 365, by = 1)) {
  model_type <- match.arg(model_type)
  if (model_type == "baseline") {
    model <- new_baseline_model(list(s_b = fit_cost, psi_r = trt_res))
  } else {
    model <- new_hgt_model(list(s_b = fit_cost, psi_r = trt_res))
  }

  output <- model$run(times)
  resistant <- output$Cp_r + output$Ip_r
  sensitive <- output$Cp_s + output$Ip_s
  auc_res <- pracma::trapz(output$time, resistant)
  auc_sus <- pracma::trapz(output$time, sensitive)
  auc_total <- auc_res + auc_sus
  aucC_res <- pracma::trapz(output$time, output$Cp_r)
  aucC_sus <- pracma::trapz(output$time, output$Cp_s)
  aucI_res <- pracma::trapz(output$time, output$Ip_r)
  aucI_sus <- pracma::trapz(output$time, output$Ip_s)

  list(
    aucC_res = aucC_res,
    aucC_sus = aucC_sus,
    aucI_res = aucI_res,
    aucI_sus = aucI_sus,
    auc_sus = auc_sus,
    auc_res = auc_res,
    auc_tot = auc_total,
    prev_col = aucC_res / (aucC_res + aucC_sus),
    prev_inf = aucI_res / (aucI_res + aucI_sus),
    prev_tot = auc_res / auc_total,
    time_series = output
  )
}

run_grid_summary <- function(fit_cost, trt_res, model_type) {
  result <- run_grid(fit_cost, trt_res, model_type)
  result$time_series <- NULL
  tibble::as_tibble(result)
}

### Baseline parameter grid ###

baseline_grid <- expand.grid(
  fit_cost = seq(0.03, 0.20, length.out = 30),
  trt_res = seq(1 / 32, 1 / 19, length.out = 5)
)
baseline_grid_results <- purrr::map2_dfr(
  baseline_grid$fit_cost,
  baseline_grid$trt_res,
  run_grid_summary,
  model_type = "baseline"
)
baseline_grid <- dplyr::bind_cols(baseline_grid, baseline_grid_results)
readr::write_csv(baseline_grid, file.path("data", "processed", "baseline_grid.csv"))

### Baseline grid plot ###

baseline_plot <- plotly::plot_ly(
  baseline_grid,
  x = ~fit_cost, y = ~I(1 / trt_res), z = ~prev_tot,
  type = "contour", contours = list(coloring = "heatmap", showlabels = TRUE)
) |>
  plotly::layout(xaxis = list(title = "Fitness cost"), yaxis = list(title = "Treatment rate for resistant infections"))
htmlwidgets::saveWidget(baseline_plot, file.path(results_dir, "baseline_grid.html"), selfcontained = TRUE)

### HGT parameter grid ###

hgt_grid <- expand.grid(
  fit_cost = seq(0.03, 0.20, length.out = 30),
  trt_res = seq(1 / 32, 1 / 19, length.out = 5)
)
hgt_grid_results <- purrr::map2_dfr(
  hgt_grid$fit_cost,
  hgt_grid$trt_res,
  run_grid_summary,
  model_type = "hgt"
)
hgt_grid <- dplyr::bind_cols(hgt_grid, hgt_grid_results)
readr::write_csv(hgt_grid, file.path("data", "processed", "hgt_grid.csv"))

### HGT grid plot ###

hgt_plot <- plotly::plot_ly(
  hgt_grid,
  x = ~fit_cost, y = ~I(1 / trt_res), z = ~prev_tot,
  type = "contour", contours = list(coloring = "heatmap", showlabels = TRUE)
) |>
  plotly::layout(xaxis = list(title = "Fitness cost"), yaxis = list(title = "Treatment rate for resistant infections"))
htmlwidgets::saveWidget(hgt_plot, file.path(results_dir, "hgt_grid.html"), selfcontained = TRUE)

### Response surface analysis ###

response_surface <- rsm::rsm(prev_tot ~ rsm::FO(fit_cost, trt_res) + rsm::TWI(fit_cost, trt_res), data = hgt_grid)
writeLines(capture.output(summary(response_surface)), file.path("data", "processed", "hgt_response_surface.txt"))

### HGT Monte Carlo trajectories ###

set.seed(140725)
hgt_ranges <- list(
  a = c(8, 15), x = c(0.15, 0.35), n = c(0.5, 0.65),
  s_b = c(0.03, 0.20), theta = c(0.35, 0.85), s_p = c(0.85, 0.98),
  low = c(2, 8), high = c(8, 24), psi_s = c(1 / 12, 1 / 7),
  psi_r = c(1 / 32, 1 / 19), iota = c(0.5, 0.8), eta = c(0.01, 0.05),
  phi = c(0.02, 0.1), mu_HGT = c(0.005, 0.015)
)
n_simulations <- 1000
sample_parameters <- function(ranges, n) {
  as.data.frame(lapply(ranges, function(range) runif(n, range[1], range[2])))
}

baseline_samples <- sample_parameters(hgt_ranges, n_simulations)
hgt_samples <- sample_parameters(hgt_ranges, n_simulations)
trajectory_rows <- vector("list", n_simulations * 2)
auc_rows <- vector("list", n_simulations * 2)

times <- seq(0, 150, by = 1)
for (simulation in seq_len(n_simulations)) {
  baseline_model <- new_baseline_model(as.list(baseline_samples[simulation, ]))
  hgt_model <- new_hgt_model(as.list(hgt_samples[simulation, ]))
  baseline_output <- baseline_model$run(times)
  hgt_output <- hgt_model$run(times)

  trajectory_rows[[2 * simulation - 1]] <- dplyr::mutate(baseline_output, simulation = simulation, scenario = "baseline")
  trajectory_rows[[2 * simulation]] <- dplyr::mutate(hgt_output, simulation = simulation, scenario = "hgt")
  auc_rows[[2 * simulation - 1]] <- tibble::tibble(
    simulation = simulation, scenario = "baseline",
    auc_sensitive = calculate_auc(baseline_output, c("Cp_s", "Ip_s")),
    auc_resistant = calculate_auc(baseline_output, c("Cp_r", "Ip_r")),
    auc_cocolonized = 0
  )
  auc_rows[[2 * simulation]] <- tibble::tibble(
    simulation = simulation, scenario = "hgt",
    auc_sensitive = calculate_auc(hgt_output, c("Cp_s", "Ip_s")),
    auc_resistant = calculate_auc(hgt_output, c("Cp_r", "Ip_r")),
    auc_cocolonized = calculate_auc(hgt_output, c("Cc", "Ic"))
  )
}

trajectories <- dplyr::bind_rows(trajectory_rows)
auc_summary <- dplyr::bind_rows(auc_rows)
readr::write_csv(auc_summary, file.path("data", "processed", "hgt_auc_summary.csv"))

### Median trajectory plot ###

median_trajectories <- trajectories |>
  tidyr::pivot_longer(
    cols = c("Cp_s", "Ip_s", "Cp_r", "Ip_r", "Cc", "Ic"),
    names_to = "compartment", values_to = "value"
  ) |>
  dplyr::group_by(scenario, compartment, time) |>
  dplyr::summarise(median_value = median(value), .groups = "drop")

png(file.path(results_dir, "hgt_median_trajectories.png"), width = 2400, height = 1800, res = 300)
print(ggplot2::ggplot(median_trajectories, ggplot2::aes(time, median_value, colour = scenario)) +
  ggplot2::geom_line() + ggplot2::facet_wrap(~compartment, scales = "free_y") +
  ggplot2::theme_classic())
dev.off()

### AUC burden plot ###

auc_long <- auc_summary |>
  dplyr::mutate(total_burden = auc_sensitive + auc_resistant + auc_cocolonized) |>
  tidyr::pivot_longer(c(auc_sensitive, auc_resistant, auc_cocolonized, total_burden), names_to = "measure", values_to = "auc")
png(file.path(results_dir, "hgt_auc_burden.png"), width = 2400, height = 1800, res = 300)
print(ggplot2::ggplot(auc_long, ggplot2::aes(scenario, auc, colour = scenario)) +
  ggplot2::geom_boxplot(outlier.shape = NA) + ggplot2::facet_wrap(~measure, scales = "free_y") +
  ggplot2::theme_classic())
dev.off()
