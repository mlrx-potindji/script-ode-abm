### Simulation visualizer ###

SimulationVisualizer <- R6::R6Class(
  "SimulationVisualizer",
  public = list(
    data = NULL,

    initialize = function(data) {
      self$data <- as.data.frame(data)
      invisible(self)
    },

    plot_compartments = function() {
      ggplot2::ggplot(self$data, ggplot2::aes(x = time)) +
        ggplot2::geom_line(ggplot2::aes(y = Sp, colour = "Susceptible")) +
        ggplot2::geom_line(ggplot2::aes(y = Cp_s, colour = "Colonized (S)")) +
        ggplot2::geom_line(ggplot2::aes(y = Cp_r, colour = "Colonized (R)")) +
        ggplot2::geom_line(ggplot2::aes(y = Ip_s, colour = "Infected (S)")) +
        ggplot2::geom_line(ggplot2::aes(y = Ip_r, colour = "Infected (R)")) +
        ggplot2::labs(
          title = "SA-MRSA Transmission Dynamics",
          x = "Time (days)", y = "Number of Individuals", colour = "Compartment"
        ) +
        ggplot2::theme_classic() +
        ggplot2::theme(legend.position = "bottom")
    },

    summary = function() {
      final <- tail(self$data, 1)
      patient_columns <- intersect(c("Sp", "Cp_s", "Ip_s", "Cp_r", "Ip_r", "Cc", "Ic"), names(final))
      list(
        total_population = sum(unlist(final[patient_columns])),
        final_state = final
      )
    },

    save_plots = function(output_dir, prefix = "model") {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      ggplot2::ggsave(file.path(output_dir, paste0(prefix, "_compartments.png")), self$plot_compartments(), width = 8, height = 6, dpi = 300)
      invisible(self)
    }
  )
)

### Visualization wrapper ###

run_sa_mrsa_model <- function(params, state, times = seq(0, 150, by = 1)) {
  model <- BaselineMRSA_Model$new(params, state)
  SimulationVisualizer$new(model$run(times))
}
