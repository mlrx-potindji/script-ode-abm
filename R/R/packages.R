### Package loading ###

load_model_packages <- function(include_analysis = FALSE) {
  packages <- c("R6", "deSolve", "ggplot2", "readr")
  if (include_analysis) {
    packages <- c(packages, "pracma", "dplyr", "purrr", "tidyr", "tibble", "readr", "future", "furrr", "sensitivity", "plotly", "htmlwidgets", "rsm", "ggpubr")
  }

  missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop("Install required R packages: ", paste(missing_packages, collapse = ", "))
  }

  invisible(lapply(packages, library, character.only = TRUE))
}
