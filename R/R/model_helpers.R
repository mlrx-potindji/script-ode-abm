### Model construction helpers ###

new_baseline_model <- function(parameters = list(), state = list()) {
  BaselineMRSA_Model$new(
    make_baseline_parameters(parameters),
    make_baseline_state(state)
  )
}

new_hgt_model <- function(parameters = list(), state = list()) {
  HGTModel$new(
    make_hgt_parameters(parameters),
    make_hgt_state(state)
  )
}

### deSolve-compatible model functions ###

bi_ode <- function(time, state_vector, parameters) {
  model <- new_baseline_model(
    parameters = as.list(parameters),
    state = as.list(state_vector)
  )
  model$derivatives(time, state_vector, parameters)
}

bi_ode_hgt <- function(time, state_vector, parameters) {
  model <- new_hgt_model(
    parameters = as.list(parameters),
    state = as.list(state_vector)
  )
  model$derivatives(time, state_vector, parameters)
}

run_baseline_model <- function(state, times, parameters) {
  model <- new_baseline_model(
    parameters = as.list(parameters),
    state = as.list(state)
  )
  model$run(times)
}

run_hgt_model <- function(state, times, parameters) {
  model <- new_hgt_model(
    parameters = as.list(parameters),
    state = as.list(state)
  )
  model$run(times)
}

### Result helpers ###

calculate_auc <- function(results, compartments) {
  stopifnot(all(compartments %in% names(results)))
  pracma::trapz(results$time, rowSums(results[, compartments, drop = FALSE]))
}

### Default vectors for legacy analysis inputs ###

baseline_parameters_vector <- function(overrides = list()) {
  make_baseline_parameters(overrides)$as_list()
}

hgt_parameters_vector <- function(overrides = list()) {
  make_hgt_parameters(overrides)$as_list()
}

baseline_state_vector <- function(overrides = list()) {
  make_baseline_state(overrides)$as_vector()
}

hgt_state_vector <- function(overrides = list()) {
  make_hgt_state(overrides)$as_vector()
}
