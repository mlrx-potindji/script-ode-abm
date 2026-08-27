### Baseline model factory ###

make_baseline_parameters <- function(overrides = list()) {
  defaults <- list(
    a = 8, x = 0.2, n = 0.65, s_b = 0.2, theta = 0.45,
    s_p = 0.95, psi_s = 0.0833, psi_r = 0.0454,
    iota = 0.8, low = 2, high = 8
  )
  values <- modifyList(defaults, overrides)
  values <- values[names(defaults)]
  model_parameters <- do.call(ModelParameters$new, values)
  model_parameters$validate()
  model_parameters
}

make_baseline_state <- function(overrides = list()) {
  defaults <- list(Sp = 560, Cp_s = 0, Ip_s = 0, Cp_r = 0,
                   Ip_r = 0, Un = 148, Cn_s = 1, Cn_r = 1)
  model_state <- do.call(ModelState$new, modifyList(defaults, overrides))
  model_state$validate()
  model_state
}

### HGT model factory ###

make_hgt_parameters <- function(overrides = list()) {
  defaults <- list(
    a = 8, x = 0.2, n = 0.65, s_b = 0.2, theta = 0.45,
    s_p = 0.95, psi_s = 0.0833, psi_r = 0.0454,
    iota = 0.8, low = 2, high = 8, eta = 0.01,
    phi = 0.02, mu_HGT = 0.005, nu_admission = 60,
    mu_discharge = 0.1
  )
  values <- modifyList(defaults, overrides)
  values <- values[names(defaults)]
  hgt_parameters <- do.call(HGTParameters$new, values)
  hgt_parameters$validate()
  hgt_parameters
}

make_hgt_state <- function(overrides = list()) {
  defaults <- list(Sp = 560, Cp_s = 0, Ip_s = 0, Cp_r = 0,
                   Ip_r = 0, Cc = 0, Ic = 0, Un = 148, Cn_s = 1, Cn_r = 1)
  hgt_state <- do.call(HGTState$new, modifyList(defaults, overrides))
  hgt_state$validate()
  hgt_state
}
