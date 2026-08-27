### Baseline MRSA model ###

BaselineMRSA_Model <- R6::R6Class(
  "BaselineMRSA_Model",
  public = list(
    params = NULL,
    state = NULL,
    initial_state = NULL,
    results = NULL,
    times = NULL,

    initialize = function(params, state) {
      stopifnot(inherits(params, "ModelParameters"), inherits(state, "ModelState"))
      self$params <- params
      self$state <- state
      self$initial_state <- state$clone(deep = TRUE)
      self$params$validate()
      self$state$validate()
      invisible(self)
    },

    calculate_beta_np_s = function() {
      p <- self$params
      s <- self$state
      p$a * p$b_np * (1 - p$theta) * s$Cn_s / s$get_Nn()
    },

    calculate_beta_pn_s = function() {
      p <- self$params
      s <- self$state
      phi <- s$get_Np() / s$get_Nn()
      p$a * p$b_pn * (1 - p$theta) * phi * (p$n * s$Cp_s + s$Ip_s) / s$get_Np()
    },

    calculate_beta_np_r = function() {
      p <- self$params
      s <- self$state
      p$a * p$b_np * (1 - p$theta) * (1 - p$s_b) * s$Cn_r / s$get_Nn()
    },

    calculate_beta_pn_r = function() {
      p <- self$params
      s <- self$state
      phi <- s$get_Np() / s$get_Nn()
      p$a * p$b_pn * (1 - p$theta) * phi * (1 - p$s_b) * (p$n * s$Cp_r + s$Ip_r) / s$get_Np()
    },

    calculate_mu_i_s = function() self$params$psi_s * (1 - self$params$iota),
    calculate_mu_i_r = function() self$params$psi_r * (1 - self$params$iota * (1 - self$params$s_p)),
    calculate_Psi_s = function() self$params$psi_s * self$params$iota,
    calculate_Psi_r = function() self$params$psi_r * self$params$iota * (1 - self$params$s_p),

    calculate_Lambda = function() {
      p <- self$params
      s <- self$state
      p$tau_s * (s$Sp + s$Cp_s) +
        p$mu_c * (s$Cp_r + s$Cp_s) +
        p$tau_r * s$Cp_r +
        self$calculate_Psi_r() * s$Ip_r +
        self$calculate_Psi_s() * s$Ip_s +
        self$calculate_mu_i_s() * s$Ip_s +
        self$calculate_mu_i_r() * s$Ip_r
    },

    derivatives = function(time, state_vector, parameters) {
      self$state$from_vector(state_vector)
      p <- self$params
      s <- self$state

      beta_np_s <- self$calculate_beta_np_s()
      beta_pn_s <- self$calculate_beta_pn_s()
      beta_np_r <- self$calculate_beta_np_r()
      beta_pn_r <- self$calculate_beta_pn_r()
      mu_i_s <- self$calculate_mu_i_s()
      mu_i_r <- self$calculate_mu_i_r()
      Psi_s <- self$calculate_Psi_s()
      Psi_r <- self$calculate_Psi_r()
      Lambda <- self$calculate_Lambda()

      dSp <- (1 - p$lambda_cr - p$lambda_cs - p$lambda_ir - p$lambda_is) * Lambda -
        (beta_np_r + beta_np_s) * s$Sp - p$tau_s * s$Sp
      dCp_s <- p$lambda_cs * Lambda + (1 - p$x) * beta_np_s * s$Sp -
        (p$gamma_s + p$tau_s + p$mu_c) * s$Cp_s
      dIp_s <- p$lambda_is * Lambda + p$x * beta_np_s * s$Sp + p$gamma_s * s$Cp_s -
        (Psi_s + mu_i_s) * s$Ip_s
      dCp_r <- p$lambda_cr * Lambda + (1 - p$x) * beta_np_r * s$Sp -
        (p$gamma_r + p$tau_r + p$mu_c) * s$Cp_r
      dIp_r <- p$lambda_ir * Lambda + p$x * beta_np_r * s$Sp + p$gamma_r * s$Cp_r -
        (Psi_r + mu_i_r) * s$Ip_r
      dUn <- -(beta_pn_r + beta_pn_s) * s$Un + p$delta * (s$Cn_r + s$Cn_s)
      dCn_s <- beta_pn_s * s$Un - p$delta * s$Cn_s
      dCn_r <- beta_pn_r * s$Un - p$delta * s$Cn_r

      list(c(dSp, dCp_s, dIp_s, dCp_r, dIp_r, dUn, dCn_s, dCn_r))
    },

    run = function(times = seq(0, 150, by = 1)) {
      self$times <- times
      self$state <- self$initial_state$clone(deep = TRUE)
      self$results <- deSolve::ode(
        y = self$state$as_vector(),
        times = times,
        func = self$derivatives,
        parms = self$params$as_list()
      )
      self$state$from_vector(tail(self$results[, -1, drop = FALSE], 1))
      as.data.frame(self$results)
    },

    get_results = function() {
      if (is.null(self$results)) stop("Model has not been run yet.")
      as.data.frame(self$results)
    }
  )
)
