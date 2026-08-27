### Corrected HGT model ###

HGTModel <- R6::R6Class(
  "HGTModel",
  inherit = BaselineMRSA_Model,
  public = list(
    initialize = function(params, state) {
      stopifnot(inherits(params, "HGTParameters"), inherits(state, "HGTState"))
      super$initialize(params, state)
    },

    calculate_beta_pn_s = function() {
      p <- self$params
      s <- self$state
      phi_ratio <- s$get_Np() / s$get_Nn()
      p$a * p$b_pn * (1 - p$theta) * phi_ratio *
        (p$n * s$Cp_s + s$Ip_s + p$n * s$Cc + s$Ic) / s$get_Np()
    },

    calculate_beta_pn_r = function() {
      p <- self$params
      s <- self$state
      phi_ratio <- s$get_Np() / s$get_Nn()
      p$a * p$b_pn * (1 - p$theta) * (1 - p$s_b) * phi_ratio *
        (p$n * s$Cp_r + s$Ip_r + p$n * s$Cc + s$Ic) / s$get_Np()
    },

    calculate_discordant_rates = function() {
      p <- self$params
      s <- self$state
      list(
        Cp_s_to_Cc = p$a * p$b_np * (1 - p$theta) * (1 - p$s_b) * s$Cn_r / s$get_Nn() * s$Cp_s,
        Cp_r_to_Cc = p$a * p$b_np * (1 - p$theta) * s$Cn_s / s$get_Nn() * s$Cp_r,
        Ip_s_to_Ic = p$a * p$b_np * (1 - p$theta) * (1 - p$s_b) * s$Cn_r / s$get_Nn() * s$Ip_s,
        Ip_r_to_Ic = p$a * p$b_np * (1 - p$theta) * s$Cn_s / s$get_Nn() * s$Ip_r
      )
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
      rates <- self$calculate_discordant_rates()

      dSp <- p$nu_admission -
        (beta_np_s + beta_np_r) * s$Sp - p$tau_s * s$Sp +
        p$mu_c * s$Cp_s + p$mu_c * s$Cp_r + p$mu_c * s$Cc +
        mu_i_s * s$Ip_s + mu_i_r * s$Ip_r +
        mu_i_s * s$Ic + mu_i_r * s$Ic +
        p$tau_r * s$Cp_r + p$tau_r * s$Cc + p$tau_r * s$Ic -
        p$mu_discharge * s$Sp

      dCp_s <- (1 - p$x) * beta_np_s * s$Sp + p$tau_s * s$Sp -
        (p$gamma_s + p$mu_c + p$n * p$eta) * s$Cp_s -
        rates$Cp_s_to_Cc - p$mu_discharge * s$Cp_s

      dCp_r <- (1 - p$x) * beta_np_r * s$Sp + p$mu_HGT * s$Cc -
        (p$gamma_r + p$mu_c + p$n * p$eta) * s$Cp_r -
        rates$Cp_r_to_Cc - p$mu_discharge * s$Cp_r

      dCc <- p$n * p$eta * (s$Cp_s + s$Cp_r) +
        rates$Cp_s_to_Cc + rates$Cp_r_to_Cc -
        (p$phi + p$mu_c + p$tau_r + p$mu_HGT) * s$Cc -
        p$mu_discharge * s$Cc

      dIp_s <- p$x * beta_np_s * s$Sp + p$gamma_s * s$Cp_s -
        (Psi_s + mu_i_s + p$eta) * s$Ip_s -
        rates$Ip_s_to_Ic - p$mu_discharge * s$Ip_s

      dIp_r <- p$x * beta_np_r * s$Sp + p$gamma_r * s$Cp_r + p$mu_HGT * s$Ic -
        (Psi_r + mu_i_r + p$eta) * s$Ip_r -
        rates$Ip_r_to_Ic - p$mu_discharge * s$Ip_r

      dIc <- p$phi * s$Cc + p$eta * (s$Ip_s + s$Ip_r) +
        rates$Ip_s_to_Ic + rates$Ip_r_to_Ic -
        (Psi_r + Psi_s + mu_i_r + mu_i_s + p$tau_r + p$mu_HGT) * s$Ic -
        p$mu_discharge * s$Ic

      dUn <- -(beta_pn_r + beta_pn_s) * s$Un + p$delta * (s$Cn_r + s$Cn_s)
      dCn_s <- beta_pn_s * s$Un - p$delta * s$Cn_s
      dCn_r <- beta_pn_r * s$Un - p$delta * s$Cn_r

      list(c(dSp, dCp_s, dIp_s, dCp_r, dIp_r, dCc, dIc, dUn, dCn_s, dCn_r))
    }
  )
)
