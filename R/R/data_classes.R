### Baseline parameter class ###

ModelParameters <- R6::R6Class(
  "ModelParameters",
  public = list(
    a = NULL,
    x = NULL,
    n = NULL,
    s_b = NULL,
    theta = NULL,
    s_p = NULL,
    psi_s = NULL,
    psi_r = NULL,
    iota = NULL,
    low = NULL,
    high = NULL,
    delta = NULL,
    gamma_s = NULL,
    gamma_r = NULL,
    tau_s = NULL,
    tau_r = NULL,
    kappa = 0.1428,
    m_s = 0.3,
    delta_m = 0.25,
    mu_c = 0.015,
    b_np = 0.09,
    b_pn = 0.3,
    lambda_cs = 0.2,
    lambda_cr = 0.02,
    lambda_is = 0.01,
    lambda_ir = 0.01,

    initialize = function(a, x, n, s_b, theta, s_p, psi_s, psi_r, iota, low, high) {
      self$a <- a
      self$x <- x
      self$n <- n
      self$s_b <- s_b
      self$theta <- theta
      self$s_p <- s_p
      self$psi_s <- psi_s
      self$psi_r <- psi_r
      self$iota <- iota
      self$low <- low
      self$high <- high

      self$delta <- (1 - theta) * low + theta * high
      self$gamma_s <- self$kappa * self$m_s
      self$gamma_r <- self$kappa * self$m_s * (1 + self$delta_m)
      self$tau_s <- self$kappa * (1 - self$m_s)
      self$tau_r <- self$kappa * (1 - self$m_s * (1 + self$delta_m))
      invisible(self)
    },

    validate = function() {
      stopifnot(
        "a must be positive" = length(self$a) == 1 && self$a > 0,
        "x must be between 0 and 1" = self$x >= 0 && self$x <= 1,
        "n must be between 0 and 1" = self$n >= 0 && self$n <= 1,
        "s_b must be between 0 and 1" = self$s_b >= 0 && self$s_b <= 1,
        "theta must be between 0 and 1" = self$theta >= 0 && self$theta <= 1,
        "s_p must be between 0 and 1" = self$s_p >= 0 && self$s_p <= 1,
        "treatment rates must be positive" = self$psi_s > 0 && self$psi_r > 0,
        "iota must be between 0 and 1" = self$iota >= 0 && self$iota <= 1,
        "low must be non-negative" = self$low >= 0,
        "high must be at least low" = self$high >= self$low
      )
      invisible(self)
    },

    as_list = function() {
      list(
        a = self$a, x = self$x, n = self$n, s_b = self$s_b,
        theta = self$theta, s_p = self$s_p,
        psi_s = self$psi_s, psi_r = self$psi_r, iota = self$iota,
        low = self$low, high = self$high, delta = self$delta,
        gamma_s = self$gamma_s, gamma_r = self$gamma_r,
        tau_s = self$tau_s, tau_r = self$tau_r,
        kappa = self$kappa, m_s = self$m_s, delta_m = self$delta_m,
        mu_c = self$mu_c, b_np = self$b_np, b_pn = self$b_pn,
        lambda_cs = self$lambda_cs, lambda_cr = self$lambda_cr,
        lambda_is = self$lambda_is, lambda_ir = self$lambda_ir
      )
    }
  )
)

### Baseline state class ###

ModelState <- R6::R6Class(
  "ModelState",
  public = list(
    Sp = NULL, Cp_s = NULL, Ip_s = NULL, Cp_r = NULL,
    Ip_r = NULL, Un = NULL, Cn_s = NULL, Cn_r = NULL,

    initialize = function(Sp, Cp_s, Ip_s, Cp_r, Ip_r, Un, Cn_s, Cn_r) {
      self$Sp <- Sp
      self$Cp_s <- Cp_s
      self$Ip_s <- Ip_s
      self$Cp_r <- Cp_r
      self$Ip_r <- Ip_r
      self$Un <- Un
      self$Cn_s <- Cn_s
      self$Cn_r <- Cn_r
      invisible(self)
    },

    validate = function() {
      values <- self$as_vector()
      stopifnot("state values must be finite and non-negative" = all(is.finite(values) & values >= 0))
      invisible(self)
    },

    as_vector = function() {
      c(Sp = self$Sp, Cp_s = self$Cp_s, Ip_s = self$Ip_s,
        Cp_r = self$Cp_r, Ip_r = self$Ip_r, Un = self$Un,
        Cn_s = self$Cn_s, Cn_r = self$Cn_r)
    },

    from_vector = function(vec) {
      stopifnot(length(vec) == 8)
      self$Sp <- unname(vec[1])
      self$Cp_s <- unname(vec[2])
      self$Ip_s <- unname(vec[3])
      self$Cp_r <- unname(vec[4])
      self$Ip_r <- unname(vec[5])
      self$Un <- unname(vec[6])
      self$Cn_s <- unname(vec[7])
      self$Cn_r <- unname(vec[8])
      invisible(self)
    },

    get_Np = function() sum(self$Sp, self$Cp_s, self$Ip_s, self$Cp_r, self$Ip_r),
    get_Nn = function() sum(self$Un, self$Cn_s, self$Cn_r)
  )
)

### HGT parameter class ###

HGTParameters <- R6::R6Class(
  "HGTParameters",
  inherit = ModelParameters,
  public = list(
    eta = NULL,
    phi = NULL,
    mu_HGT = NULL,
    nu_admission = NULL,
    mu_discharge = NULL,

    initialize = function(..., eta, phi, mu_HGT, nu_admission, mu_discharge) {
      super$initialize(...)
      self$eta <- eta
      self$phi <- phi
      self$mu_HGT <- mu_HGT
      self$nu_admission <- nu_admission
      self$mu_discharge <- mu_discharge
      self$validate()
      invisible(self)
    },

    validate = function() {
      super$validate()
      stopifnot(
        "HGT rate must be non-negative" = self$eta >= 0,
        "co-infection rate must be non-negative" = self$phi >= 0,
        "HGT transfer rate must be non-negative" = self$mu_HGT >= 0,
        "admission rate must be non-negative" = self$nu_admission >= 0,
        "discharge rate must be non-negative" = self$mu_discharge >= 0
      )
      invisible(self)
    },

    as_list = function() {
      c(super$as_list(), list(
        eta = self$eta, phi = self$phi, mu_HGT = self$mu_HGT,
        nu_admission = self$nu_admission, mu_discharge = self$mu_discharge
      ))
    }
  )
)

### HGT state class ###

HGTState <- R6::R6Class(
  "HGTState",
  inherit = ModelState,
  public = list(
    Cc = NULL,
    Ic = NULL,

    initialize = function(Sp, Cp_s, Ip_s, Cp_r, Ip_r, Cc, Ic, Un, Cn_s, Cn_r) {
      super$initialize(Sp, Cp_s, Ip_s, Cp_r, Ip_r, Un, Cn_s, Cn_r)
        self$Cc <- Cc
        self$Ic <- Ic
      self$validate()
      invisible(self)
    },

    validate = function() {
      super$validate()
      stopifnot("HGT state values must be finite and non-negative" = all(is.finite(c(self$Cc, self$Ic)) & c(self$Cc, self$Ic) >= 0))
      invisible(self)
    },

    as_vector = function() {
      c(Sp = self$Sp, Cp_s = self$Cp_s, Ip_s = self$Ip_s,
        Cp_r = self$Cp_r, Ip_r = self$Ip_r, Cc = self$Cc,
        Ic = self$Ic, Un = self$Un, Cn_s = self$Cn_s, Cn_r = self$Cn_r)
    },

    from_vector = function(vec) {
      stopifnot(length(vec) == 10)
      self$Sp <- unname(vec[1]); self$Cp_s <- unname(vec[2])
      self$Ip_s <- unname(vec[3]); self$Cp_r <- unname(vec[4])
      self$Ip_r <- unname(vec[5]); self$Cc <- unname(vec[6])
      self$Ic <- unname(vec[7]); self$Un <- unname(vec[8])
      self$Cn_s <- unname(vec[9]); self$Cn_r <- unname(vec[10])
      invisible(self)
    },

    get_Np = function() sum(self$Sp, self$Cp_s, self$Ip_s, self$Cp_r, self$Ip_r, self$Cc, self$Ic)
  )
)
