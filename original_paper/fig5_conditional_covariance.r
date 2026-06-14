library(vars)

H <- 15
RUNS <- 2000
SEED <- 42

load_model <- function(path) {
  e <- new.env()
  load(path, envir = e)
  get("var_model", envir = e)
}

m_ret <- load_model("original_paper_results/svar_results.RData")
m_inf <- load_model("original_paper_results/svar_results-v2.RData")

stopifnot(
  m_ret$p == m_inf$p,
  m_ret$K == m_inf$K,
  nrow(residuals(m_ret)) == nrow(residuals(m_inf))
)

p <- m_ret$p
K <- m_ret$K
n <- nrow(residuals(m_ret))

ret_name <- colnames(m_ret$y)[K]
inf_name <- colnames(m_inf$y)[K]

# Условная ковариация между откликами американских доходностей и инфляцией
cond_cov <- function(fit_r, fit_p) {
  Pr <- Psi(fit_r, nstep = H)
  Pp <- Psi(fit_p, nstep = H)

  r_imp <- Pr[K, 1:3, ]
  pi_imp <- Pp[K, 1:3, ]

  r_imp * pi_imp
}

C_hat <- cond_cov(m_ret, m_inf)

# Recursive-design wild bootstrap.
# Один и тот же множитель eta_t применяется ко всему вектору остатков
# в момент t. Для двух VAR используется одна и та же последовательность eta,
# чтобы сохранить зависимость между оценками доходностей и инфляции.
sim_var <- function(fit, eta) {
  A <- Bcoef(fit)

  E <- residuals(fit)
  E <- sweep(E, 2, colMeans(E), FUN = "-")
  E <- sweep(E, 1, eta, FUN = "*")

  y0 <- fit$y

  ys <- matrix(
    NA_real_,
    nrow = p + n,
    ncol = K,
    dimnames = list(NULL, colnames(fit$y))
  )

  # Первые p наблюдений фиксируем на исходных значениях.
  ys[1:p, ] <- y0[1:p, ]

  for (t in (p + 1):(p + n)) {
    # Порядок регрессоров:
    # все переменные с лагом 1, затем все переменные с лагом 2 и т.д.
    lags <- as.vector(t(ys[(t - 1):(t - p), , drop = FALSE]))

    ys[t, ] <- drop(A %*% c(lags, 1)) + E[t - p, ]
  }

  ys
}

C_boot <- array(
  NA_real_,
  dim = c(RUNS, 3, H + 1),
  dimnames = list(
    replication = seq_len(RUNS),
    shock = c("delta_prod", "rea", "rpo"),
    horizon = 0:H
  )
)

set.seed(SEED)

for (b in seq_len(RUNS)) {
  eta <- rnorm(n)

  y_ret_star <- sim_var(m_ret, eta)
  y_inf_star <- sim_var(m_inf, eta)

  fr <- VAR(y_ret_star, p = p, type = "const")
  fp <- VAR(y_inf_star, p = p, type = "const")

  C_boot[b, , ] <- cond_cov(fr, fp)

  if (b %% 100 == 0 || b == RUNS) {
    cat("Bootstrap:", b, "из", RUNS, "\n")
  }
}

# В отличие от percentile-интервалов, здесь строятся полосы
# вокруг исходной точечной оценки: точка +/- 1 и 2 bootstrap SE.
C_se <- apply(C_boot, c(2, 3), sd, na.rm = TRUE)

lo1 <- C_hat - C_se
hi1 <- C_hat + C_se
lo2 <- C_hat - 2 * C_se
hi2 <- C_hat + 2 * C_se

titles <- c(
  "Oil supply shock",
  "Aggregate demand shock",
  "Oil-specific demand shock"
)

pdf(
  "original_paper_results/fig5_conditional_covariance.pdf",
  width = 10,
  height = 3.5
)

par(
  mfrow = c(1, 3),
  mar = c(4, 4, 3, 1)
)

h <- 0:H

ylim <- range(C_hat, lo2, hi2, finite = TRUE)
padding <- diff(ylim) * 0.08

if (!is.finite(padding) || padding == 0) {
  padding <- 1
}

ylim <- ylim + c(-padding, padding)

for (j in 1:3) {
  plot(
    h,
    C_hat[j, ],
    type = "l",
    lwd = 1.7,
    ylim = ylim,
    xlab = "Months",
    ylab = "Conditional covariance",
    main = titles[j],
    bty = "l",
    xaxs = "i",
    xaxt = "n"
  )

  axis(1, at = seq(0, H, by = 5))
  abline(h = 0, col = "grey60", lwd = 0.8)

  lines(h, lo2[j, ], lty = 3, lwd = 1.2)
  lines(h, hi2[j, ], lty = 3, lwd = 1.2)

  lines(h, lo1[j, ], lty = 2, lwd = 1.2)
  lines(h, hi1[j, ], lty = 2, lwd = 1.2)

  lines(h, C_hat[j, ], lwd = 1.7)
}

dev.off()

saveRDS(
  list(
    point = C_hat,
    bootstrap = C_boot,
    se = C_se,
    lo1 = lo1,
    hi1 = hi1,
    lo2 = lo2,
    hi2 = hi2,
    runs = RUNS,
    seed = SEED
  ),
  file = "original_paper_results/fig5_conditional_covariance_bootstrap.rds"
)

cat(
  "Сохранено: original_paper_results/fig5_conditional_covariance.pdf\n"
)
