library(vars)

H <- 15
RUNS <- 1000
set.seed(42)

load_model <- function(path) {
  e <- new.env()
  load(path, envir = e)
  get("var_model", envir = e)
}
m_ret <- load_model("original_paper_results/svar_results.RData")
m_inf <- load_model("original_paper_results/svar_results-v2.RData")

stopifnot(
  m_ret$p == m_inf$p,
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

sim_var <- function(fit, eta) {
  A <- Bcoef(fit)
  E <- residuals(fit) * eta
  y0 <- fit$y
  ys <- matrix(NA_real_, p + n, K, dimnames = list(NULL, colnames(fit$y)))
  ys[1:p, ] <- y0[1:p, ]
  for (t in (p + 1):(p + n)) {
    lags <- as.vector(t(ys[(t - 1):(t - p), , drop = FALSE]))
    ys[t, ] <- A %*% c(lags, 1) + E[t - p, ]
  }
  ys
}

C_boot <- array(NA_real_, c(RUNS, 3, H + 1))
for (b in 1:RUNS) {
  eta <- rnorm(n)
  fr <- VAR(sim_var(m_ret, eta), p = p, type = "const")
  fp <- VAR(sim_var(m_inf, eta), p = p, type = "const")
  C_boot[b, , ] <- cond_cov(fr, fp)
  if (b %% 100 == 0) cat("bootstrap:", b, "/", RUNS, "\n")
}

q <- function(probs) apply(C_boot, c(2, 3), quantile, probs = probs)
lo90 <- q(0.05); hi90 <- q(0.95)
lo80 <- q(0.10); hi80 <- q(0.90)

titles <- c(
  "Oil supply shock",
  "Aggregate demand shock",
  "Oil-specific demand shock"
)

pdf("original_paper_results/fig5_conditional_covariance.pdf", width = 10, height = 3.5)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
h <- 0:H
ylim <- range(C_hat, lo90, hi90) * 1.1
for (j in 1:3) {
  plot(h, C_hat[j, ], type = "l", lwd = 1.5, ylim = ylim,
       xlab = "Months", ylab = "Conditional covariance",
       main = titles[j], bty = "l", xaxs = "i")
  abline(h = 0, col = "grey60", lwd = 0.8)
  lines(h, lo90[j, ], lty = 3)
  lines(h, hi90[j, ], lty = 3)
  lines(h, lo80[j, ], lty = 2)
  lines(h, hi80[j, ], lty = 2)
  lines(h, C_hat[j, ], lwd = 1.5)
}
dev.off()
