# ============================================================
#  Kilian & Park (2009) — Figure 5
#  Conditional covariance between responses of U.S. real stock
#  returns and inflation:  C_j(h) = r_imp_j(h) * pi_imp_j(h)
#
#  Особенности, из-за которых готовым irf() не обойтись:
#   1. C(h) — произведение откликов из ДВУХ разных VAR
#      (модель с доходностями и модель с инфляцией);
#   2. бутстрап должен быть СОВМЕСТНЫМ: статья требует сохранить
#      корреляцию остатков между двумя моделями. Независимый
#      бутстрап каждой модели (как в vars) занизил бы ширину полос.
#      Решение: recursive-design wild bootstrap (Goncalves-Kilian),
#      где один и тот же вектор eta_t умножает остатки обеих
#      моделей в момент t — кросс-корреляция сохраняется.
#
#  Знаки шоков нормировать НЕ нужно: C(h) — произведение двух
#  откликов на один и тот же шок, при смене знака шока меняются
#  оба сомножителя и произведение инвариантно.
#
#  Отклики НЕ кумулируются: в статье r_imp(h) и pi_imp(h) — это
#  отклики самих темпов (returns, inflation), оба затухают к нулю,
#  что и видно по форме Figure 5.
# ============================================================

library(vars)

H    <- 15
RUNS <- 1000          # в статье не указано; 1000 достаточно для 90% полос
set.seed(42)

# --- 1. Загрузка двух моделей (объекты называются одинаково) --
load_model <- function(path) {
  e <- new.env()
  load(path, envir = e)
  get("var_model", envir = e)
}
m_ret <- load_model("original_paper_results/svar_results.RData")     # ..., r
m_inf <- load_model("original_paper_results/svar_results-v2.RData")  # ..., pi

stopifnot(m_ret$p == m_inf$p,
          nrow(residuals(m_ret)) == nrow(residuals(m_inf)))
p <- m_ret$p
K <- m_ret$K
n <- nrow(residuals(m_ret))

ret_name <- colnames(m_ret$y)[K]    # 4-я переменная каждой модели
inf_name <- colnames(m_inf$y)[K]

# --- 2. C(h) из пары оценённых моделей ------------------------
# Psi() = Phi_s %*% chol(Sigma)' — структурные (one-SD) отклики
cond_cov <- function(fit_r, fit_p) {
  Pr <- Psi(fit_r, nstep = H)       # K x K x (H+1)
  Pp <- Psi(fit_p, nstep = H)
  r_imp  <- Pr[K, 1:3, ]            # отклик доходностей на 3 нефтяных шока
  pi_imp <- Pp[K, 1:3, ]
  r_imp * pi_imp                    # 3 x (H+1)
}
C_hat <- cond_cov(m_ret, m_inf)

# --- 3. Совместный recursive-design wild bootstrap ------------
sim_var <- function(fit, eta) {
  A  <- Bcoef(fit)                  # K x (K*p + 1), последний столбец const
  E  <- residuals(fit) * eta        # wild: общий eta для обеих моделей
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
  eta <- rnorm(n)                   # ОДИН вектор на обе модели
  fr  <- VAR(sim_var(m_ret, eta), p = p, type = "const")
  fp  <- VAR(sim_var(m_inf, eta), p = p, type = "const")
  C_boot[b, , ] <- cond_cov(fr, fp)
  if (b %% 100 == 0) cat("bootstrap:", b, "/", RUNS, "\n")
}

q <- function(probs) apply(C_boot, c(2, 3), quantile, probs = probs)
lo90 <- q(0.05); hi90 <- q(0.95)
lo80 <- q(0.10); hi80 <- q(0.90)

# --- 4. График в стиле Figure 5 -------------------------------
titles <- c("Oil supply shock",
            "Aggregate demand shock",
            "Oil-specific demand shock")

pdf("fig5_conditional_covariance.pdf", width = 10, height = 3.5)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
h    <- 0:H
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
cat("Сохранено: fig5_conditional_covariance.pdf\n")
