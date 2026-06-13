# ============================================================
#  Kilian & Park (2009) — Figure 2
#  Historical decomposition of the real price of oil
#
#  Метод (сноска 6 статьи): вклад шока j в rpo_t — это свёртка
#  структурных IRF с историческими реализациями шока j:
#      hd_j(t) = sum_{s=0}^{t-1} Theta_s[rpo, j] * eps_{j, t-s},
#  где Theta_s = Phi_s %*% P — структурные MA-коэффициенты,
#  P = chol(Sigma_e)' — импакт-матрица (Холецкий),
#  eps_t = P^{-1} e_t — восстановленные структурные шоки.
#
#  Знаки НЕ нормируются: декомпозиция показывает фактическую
#  историю, а не отклик на стилизованный шок.
#
#  В статье график начинается с 1975:1 именно потому, что при
#  выборке с 1973:1 и p = 24 первые остатки появляются в 1975:1.
# ============================================================

library(readxl)
library(vars)

load("original_paper_results/svar_results.RData")   # var_model
df <- read_excel("original_data/processed/DATASET.xlsx")

# --- 1. Структурные шоки --------------------------------------
K   <- var_model$K
p   <- var_model$p
E   <- residuals(var_model)                          # (T - p) x K
n   <- nrow(E)
P   <- t(chol(summary(var_model)$covres))            # импакт-матрица
eps <- E %*% t(solve(P))                             # eps_t' = e_t' P^{-1'}
colnames(eps) <- c("supply", "agg_demand", "oil_specific", "other")

# Проверка: структурные шоки ортонормированы.
# Важно: covres в vars использует знаменатель n - (K*p + 1)
# (df-поправка на число регрессоров), а cov() в R делит на n - 1,
# поэтому сравнивать нужно с тем же знаменателем, что и у P.
n_reg <- K * p + 1                                   # лаги + константа
S_eps <- crossprod(eps) / (n - n_reg)
stopifnot(max(abs(S_eps - diag(K))) < 1e-8)

# --- 2. Структурные MA-коэффициенты для строки rpo ------------
rpo_i <- which(colnames(var_model$y) == "rpo")
Phi_a <- Phi(var_model, nstep = n - 1)               # K x K x n

th <- matrix(NA_real_, n, K)                         # th[s+1, j] = Theta_s[rpo, j]
for (s in 0:(n - 1)) {
  th[s + 1, ] <- (Phi_a[, , s + 1] %*% P)[rpo_i, ]
}

# --- 3. Свёртка: вклад каждого шока в rpo_t -------------------
hd <- matrix(0, n, K)
for (t in 1:n) {
  hd[t, ] <- colSums(th[1:t, , drop = FALSE] * eps[t:1, , drop = FALSE])
}

# Контроль: сумма вкладов 4 шоков = rpo минус детерминированная
# компонента (константа + затухающее влияние стартовых значений),
# поэтому rowSums(hd) повторяет колебания rpo вокруг тренда базы.

# --- 4. Даты и масштаб ----------------------------------------
dates <- tail(as.Date(df$date), n)                   # остатки = последние n дат

# Если rpo в датасете — чистый логарифм (не лог x 100),
# переводим вклад в проценты, как по осям Figure 2:
scale <- if (max(abs(df$rpo), na.rm = TRUE) < 20) 100 else 1
hd <- hd * scale

# --- 5. График в стиле Figure 2 -------------------------------
titles <- c(
  "Cumulative Effect of Oil Supply Shock on Real Price of Crude Oil",
  "Cumulative Effect of Aggregate Demand Shock on Real Price of Crude Oil",
  "Cumulative Effect of Oil-Market Specific Demand Shock on Real Price of Crude Oil"
)

pdf("hd_fig2_rpo.pdf", width = 8, height = 9)
par(mfrow = c(3, 1), mar = c(3, 4, 2.5, 1))
ylim <- range(hd[, 1:3]) * 1.1
for (j in 1:3) {
  plot(dates, hd[, j], type = "l", lwd = 1.2,
       ylim = ylim, xlab = "", ylab = "Percent",
       main = titles[j], cex.main = 0.95, bty = "l")
  abline(h = 0, col = "grey60", lwd = 0.8)
}
dev.off()
cat("Сохранено: hd_fig2_rpo.pdf\n")
