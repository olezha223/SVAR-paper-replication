library(readxl)
library(vars)

load("russian_paper_results/model_3/svar_results.RData")
df <- read_excel("russian_data/processed/DATASET-extended.xlsx")

K <- var_model$K
p <- var_model$p
E <- residuals(var_model)
n <- nrow(E)
P <- t(chol(summary(var_model)$covres))
eps <- E %*% t(solve(P))
colnames(eps) <- c("supply", "agg_demand", "oil_specific", "other")

# проверим что структурные шоки ортонормированы
n_reg <- K * p + 1 # лаги + константа
S_eps <- crossprod(eps) / (n - n_reg)
stopifnot(max(abs(S_eps - diag(K))) < 1e-8)

rpo_i <- which(colnames(var_model$y) == "rpo")
Phi_a <- Phi(var_model, nstep = n - 1)

th <- matrix(NA_real_, n, K)
for (s in 0:(n - 1)) {
  th[s + 1, ] <- (Phi_a[,, s + 1] %*% P)[rpo_i, ]
}

hd <- matrix(0, n, K)
for (t in 1:n) {
  hd[t, ] <- colSums(th[1:t, , drop = FALSE] * eps[t:1, , drop = FALSE])
}

# остатки последние n дат
dates <- tail(as.Date(df$date), n)

# переводим в проценты
scale <- if (max(abs(df$rpo), na.rm = TRUE) < 20) 100 else 1
hd <- hd * scale

titles <- c(
  "Cumulative Effect of Oil Supply Shock on Real Price of Crude Oil",
  "Cumulative Effect of Aggregate Demand Shock on Real Price of Crude Oil",
  "Cumulative Effect of Oil-Market Specific Demand Shock on Real Price of Crude Oil"
)

pdf("russian_paper_results/model_3/fig2_hist_decomp.pdf", width = 8, height = 6)
par(mfrow = c(3, 1), mar = c(3, 4, 2.5, 1))
ylim <- range(hd[, 1:3]) * 1.1
for (j in 1:3) {
  plot(
    dates,
    hd[, j],
    type = "l",
    lwd = 1.2,
    ylim = ylim,
    xlab = "",
    ylab = "Percent",
    main = titles[j],
    cex.main = 0.95,
    bty = "l"
  )
  abline(h = 0, col = "grey60", lwd = 0.8)
}
dev.off()
