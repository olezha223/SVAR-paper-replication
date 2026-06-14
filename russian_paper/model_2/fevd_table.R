library(vars)

load("russian_paper_results/model_2/svar_results.RData")

HORIZONS <- c(1, 2, 3, 12)
H_INF <- 500

fe <- fevd(var_model, n.ahead = H_INF)$r

tab <- fe[c(HORIZONS, H_INF), ] * 100
colnames(tab) <- c("Oil Supply Shock",
                   "Aggregate Demand Shock",
                   "Oil-specific Demand Shock",
                   "Other Shocks")
rownames(tab) <- c(HORIZONS, "Inf")

# проверим, что на большом горизонте сходятся результаты
stopifnot(max(abs(fe[400, ] - fe[H_INF, ])) < 1e-4)

# полученная таблица
print(round(tab, 2))
write.csv(round(tab, 2), "russian_paper_results/model_2/fevd_stock_returns.csv")
