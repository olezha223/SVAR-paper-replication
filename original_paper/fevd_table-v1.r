# ============================================================
#  Kilian & Park (2009) — Table 1
#  Percent contribution of demand and supply shocks in the
#  crude oil market to the variability of U.S. real stock returns
#
#  Это forecast error variance decomposition (FEVD) доходностей
#  при той же рекурсивной идентификации. Знаки шоков роли не
#  играют (дисперсия), нормировки не нужны.
#
#  Горизонт "бесконечность" аппроксимируется большим h: для
#  стационарной системы FEVD сходится; h = 500 хватает с запасом.
# ============================================================

library(vars)

load("original_paper_results/svar_results.RData")   # var_model

HORIZONS <- c(1, 2, 3, 12)
H_INF    <- 500

# --- 1. FEVD (Холецкий внутри, как и в irf(ortho = TRUE)) -----
fe <- fevd(var_model, n.ahead = H_INF)$r            # H_INF x K, доли

tab <- fe[c(HORIZONS, H_INF), ] * 100
colnames(tab) <- c("Oil Supply Shock",
                   "Aggregate Demand Shock",
                   "Oil-specific Demand Shock",
                   "Other Shocks")
rownames(tab) <- c(HORIZONS, "Inf")

# Контроль сходимости на "бесконечности": вклад на h = 400 и
# h = 500 должен совпадать, иначе увеличить H_INF
stopifnot(max(abs(fe[400, ] - fe[H_INF, ])) < 1e-4)

# --- 2. Вывод -------------------------------------------------
cat("\nTable 1. Percent contribution to the variability",
    "of U.S. real stock returns\n\n")
print(round(tab, 2))

oil_total <- rowSums(tab[, 1:3])
cat("\nСуммарный вклад нефтяных шоков по горизонтам:\n")
print(round(oil_total, 2))

write.csv(round(tab, 2), "table1_fevd_stock_returns.csv")
cat("\nСохранено: table1_fevd_stock_returns.csv\n")
