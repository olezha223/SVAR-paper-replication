# ============================================================
#  Kilian & Park (2009) — оценка модели
#  Рекурсивная идентификация = разложение Холецкого,
#  поэтому vars::SVAR() не нужен вовсе: irf(..., ortho = TRUE)
#  на reduced-form объекте даёт one-SD структурные шоки,
#  ровно как в статье.
# ============================================================

library(readxl)
library(vars)

# --- 1. Данные ------------------------------------------------
# Лучше брать ряды из репликационного архива (точные данные статьи,
# выборка 1973:2-2006:12). Свой датасет — для робастности.
df <- read_excel("original_data/processed/DATASET.xlsx")

# Порядок переменных строго как в уравнении (2) статьи
yt <- as.matrix(df[, c("delta_world", "rea_t", "rpo", "ret_t")])
colnames(yt) <- c("delta_prod", "rea", "rpo", "r")

stopifnot(!anyNA(yt))
cat("Наблюдений:", nrow(yt), "\n")

# --- 2. Reduced-form VAR(24) ----------------------------------
var_model <- VAR(yt, p = 24, type = "const")

# --- 3. Импакт-матрица (для справки) --------------------------
# Холецкий ковариационной матрицы остатков = A0^{-1} * diag(sd):
# столбец j — мгновенная реакция переменных на one-SD шок j
P <- t(chol(summary(var_model)$covres))
cat("\nИмпакт-матрица (one-SD шоки):\n")
print(round(P, 4))

# --- 4. Сохранение --------------------------------------------
dir.create("original_paper_results", showWarnings = FALSE)
save(var_model, file = "original_paper_results/svar_results.RData")
cat("\nСохранено: original_paper_results/svar_results.RData\n")
