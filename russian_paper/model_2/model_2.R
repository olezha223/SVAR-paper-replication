library(readxl)
library(vars)

df <- read_excel("russian_data/processed/DATASET.xlsx")

yt <- as.matrix(df[, c("delta_non_rus", "rea_t", "rpo", "ret_t")])
colnames(yt) <- c("delta_prod", "rea", "rpo", "r")

stopifnot(!anyNA(yt))
cat("Наблюдений:", nrow(yt), "\n")

var_model <- VAR(yt, p = 24, type = "const")

P <- t(chol(summary(var_model)$covres))
cat("\nИмпакт-матрица (one-SD шоки):\n")
print(round(P, 4))

save(var_model, file = "russian_paper_results/model_2/svar_results.RData")
cat("\nСохранено: russian_paper_results/svar_results.RData\n")
