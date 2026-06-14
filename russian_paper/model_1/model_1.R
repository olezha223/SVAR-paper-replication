library(readxl)
library(vars)

df <- read_excel("russian_data/processed/DATASET.xlsx")

yt <- as.matrix(df[, c("delta_world", "rea_t", "rpo", "ret_t")])
colnames(yt) <- c("delta_prod", "rea", "rpo", "r")

stopifnot(!anyNA(yt))
cat("Наблюдений:", nrow(yt), "\n")

var_model <- VAR(yt, p = 24, type = "const")

P <- t(chol(summary(var_model)$covres))
print(round(P, 4))

save(var_model, file = "russian_paper_results/model_1/svar_results.RData")
