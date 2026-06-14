library(readxl)
library(vars)

df <- read_excel("original_data/processed/DATASET-extended.xlsx")

var_name <- "oil_industry" # МЕНЯТЬ ЗДЕСЬ НАЗВАНИЕ ПЕРЕМЕННОЙ
yt <- as.matrix(df[, c("delta_world", "rea_t", "rpo", var_name)])
colnames(yt) <- c("delta_prod", "rea", "rpo", var_name)

stopifnot(!anyNA(yt))
cat("Наблюдений:", nrow(yt), "\n")

var_model <- VAR(yt, p = 24, type = "const")

P <- t(chol(summary(var_model)$covres))
print(round(P, 4))

industry_dir <- file.path("original_paper_results", "industry_results")
output_file <- file.path(industry_dir, paste0(var_name, ".RData"))
save(var_model, file = output_file)
