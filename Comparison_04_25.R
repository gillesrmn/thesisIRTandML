library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(cocor)
library(openxlsx)

items_long <- Use.case.data %>%
  dplyr::select(EI3, EI9, EI10, EI12, EI14, EI16,
                EI17, EI18, EI19, EI20, EI22, EI23,
                EI27, EI29, EI30, EI31, EI32)

items_irt <- items_long %>%
  dplyr::select(EI20, EI31, EI17,
                EI23)

items_rf <- items_long %>%
  dplyr::select(EI12, EI22, EI32,
                EI29)

items_cit <- items_long %>%
  dplyr::select(EI12, EI22, EI20,
                EI31)

items_ga <- items_long %>%
  dplyr::select(EI12, EI23, EI27, EI31)

# Build test_04 with EI items + PC items + scores
test_04 <- items_long %>%
  bind_cols(Use.case.data %>% select(starts_with("PC"), starts_with("PN")))

test_04$EI_long <- rowMeans(test_04[, c("EI3", "EI9", "EI10", "EI12", "EI14", "EI16",
                                        "EI17", "EI18", "EI19", "EI20", "EI22", "EI23",
                                        "EI27", "EI29", "EI30", "EI31", "EI32")])

big5_vars_pc <- test_04 %>%
  select(starts_with("PC")) %>%
  names()

big5_vars_pn <- test_04 %>%
  select(starts_with("PN")) %>%
  names()

test_04$PC_score <- rowMeans(test_04[, big5_vars_pc])
big5_vars_pc <- "PC_score"
test_04$PN_score <- rowMeans(test_04[, big5_vars_pn])
big5_vars_pn <- "PN_score"

big5_vars <- c("PC_score", "PN_score")

n <- nrow(test_04)

mean_r_big5 <- function(ei_var) {
  mean(sapply(big5_vars, function(b5) cor(test_04[[ei_var]], test_04[[b5]])))
}


#### LONG SCALE ####
model_long <- "F =~ EI3 + EI9 + EI10 + EI12 + EI14 + EI16 +
                    EI17 + EI18 + EI19 + EI20 + EI22 + EI23 +
                    EI27 + EI29 + EI30 + EI31 + EI32"

fit_long <- cfa(model_long,
                data = test_04,
                std.lv = TRUE,
                estimator = "WLSMV",
                ordered = TRUE)
summary(fit_long, fit.measures = TRUE, standardized = TRUE)

alpha_long <- psych::alpha(items_long)
alpha_long$total$raw_alpha

omega_long <- semTools::reliability(fit_long)
omega_long


#### IRT SHORT SCALE ####
model_irt <- "F =~ EI20 + EI31 + EI17 +
                   EI23"

fit_irt <- cfa(model_irt,
               data = test_04,
               std.lv = TRUE,
               estimator = "WLSMV",
               ordered = TRUE)
summary(fit_irt, fit.measures = TRUE, standardized = TRUE)

alpha_irt <- psych::alpha(items_irt)
alpha_irt$total$raw_alpha

omega_irt <- semTools::reliability(fit_irt)
omega_irt

test_04$EI_irt <- rowMeans(test_04[, c("EI20", "EI31", "EI17",
                                       "EI23")])
r_long_irt <- cor(test_04$EI_long, test_04$EI_irt)

for (b5 in big5_vars) {
  r_long  <- cor(test_04$EI_long, test_04[, b5])
  r_short <- cor(test_04$EI_irt,  test_04[, b5])
  result  <- cocor.dep.groups.overlap(r.jk = r_long, r.jh = r_short,
                                      r.kh = r_long_irt, n = n)
  cat(b5, "- r(long):", round(r_long, 3),
      "| r(irt):", round(r_short, 3),
      "| Williams p:", round(result@williams1959$p.value, 4), "\n")
}


#### RF SHORT SCALE ####
model_rf <- "F =~ EI12 + EI22 + EI32 +
                  EI29"

fit_rf <- cfa(model_rf,
              data = test_04,
              std.lv = TRUE,
              estimator = "WLSMV",
              ordered = TRUE)
summary(fit_rf, fit.measures = TRUE, standardized = TRUE)

alpha_rf <- psych::alpha(items_rf)
alpha_rf$total$raw_alpha

omega_rf <- semTools::reliability(fit_rf)
omega_rf

test_04$EI_rf <- rowMeans(test_04[, c("EI12", "EI22", "EI32",
                                      "EI29")])
r_long_rf <- cor(test_04$EI_long, test_04$EI_rf)

for (b5 in big5_vars) {
  r_long  <- cor(test_04$EI_long, test_04[, b5])
  r_short <- cor(test_04$EI_rf,   test_04[, b5])
  result  <- cocor.dep.groups.overlap(r.jk = r_long, r.jh = r_short,
                                      r.kh = r_long_rf, n = n)
  cat(b5, "- r(long):", round(r_long, 3),
      "| r(rf):", round(r_short, 3),
      "| Williams p:", round(result@williams1959$p.value, 4), "\n")
}


#### CIT SHORT SCALE ####
model_cit <- "F =~ EI12 + EI22 + EI20 +
                   EI31"

fit_cit <- cfa(model_cit,
               data = test_04,
               std.lv = TRUE,
               estimator = "WLSMV",
               ordered = TRUE)
summary(fit_cit, fit.measures = TRUE, standardized = TRUE)

alpha_cit <- psych::alpha(items_cit)
alpha_cit$total$raw_alpha

omega_cit <- semTools::reliability(fit_cit)
omega_cit

test_04$EI_cit <- rowMeans(test_04[, c("EI12", "EI22", "EI20",
                                       "EI31")])
r_long_cit <- cor(test_04$EI_long, test_04$EI_cit)

for (b5 in big5_vars) {
  r_long  <- cor(test_04$EI_long, test_04[, b5])
  r_short <- cor(test_04$EI_cit,  test_04[, b5])
  result  <- cocor.dep.groups.overlap(r.jk = r_long, r.jh = r_short,
                                      r.kh = r_long_cit, n = n)
  cat(b5, "- r(long):", round(r_long, 3),
      "| r(cit):", round(r_short, 3),
      "| Williams p:", round(result@williams1959$p.value, 4), "\n")
}


#### GA SHORT SCALE ####
model_ga <- "F =~ EI12 + EI23 + EI27 + EI31"

fit_ga <- cfa(model_ga,
              data = test_04,
              std.lv = TRUE,
              estimator = "WLSMV",
              ordered = TRUE)
summary(fit_ga, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

alpha_ga <- psych::alpha(items_ga)
alpha_ga$total$raw_alpha

omega_ga <- semTools::reliability(fit_ga)
omega_ga

test_04$EI_ga <- rowMeans(test_04[, c("EI12", "EI23", "EI27", "EI31")])
r_long_ga <- cor(test_04$EI_long, test_04$EI_ga)

for (b5 in big5_vars) {
  r_long  <- cor(test_04$EI_long, test_04[, b5])
  r_short <- cor(test_04$EI_ga,   test_04[, b5])
  result  <- cocor.dep.groups.overlap(r.jk = r_long, r.jh = r_short,
                                      r.kh = r_long_ga, n = n)
  cat(b5, "- r(long):", round(r_long, 3),
      "| r(ga):", round(r_short, 3),
      "| Williams p:", round(result@williams1959$p.value, 4), "\n")
}

# Compute Williams p values per scale and trait
get_williams_p <- function(ei_short, r_long_short) {
  sapply(big5_vars, function(b5) {
    r_long  <- cor(test_04$EI_long,   test_04[[b5]])
    r_short <- cor(test_04[[ei_short]], test_04[[b5]])
    result  <- cocor.dep.groups.overlap(r.jk = r_long, r.jh = r_short,
                                        r.kh = r_long_short, n = n)
    result@williams1959$p.value
  })
}

williams_irt <- get_williams_p("EI_irt", r_long_irt)
williams_rf  <- get_williams_p("EI_rf",  r_long_rf)
williams_cit <- get_williams_p("EI_cit", r_long_cit)
williams_ga  <- get_williams_p("EI_ga",  r_long_ga)


#### EASIER COMPARISON OF FIT ####
fit_indices <- data.frame(
  scale   = c("Long", "IRT", "RF", "CIT", "GA"),
  n_items = c(17, 4, 4, 4, 4),  # ← update counts once short scales are filled in
  chi2 = c(fitMeasures(fit_long, "chisq"),
           fitMeasures(fit_irt,  "chisq"),
           fitMeasures(fit_rf,   "chisq"),
           fitMeasures(fit_cit,  "chisq"),
           fitMeasures(fit_ga,   "chisq")),
  df = c(fitMeasures(fit_long, "df"),
         fitMeasures(fit_irt,  "df"),
         fitMeasures(fit_rf,   "df"),
         fitMeasures(fit_cit,  "df"),
         fitMeasures(fit_ga,   "df")),
  CFI = c(fitMeasures(fit_long, "cfi"),
          fitMeasures(fit_irt,  "cfi"),
          fitMeasures(fit_rf,   "cfi"),
          fitMeasures(fit_cit,  "cfi"),
          fitMeasures(fit_ga,   "cfi")),
  TLI = c(fitMeasures(fit_long, "tli"),
          fitMeasures(fit_irt,  "tli"),
          fitMeasures(fit_rf,   "tli"),
          fitMeasures(fit_cit,  "tli"),
          fitMeasures(fit_ga,   "tli")),
  RMSEA = c(fitMeasures(fit_long, "rmsea"),
            fitMeasures(fit_irt,  "rmsea"),
            fitMeasures(fit_rf,   "rmsea"),
            fitMeasures(fit_cit,  "rmsea"),
            fitMeasures(fit_ga,   "rmsea")),
  SRMR = c(fitMeasures(fit_long, "srmr"),
           fitMeasures(fit_irt,  "srmr"),
           fitMeasures(fit_rf,   "srmr"),
           fitMeasures(fit_cit,  "srmr"),
           fitMeasures(fit_ga,   "srmr")),
  alpha = c(alpha_long$total$raw_alpha,
            alpha_irt$total$raw_alpha,
            alpha_rf$total$raw_alpha,
            alpha_cit$total$raw_alpha,
            alpha_ga$total$raw_alpha),
  omega = c(omega_long["omega", 1],
            omega_irt["omega",  1],
            omega_rf["omega",   1],
            omega_cit["omega",  1],
            omega_ga["omega",   1]),
  mean_r_Big5 = c(mean_r_big5("EI_long"),
                  mean_r_big5("EI_irt"),
                  mean_r_big5("EI_rf"),
                  mean_r_big5("EI_cit"),
                  mean_r_big5("EI_ga")),
  williams_p_PC = c(NA, williams_irt["PC_score"], 
                    williams_rf["PC_score"],
                    williams_cit["PC_score"], 
                    williams_ga["PC_score"]),
  williams_p_PN = c(NA, williams_irt["PN_score"], 
                    williams_rf["PN_score"],
                    williams_cit["PN_score"], 
                    williams_ga["PN_score"])
)

result_table <- round(fit_indices[, -1], 3)
result_table <- cbind(scale = fit_indices$scale, result_table)

# Transpose: scales become columns, metrics become rows
result_transposed <- as.data.frame(t(result_table[, -1]))
colnames(result_transposed) <- fit_indices$scale
result_transposed <- cbind(Metric = rownames(result_transposed), result_transposed)
rownames(result_transposed) <- NULL

print(result_transposed)

write.xlsx(result_transposed, file = "fit_indices_04_25.xlsx", rowNames = FALSE)