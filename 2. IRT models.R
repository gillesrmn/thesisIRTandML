library(mirt)
library(tidyverse)
library(ggmirt)
library(lavaan)
#install.packages("caret")
library(caret)
#install.packages("skimr")
library(skimr) #visualizing distributions 
#install.packages("bp") #benchmark procedure, also for shortening scales
library(bp) 
library(psych)

set.seed(123)

#------------------ MODEL DESCRIPTIONS AND IMPLEMENTATION ----------------------
# The models were chosen on conceptual grounds. From the three most popular 
# approaches to Likert-type scales, GRM, GPCM, and RSM,the latter was discarded
# as it assumes every item shares the same category structure, which does not 
# accommodate the heterogeneous nature of the SEIS. It would be surprising if the 
# psychological distance between, say, "rarely" and "sometimes" were truly
# identical for an item about noticing others' emotions versus one about 
# regulating your own.
# 
# Then, some studies have shown the underperformance of the GRM over the GPCM. 
# Conceptually,a GRM model is more defensible, as it maps people's answering patterns 
# better. However, both models were fitted and compared using AIC, BIC and other 
# model fit statistics. 
# 
# Already, this kind effort into specifying the model, contrasts with the ML 
# approach, which is more easily automated and does not require psychometric 
# considerations whenever a new model is added. 
# 
#MODELS:
#   1) grm25: simple GRM on EI_25_train
#   
#   2) gpcm25: GPCM model on the same scale 
#   
#   3) grm17: GRM on EI_17_train
#   
#   4)gpcm25: GPCM model on EI_17_train scale
#   
# From the SEIS scale, we have produced two with different cutoffs for factor 
# loadings, 0.3 and 0.4, which respectively yield a 25-item scale and a 17-item 
# scale. 
#   
#Guiding tutorials: 
#Masur, P. (2022). Item Response Theory: Graded Response Models [Computer software].
#Github.https://github.com/ccs-amsterdam/r-course-material/blob/master/tutorials/R_test-theory_3_irt_graded.md
#
#
#Zein, R.A., & Akhtar, H. (2024). Getting Started with the graded response model: 
#An introduction and tutorial in R. International Journal of Psychology, 60(1), e13265. 
#https://onlinelibrary.wiley.com/doi/full/10.1002/ijop.13265
#-------------------------------------------------------------------------------
#DESCRIPTIVES 
describe(EI_25_train)
describe(EI_17_train)
#In both cases, the mean of most items lies above 3, and so does the median. 
#Though we may have to further interpret, this seems to be in line with
#the outcomes of self-reported measures. 

skim(EI_25_train)
skim(EI_17_train)

#Observations regarding the distribution: 
# - Skewness: all negatively skewed
# - Kurtosis: playkurtic for all except EI20. Not much of a problem. Signals that
#   response categories are being used. This will not be too much of a problem,
#   as the only distributional assumption is that theta is continuously distributed.

#response frequency per item 
EI_25_train[, 1:6] |>
  pivot_longer(everything(), names_to = "item", values_to = "response") |>
  count(item, response) |>
  group_by(item) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  print(n = 30)

EI_17_train[, 1:6] |>
  pivot_longer(everything(), names_to = "item", values_to = "response") |>
  count(item, response) |>
  group_by(item) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  print(n = 30)

#POLYCHORIC CORRELATIONS AND PARALLEL ANALYSIS
#Polychoric correlation matrix: better suited than Pearson correlations for the 
#distribution of our data. Observed categories are assumed to reflect an underlying 
#continuous latent variable Additionally, mirt uses polychoric correlations internally. 
#Here, we mostly look out for near-zero correlations, which mean that items
#are unrelated. 

#Parallel analysis: given the items retained, how many dimensions are needed to 
#adequately explain the pattern of correlations among them? This is done to 
#organize meaningful residual structure.If we notice an underlying structure, a 
#multidimensional (or bifactor) model is justified. 

poly_cor26 <- polychoric(EI_25_train)
cat("\nMean inter-item polychoric r:",
    round(mean(poly_cor26$rho[lower.tri(poly_cor26$rho)]), 3), "\n")
#0.255. Items share modest common variance, roughly about 0.625. This may point 
#to some heterogeneity, but we can only confirm this with the parallel analysis. 

poly_cor17 <- polychoric(EI_17_train)
cat("\nMean inter-item polychoric r:",
    round(mean(poly_cor17$rho[lower.tri(poly_cor17$rho)]), 3), "\n")
#0.301

#A dominant first factor should support the use of a unidimensional
fa.parallel(EI_25_train, fa = "pc", n.iter = 50, sim = FALSE,
            main = "Parallel Analysis — SEIS-25")
#Graph supports unidimensionality 

fa.parallel(EI_17_train, fa = "pc", n.iter = 50, sim = FALSE,
            main = "Parallel Analysis — SEIS-17")

#MODELS ------------------------------------------------------------------------
#
#INTERPRETATION OF PARAMETERS
#difficulty parameters are interpreted as the value of theta that corresponds
# to a .5 probability of responding at or above that location on an item

#g = c = guessing parameter 
# a = differentiation degree at different trait levels  
#     ranges from <0.5 to >2, where 0.8 - 1.2 is good. 
# b = threshold(s) = points on theta continuum where p of responding in category
# k is 50%
#     b1 = theta level for chance of choosing 2+ over 1 
#     b2 = .. of choosing 3+ over 1 or 2
#     b3 = ... 4 over lower. 


#1) grm25
grm25 <- mirt(EI_25_train, model = 1, # 1 = we assume only 1 theta
              itemtype = "graded", SE = TRUE, verbose = FALSE)
summary(grm25)

par_grm25 <- coef(grm25, IRTpars = TRUE, simplify = TRUE)
print(par_grm25)#this gives estimated item parameters (discr. and thresholds)

round(par_grm25$items, 2) 
#   - a: most items are good. The ,most discriminating are: EI12, EI16, EI17, EI20, 
#         EI22, EI23, EI27, EI30, EI31, EI32 
#   - Thresholds are negative, meaning that items are most informative for people 
#     below mean on EI. Makes sense given left skew. 
#   - Spread: wide spread means it is more informative. 
#   - We will observe common occurrences with the following models. 

M2(grm25, type = "C2", calcNULL = FALSE) #model fit
itemfit(grm25)
head(personfit(grm25))

#2)gpcm25
gpcm25 <- mirt(
  data     = EI_25_train,
  model    = 1,
  itemtype = "gpcm",
  SE       = TRUE,
  verbose  = FALSE
)
summary(gpcm25)
par_gpcm25 <- coef(gpcm25, IRTpars = TRUE, simplify = TRUE)
print(par_gpcm25)
round(par_gpcm25$items, 2) 

M2(gpcm25, type = "C2", calcNULL = FALSE)
itemfit(gpcm25)
head(personfit(gpcm25))

#3)grm17
grm17 <- mirt(EI_17_train, model = 1, itemtype = "graded", SE = TRUE, verbose = FALSE)
summary(grm17)

par_grm17 <- coef(grm17, IRTpars = TRUE, simplify = TRUE)
round(par_grm17$items, 2) 

M2(grm17, type = "C2", calcNULL = FALSE) 
itemfit(grm17) 
head(personfit(grm17))

#4)gpcm17
gpcm17 <- mirt(
  data     = EI_17_train,
  model    = 1,
  itemtype = "gpcm",
  SE       = TRUE,
  verbose  = FALSE
)
par_gpcm17 <- coef(gpcm17, IRTpars = TRUE, simplify = TRUE)
round(par_gpcm17$items, 2) #g = c = guessing parameter 

M2(gpcm17, type = "C2", calcNULL = FALSE) 
itemfit(gpcm17)
head(personfit(gpcm17))

#MODEL MISSPECIFICATION? -------------------------------------------------------
#manually checked for each model 
ld <- residuals(gpcm17, type = "LD")
up <- which(upper.tri(ld), arr.ind = T)
lar <- up[ld[up] > 0.2 | ld[up] < -0.2, ]

for (i in 1:nrow(lar)) {
  row <- lar[i, 1]
  col <- lar[i, 2]
  value <- ld[row, col]
  cat(sprintf("A large residual correlation is found between item %d and item %d: %f\n", row, col, value))
} # Now we detect the problematic pairs.
# A small number of item pairs showed residual correlations modestly exceeding 
# the .20 threshold, consistent with the subscale structure identified in the 
# parallel analysis. This represents a known limitation of the unidimensional model 
# and may warrant further exploration through a bifactor approach in the future. 

#-------------------------------------------------------------------------------
#MODEL COMPARISON 
#Comparing GRM & GPCM models for both datasets 

get_fit <- function(model, model_name) { 
  safe <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA)
    if (length(x) == 1) return(x)
    return(x[1])
  }
  
  npar <- safe(extract.mirt(model, "nest"))
  ll   <- safe(as.numeric(logLik(model)))
  
  m2_res <- tryCatch(
    M2(model, type = "C2", calcNULL = FALSE, QMC = TRUE),
    error   = function(e) NULL,
    warning = function(w) NULL
  )
  
  data.frame(
    Model  = model_name,
    LogLik = ll,
    Npar   = npar,
    M2     = safe(m2_res$M2),
    M2_df  = safe(m2_res$df),
    M2_p   = round(safe(m2_res$p),      3),
    RMSEA  = round(safe(m2_res$RMSEA),  3),
    SRMSR  = round(safe(m2_res$SRMSR),  3),
    CFI    = round(safe(m2_res$CFI),    3),
    TLI    = round(safe(m2_res$TLI),    3)
  )
}

fit_results <- rbind(
  get_fit(grm25,  "EI25_GRM"),
  get_fit(gpcm25, "EI25_GPCM"),
  get_fit(grm17,  "EI17_GRM"),
  get_fit(gpcm17, "EI17_GPCM")
)

print(fit_results, row.names = FALSE) 

ic_table <- do.call(rbind, Map(function(model, label) {
  ll   <- as.numeric(logLik(model))
  npar <- extract.mirt(model, "nest")
  n    <- nrow(model@Data$data)
  data.frame(
    Model = label,
    LogLik = round(ll,                       2),
    Npar   = npar,
    AIC    = round(-2 * ll + 2 * npar,       2),
    BIC    = round(-2 * ll + log(n) * npar,  2)
  )
},
list(grm25,      gpcm25,       grm17,      gpcm17),
list("EI25_GRM", "EI25_GPCM",  "EI17_GRM", "EI17_GPCM")
))

cat("\n=== AIC / BIC (lower = better) ===\n")
print(ic_table, row.names = FALSE)

library(ggplot2)

ggplot(fit_results, aes(x = Model, y = RMSEA)) +
  geom_col() +
  geom_hline(yintercept = 0.08, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "RMSEA Comparison") +
  theme_minimal()

#Based on the statistics obtained, we select the GRM models over the GPCM models. 

#PLOTS -------------------------------------------------------------------------
tracePlot(grm25, title = "Item Probability Functions") + 
  labs (color = "Response Categories")
itemInfoPlot(grm25, facet = TRUE, title = "Item Information Functions")
testInfoPlot(grm25, title="Test Information Function")

tracePlot(grm17, title = "Item Probability Functions") + 
  labs (color = "Response Categories")
itemInfoPlot(grm17, facet = TRUE, title = "Item Information Functions")
testInfoPlot(grm17, title="Test Information Function")

plot(grm25, type = "info")   # Test information (TIF)
plot(grm25, type = "SE")     # Test standard error - SE's are large, why? 
plot(grm25, type = "rxx")    # Test reliability

plot(grm17, type = "info")   # Test information (TIF)
plot(grm17, type = "SE")     # Test standard error - SE's are large, why? 
plot(grm17, type = "rxx")    # Test reliability

#-------------------------------------------------------------------------------
library(mirt)
library(tidyverse)
library(ggmirt)
library(lavaan)
#install.packages("caret")
library(caret)
#install.packages("skimr")
library(skimr) #visualizing distributions 
#install.packages("bp") #benchmark procedure, also for shortening scales
library(bp) 
library(psych)

set.seed(123)

#------------------ MODEL DESCRIPTIONS AND IMPLEMENTATION ----------------------
# The models were chosen on conceptual grounds. From the three most popular 
# approaches to Likert-type scales, GRM, GPCM, and RSM,the latter was discarded
# as it assumes every item shares the same category structure, which does not 
# accommodate the heterogeneous nature of the SEIS. It would be surprising if the 
# psychological distance between, say, "rarely" and "sometimes" were truly
# identical for an item about noticing others' emotions versus one about 
# regulating your own.
# 
# Then, some studies have shown the underperformance of the GRM over the GPCM. 
# Conceptually,a GRM model is more defensible, as it maps people's answering patterns 
# better. However, both models were fitted and compared using AIC, BIC and other 
# model fit statistics. 
# 
# Already, this kind effort into specifying the model, contrasts with the ML 
# approach, which is more easily automated and does not require psychometric 
# considerations whenever a new model is added. 
# 
#MODELS:
#   1) grm25: simple GRM on EI_25_train
#   
#   2) gpcm25: GPCM model on the same scale 
#   
#   3) grm17: GRM on EI_17_train
#   
#   4)gpcm25: GPCM model on EI_17_train scale
#   
# From the SEIS scale, we have produced two with different cutoffs for factor 
# loadings, 0.3 and 0.4, which respectively yield a 25-item scale and a 17-item 
# scale. 
#   
#Guiding tutorials: 
#Masur, P. (2022). Item Response Theory: Graded Response Models [Computer software].
#Github.https://github.com/ccs-amsterdam/r-course-material/blob/master/tutorials/R_test-theory_3_irt_graded.md
#
#
#Zein, R.A., & Akhtar, H. (2024). Getting Started with the graded response model: 
#An introduction and tutorial in R. International Journal of Psychology, 60(1), e13265. 
#https://onlinelibrary.wiley.com/doi/full/10.1002/ijop.13265
#-------------------------------------------------------------------------------
#DESCRIPTIVES 
describe(EI_25_train)
describe(EI_17_train)
#In both cases, the mean of most items lies above 3, and so does the median. 
#Though we may have to further interpret, this seems to be in line with
#the outcomes of self-reported measures. 

skim(EI_25_train)
skim(EI_17_train)

#Observations regarding the distribution: 
# - Skewness: all negatively skewed
# - Kurtosis: playkurtic for all except EI20. Not much of a problem. Signals that
#   response categories are being used. This will not be too much of a problem,
#   as the only distributional assumption is that theta is continuously distributed.

#response frequency per item 
EI_25_train[, 1:6] |>
  pivot_longer(everything(), names_to = "item", values_to = "response") |>
  count(item, response) |>
  group_by(item) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  print(n = 30)

EI_17_train[, 1:6] |>
  pivot_longer(everything(), names_to = "item", values_to = "response") |>
  count(item, response) |>
  group_by(item) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  print(n = 30)

#POLYCHORIC CORRELATIONS AND PARALLEL ANALYSIS
#Polychoric correlation matrix: better suited than Pearson correlations for the 
#distribution of our data. Observed categories are assumed to reflect an underlying 
#continuous latent variable Additionally, mirt uses polychoric correlations internally. 
#Here, we mostly look out for near-zero correlations, which mean that items
#are unrelated. 

#Parallel analysis: given the items retained, how many dimensions are needed to 
#adequately explain the pattern of correlations among them? This is done to 
#organize meaningful residual structure.If we notice an underlying structure, a 
#multidimensional (or bifactor) model is justified. 

poly_cor26 <- polychoric(EI_25_train)
cat("\nMean inter-item polychoric r:",
    round(mean(poly_cor26$rho[lower.tri(poly_cor26$rho)]), 3), "\n")
#0.251. Items share modest common variance, roughly about 0.625. This may point 
#to some heterogeneity, but we can only confirm this with the parallel analysis. 

poly_cor17 <- polychoric(EI_17_train)
cat("\nMean inter-item polychoric r:",
    round(mean(poly_cor17$rho[lower.tri(poly_cor17$rho)]), 3), "\n")
#0.3. 

#A dominant first factor should support the use of a unidimensional
fa.parallel(EI_25_train, fa = "pc", n.iter = 50, sim = FALSE,
            main = "Parallel Analysis — SEIS-26")
#Graph supports unidimensionality 

fa.parallel(EI_17_train, fa = "pc", n.iter = 50, sim = FALSE,
            main = "Parallel Analysis — SEIS-17")

#MODELS ------------------------------------------------------------------------
#
#INTERPRETATION OF PARAMETERS
#difficulty parameters are interpreted as the value of theta that corresponds
# to a .5 probability of responding at or above that location on an item

#g = c = guessing parameter 
# a = differentiation degree at different trait levels  
#     ranges from <0.5 to >2, where 0.8 - 1.2 is good. 
# b = threshold(s) = points on theta continuum where p of responding in category
# k is 50%
#     b1 = theta level for chance of choosing 2+ over 1 
#     b2 = .. of choosing 3+ over 1 or 2
#     b3 = ... 4 over lower. 


#1) grm25
grm25 <- mirt(EI_25_train, model = 1, # 1 = we assume only 1 theta
              itemtype = "graded", SE = TRUE, verbose = FALSE)
summary(grm25)

par_grm25 <- coef(grm25, IRTpars = TRUE, simplify = TRUE)
print(par_grm25)#this gives estimated item parameters (discr. and thresholds)

round(par_grm25$items, 2) 
#   - a: most items are good. The ,most discriminating are: EI12, EI16, EI17, EI20, 
#         EI22, EI23, EI27, EI30, EI31, EI32 
#   - Thresholds are negative, meaning that items are most informative for people 
#     below mean on EI. Makes sense given left skew. 
#   - Spread: wide spread means it is more informative. 
#   - We will observe common occurrences with the following models. 

M2(grm25, type = "C2", calcNULL = FALSE) #model fit
itemfit(grm25)
head(personfit(grm25))

#2)gpcm25
gpcm25 <- mirt(
  data     = EI_25_train,
  model    = 1,
  itemtype = "gpcm",
  SE       = TRUE,
  verbose  = FALSE
)
summary(gpcm25)
par_gpcm25 <- coef(gpcm25, IRTpars = TRUE, simplify = TRUE)
print(par_gpcm25)
round(par_gpcm25$items, 2) 

M2(gpcm25, type = "C2", calcNULL = FALSE)
itemfit(gpcm25)
head(personfit(gpcm25))

#3)grm17
grm17 <- mirt(EI_17_train, model = 1, itemtype = "graded", SE = TRUE, verbose = FALSE)
summary(grm17)

par_grm17 <- coef(grm17, IRTpars = TRUE, simplify = TRUE)
round(par_grm17$items, 2) 

M2(grm17, type = "C2", calcNULL = FALSE) 
itemfit(grm17) 
head(personfit(grm17))

#4)gpcm17
gpcm17 <- mirt(
  data     = EI_17_train,
  model    = 1,
  itemtype = "gpcm",
  SE       = TRUE,
  verbose  = FALSE
)
par_gpcm17 <- coef(gpcm17, IRTpars = TRUE, simplify = TRUE)
round(par_gpcm17$items, 2) #g = c = guessing parameter 

M2(gpcm17, type = "C2", calcNULL = FALSE) 
itemfit(gpcm17)
head(personfit(gpcm17))

#MODEL MISSPECIFICATION? -------------------------------------------------------
#manually checked for each model 
ld <- residuals(gpcm17, type = "LD")
up <- which(upper.tri(ld), arr.ind = T)
lar <- up[ld[up] > 0.2 | ld[up] < -0.2, ]

for (i in 1:nrow(lar)) {
  row <- lar[i, 1]
  col <- lar[i, 2]
  value <- ld[row, col]
  cat(sprintf("A large residual correlation is found between item %d and item %d: %f\n", row, col, value))
} # Now we detect the problematic pairs.
# A small number of item pairs showed residual correlations modestly exceeding 
# the .20 threshold, consistent with the subscale structure identified in the 
# parallel analysis. This represents a known limitation of the unidimensional model 
# and may warrant further exploration through a bifactor approach in the future. 

#-------------------------------------------------------------------------------
#MODEL COMPARISON 
#Comparing GRM & GPCM models for both datasets 

get_fit <- function(model, model_name) { 
  safe <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA)
    if (length(x) == 1) return(x)
    return(x[1])
  }
  
  npar <- safe(extract.mirt(model, "nest"))
  ll   <- safe(as.numeric(logLik(model)))
  
  m2_res <- tryCatch(
    M2(model, type = "C2", calcNULL = FALSE, QMC = TRUE),
    error   = function(e) NULL,
    warning = function(w) NULL
  )
  
  data.frame(
    Model  = model_name,
    LogLik = ll,
    Npar   = npar,
    M2     = safe(m2_res$M2),
    M2_df  = safe(m2_res$df),
    M2_p   = round(safe(m2_res$p),      3),
    RMSEA  = round(safe(m2_res$RMSEA),  3),
    SRMSR  = round(safe(m2_res$SRMSR),  3),
    CFI    = round(safe(m2_res$CFI),    3),
    TLI    = round(safe(m2_res$TLI),    3)
  )
}

fit_results <- rbind(
  get_fit(grm25,  "EI25_GRM"),
  get_fit(gpcm25, "EI25_GPCM"),
  get_fit(grm17,  "EI17_GRM"),
  get_fit(gpcm17, "EI17_GPCM")
)

print(fit_results, row.names = FALSE)

ic_table <- do.call(rbind, Map(function(model, label) {
  ll   <- as.numeric(logLik(model))
  npar <- extract.mirt(model, "nest")
  n    <- nrow(model@Data$data)
  data.frame(
    Model = label,
    LogLik = round(ll,                       2),
    Npar   = npar,
    AIC    = round(-2 * ll + 2 * npar,       2),
    BIC    = round(-2 * ll + log(n) * npar,  2)
  )
},
list(grm25,      gpcm25,       grm17,      gpcm17),
list("EI25_GRM", "EI25_GPCM",  "EI17_GRM", "EI17_GPCM")
))

cat("\n=== AIC / BIC (lower = better) ===\n")
print(ic_table, row.names = FALSE)

library(ggplot2)

ggplot(fit_results, aes(x = Model, y = RMSEA)) +
  geom_col() +
  geom_hline(yintercept = 0.08, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "RMSEA Comparison") +
  theme_minimal()

#Based on the statistics obtained, we select the GRM models over the GPCM models. 

#PLOTS -------------------------------------------------------------------------
tracePlot(grm25, title = "Item Probability Functions") + 
  labs (color = "Response Categories")
itemInfoPlot(grm25, facet = TRUE, title = "Item Information Functions")
testInfoPlot(grm25, title="Test Information Function")

tracePlot(grm17, title = "Item Probability Functions") + 
  labs (color = "Response Categories")
itemInfoPlot(grm17, facet = TRUE, title = "Item Information Functions")
testInfoPlot(grm17, title="Test Information Function")

plot(grm25, type = "info")   # Test information (TIF)
plot(grm25, type = "SE")     # Test standard error - SE's are large, why? 
plot(grm25, type = "rxx")    # Test reliability

plot(grm17, type = "info")   # Test information (TIF)
plot(grm17, type = "SE")     # Test standard error - SE's are large, why? 
plot(grm17, type = "rxx")    # Test reliability

#-------------------------------------------------------------------------------