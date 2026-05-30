#ABBREVIATION WITH GENETIC ALGORITHMS-------------------------------------------
#The majority of this code was derived from Scrucca's (2013)
#manual for the GA package

#Scrucca, L. (2013). GA: A Package for Genetic Algorithms in R. 
#Journal of Statistical Software, 53(4), 1-37. 
#https://cms.dm.uba.ar/academico/materias/2docuat2017/sem_herr_avan/GA-%20A%20Package%20for%20Genetic%20Algorithms%20in%20R.pdf
#-------------------------------------------------------------------------------
#PACKAGES AND PREPARATION
library(GA)
library(psych)
#Though GAabbreviate is especially designed for this,it is not longer available
#in the CRAN repositories.  GA works just fine and is more flexible.
# Mostly, they differ when it comes to terminology. 

library(ggplot2)
library(corrplot)
library(gridExtra)
library(reshape2)
library(dplyr)

set.seed(123)

#PREPARATION OF MATRICES 
#Luckily, no NA's. No sensitivity to missingness. 

EI_items_matrix25 <- as.matrix(EI_25_train)
EI_items_matrix17 <- as.matrix(EI_17_train)

#scales matrix by summing all items as total scale score. 
EI_total_score25 <- rowSums(EI_items_matrix25) # Or rowMeans()
EI_total_score17 <- rowSums(EI_items_matrix17)

EI_scales_matrix25 <- as.matrix(EI_total_score25, ncol = 1)
EI_scales_matrix17 <- as.matrix(EI_total_score17, ncol = 1)

# TARGET ITEM COUNTS:
# For EI_25_train: 13 items for 50% (25 * 0.5 = 12.5 -> 13)
#                 6 items for 25% (25 * 0.25 = 6.25 -> 6)
# For IE_17_train: 9 items for 50% (17 * 0.5 = 8.5 -> 9)
#                 4 items for 25% (17 * 0.25 = 4.25 -> 4)

# All targets are translated into a single list that can me modified and called, 
# so, when the shortening process is extended to newer goals, this is easily 
# adapted. 

targets <- list(
  EI25_50 = list(matrix = EI_items_matrix25, total = EI_total_score25,
                 n = 13, label = "EI-25 → 50% (13 items)"),
  EI25_25 = list(matrix = EI_items_matrix25, total = EI_total_score25,
                 n = 6,  label = "EI-25 → 25% (6 items)"),
  EI17_50 = list(matrix = EI_items_matrix17, total = EI_total_score17,
                 n = 9,  label = "EI-17 → 50% (9 items)"),
  EI17_25 = list(matrix = EI_items_matrix17, total = EI_total_score17,
                 n = 4,  label = "EI-17 → 25% (4 items)"))

#FITNESS FUNCTION (FOR TARGET LIST)--------------------------------------------
#-Inf = wrong number of items is selected. 

#Major tradeoff: correlation vs. reliability. The first definition of the fitness
#function almost exclusively prioritized correlation -> prioritization of items
#that inflated the sum score regardless of their quality. This led me to opt for a 
#composite fitness function (a benefit of GA, to balance multiple criteria)

fitness_fn <- function(binary_string, items_matrix, total_score, target_n, 
                       w_cor = 0.40, w_alpha = 0.45, w_meanr = 0.15) {
  selected <- which(binary_string == 1)
  #EXACT item count: 
  if (length(selected) != target_n) return(-Inf)
  if (length(selected) < 2)        return(-Inf)
  sf_matrix <- items_matrix[,selected,drop = FALSE]
  
  short_total <- rowSums(sf_matrix) #max correlation with validation set 
  r_full      <- cor(short_total, total_score, use = "complete.obs")
  
  alpha_val <- tryCatch({ #for reliability 
    a <- psych::alpha(as.data.frame(sf_matrix), warnings = FALSE)
    a$total$raw_alpha
  }, error = function(e) 0)
  alpha_val <- max(0, min(1, alpha_val)) #bounded to prevent negative values,
  #but this can be changed. 
  
  #deals with mean inter-item correlation: 
  cor_matrix <- cor(sf_matrix, use = "pairwise.complete.obs")
  mean_r     <- mean(cor_matrix[lower.tri(cor_matrix)])
  #with penalties: 
  optimal_r  <- 0.30
  penalty_r  <- 1 - abs(mean_r - optimal_r) / optimal_r
  penalty_r  <- max(0, penalty_r)
  
  #then, we can get a composite score
  w_cor * r_full + w_alpha * alpha_val + w_meanr * penalty_r
}

#According to Scrucca, 2013, p.11. Dealing with function on one dimension. 

#SHORTENING AND PARAMETERS --------------------------------------------------------------------

#Most tuning deals with maxItems and itemCost penalization, but this is deter-
#mined heuristically. This is important, especially for the more extreme SFs,
#as it will prevent premature convergence and flag plateaus. 

?ga
run_ga <- function(spec, w_cor = 0.40, w_alpha = 0.45, w_meanr = 0.15) { 
  ga(
    type      = "binary",
    fitness   = fitness_fn,
    nBits     = ncol(spec$matrix),
    items_matrix = spec$matrix,
    total_score  = spec$total,
    target_n     = spec$n,
    w_cor = w_cor,
    w_alpha = w_alpha, 
    w_meanr = w_meanr, 
    popSize   = 100,#Yarkoni recommends 200, but he works with a larger battery
                    #can change if needed. 
    maxiter   = 500,#Not too many iterations 
    pcrossover = 0.65, #defined heuristically and based on literature.  
    pmutation = 0.05,#mutation rate probability. 
    elitism   = 5,#top solutions carried forward (in line with 20% suggested)
    keepBest  = TRUE,
    run       = 50, # early stopping: 50 gens with no improvement
    monitor   = TRUE #Important to track generations, but not necessary.
  )}
#This will return an S4 object of class "ga", which contains the number of 
#iterations, population matrix, fitness, best fitness value at each iteration,
#average fitness value at each iteration (mean), and the matrix of solution 
#strings. 

#Some comments on the different specifications: 
# - When we specify 'monitor = TRUE', we can see how the 'best' plateaus quite a lot 
# - pmutation: should be inversely related to the number of bits. A common heuristic, 
#   is 1/nBits (Verschoor, 2004). Here, a compromise.of 0.5 is achieved. However, 
#   the algorithm also ran on 0.8 to avoid getting stuck at local optima.This 
#   minimanlly affected scale properties, and, in both cases, the GA finalized
#   just fine. 
# - pcrossover: recommended to lower pcrossover for SA. To avoid two parent solutions 
#   being penalized for selecting wrong number of items. 

ga_results <- lapply(targets, run_ga) #May produce some warnings regarding similar
# package calls. 

#SELECTED ITEMS
extract_items <- function(ga_obj, spec, val_data) {
  best_bits    <- ga_obj@solution[1, ]
  selected_idx <- which(best_bits == 1)
  item_names   <- colnames(spec$matrix)[selected_idx]
  #mimics GAabbreviate functions. 
  # Fitness on training data
  train_r <- fitness_fn(best_bits, spec$matrix, spec$total, spec$n)
  # Correlation with full scale on validation data. 
  val_matrix    <- as.matrix(val_data[, colnames(spec$matrix), drop = FALSE])
  val_full      <- rowSums(val_matrix)
  val_short     <- rowSums(val_matrix[, selected_idx, drop = FALSE])
  val_r         <- cor(val_short, val_full, use = "complete.obs")
  
  list(
    label        = spec$label,
    n_target     = spec$n,
    selected_idx = selected_idx,
    item_names   = item_names,
    train_r      = round(train_r, 4),
    val_r        = round(val_r,   4)
  )
}

#validation
val_lookup <- list(
  EI25_50 = EI_25_val,
  EI25_25 = EI_25_val,
  EI17_50 = EI_17_val,
  EI17_25 = EI_17_val
)

solutions <- mapply(extract_items,
                    ga_results, targets, val_lookup,
                    SIMPLIFY = FALSE) #same warning as before, but nothing serious 
#SUMMARY TABLE 
summary_table <- do.call(rbind, lapply(solutions, function(s) {
  data.frame(
    Goal        = s$label,
    N_items     = s$n_target,
    Items       = paste(s$item_names, collapse = ", "),
    Train_r     = s$train_r,
    Val_r       = s$val_r
  )
}))

print(summary_table, row.names = FALSE)

#RELIABILITY, VALIDITY, ETCETERA...---------------------------------------------
#Target 1
items_EI25_50 <- solutions$EI25_50$item_names
sf_EI25_50 <- EI_25_val[, items_EI25_50]
rel_EI25_50 <- omega(sf_EI25_50, plot = FALSE)
paste(items_EI25_50, collapse = ", ")
round(rel_EI25_50$omega.tot, 3) #Omega = 0.844. 
round(rel_EI25_50$omega_h, 3) #Hierarchical O = 0. 726
round(rel_EI25_50$alpha, 3) #Cronbach's alpha = 0.817
round(solutions$EI25_50$val_r, 3) # correlation with FF = 0.944

#Target 2
items_EI25_25 <- solutions$EI25_25$item_names
sf_EI25_25 <- EI_25_val[, items_EI25_25]
rel_EI25_25 <- omega(sf_EI25_25, plot = FALSE)
paste(items_EI25_25, collapse = ", ")
round(rel_EI25_25$omega.tot, 3) #Omega = 0.819
round(rel_EI25_25$omega_h, 3) #Hierarchical O = 0.488
round(rel_EI25_25$alpha, 3) #Cronbach's alpha = 0.708
round(solutions$EI25_25$val_r, 3) # correlation with FF = 0.854

#Target 3
items_EI17_50 <- solutions$EI17_50$item_names
sf_EI17_50 <- EI_17_val[, items_EI17_50]
rel_EI17_50 <- omega(sf_EI17_50, plot = FALSE)
paste(items_EI17_50, collapse = ", ")
round(rel_EI17_50$omega.tot, 3) #Omega = 0.828
round(rel_EI17_50$omega_h, 3) #Hierarchical O = 0.681
round(rel_EI17_50$alpha, 3) #Cronbach's alpha = 0.803
round(solutions$EI17_50$val_r, 3) # correlation with FF = 0.908

#Target 4
items_EI17_25 <- solutions$EI17_25$item_names
sf_EI17_25 <- EI_17_val[, items_EI17_25]
rel_EI17_25 <- omega(sf_EI17_25, plot = FALSE)
paste(items_EI17_25, collapse = ", ")
round(rel_EI17_25$omega.tot, 3) #Omega = 0.634
round(rel_EI17_25$omega_h, 3) #Hierarchical O = 0.55
round(rel_EI17_25$alpha, 3) #Cronbach's alpha = 0.606
round(solutions$EI17_25$val_r, 3) # correlation with FF = 0.794

#SF FACTOR STRUCTURE -----------------------------------------------------------
library(lavaan)

run_cfa <- function(item_names, val_data, label) {
  model <- paste0("EI =~ ", paste(item_names, collapse = " + "))
  fit   <- cfa(model, data = val_data, estimator = "WLSMV")
  fits  <- fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr"))
  cat("\nCFA fit —", label, "\n")
  print(round(fits, 3))
}

run_cfa(items_EI25_50, EI_25_val, "EI25 50%") #ignore warnings 
run_cfa(items_EI25_25, EI_25_val, "EI25 25%")
run_cfa(items_EI17_50, EI_17_val, "EI17 50%")
run_cfa(items_EI17_25, EI_17_val, "EI17 25%")

#PLOTS--------------------------------------------------------------------------
#First, it is important to ensure the GA object is in the correct format... 
str(ga_results, 1)  # shows structure of the list
length(ga_results)  # how many elements?
class(ga_results[[1]])  # should be "ga" if you stored GA objects

ga_obj <- ga_results[[1]]

#best run 
# Find run with highest final fitness
best_run_index <- which.max(sapply(ga_results, function(x) max(x@fitness)))
ga_obj <- ga_results[[best_run_index]]

# Evolution plot
# Extract best and mean fitness per generation
plot(ga_obj)

### For all : 
df_list <- list()
for (i in seq_along(ga_results)) {
  df <- data.frame(
    Generation = 1:nrow(ga_results[[i]]@summary),
    Best = ga_results[[i]]@summary[, "max"],
    Run = paste0("Target_", i)
  )
  df_list[[i]] <- df
}
df_all <- bind_rows(df_list)

ggplot(df_all, aes(x = Generation, y = Best)) +
  geom_line(color = "steelblue", linewidth = 1) +
  facet_wrap(~ Run, scales = "free_x") +  # use "free_y" if fitness ranges differ a lot
  theme_minimal() +
  labs(title = "GA Evolution per Shortening Target", y = "Best Fitness")
##
##
df_list <- list()
for (i in seq_along(ga_results)) {
  df <- data.frame(
    Generation = 1:nrow(ga_results[[i]]@summary),
    Best = ga_results[[i]]@summary[, "max"],
    Run = paste0("Target_", i)
  )
  df_list[[i]] <- df
}
df_all <- bind_rows(df_list)

ggplot(df_all, aes(x = Generation, y = Best)) +
  geom_line(color = "#2c7bb6", linewidth = 1.2) +          # nicer blue
  facet_wrap(~ Run, scales = "free_x", 
             labeller = labeller(Run = label_both)) +       # shows "Run: Target_1"
  labs(title = "Genetic Algorithm Evolution",
       subtitle = "Best fitness per shortening target",
       x = "Generation", y = "Best Fitness") +
  theme_bw(base_size = 12) +                               # clean background
  theme(
    strip.background = element_rect(fill = "#e0e0e0"),     # light grey facet header
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    panel.grid.minor = element_blank(),                    # remove minor grid
    panel.grid.major = element_line(linewidth = 0.3, colour = "grey90")
  )


#Validation with FF 
plot_scatter <- function(sol, val_data) {
  items      <- sol$item_names
  val_matrix <- as.matrix(val_data[, colnames(val_data) %in%
                                     colnames(as.matrix(val_data)), drop = FALSE])
  full_score  <- rowSums(val_data, na.rm = TRUE)
  short_score <- rowSums(val_data[, items, drop = FALSE], na.rm = TRUE)
  
  df <- data.frame(Full = full_score, Short = short_score)
  r  <- sol$val_r
  
  ggplot(df, aes(x = Full, y = Short)) +
    geom_point(alpha = 0.4, colour = "#7F77DD") +
    geom_smooth(method = "lm", se = TRUE, colour = "#D85A30") +
    annotate("text", x = min(full_score), y = max(short_score),
             label = paste0("r = ", r), hjust = 0, vjust = 1, size = 4.5) +
    labs(title = paste("Short Form vs Full Scale —", sol$label),
         x = "Full Scale Score", y = "Short Form Score") +
    theme_minimal(base_size = 12)
}

scatter_plots <- list(
  plot_scatter(solutions$EI25_50, EI_25_val_num),
  plot_scatter(solutions$EI25_25, EI_25_val_num),
  plot_scatter(solutions$EI17_50, EI_17_val_num),
  plot_scatter(solutions$EI17_25, EI_17_val_num)
)

grid.arrange(grobs = scatter_plots, ncol = 2)