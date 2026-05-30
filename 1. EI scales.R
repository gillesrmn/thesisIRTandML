#SCALE CREATION ----------------------------------------------------------------
getwd()

library(psych)
library(dplyr)
library(corrplot)
#install.packages("splitstackshape")
library(splitstackshape)
#install.packages("EGAnet")
library(EGAnet)
#install.packages("htmlTable")
library(htmlTable)
library(caret)

df <- datasetR 

#Create Correlation plot
ei_items <- df %>%
  select(starts_with("EI"))

cor_matrix_ei <- cor(ei_items, use = "complete.obs")
#No missings. All good.  

png(filename = "ei_items_correlation.png", width = 1000, height = 1000, res = 150)

corrplot(cor_matrix_ei, 
         method = "circle",
         type = "lower",
         tl.col = "black",
         tl.srt = 90,
         tl.cex = 0.7,
         diag = FALSE,
         title = "Correlation Matrix of 33 EI Items",
         mar = c(0, 0, 2, 0))
dev.off()

#Reverse code 
reverse_items_ei <- c("EI5", "EI28", "EI33")

ei_items <- ei_items %>%
  mutate(across(all_of(reverse_items_ei), ~ 5 - .x))

#Cronbach's alpha (rule of thumb: above 0.7 is good, above 0.8 is excellent)
alpha_EI <- psych::alpha(ei_items)
alpha_EI


#EFA ---------------------------------------------------------------------------
#Assumption Checks
KMO(ei_items) #should be above 0.6

cortest.bartlett(cor(ei_items, use = "complete.obs"), n = nrow(ei_items))

#Scree plot
png("ei_screeplot.png", width=800, height=600, res=150)
pa_results <- fa.parallel(ei_items, 
                          fm = "minres", 
                          fa = "fa", 
                          main = "Scree Plot for EI")
dev.off()

#EFA execution
efa_model <- fa(ei_items, 
                nfactors = 1,          
                fm = "minres",         
                rotate = "oblimin")  

print(efa_model$loadings, cutoff = 0.3, sort = TRUE) #cutoff of 0.3
print(efa_model$loadings, cutoff = 0.4, sort = TRUE) #cutoff of 0.4

png("ei_factor_diagram.png", width=800, height=1000, res=150)
fa.diagram(efa_model, 
           main = "EFA Diagram for Emotional Intelligence") 
dev.off()


#DATSETS CREATION --------------------------------------------------------------
load_mat <- as.matrix(efa_model$loadings)
max_load <- apply(abs(load_mat), 1, max, na.rm = TRUE)

items_03 <- names(max_load[max_load >= 0.3])
items_04 <- names(max_load[max_load >= 0.4])
df_items_03 <- df[, items_03, drop = FALSE]
df_items_04 <- df[, items_04, drop = FALSE]
head(df_items_03) #retains 25/33 items 
head(df_items_04) #retains 17/33 items 

#split: 
set.seed(123)

# grouping Age: binary split on median (more robust than mean given right skew)
df <- df %>%
  mutate(
    age_group = ifelse(Age <= median(Age, na.rm = TRUE), "Young", "Old"),
    strata    = paste(Gender, age_group, sep = "_")
  )

split_index <- createDataPartition(df$strata, p = 0.8, list = FALSE)
train_idx   <- split_index[, 1]
test_idx    <- setdiff(seq_len(nrow(df)), train_idx)

# 25-item datasets
EI_25_train <- df_items_03[train_idx, , drop = FALSE]
EI_25_val   <- df_items_03[test_idx, , drop = FALSE]

# 17-item datasets
EI_17_train <- df_items_04[train_idx, , drop = FALSE]
EI_17_val   <- df_items_04[test_idx, , drop = FALSE]

cat("Total N       :", nrow(df), "\n") #2122
cat("Training N    :", nrow(EI_25_train), "\n") #1699
cat("Validation N  :", nrow(EI_25_val), "\n") #423

write.csv(EI_25_train, "EI_25_train.csv", row.names = FALSE)
write.csv(EI_25_val,   "EI_25_val.csv",   row.names = FALSE)
write.csv(EI_17_train, "EI_17_train.csv", row.names = FALSE)
write.csv(EI_17_val,   "EI_17_val.csv",   row.names = FALSE)

#OVERVIEW ----------------------------------------------------------------------
print(df %>%
  mutate(Set = ifelse(row_number() %in% train_idx, "Train", "Test")) %>%
  group_by(Set, Gender, age_group) %>%
  summarise(
    N        = n(),
    Mean_Age = round(mean(Age, na.rm = TRUE), 2),
    SD_Age   = round(sd(Age,   na.rm = TRUE), 2),
    .groups  = "drop"
  ) %>%
  arrange(Set, Gender, age_group))

# Totals per Set × Gender (collapsing age groups)
print(df %>%
  mutate(Set = ifelse(row_number() %in% train_idx, "Train", "Test")) %>%
  group_by(Set, Gender) %>%
  summarise(
    N        = n(),
    Mean_Age = round(mean(Age, na.rm = TRUE), 2),
    SD_Age   = round(sd(Age,   na.rm = TRUE), 2),
    .groups  = "drop"
  ))

write.csv(ei_items, "EI_FF.csv",row.names = FALSE) #Just in case it is needed later

#DESCRIPTIVES ------------------------------------------------------------------
# Clean Grade variable
df$Grade <- trimws(df$Grade)
demo <- df %>% select(Gender, Age, Grade)

gender_freq <- demo %>%
  count(Gender) %>%
  mutate(
    Pct    = round(n / sum(n) * 100, 1),
    Valid  = round(n / sum(!is.na(demo$Gender)) * 100, 1)
  ) %>%
  rename(N = n) %>%
  bind_rows(
    summarise(., Gender = "Total", N = sum(N), Pct = sum(Pct), Valid = sum(Valid))
  )
print(gender_freq)

#age
age_desc <- demo %>%
  summarise(
    N      = sum(!is.na(Age)),
    Mean   = round(mean(Age, na.rm = TRUE), 2),
    SD     = round(sd(Age,   na.rm = TRUE), 2),
    Median = median(Age,     na.rm = TRUE),
    Min    = min(Age,        na.rm = TRUE),
    Max    = max(Age,        na.rm = TRUE),
    Q1     = quantile(Age, .25, na.rm = TRUE),
    Q3     = quantile(Age, .75, na.rm = TRUE)
  )
print(age_desc)

age_freq <- demo %>%
  count(Age) %>%
  mutate(Pct = round(n / sum(n) * 100, 1)) %>%
  rename(N = n)
print(age_freq)


age_by_gender <- demo %>%
  group_by(Gender) %>%
  summarise(
    N      = sum(!is.na(Age)),
    Mean   = round(mean(Age, na.rm = TRUE), 2),
    SD     = round(sd(Age,   na.rm = TRUE), 2),
    Median = median(Age,     na.rm = TRUE),
    Min    = min(Age,        na.rm = TRUE),
    Max    = max(Age,        na.rm = TRUE),
    .groups = "drop"
  )
print(age_by_gender)

