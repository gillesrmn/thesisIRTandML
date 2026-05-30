#WE CAN FINALLY ABBREVIATE THE SCALES! 

#This approach was slightly lengthier. However, best-fitting models were supported 
#by item parameters that best represent the latent trait rather than heuristics. 

#BEST IRT MODEL-----------------------------------------------------------------

#EXTRACTING MODEL PARAMETERS 
# For unidimensional GRM25 and GRM17 
#Similar to previous script. Slightly adapted to extract item slopes. 
coef25 <- coef(grm25, IRTpars = TRUE, simplify = TRUE)
item_params25 <- coef25$items   # rows: items, columns: a (discrimination), b1, b2, ...
item_slopes25 <- item_params25[, "a"]
item_params25

coef17 <- coef(grm17, IRTpars = TRUE, simplify = TRUE)
item_params17 <- coef17$items   # rows: items, columns: a (discrimination), b1, b2, ...
item_slopes17 <- item_params17[, "a"]
item_params17

#RANK ITEMS BY SUITABLE CRITERION ----------------------------------------------
#Ranking by suitable criterion: slope, item information, factor loading, item 
#reliability/communality 

#These will meet our 4 shortening targets: 
## For EI_25_train: 13 items for 50% (25 * 0.5 = 12.5 -> 13)
#                 6 items for 25% (25 * 0.25 = 6.25 -> 6)
# For IE_17_train: 9 items for 50% (17 * 0.5 = 8.5 -> 9)
#                 4 items for 25% (17 * 0.25 = 4.25 -> 4)

half_item_rank25 <- order(item_slopes25, decreasing = TRUE)
half_selected_items_indices25 <- half_item_rank25[1:13]   # 13 for 50% of 25

half_item_rank17 <- order(item_slopes17, decreasing = TRUE)
half_selected_items_indices17 <- half_item_rank17[1:9]   # 12 for 50% of 25

quarter_item_rank25 <- order(item_slopes25, decreasing = TRUE)
quarter_selected_items_indices25 <- quarter_item_rank25[1:6]   # 6 for 25% of 25

quarter_item_rank17 <- order(item_slopes17, decreasing = TRUE)
quarter_selected_items_indices17 <- quarter_item_rank17[1:4]   # 4 for 25% of 25

#TOP ITEM SELECTION ------------------------------------------------------------
og_names25 <- colnames(EI_25_train)
og_names17 <- colnames(EI_17_train)

half_selected_names25 <- og_names25[half_selected_items_indices25]
half_selected_names25
quarter_selected_names25 <- og_names25[quarter_selected_items_indices25]
half_selected_names17 <- og_names17[half_selected_items_indices17]
quarter_selected_names17 <- og_names17[quarter_selected_items_indices17]

#for validation in IRT case.These are the Short Forms. 
SFH25 <- EI_25_val[,half_selected_names25]
SFQ25 <- EI_25_val[,quarter_selected_names25]
SFH17 <- EI_17_val[,half_selected_names17] 
SFQ17 <- EI_17_val[,quarter_selected_names17]
SFH25
SFQ25
SFH17
SFQ17

#-------------------------------------------------------------------------------
#This is needed for plots: 
SFH25_num <- SFH25 %>% select(where(is.numeric))
SFQ25_num <- SFQ25 %>% select(where(is.numeric))
EI_25_val_num <- EI_25_val %>% select(where(is.numeric))

SFH17_num <- SFH17 %>% select(where(is.numeric))
SFQ17_num <- SFQ17 %>% select(where(is.numeric))
EI_17_val_num <- EI_17_val %>% select(where(is.numeric))

#_------------------------------------------------------------------------------

#PLOTS
get_test_info <- function(data, theta_grid = seq(-4, 4, length.out = 100)) {
  fit <- mirt(data, model = 1, itemtype = "graded", verbose = FALSE)
  info <- testinfo(fit, Theta = theta_grid)
  return(data.frame(theta = theta_grid, info = info))
}

theta <- seq(-4, 4, length.out = 100)

info_full25 <- get_test_info(EI_25_val_num, theta)
info_half25 <- get_test_info(SFH25_num, theta)
info_quart25 <- get_test_info(SFQ25_num, theta)

info_25 <- bind_rows(
  info_full25 %>% mutate(version = "Full (25 items)"),
  info_half25 %>% mutate(version = "Half (13 items)"),
  info_quart25 %>% mutate(version = "Quarter (6 items)")
)

info_full17 <- get_test_info(EI_17_val_num, theta)
info_half17 <- get_test_info(SFH17_num, theta)
info_quart17 <- get_test_info(SFQ17_num, theta)

info_17 <- bind_rows(
  info_full17 %>% mutate(version = "Full (17 items)"),
  info_half17 %>% mutate(version = "Half (9 items)"),
  info_quart17 %>% mutate(version = "Quarter (4 items)")
)

# Plot for 25‑item pool
p25 <- ggplot(info_25, aes(x = theta, y = info, color = version, linetype = version)) +
  geom_line(size = 1.2) +
  labs(title = "Test Information Curves – 25‑item pool",
       x = expression(theta ~ "(Latent Trait)"),
       y = "Test Information",
       color = "Version", linetype = "Version") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Full (25 items)" = "black",
                                "Half (13 items)" = "blue",
                                "Quarter (6 items)" = "red"))

# Plot for 17‑item pool
p17 <- ggplot(info_17, aes(x = theta, y = info, color = version, linetype = version)) +
  geom_line(size = 1.2) +
  labs(title = "Test Information Curves – 17‑item pool",
       x = expression(theta ~ "(Latent Trait)"),
       y = "Test Information",
       color = "Version", linetype = "Version") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Full (17 items)" = "black",
                                "Half (9 items)" = "blue",
                                "Quarter (4 items)" = "green"))

# Display plots
print(p25)
print(p17)

ggsave("test_info_25item_pool.png", p25, width = 8, height = 5, dpi = 300)
ggsave("test_info_17item_pool.png", p17, width = 8, height = 5, dpi = 300)
