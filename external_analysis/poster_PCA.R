
library(tidyverse)

data <- readxl::read_xlsx("~/FINAL_DATA/abstract_1/poster/2020 07 14 Day_one_1802_no outliers.xlsx")

data_subset <- data %>%
  janitor::clean_names() %>% # clean column names
  select_if(~ sum(!is.na(.)) > 0) %>% # drop columns that only have NAs
  mutate_at(vars(tidyr::starts_with("tn_")), ~ as.numeric(as.character(.))) %>%
  mutate_at(vars(tidyr::starts_with("n_")), ~ as.numeric(as.character(.))) %>%
  mutate_at(vars(tidyr::starts_with("f_")), ~ as.factor(as.character(.))) %>%
  mutate_at(vars(tidyr::starts_with("c_")), as.character) %>%
  mutate_at(vars(tidyr::starts_with("id_")), as.character) %>%
  rename_at(vars(contains("date")), ~ "date") %>% # modify var name
  rename_at(vars(tidyr::starts_with("id_")), ~ "id") %>% # modify var name
  select_if(~ !(sum(is.na(.))/nrow(data))*100 > 20) %>% # remove na
  mutate_at(vars(tidyr::starts_with("tn_")), function(x)ifelse(x == 0, 0.01, x)) %>% # replace zeros
  # mutate_at(vars(tidyr::starts_with("tn_")), log10) %>% # log10 transformation
  # rename_at(vars(tidyr::starts_with("tn_")), ~ paste0(., "_log10")) %>% # modify log10 transformed var names
  dplyr::select(id, date, everything())

data_subset <- data_subset %>%
  filter(f_outcome_final %in% c("HOME", "EXITUS")) %>%
  filter(!(id %in% c("19845708", "14805837")))

# data_subset <- data_subset[ -as.numeric(which(apply(data_subset, 2, var) == 0))] # remove 0 var columns

data_pca <- data_subset %>%
  select_at(vars(starts_with("tn_") | starts_with("n_"))) %>%
  select(-n_day_follow_up_calc, -n_total_samples_follow_up) %>%
  mutate_at(vars(starts_with("tn_")), ~ (log10(. + 1) - mean(log10(. + 1), na.rm = TRUE))/sd(log10(. + 1), na.rm = TRUE)) # log scaling

target <- data_subset %>%
  select(id, f_outcome_final)

pca_res <- mixOmics::pca(data_pca, center = TRUE, scale = TRUE)

PCi <- tibble(id = target$id, group = target$f_outcome_final, as_tibble(pca_res$x))

lam <- (pca_res$sdev[1:2] * sqrt(nrow(PCi)))^0.775
len <- t(t(pca_res$loadings$X[, 1:2]) * lam) * 0.8
PCAloadings <- data.frame(pca_res$loadings$X, to_x = len[, 1], to_y = len[, 2]) %>%
  arrange(desc(abs(PC1))) %>%
  slice(1:5) %>%
  rownames_to_column("ID") %>%
  mutate(ID = stringr::str_remove(ID, "tn_"),
         ID = stringr::str_remove(ID, "n_")) %>%
  column_to_rownames("ID")

# png("external_analysis/pca_poster.png", res = 300, units = "cm", width = 22, height = 18)
ggplot(PCi, aes(x = PC1, y = PC2)) + 
  geom_point(aes(color = group, shape = group), size = 3, alpha = 0.8) + 
  xlab(paste0("PC1 (", round(100 * (pca_res$explained_variance)[1], 2), "%)")) + 
  ylab(paste0("PC2 (", round(100 * (pca_res$explained_variance)[2],   2), "%)")) + 
  stat_ellipse(aes(fill = group), geom = "polygon", type = "norm", alpha = 0.25, show.legend = FALSE) +
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = to_x, yend = to_y), arrow = arrow(length = unit(1/2, "picas")), color = "grey19") + 
  ggrepel::geom_label_repel(data = PCAloadings, aes(x = to_x, y = to_y, label = rownames(PCAloadings)), size = 4) +
  theme_bw() +
  theme(legend.position = "top",
        legend.title = element_blank())
# dev.off()

## Random Forest

library(POMA)

# write.csv(target, "external_analysis/target.csv", row.names = FALSE)
target <- read_csv("external_analysis/target.csv")

poma_data <- PomaMSnSetClass(target = target, features = data_pca)
rf_res <- poma_data %>%
  PomaImpute(method = "median") %>%
  PomaRandForest(ntest = 5, ntree = 500, nvar = 15)

# png("external_analysis/random_forest.png", res = 300, units = "cm", width = 22, height = 18)
rf_res$gini_plot + theme(text = element_text(size = 14))
# dev.off()

