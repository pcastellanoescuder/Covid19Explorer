library(qgraph)
library(tidyverse)

data <- readxl::read_excel("../2020-04-COVID19-HUVH/FINAL_DATA/new_Export for UEB VHIR 14_04_2020_field equiv_f1.xlsx")

####

data_subset <- data %>%
  slice(-1) %>%
  janitor::clean_names() %>% # clean column names
  select_if(~ sum(!is.na(.)) > 0) %>% # drop columns that only have NAs
  mutate_at(vars(tidyr::starts_with("tn_")), ~ as.numeric(as.character(.))) %>%
  mutate_at(vars(tidyr::starts_with("n_")), ~ as.numeric(as.character(.))) %>%
  mutate_at(vars(tidyr::starts_with("f_")), ~ as.factor(as.character(.))) %>%
  mutate_at(vars(tidyr::starts_with("c_")), as.character) %>%
  select_if(~ !(sum(is.na(.))/nrow(data))*100 > 100) %>% 
  mutate_at(vars(tidyr::starts_with("tn_")), function(x)ifelse(x == 0, 1363465436, x)) %>%
  rename_at(vars(tidyr::starts_with("id_")), ~ "id") # modify var name


data_numeric <- data_subset %>%
  select_if(is.numeric)

cor_matrix2 <- round(cor(data_numeric, use = "pairwise.complete.obs"), 3)

rownames(cor_matrix2)
