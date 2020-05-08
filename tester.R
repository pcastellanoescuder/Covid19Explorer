library(tidyverse)

data <- readxl::read_excel("../2020-04-COVID19-HUVH/FINAL_DATA/new_COVID explorer1.xlsx")

####

data_subset <- data %>%
  janitor::clean_names() %>% # clean column names
  select_if(~ sum(!is.na(.)) > 0) %>% # drop columns that only have NAs
  mutate_at(vars(tidyr::starts_with("tn_")), ~ as.numeric(as.character(.))) %>%
  mutate_at(vars(tidyr::starts_with("n_")), ~ as.numeric(as.character(.))) %>%
  mutate_at(vars(tidyr::starts_with("f_")), ~ as.factor(as.character(.))) %>%
  mutate_at(vars(tidyr::starts_with("c_")), as.character) %>%
  select_if(~ !(sum(is.na(.))/nrow(data))*100 > 70) %>% 
  mutate_at(vars(tidyr::starts_with("tn_")), function(x)ifelse(x == 0, 0.01, x)) %>%
  rename_at(vars(tidyr::starts_with("id_")), ~ "id")

##

