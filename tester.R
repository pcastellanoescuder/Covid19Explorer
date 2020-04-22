library(qgraph)
library(tidyverse)

data <- readxl::read_excel("../2020-04-COVID19-HUVH/FINAL_DATA/new_Export for UEB VHIR 14_04_2020_field equiv_f1.xlsx")

no_num <- c("date_sample", "record_num_hvh", "sample_num", 
            "age_years", "gender", "department", 
            "area_of_care", "outcome", "follow_up_days", 
            "follow_up_samples_total_n", "tzc_onset", "tzc_final")

data_subset <- data %>%
  slice(-1) %>%
  janitor::clean_names() %>% # clean column names
  select_if(~ sum(!is.na(.)) > 0) %>% # drop columns that only have NAs
  mutate_at(vars(-matches(no_num)), ~ as.numeric(as.character(.))) %>% # char to num
  select_if(~ !(sum(is.na(.))/nrow(data))*100 > 70) %>% # remove na
  mutate_if(is.numeric, function(x)ifelse(x == 0, 0.01, x)) %>% # replace zeros
  mutate(date_sample = janitor::excel_numeric_to_date(as.numeric(as.character(date_sample)))) %>% # format date
  mutate_at(vars(contains("record_nu")), as.character) %>% # modify var type
  mutate_at(vars(contains("sample_nu")), as.character) %>% # modify var type
  mutate_at(vars(contains("age")), as.character) %>% # modify var type
  mutate_at(vars(contains("follow_up_days")), as.character) %>% # modify var type
  mutate_at(vars(contains("follow_up_samples")), as.character) %>% # modify var type
  mutate_at(vars(contains("tzc")), as.character) %>% # modify var type
  mutate_at(vars(contains("gender")), as.factor) %>% # modify var type
  mutate_at(vars(contains("department")), as.factor) %>% # modify var type
  mutate_at(vars(contains("area")), as.factor) %>% # modify var type
  mutate_at(vars(contains("outcome")), as.factor) %>% # modify var type
  rename_at(vars(contains("age")), ~ "age") %>% # modify var name
  rename_if(lubridate::is.Date, ~ "date") %>% # modify var name
  mutate_if(is.numeric, log) %>% # modify all numeric var names
  rename_if(is.numeric, ~ paste0(., "_proc")) %>% # modify all numeric var names
  mutate_at(vars(contains("age")), as.numeric)

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
  mutate_at(vars(tidyr::starts_with("tn_")), function(x)ifelse(x == 0, 1363465436, x))

       