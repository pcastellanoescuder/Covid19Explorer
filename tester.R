library(FactoMineR)
library(factoextra)
library(tidyverse)

data <- readxl::read_excel("../2020-04-COVID19-HUVH/FINAL_DATA/Export for UEB VHIR 14_04_2020_field equiv_f1.xlsx")

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

data_factors <- data_subset %>%
  select_if(is.factor)

data_variables <- data_subset %>%
  select_at(vars(ends_with("_proc")))

x <- colnames(data_factors)
y <- colnames(data_variables)

####

# my_factor <- x[1]
# my_factor2 <- x[2]

my_factor <- "None"
my_factor2 <- "None"

my_vars <- y[1:3]
my_vars2 <- y[4]

# my_vars2 <- "None"

total_fac <- c(my_factor, my_factor2)
total_vars <- c(my_vars, my_vars2)

##

data_subset1 <- data_subset %>%
  select_at(vars(matches(total_vars)))

data_factor <- data_subset %>%
  select_at(vars(matches(total_fac)))

data_names <- data_subset %>%
  select_at(vars(contains("record")))

data_subset <- bind_cols(data_factor, data_subset1) # , data_names)


data_subset <- data_subset[1:50,]

data_names <- data_names[1:50,]

#### OK

idx_fac <- c(which(colnames(data_subset) %in% my_factor))

idx_fac2 <- c(which(colnames(data_subset) %in% my_factor2))

idx_fac_total <- c(idx_fac, idx_fac2)
if(length(idx_fac_total) == 0){idx_fac_total <- NULL}

idx_var <- c(which(colnames(data_subset) %in% my_vars))
idx_var2 <- c(which(colnames(data_subset) %in% my_vars2))

res_pca <- PCA(data_subset,
               ind.sup = NULL, 
               quanti.sup = idx_var2,
               quali.sup = idx_fac_total, 
               graph = F)

data_names <- bind_cols(data_names, as.data.frame(res_pca$ind$coord))

fviz_pca_biplot(res_pca, repel = FALSE, title = "", label = "var", axes = c(1,2), palette = "rickandmorty", col.var = "red") + theme_bw()





