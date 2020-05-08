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

mynum <- "f_gender"
myfac <- "tn_hemoglobin"

##

data_fac <- data_subset %>%
  dplyr::select_at(vars(matches(myfac)))
data_num <- data_subset  %>%
  dplyr::select_at(vars(matches(mynum)))
data_subset <- cbind(data_fac, data_num)
colnames(data_subset) <- c("my_factor", "my_numeric")

if(!is.null(input$contents_proc_rows_selected)){
  data_subset <- data_subset[input$contents_proc_rows_selected ,]
} 

ggstatsplot::ggbetweenstats(
  data = data_subset,
  x = my_factor,
  y = my_numeric,
  title = "",
  messages = FALSE,
  type = "parametric",
  pairwise.display = "all",
  pairwise.comparisons = TRUE,
  p.adjust.method = "holm",
  results.subtitle = TRUE,
  xlab = myfac,
  ylab = mynum)
