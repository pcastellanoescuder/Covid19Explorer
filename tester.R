
data <- readxl::read_excel("../2020-04-COVID19-HUVH/dades/300320IMM COVID IL6U_TCZtodosbis.xlsx")

data_subset <- data %>%
  janitor::clean_names() %>%
  mutate_at(vars(contains("data")), dmy) %>% 
  mutate_at(vars(contains("edad")), as.character) %>%        
  mutate_at(vars(contains("numer")), as.character) %>%  
  mutate_at(vars(contains("codi")), as.character) %>% 
  mutate_at(vars(contains("seguim")), as.character) %>% 
  mutate_at(vars(contains("gend")), as.factor) %>% 
  mutate_at(vars(contains("serve")), as.factor) %>% 
  mutate(tcz = as.factor(ifelse(is.na(tcz), 0, tcz))) %>% 
  mutate_if(is.numeric, log) %>%
  rename_if(is.numeric, ~ paste0(., "_numeric")) %>%
  pivot_longer(cols = ends_with("numeric")) %>% 
  rename_if(is.Date, ~ "data_calendar") %>%
  arrange(desc(data_calendar)) %>%
  rename(variable = name)
  

####




  group_by(codi_extern, data_calendar) %>%
  slice(n(), 1) %>% 
  ungroup()



ggplot(data_subset) +
  geom_line(aes(data_calendar, value, color = variable, shape = variable), size = 1, alpha = 0.6) +
  geom_point(aes(data_calendar, value, color = variable, shape = variable), size = 2.5) +
  ylab("log(variables)") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") +
  labs(color = "", shape = "")





library(plyr)

code <- as.data.frame(data$`Codi Extern`)
data_subset1 <- as.data.frame(data$Leucòcits_Sang)
data_subset2 <- as.data.frame(data$`Ferritina Fusion`)
data_subset3 <- as.data.frame(data$Gender)

data_subset <- bind_cols(code, data_subset1, data_subset2, data_subset3)
colnames(data_subset) <- c("code", "Variable1", "Variable2", "Factor")

cors <- plyr::ddply(data_subset, c("Factor"), summarise, cor = round(cor(Variable1, Variable2,
                                                                         method = "pearson", use = "complete.obs"), 2))

ggplot(data_subset) + 
  geom_point(aes(x = Variable1, y = Variable2, color = Factor, label = code), alpha = 0.8, size = 1.5) +
  theme_bw() + 
  facet_wrap(~ Factor) +
  geom_text(data = cors, aes(label = paste0("R = ", cor)),
            x = quantile(data_subset$Variable1, na.rm = TRUE, probs = 0.75), 
            y = quantile(data_subset$Variable2, na.rm = TRUE, probs = 0.25)) 






