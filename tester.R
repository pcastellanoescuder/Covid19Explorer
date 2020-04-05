data <- readxl::read_excel("../2020-04-COVID19-HUVH/dades/300320IMM COVID IL6U_TCZtodosbis.xlsx")
colnames(data) <- janitor::make_clean_names(colnames(data))
data <- as.data.frame(data)

####

subjects <- c("12619381")
features <- c("pcr_fusion", "il6_fusion", "limfocits_c_nom_leucocits")
  
data_subset <- data %>%
  mutate(data_calendar = dmy(data_calendar)) %>%
  mutate_at(c("edad_number", "numero", "codi_extern"), as.character) %>%
  mutate_if(is.numeric, log) %>% 
  reshape2::melt(id.vars = c("codi_extern", "numero", "data_calendar", "edad_number", "gender", "servei")) %>%
  filter(variable %in% features) %>%
  arrange(desc(data_calendar)) %>%
  group_by(codi_extern, data_calendar) %>%
  slice(n(), 1) %>% 
  ungroup()

wrap_props <- "servei"

ggplot(data_subset) +
  geom_line(aes(data_calendar, value, color = variable, shape = variable), size = 1, alpha = 0.6) +
  geom_point(aes(data_calendar, value, color = variable, shape = variable), size = 2.5) +
  ylab("log(variables)") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") +
  facet_wrap(vars(wrap_props)) +
  labs(color = "", shape = "")


