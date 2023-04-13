library(tidyverse)

revpar_and_occupancy <- read_csv2("revpar_and_occupancy.csv") %>%
  t() %>%
  `colnames<-`(.[1, ]) %>%
  .[-1,] %>%
  as_tibble() %>%
  gather(key = "kpi", value = "rel_change", 2:3) %>%
  mutate(ADR = as.numeric(ADR)/100,
         rel_change = as.numeric(rel_change)/100)

revpar_and_occupancy %>%
  ggplot(aes(x = ADR, y = rel_change, colour = kpi)) +
  geom_line() +
  geom_line(stat = "smooth", method = "lm", se = F, linetype = "dashed", alpha = .7) +
  labs(x = "Relativ difference in ADR",
       y = "Relative difference from the competition",
       title = "RevPAR and occupancy differences in European hotels, 2004-2013",
       colour = "") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"))

lm_occupancy <- revpar_and_occupancy %>%
  filter(kpi == "Occupancy") %>%
  lm(rel_change ~ ADR, .) %>%
  .$coefficients
  
lm_revpar <- revpar_and_occupancy %>%
  filter(kpi == "RevPAR") %>%
  lm(rel_change ~ ADR, .) %>%
  .$coefficients
