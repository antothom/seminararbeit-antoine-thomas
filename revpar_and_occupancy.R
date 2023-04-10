library(tidyverse)

revpar_and_occupancy <- read_csv2("revpar_and_occupancy.csv") %>%
  t() %>%
  `colnames<-`(.[1, ]) %>%
  .[-1,] %>%
  as_tibble() %>%
  gather(key = "kpi", value = "rel_change", 2:3) %>%
  mutate(ADR = as.numeric(ADR),
         rel_change = as.numeric(rel_change))

revpar_and_occupancy %>%
  ggplot(aes(x = ADR, y = rel_change, colour = kpi)) +
  geom_line() +
  labs(x = "Relativ Difference in ADR",
       y = "Relative Difference from the competition",
       title = "RevPAR and occupancy differences in EUropean hotels, 2004-2013",
       colour = "") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"))
  
  
  
   
