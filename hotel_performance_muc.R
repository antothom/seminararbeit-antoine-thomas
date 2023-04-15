library(tidyverse)


hotel_performance_muc <- read_csv2("munich_arr_revpar_occ.csv") %>%
  mutate(`OCC (%)` = `OCC (%)`/100,
         Year = as.factor(Year))

hotel_performance_muc$`ARR (€)` <- as.numeric(gsub(" €", "", hotel_performance_muc$`ARR (€)`) %>%
             gsub(",", ".", .))
hotel_performance_muc$`RevPAR (€)` <- as.numeric(gsub(" €", "", hotel_performance_muc$`RevPAR (€)`) %>%
                            gsub(",", ".", .))

hotel_performance_muc <- hotel_performance_muc %>%
  gather(key = "KPI", value = "Value", 2:4)


hotel_performance_muc %>%
  filter(KPI != "OCC (%)") %>%
  ggplot(aes(x = Year, y = Value, fill = KPI)) +
  geom_col(position="dodge", ) +
  geom_text(aes(label = Value), data = hotel_performance_muc%>%filter(KPI == "RevPAR (€)"), nudge_y = -3.5 , nudge_x = 0.22, color = "white", fontface= "bold",size=3) +
  geom_text(aes(label = Value), data = hotel_performance_muc%>%filter(KPI == "ARR (€)"), nudge_y = -3.5 , nudge_x = -0.22, color = "white", fontface= "bold",size=3) +
  geom_line(mapping= aes(x = Year, y = (Value*100)/0.83), data = hotel_performance_muc%>%filter(KPI == "OCC (%)"),group = 1, linewidth = 1, color = "#00BA38") +
  scale_y_continuous(sec.axis = sec_axis(~ . * 0.83, name = "Performance (%)")) +
  labs(y = "Performance (€)", fill = "", title = "Average performance of hotels in Munich from 2009 to 2019") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"))


# Average occupancy in 2019 and 2018
avg_occ_171819 <- hotel_performance_muc %>%
  filter(Year %in% c("2019" , "2018", "2017"), KPI == "OCC (%)") %>%
  select(-KPI) %>%
  pull(Value) %>%
  mean()

# We assume an occupancy of 100% during the time of Oktoberfest

# Duration of the Oktoberfest (in days)
duration_oktoberfest <- int_length(interval(ymd("2023-09-16"), ymd("2023-10-03")))/(60*60*24)

# Calculation of average occupancy during no event phases
avg_occ_no_event <- (((avg_occ_20182019*100)*365-100*duration_okotberfest)/(365-duration_oktoberfest))/100

0.3*lm_occupancy[[2]]



