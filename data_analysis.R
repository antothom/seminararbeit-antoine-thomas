library(tidyverse)
library(lubridate)
library(xtable)
library(knitr)
library(ggpubr)


# Import of hotel room rates
hotel_bb <- read_csv2(file = "hotel_room_rates/B&B_Hotel_München-Hbf.csv")
hotel_bold <- read_csv2(file = "hotel_room_rates/Bold_Hotel_München.csv")
hotel_brunnenhof <- read_csv2(file = "hotel_room_rates/Brunnenhof_City_Center.csv")
hotel_centro_mondial <- read_csv2(file = "hotel_room_rates/Centro_Hotel_Mondial.csv")
hotel_demas <- read_csv2(file = "hotel_room_rates/Hotel_Demas_München.csv")
hotel_gio <- read_csv2(file = "hotel_room_rates/Hotel_GIO.csv")
hotel_mirabell <- read_csv2(file = "hotel_room_rates/Hotel_Mirabell.csv")
hotel_city_center <- read_csv2(file = "hotel_room_rates/Hotel_München_City_Center.csv")
hotel_munchen_city <- read_csv2(file = "hotel_room_rates/Hotel_Munich_City.csv")
hotel_relexa <- read_csv2(file = "hotel_room_rates/relexa_hotel_München.csv")

# Filter for simple double bedrooms and keep the cheapest possible price per day
hotel_bb_clean <- hotel_bb %>%
  filter(room == "Doppelzimmer") %>%
  group_by(date, room) %>%
  slice(which.min(price))

hotel_bb_clean <- hotel_bb %>%
  filter(room == "Doppelzimmer") %>%
  group_by(date, room) %>%
  slice(which.min(price))

hotel_bold_clean <- hotel_bold %>%
  filter(room == "Bold Doppelzimmer") %>%
  group_by(date, room) %>%
  slice(which.min(price))

hotel_brunnenhof_clean <- hotel_brunnenhof %>%
  filter(room == "Doppelzimmer") %>%
  group_by(date, room) %>%
  slice(which.min(price))

hotel_centro_mondial_clean <- hotel_centro_mondial %>%
  filter(room == "Standard Doppelzimmer") %>%
  group_by(date, room) %>%
  slice(which.min(price))

hotel_demas_clean <- hotel_demas %>%
  filter(room == "Standard Doppelzimmer") %>%
  group_by(date, room) %>%
  slice(which.min(price))

hotel_gio_clean <- hotel_gio %>%
  filter(room == "Doppelzimmer") %>%
  group_by(date, room) %>%
  slice(which.min(price))

hotel_mirabell_clean <- hotel_mirabell %>%
  filter(room == "Superior Zimmer") %>%
  group_by(date, room) %>%
  slice(which.min(price))

hotel_city_center_clean <- hotel_city_center %>%
  filter(room == "Standard Zimmer") %>%
  group_by(date, room) %>%
  slice(which.min(price))

hotel_munchen_city_clean <- hotel_munchen_city %>%
  filter(room == "Doppel- oder Zweibettzimmer") %>%
  group_by(date, room) %>%
  slice(which.min(price))

hotel_relexa_clean <- hotel_relexa %>%
  filter(room == "Comfort Doppelzimmer") %>%
  group_by(date, room) %>%
  slice(which.min(price))

# Remove raw datas
remove(hotel_bb, 
       hotel_bold, 
       hotel_brunnenhof, 
       hotel_centro_mondial, 
       hotel_demas, 
       hotel_gio, 
       hotel_mirabell, 
       hotel_city_center, 
       hotel_munchen_city, 
       hotel_relexa)

# binding data of all hotels
hotel_data <- rbind(hotel_bb_clean,
                    hotel_bold_clean,
                    hotel_brunnenhof_clean,
                    hotel_centro_mondial_clean,
                    hotel_city_center_clean,
                    hotel_demas_clean,
                    hotel_gio_clean,
                    hotel_mirabell_clean,
                    hotel_munchen_city_clean,
                    hotel_relexa_clean)

# function returning true or false wether a given date is within the period of Oktoberfest 2023 or not
checkOktoberfest <- function(roomDate) {
  oktoberfestDateInterval = interval(ymd("2023-09-16"), ymd("2023-10-03"))
  roomDate %within% oktoberfestDateInterval
}

# mutating variable 'oktoberfest' - return of funtion 'checkOktoberfest'
hotel_data <- hotel_data  %>%
  mutate(oktoberfest = unlist(map(.x = date, .f = checkOktoberfest)))


## Room rates in Munich

# Average room rate per day
hotel_data %>%
  group_by(date) %>%
  summarise(price = mean(price)) %>%
  ggplot(aes(x = date, y = price)) +
  geom_line() +
  geom_vline(xintercept = date("2023-09-16"), colour = "red", alpha =.3) +
  geom_vline(xintercept = date("2023-10-03"), colour = "red", alpha = .3) +
  theme_bw() +
  labs(x = "Date", y = "Room Rate (EUR)", title = "Average price over time for a double bedroom in a hotel in the center of Munich") +
  theme(plot.title = element_text(face = "bold", size = "11"))

# Room rate per day for each hotel
hotel_data %>%
  ggplot(aes(x = date, y = price)) +
  geom_line() +
  geom_vline(xintercept = date("2023-09-16"), colour = "red", alpha =.3) +
  geom_vline(xintercept = date("2023-10-03"), colour = "red", alpha = .3) +
  facet_wrap(~name) +
  theme_bw() +
  labs(x = "Date",y = "Room Rate (EUR)", title = "Price over time for a double bedroom in 10 similar hotels in the center of Munich") +
  theme(plot.title = element_text(face = "bold",size = "11"),strip.text.x = element_text(size = 6))


# Filtering room rates during the time of the Oktoberfest
hotel_data_oktoberfest <- filter(hotel_data, oktoberfest == TRUE)
# Filtering room rates during times not related to the Oktoberfest
hotel_data_no_oktoberfest <- filter(hotel_data, oktoberfest == FALSE)



## Price distribution in times not related to Oktoberfest

# Boxplot visualizing all the price distribution during times not related to the Oktoberfest
hotel_data_no_oktoberfest %>%
  ggplot(aes(x = price)) +
  geom_boxplot(outlier.alpha = .5) +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(face = "bold")) +
  theme_bw() +
  labs(x = "Room rate", title = "Room rate distribution in times not related to the Oktoberfest 2023") +
  theme(plot.title = element_text(face = "bold"))

# Diagram visualizing the amount of outliers per day of the week during times not related to the Oktoberfest
hotel_data_no_oktoberfest %>%
  filter(price > quantile(hotel_data_no_oktoberfest$price, .875)) %>%
  mutate(weekday = wday(date, label = TRUE)) %>%
  group_by(weekday) %>%
  summarise(amount = n()) %>%
  ggplot(aes(x = weekday, y = amount)) +
  geom_col() +
  theme_bw() +
  labs(x = "Weekday", y = "Amount of observations", title = "Amount of outliers per weekday during times not related to the Oktoberfest 2023") +
  theme(plot.title = element_text(face = "bold", size = "12"))



## Computing average and mean room rate for different phases of the year

# Average room rate for whole year 2023 
hotel_data %>%
  group_by(date) %>%
  summarise(price = mean(price)) %>%
  summarise(price = mean(price)) %>%
  kable()

# Mean room rate for whole year 2023
hotel_data %>%
  group_by(date) %>%
  summarise(price = mean(price)) %>%
  summarise(price = median(price)) %>%
  kable()

# Average room rates for normal demand phase as well as the period of Oktoberfest
# Normal demand phases represents all room rates excluding outliers (87.5% quantile)
hotel_data_no_oktoberfest %>%
  filter(price < quantile(hotel_data_no_oktoberfest$price, .875)) %>%
  rbind(hotel_data_oktoberfest) %>%
  group_by(oktoberfest,date) %>%
  summarise(avg_price = mean(price)) %>%
  group_by(oktoberfest) %>%
  summarise(avg_price = mean(avg_price)) %>%
  kable()


## Average room rates in different demand phases for each hotel

# Average room rate during normal demand phases for each hotel
avg_room_rates_no_event <- hotel_data_no_oktoberfest %>%
  filter(price < quantile(hotel_data_no_oktoberfest$price, .875)) %>%
  group_by(name) %>%
  summarise(room_rate_no_event = mean(price))

# Average room rate during Oktobefest period for each hotel
avg_room_rates_oktoberfest <- hotel_data_oktoberfest %>%
  group_by(name) %>%
  summarise(room_rate_oktoberfest = mean(price))

# Binding of demand periods and computing the relative difference in price between both demand phases
avg_room_rates_no_event %>%
  left_join(avg_room_rates_oktoberfest, by = "name") %>%
  mutate(rel_diff = room_rate_oktoberfest/room_rate_no_event) %>%
  arrange(rel_diff) %>%
  kable()

# Summary of rel_diff values
avg_room_rates_no_event %>%
  left_join(avg_room_rates_oktoberfest, by = "name") %>%
  mutate(rel_diff = room_rate_oktoberfest/room_rate_no_event) %>%
  arrange(rel_diff) %>%
  pull(rel_diff) %>%
  summary()

# Standard deviation of rel_diff
avg_room_rates_no_event %>%
  left_join(avg_room_rates_oktoberfest, by = "name") %>%
  mutate(rel_diff = room_rate_oktoberfest/room_rate_no_event) %>%
  arrange(rel_diff) %>%
  pull(rel_diff) %>%
  sd() %>%
  kable(col.names = "Standard deviation")



## Hotel performance data in Munich (data source: Colliers International Deutschland GmbH)

# Import of hotel performance data
hotel_performance_muc <- read_csv2("data_price_demand_rel/munich_arr_revpar_occ.csv") %>% 
  mutate(`OCC (%)` = `OCC (%)`/100,
         Year = as.factor(Year)) # Converting number formats and data types to vizualisable 

# Converting 'ARR (€)' and 'RevPAR (€)' to numeric values
hotel_performance_muc$`ARR (€)` <- as.numeric(gsub(" €", "", hotel_performance_muc$`ARR (€)`) %>%
                                                gsub(",", ".", .))
hotel_performance_muc$`RevPAR (€)` <- as.numeric(gsub(" €", "", hotel_performance_muc$`RevPAR (€)`) %>%
                                                   gsub(",", ".", .))

# Wide format to long format
hotel_performance_muc <- hotel_performance_muc %>%
  gather(key = "KPI", value = "Value", 2:4)

# Plot of hotel performance in munich according to Colliers International 
hotel_performance_muc %>%
  filter(KPI != "OCC (%)") %>%
  ggplot(aes(x = Year, y = Value, fill = KPI)) +
  geom_col(position="dodge", ) +
  geom_text(aes(label = Value), data = hotel_performance_muc%>%filter(KPI == "RevPAR (€)"), nudge_y = -3.5 , nudge_x = 0.22, color = "white", fontface= "bold",size=3) +
  geom_text(aes(label = Value), data = hotel_performance_muc%>%filter(KPI == "ARR (€)"), nudge_y = -3.5 , nudge_x = -0.22, color = "white", fontface= "bold",size=3) +
  geom_line(mapping= aes(x = Year, y = (Value*100)/0.83), data = hotel_performance_muc%>%filter(KPI == "OCC (%)"),group = 1, linewidth = 1, color = "#00BA38") +
  scale_y_continuous(sec.axis = sec_axis(~ . * 0.83, name = "Performance (%)")) +
  labs(y = "Performance (EUR)", fill = "", title = "Average performance of hotels in Munich from 2009 to 2019") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

# Average of occupancy for 2019 and 2018
avg_occ_1819 <- hotel_performance_muc %>%
  filter(Year %in% c("2019" , "2018"), KPI == "OCC (%)") %>%
  summarise(`Average occupancy`= mean(Value))

avg_occ_1819 %>%
  kable()


# Adjusting hotel occupancy for times not related to the Oktoberfest according to eq. 4
# An occupancy rate of 100% is assumed during the time of Oktoberfest

# Duration of the Oktoberfest (in days)
duration_oktoberfest <- int_length(interval(ymd("2023-09-16"), ymd("2023-10-03")))/(60*60*24)

# Calculation adjusted occupancy rate
avg_occ_no_event <- (365*avg_occ_1819$`Average occupancy`-duration_oktoberfest*1)/(365-duration_oktoberfest)

kable(avg_occ_no_event, col.names = "avg_adj_occ", caption = "Average adjusted occupancy rate in times not related to the Oktoberfest")



## Data on "price-demand relationship"/"price elasticity of demand" (data source: Enz & Canina (2010))

# Import of data on RevPAR and occupancy differences 
revpar_and_occupancy <- read_csv2(file= "data_price_demand_rel/revpar_and_occupancy.csv") %>%
  gather(key = "kpi", value = "rel_change", 2:3) %>%
  mutate(ADR = as.numeric(ADR)/100,
         rel_change = as.numeric(rel_change)/100)

# Plotting according to the Figure in the named report
revpar_and_occupancy %>%
  ggplot(aes(x = ADR, y = rel_change, colour = kpi)) +
  geom_line() +
  geom_line(stat = "smooth", method = "lm", se = F, linetype = "dashed", alpha = .7) +
  labs(x = "Relative difference in ADR",
       y = "Relative difference from the competition",
       title = "RevPAR and occupancy differences in European hotels, 2004-2013",
       colour = "") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent)


# Import of data on RevPAR and occupancy differences (chain v independent)
revpar_and_occupancy_chain_ind <- read_csv2(file= "revpar_and_occupancy_chain_ind.csv") %>%
  gather(key = "kpi", value = "rel_change", 2:5) %>%
  mutate(ADR = as.numeric(ADR)/100,
         rel_change = as.numeric(rel_change)/100)

# Plotting according to the Figure in the named report
revpar_and_occupancy_chain_ind %>%
  ggplot(aes(x = ADR, y = rel_change, colour = kpi)) +
  geom_line() +
  geom_line(stat = "smooth", method = "lm", se = F, linetype = "dashed", alpha = .7) +
  labs(x = "Relative difference in ADR",
       y = "Relative difference from the competition",
       title = "RevPAR and occupancy differences in European hotels, 2004-2013",
       subtitle = "Differentiation between being chain-affiliated and independent",
       colour = "") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent)



## Creation and adjustment of linear regression models based on the data from Enz & Canina (2010)

# Create linear regression for price-demand relationship
lm_occupancy <- revpar_and_occupancy %>%
  filter(kpi == "Occupancy") %>%
  lm(rel_change ~ ADR, data = .)

# Adjusting intercept to 0
lm_occupancy$coefficients[[1]] <- 0


# Create linear regression for price-RevPAR relationship
lm_revpar <- revpar_and_occupancy %>%
  filter(kpi == "RevPAR") %>%
  lm(rel_change ~ ADR, data = .)

# Adjusting intercept to 0
lm_revpar$coefficients[[1]] <- 0

# Creating values for visualization and plotting
tibble(ADR = c(-0.3,0.3)) %>%
  mutate(Occupancy = predict(lm_occupancy, .),
         RevPAR = predict(lm_revpar,.)) %>%
  gather(data = .,key = "kpi", value = "rel_change", 2:3) %>%
  ggplot(aes(x = ADR, y = rel_change, colour = kpi)) +
  geom_line(stat = "smooth", method = "lm", se = F) +
  labs(x = "Relative difference in ADR",
       y = "Relative difference from the competition",
       title = "RevPAR and occupancy differences for the Munich hotel sample",
       colour = "") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent)



## Adaption(hypothetical) of the price-demand Relation of Enz and Canina (2010) to the hotel market

# Definition of the market-related multiplier
lm_occupancy$coefficients[[2]] = lm_occupancy$coefficients[[2]]*3.5


# Function creating a table with specific prices and associated occupancy rates 
# for a given price and the associated occupancy rate.
# The previously created regression model 'lm_occupancy' was used to determine new data points.
create_specific_occupancies <- function(given_rate, given_occupancy) {
  tibble(
    ADR = round(seq(-0.7, 0.7, by = 0.05), 2)
  ) %>%
    mutate(rel_change = predict(lm_occupancy, .)) %>%
    mutate(
      spec_ADR = given_rate * (1 + ADR),
      spec_OCC = given_occupancy * (1 + rel_change)
    )
}


# Creation of specific prices and associated occupancy rates for the time of the oktoberfest
# Given ADR: 412.89€ - Given occupancy rate: 100%
specific_price_demand_rel_oktoberfest <- create_specific_occupancies(412.98,100) %>%
  mutate(demand = "Oktoberfest")

# Creation of specific prices and associated occupancy rates for a normal demand phase
# Given ADR: 128.89€ - Given occupancy rate: 76.0862%
specific_price_demand_rel_normal_phase <- 
  create_specific_occupancies(128.89,76.0862) %>%
  mutate(demand = "Normal phase")

# Binding both tables for a visualization of the price-demand relationship in both demand phases
rbind(specific_price_demand_rel_oktoberfest,
      specific_price_demand_rel_normal_phase) %>%
  mutate(spec_OCC = spec_OCC/100) %>%
  ggplot(aes(x = spec_ADR, y = spec_OCC, colour = demand)) +
  geom_point(alpha = .5) +
  geom_line(stat = "smooth", method = "lm", se = F) +
  stat_regline_equation(label.x.npc = .65,label.y.npc = .75, data = . %>% filter(demand == "Oktoberfest"),key_glyph = draw_key_blank) +
  stat_regline_equation(label.x.npc = .2,label.y.npc = .2, data = . %>% filter(demand == "Normal phase"),key_glyph = draw_key_blank) +
  labs(x = "Room rate (EUR)", y = "Occupancy (%)", colour = "Demand phase", title = "Price-demand relationship in Munich") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold"),legend.title = element_blank(), legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent)


# Creation of a linear regression model for the price-demand relationship during the period of Oktoberfest
specific_price_demand_rel_oktoberfest %>%
  mutate(spec_OCC = spec_OCC/100) %>%
  lm(spec_OCC ~ spec_ADR, .)

# Creation of a linear regression model for the price-demand relationship during a normal demand phase
specific_price_demand_rel_normal_phase %>%
  mutate(spec_OCC = spec_OCC/100) %>%
  lm(spec_OCC ~ spec_ADR, .)

