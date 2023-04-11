library(tidyverse)
library(lubridate)
library(xtable)

# Data import
hotel_bb <- read_csv2(file = "B&B_Hotel_München-Hbf.csv")
hotel_bold <- read_csv2(file = "Bold_Hotel_München.csv")
hotel_brunnenhof <- read_csv2(file = "Brunnenhof_City_Center.csv")
hotel_centro_mondial <- read_csv2(file = "Centro_Hotel_Mondial.csv")
hotel_demas <- read_csv2(file = "Hotel_Demas_München.csv")
hotel_gio <- read_csv2(file = "Hotel_GIO.csv")
hotel_mirabell <- read_csv2(file = "Hotel_Mirabell.csv")
hotel_city_center <- read_csv2(file = "Hotel_München_City_Center.csv")
hotel_munchen_city <- read_csv2(file = "Hotel_Munich_City.csv")
hotel_relexa <- read_csv2(file = "relexa_hotel_München.csv")

# Filter for double bedrooms and keep the cheapest possible price per day
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

# Function returning a boolean indicating if the the date falls into the time of the oktoberfest
checkOktoberfest <- function(roomDate) {
  oktoberfestDateInterval = interval(ymd("2023-09-16"), ymd("2023-10-03"))
  roomDate %within% oktoberfestDateInterval
}

# binding data of all hotels
# mutating information about the oktoberfest
hotel_data <- rbind(hotel_bb_clean,
        hotel_bold_clean,
        hotel_brunnenhof_clean,
        hotel_centro_mondial_clean,
        hotel_city_center_clean,
        hotel_demas_clean,
        hotel_gio_clean,
        hotel_mirabell_clean,
        hotel_munchen_city_clean,
        hotel_relexa_clean) %>%
  mutate(oktoberfest = unlist(map(.x = date, .f = checkOktoberfest)))

# Filtering room rates during the time of the Oktoberfest
hotel_data_oktoberfest <- filter(hotel_data, oktoberfest == TRUE)
# Filtering room rates during times not related to the Oktoberfest
hotel_data_no_oktoberfest <- filter(hotel_data, oktoberfest == FALSE)


hotel_data %>%
  group_by(date) %>%
  summarise(price = mean(price)) %>%
  ggplot(aes(x = date, y = price)) +
  geom_line() +
  geom_vline(xintercept = date("2023-09-16"), colour = "red", alpha =.3) +
  geom_vline(xintercept = date("2023-10-03"), colour = "red", alpha = .3) +
  geom_smooth() +
  labs(x = "Date", y = "Room Rate")
  
hotel_data %>%
  ggplot(aes(x = date, y = price)) +
  geom_line() +
  geom_vline(xintercept = date("2023-09-16"), colour = "red", alpha =.3) +
  geom_vline(xintercept = date("2023-10-03"), colour = "red", alpha = .3) +
  facet_wrap(~name) +
  theme_bw() +
  labs(x = "Date",y = "Room Rate", title = "Price history for a double bedroom in 10 similar hotels in the center of Munich, Germany (04/23-12/23)")

# Boxplot showing that during the time not related to the oktoberfest, there are a lot of outliers
hotel_data_no_oktoberfest %>%
  ggplot(aes(x = price)) +
  geom_boxplot(outlier.alpha = .5) +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(face = "bold")) +
  labs(x = "Room rate", title = "Room rate distribution in times not related to the Oktoberfest 2023")

# Average room rate in Munich during a time with no particularly high demand
# Removed outliers
avg_price_no_oktoberfest <- hotel_data_no_oktoberfest %>%
  filter(price < quantile(hotel_data_no_oktoberfest$price, .75)) %>%
  pull(price) %>%
  mean()

# Average room rate in Munich during the time with no particularly high demand
avg_price_oktoberfest <- hotel_data_oktoberfest %>%
  pull(price) %>%
  mean()

## Average values per hotel

avg_room_rates_no_event <- hotel_data_no_oktoberfest %>%
  filter(price < quantile(hotel_data_no_oktoberfest$price, .75)) %>%
  group_by(name) %>%
  summarise(room_rate_no_event = mean(price)) %>%
  arrange(room_rate_no_event)

avg_room_rates_oktoberfest <- hotel_data_oktoberfest %>%
  group_by(name) %>%
  summarise(room_rate_oktoberfest = mean(price))

avg_room_rates_no_event %>%
  left_join(avg_room_rates_oktoberfest, by = "name") %>%
  mutate(rel_diff = room_rate_oktoberfest/room_rate_no_event) %>%
  arrange(rel_diff) %>%
  xtable()
  
