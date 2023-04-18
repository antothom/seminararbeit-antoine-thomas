Seminar Work - Data Analysis
================
Antoine Thomas
2023-04-17

## Data Analysis

### Hotel room rates

First, the scraped datasets with price information for selected hotels
are imported. Then, the data records for all hotels are filtered so that
they only contain price information for simple double bedrooms. In
addition, care is taken to ensure that there is a maximum of one entry
for each hotel each day. Thereby, the cheapest option is always chosen.
This is primarily due to the fact that more expensive prices usually
include various additional services, such as free cancellation up to the
day of the start of the stay or meals such as breakfast. Thus, it can be
assured that all prices cover a comparable range of services, regardless
of the hotel. Subsequently, all data of the respective hotels are
combined into one large data set.

``` r
# Data import
hotel_bb <- read_csv2(file = "B&B_Hotel_München-Hbf.csv")

# Filter for double bedrooms and keep the cheapest possible price per day
hotel_bb_clean <- hotel_bb %>%
  filter(room == "Doppelzimmer") %>%
  group_by(date, room) %>%
  slice(which.min(price))

# Further import process hidden........
```

``` r
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
```

In the dataset `hotel_data` the high demand phase, the Oktoberfest, must
now be noted. The 2023 event takes place from 16.09.2023 to 03.10.2023.
To do this, the function `checkOktoberfest` is created, which is given a
date as parameter, and checks if this date is within period of the
Oktoberfest. After that a new attribute `oktoberfest` is created in
`hotel_data`, which indicates with the help of the mentioned function,
whether the Oktoberfest takes place on a specific day or not.

``` r
checkOktoberfest <- function(roomDate) {
  oktoberfestDateInterval = interval(ymd("2023-09-16"), ymd("2023-10-03"))
  roomDate %within% oktoberfestDateInterval
}

hotel_data <- hotel_data  %>%
  mutate(oktoberfest = unlist(map(.x = date, .f = checkOktoberfest)))
```

#### Price over time

In the following figures, the temporal price progression over the period
from April 10, 2023 to December 31, 2023 is illustrated for the selected
hotels. Vertical lines have been included on these to indicate the start
and end of the Oktoberfest period. From these figures, it can be seen
that all hotels retrieve the highest average prices of the year during
the Oktoberfest period. Although there are clearly similar fluctuations
in the pricing of hotel rooms at other times of the year, these cannot
be compared with the extent of the Oktoberfest.

![](Data_Analysis_files/figure-gfm/price%20over%20time%20plots-1.png)<!-- -->![](Data_Analysis_files/figure-gfm/price%20over%20time%20plots-2.png)<!-- -->

To enable a separated analysis of the two time periods, the data sets
`hotel_data_oktoberfest` and `hotel_data_no_oktoberfest` were created by
filtering in `hotel_data` for the attribute `oktoberfest` which was
created previously.

``` r
# Filtering room rates during the time of the Oktoberfest
hotel_data_oktoberfest <- filter(hotel_data, oktoberfest == TRUE)
# Filtering room rates during times not related to the Oktoberfest
hotel_data_no_oktoberfest <- filter(hotel_data, oktoberfest == FALSE)
```

In the figures above showing the price trend, one could clearly
recognize that even in periods unrelated to the Oktoberfest,
above-average prices can occur in very short periods. A boxplot diagram
clearly depicts this characteristic through observable outliers. Thereby
these disperse from approximately 240 to 580 euros. These outliers
represent about *12.5%* of the observations. An initial assumption that
these prices are called up on weekends in order to profit from weekend
tourism was investigated in more detail. For this purpose, all outliers
in the dataset `hotel_data_no_oktoberfest` were filtered. Then, the
respective day of the week was added to each observation. However, in
the following diagram, which shows the number of outliers per day of the
week, no higher price tendency can be observed on weekends. Since these
outliers could not be linked to any other clear event or cause, it was
decided to exclude them from the data set and to disregard them in the
context of the further analysis.

![](Data_Analysis_files/figure-gfm/boxplot%20prices%20no%20oktoberfest-1.png)<!-- -->![](Data_Analysis_files/figure-gfm/boxplot%20prices%20no%20oktoberfest-2.png)<!-- -->

After the outliers (12.5% highest price-related observations) in the
time period other than the Oktoberfest were removed from the data set,
adjusted average values could be determined. In 2023, according to our
dataset, the average price of a single double room in a mid-range hotel
during Oktoberfest is around €402. Over the rest of the year, a room
costs about 124€ per night.

``` r
hotel_data_no_oktoberfest %>%
  filter(price < quantile(hotel_data_no_oktoberfest$price, .875)) %>%
  rbind(hotel_data_oktoberfest) %>%
  group_by(oktoberfest) %>%
  summarise(avg_price = mean(price)) %>%
  kable()
```

| oktoberfest | avg_price |
|:------------|----------:|
| FALSE       |  123.8194 |
| TRUE        |  401.5461 |

When looking at the the individual hotels, the average room rates in
both phases as well as their relative difference were calculated for
each of them. An average relative price increase of around 320% can be
determined for the Oktoberfest period in comparison to normal demand
phases. Here, however, the hotels tend to considerably differ from each
other. The smallest price difference between both phases is 283% in our
hotel sample, while the highest is 389%. The standard deviation is
around 32%.

``` r
# Average room rate during normal demand phases for each hotel
avg_room_rates_no_event <- hotel_data_no_oktoberfest %>%
  filter(price < quantile(hotel_data_no_oktoberfest$price, .875)) %>%
  group_by(name) %>%
  summarise(room_rate_no_event = mean(price))

# Average room rate during high demand phases for each hotel
avg_room_rates_oktoberfest <- hotel_data_oktoberfest %>%
  group_by(name) %>%
  summarise(room_rate_oktoberfest = mean(price))

# Binding of both data sets and computing the relative difference in price between both demand phases
avg_room_rates_no_event %>%
  left_join(avg_room_rates_oktoberfest, by = "name") %>%
  mutate(rel_diff = room_rate_oktoberfest/room_rate_no_event) %>%
  arrange(rel_diff) %>%
  kable()
```

| name                                          | room_rate_no_event | room_rate_oktoberfest | rel_diff |
|:----------------------------------------------|-------------------:|----------------------:|---------:|
| Hotel GIO                                     |           116.3439 |              330.0000 | 2.836418 |
| Hotel Demas City                              |           126.5355 |              364.2941 | 2.878987 |
| Brunnenhof City Center                        |           138.5043 |              409.3636 | 2.955603 |
| Bold Hotel München Zentrum                    |           118.5969 |              358.0000 | 3.018630 |
| relexa hotel München                          |           142.0854 |              432.0714 | 3.040927 |
| Hotel Mirabell by Maier Privathotels          |           123.3333 |              395.7222 | 3.208559 |
| Hotel Munich City                             |           144.6480 |              481.6667 | 3.329924 |
| Hotel München City Center affiliated by Meliá |           126.0642 |              422.0769 | 3.348112 |
| Centro Hotel Mondial                          |           105.4528 |              362.6429 | 3.438911 |
| B&B Hotel München-Hbf                         |           105.0900 |              408.4444 | 3.886614 |

``` r
# Summary of rel_diff values
avg_room_rates_no_event %>%
  left_join(avg_room_rates_oktoberfest, by = "name") %>%
  mutate(rel_diff = room_rate_oktoberfest/room_rate_no_event) %>%
  arrange(rel_diff) %>%
  pull(rel_diff) %>%
  summary()
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   2.836   2.971   3.125   3.194   3.344   3.887

``` r
# Standard deviation of rel_diff
avg_room_rates_no_event %>%
  left_join(avg_room_rates_oktoberfest, by = "name") %>%
  mutate(rel_diff = room_rate_oktoberfest/room_rate_no_event) %>%
  arrange(rel_diff) %>%
  pull(rel_diff) %>%
  sd() %>%
  kable(col.names = "Standard deviation")
```

| Standard deviation |
|-------------------:|
|          0.3197654 |

In the course of the further analysis, room rates had to be assumed for
the high demand phase, the Oktoberfest, as well as the ordinary demand
phases. For this purpose, the corresponding previously defined average
values (401€/night at times of the Oktoberfest, 123€/night in normal
demand phases) were used.

### Hotel market performance in Munich

To obtain general data on the performance of hotels in Munich, the
report on the hotel market from 2019 was used as part of the analysis.
Due to the unusual situation in 2020 and 2021 caused by the Covid
pandemic, a possible use of more recent data had to be examined. In
2020, prices on the Munich hotel market had dropped by an average of
22.8%, according to the Munich Hotel Market Report. “The massive drop in
prices is also related to the cancellation of the Munich Oktoberfest as
well as other events and the absence of demand by international guests”
(Colliers International Hotel GmbH 2021). Although the situation
improved a bit according to the following year’s report (Colliers
International Hotel GmbH 2022), numerous events, including the
Oktoberfest, did not take place that year either. Since the event did
not take place in these two years and the Munich hotel industry was in
an exceptional situation, the use of data from 2020 and 2021 does not
make sense in our analysis.

In the hotel market report Colliers International, hotel performance
KPIs from 2009 to 2019 can be taken from a graph. Its data was extracted
in order to enable a reproduction as part of the Data Collection.
According to the report, a positive development of the Munich market
could be observed in 2019. Although room occupancy declined by
approximately 1.3%, this was compensated by a slight increase in room
rates. Thus, the revenue per available room also increased slightly
(Colliers International Hotel GmbH 2020).

The following figure illustrates the average annual values of the
Average Daily Rate (ADR (€)), the Revenue Per Available Room (REVPAR
(€)) as well as the Occupancy (OCC (%)) for every year from 2009 to 2019
in Munich.

``` r
# Import of hotel performance data
hotel_performance_muc <- read_csv2("munich_arr_revpar_occ.csv") %>%
  # Converting number formats and data types to vizualisable 
  mutate(`OCC (%)` = `OCC (%)`/100,
         Year = as.factor(Year))

# Converting 'ARR (€)' and 'RevPAR (€)' to numeric values
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
```

![](Data_Analysis_files/figure-gfm/import%20of%20hotel%20performance%20kpis-1.png)<!-- -->

In order to apply Leon and Lee’s model, a room rate as well as an
associated occupancy rate must be given for each demand phase. As the
average room rates for high and normal demand were determined in the
first part of the analysis, occupancy rates had to be estimated for the
hotel market in Munich. According to an article in the “Süddeutsche
Zeitung” on hotel bookings at times of the Oktoberfest, almost all
hotels were fully booked out in 2019, not only in Munich, but also
outside the city (Katharina Haase 2022). Since the sample taken consists
of mid-range hotels in the direct proximity of the Oktoberfest, it was
assumed in the further analysis that these were always fully booked at
times of the event. With an average price of 401€, it was therefore
assumed that a hotel in our sample has an occupancy rate of 100% at the
time of the event.

For an estimation of the occupancy rate during a normal demand period,
the data in Colliers International’s 2019 market report was used. For
this purpose, the average of occupancy rates for 2019 and 2018 was
initially calculated. However, since this figure is an average value for
the whole year, which does not take into account the fact that an
occupancy rate of 100% was assumed for the 17 days on which Oktoberfest
takes place, it must be adjusted accordingly.

``` r
# Average of occupancy for 2019 and 2018
avg_occ_1819 <- hotel_performance_muc %>%
  filter(Year %in% c("2019" , "2018"), KPI == "OCC (%)") %>%
  summarise(`Average occupancy`= mean(Value))

avg_occ_1819 %>%
  kable()
```

| Average occupancy |
|------------------:|
|             0.772 |

<br />

**Formula for an adjusted occupancy:**<br />

- OCC<sub>adj</sub>: adjusted occupancy
- OCC<sub>avg</sub>: average occupancy over the entire year
- OCC<sub>okt</sub>: occupancy during the Oktoberfest

<br /> $$
OCC_{adj} = \frac{365 * OCC_{avg} - 17 * OCC_{okt}}{365-17}
$$ <br />

``` r
# An occupancy rate of 100% is assumed during the time of Oktoberfest
# Duration of the Oktoberfest (in days)
duration_oktoberfest <- int_length(interval(ymd("2023-09-16"), ymd("2023-10-03")))/(60*60*24)

# Calculation adjusted occupancy rate
avg_occ_no_event <- (365*avg_occ_1819$`Average occupancy`-17*1)/(365-17)

kable(avg_occ_no_event, col.names = "avg_adj_occ", caption = "Average adjusted occupancy rate in times not related to the Oktoberfest")
```

| avg_adj_occ |
|------------:|
|   0.7608621 |

Average adjusted occupancy rate in times not related to the Oktoberfest

<br />

### Relationship between room rate and occupancy

If hotels set their prices higher than those of their competitors and
the demand on the market remains equal, they will most probably
experience higher revenues. Contrarily, if the set prices are lower than
those of the competition, respective hotels are likely to encounter
higher occupancy. In Enz’ and Canina’s study, the impact on hotel
revenues and occupancy rates was determined on the basis of price
differences among direct competitors in a local market. They found that
price differences in the average room rate relative to competitors had
an impact on revenues and occupancy rates. However, relative price
differences are not found to cause large fluctuations in occupancy
rates. On average, a 15-30% higher room rate relative to the competition
results in a 3.46% lower occupancy rate. A 15-30% lower room rate
results in an occupancy rate increase of 5.9%. When distinguishing
between hotels belonging to a chain and independent hotels, significant
differences can be observed. It clearly stands out that independent
hotels are confronted with lower revenues and occupancy rates (Enz and
Canina 2010). These figures, which apply to the European market, were
considered to be representative of the market in Munich for the purposes
of this work. They were extracted in the following analysis and used to
determine a price-demand relationship for the hotel market in Munich.

``` r
# Import of data on RevPAR and occupancy differences (chain v independent)
revpar_and_occupancy <- read_csv2(file= "revpar_and_occupancy.csv") %>%
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
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
```

![](Data_Analysis_files/figure-gfm/revpar%20and%20occupancy%20differences-1.png)<!-- -->

``` r
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
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
```

![](Data_Analysis_files/figure-gfm/revpar%20and%20occupancy%20differences-2.png)<!-- -->

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-colliers1" class="csl-entry">

Colliers International Hotel GmbH. 2020. “München: Hotelmarkt 2019
Q1-Q4.” January 1, 2020.
[https://www.colliers.de/wp-content/uploads/2020/05/Hotelmarktbericht_München_2020_Colliers.pdf](https://www.colliers.de/wp-content/uploads/2020/05/Hotelmarktbericht_München_2020_Colliers.pdf).

</div>

<div id="ref-colliers2" class="csl-entry">

———. 2021. “München: Hotelmarkt 2020 Q1-Q4.” January 1, 2021.
[https://www.colliers.de/wp-content/uploads/2022/01/JAN_PDF_Hotelmarktbericht_München_2021_Colliers_Bild.pdf](https://www.colliers.de/wp-content/uploads/2022/01/JAN_PDF_Hotelmarktbericht_München_2021_Colliers_Bild.pdf).

</div>

<div id="ref-colliers3" class="csl-entry">

———. 2022. “München: Hotelmarkt 2021 Q1-Q4.” January 1, 2022.
[https://www.colliers.de/wp-content/uploads/2022/12/Hotelmarktbericht_München_2022_Colliers.pdf](https://www.colliers.de/wp-content/uploads/2022/12/Hotelmarktbericht_München_2022_Colliers.pdf).

</div>

<div id="ref-enz2010competitive" class="csl-entry">

Enz, Cathy A, and Linda Canina. 2010. “Competitive Pricing in European
Hotels.” In *Advances in Hospitality and Leisure*, 6:3–25. Emerald Group
Publishing Limited.

</div>

<div id="ref-haase1" class="csl-entry">

Katharina Haase, SZ. 2022. “Hoteliers Zu Möglicher Wiesn-Absage: "Es
Gibt Keinen Plan b!".” July 20, 2022.
<https://www.sueddeutsche.de/muenchen/oktoberfest-2022-muenchen-hotels-buchungen-1.5623836>.

</div>

</div>
