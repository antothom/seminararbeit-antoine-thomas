Seminar Work - Data Analysis
================
Antoine Thomas
2023-04-15

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

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-colliers2" class="csl-entry">

Colliers International Hotel GmbH. 2021. “München: Hotelmarkt 2020
Q1-Q4.” January 1, 2021.
[https://www.colliers.de/wp-content/uploads/2022/01/JAN_PDF_Hotelmarktbericht_München_2021_Colliers_Bild.pdf](https://www.colliers.de/wp-content/uploads/2022/01/JAN_PDF_Hotelmarktbericht_München_2021_Colliers_Bild.pdf).

</div>

<div id="ref-colliers3" class="csl-entry">

———. 2022. “München: Hotelmarkt 2021 Q1-Q4.” January 1, 2022.
[https://www.colliers.de/wp-content/uploads/2022/12/Hotelmarktbericht_München_2022_Colliers.pdf](https://www.colliers.de/wp-content/uploads/2022/12/Hotelmarktbericht_München_2022_Colliers.pdf).

</div>

</div>
