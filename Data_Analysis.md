Seminar Work - Data Analysis
================
Antoine Thomas
2023-04-14

## Data Analysis

### Import of hotel prices lists

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
