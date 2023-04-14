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
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](Data_Analysis_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
