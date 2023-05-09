# Seminar Work - Antoine Thomas

### Application of a hotel pricing model to the munich hotel market

Student-ID: 2614919\
E-Mail: antoine.thomas\@stud-mail.uni-wuerzburg.de

In this work, data on the Munich hotel market has been applied to a hotel pricing model in order to define room rates for middle-class hotels that maximise profits. The Munich hotel market in particular has been very competitive for a long time, so it is important to obtain very accurate data. However, due to the lack of available information, assumptions have to be made about the situation in the Munich hotel market. It has been shown that, depending on the characteristics of a market and a hotel, setting higher or lower prices than the competition can lead to higher profits. It has also shown the importance of correctly determining variable costs and, in particular, the relationship between price and demand in a specific market. Hotel managers can use the results to gain valuable insights for effective pricing and straightforward risk management.

To apply the price optimization model of Leong and Lee (2020) to the Munich hotel market, data had to be obtained and processed in various software programmes. In the following, a list is provided that explains the technical analysis steps and the associated scripts and software used.

**Data collection of hotel room rates:**

First, hotel room rates were scraped with a web Scraper programmed in python. For a secure scraping process, the web scraping API of scrapingdog.com was used. An API key for its use is not available in the script.\
The python `booking_scraper.py` script can be found in the folder `hotel_room_rates`.\
For each hotel, comma-separated files were generated and can be found in the `hotel_room_rates`-folder as well.

**Analysis of hotel room rates, the Munich hotel market performance and price-demand relationships:**

The analysis of hotel room rates, Munich hotel market performance and the price-demand relationship was conducted in R. The corresponding script `data_analysis.R` can be found in the `main` folder of the repository.
