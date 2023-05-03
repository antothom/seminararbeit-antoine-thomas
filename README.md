# Seminar Work - Antoine Thomas

### Application of a price optimisation model to the Munich hotel market

Student-ID: 2614919\
E-Mail: antoine.thomas\@stud-mail.uni-wuerzburg.de

To apply the price optimization model of Leong and Lee (2020) to the Munich hotel market, data had to be obtained and processed in various software programmes. In the following, a list is provided that explains the technical analysis steps and the associated scripts and software used.

**Data collection of hotel room rates:**

Hotel room rates were scraped with a web Scraper programmed in python. For a secure scraping process, the web scraping API of scrapingdog.com was used. An API key for its use is not available in the script.\
The python `booking_scraper.py` script can be found in the folder `hotel_room_rates`.

**Analysis of hotel room rates, the munich hotel market performance and price-demand relationships:**

The analysis of hotel room rates, munich hotel market performance and the price-demand relationship was conducted in R. The corresponding script `data_analysis.R` can be found in the `main` folder of the repository.
