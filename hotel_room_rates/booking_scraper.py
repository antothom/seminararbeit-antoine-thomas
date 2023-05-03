import re
import time
import requests
from bs4 import BeautifulSoup
from datetime import date, timedelta
import pandas as pd



def scrape_prices(accomodation_url, checkin_date, checkout_date, adults, children, no_rooms):
    g = list()
    o = {}
    k = {}

    # Setting up target url
    target_url = accomodation_url + "checkin=" + checkin_date + "&checkout=" + checkout_date + "&group_adults=" + str(adults) + "&group_children=" + str(children) + "&no_rooms=" + str(no_rooms) + "&selected_currency=EUR"
    # Scraping via Scrapingdog.com
    #target_url = "https://api.scrapingdog.com/scrape?api_key=API-KEY&url=" + target_url + "&dynamic=false"
    headers={"User-Agent":"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/107.0.0.0 Safari/537.36"}
    resp = requests.get(target_url, headers=headers)

    soup = BeautifulSoup(resp.text, 'html.parser')


    error = True
    # Safety loop in case a booking site might not load properly
    while (error == True):
        error = False
        try:
            o["name"] = soup.find("h2", {"class": "pp-header__title"}).text
        except:
            error = True
            print("Scraping Error:", resp.url)
            time.sleep(30)
            resp = requests.get(target_url, headers=headers)
            soup = BeautifulSoup(resp.text, 'html.parser')

    # Adding predefined information to the tuple
    o["date"] = checkin_date
    o["adults"] = adults
    o["children"] = children
    o["no_rooms"] = no_rooms

    # Adding hotel address to the tuple
    o["address"] = soup.find("span", {"class": "hp_address_subtitle"}).text.strip("\n")
    # Adding hotel rating, if available
    try:
        o["rating"] = float(soup.find("div", {"class": "d10a6220b4"}).text.replace(",","."))
    except:
        o["rating"] = None


    # Extracting available prices for all available room types
    # Every room category is in a tr element having a 'data-block-id'. These 'data-block-id' values have to be retrieved for specific targeting afterwards
    ids = list()
    try:
        tr = soup.find_all("tr")
    except:
        tr = None

    for y in range(0, len(tr)):
        try:
            id = tr[y].get('data-block-id')
        except:
            id = None
        if (id is not None):
            ids.append(id)

    # Iterate over all 'data-block-id's to retrieve associated prices for respective room categories
    for i in range(0, len(ids)):

        try:
            allData = soup.find("tr", {"data-block-id": ids[i]})
            try:
                rooms = allData.find("span", {"class": "hprt-roomtype-icon-link"})
            except:
                rooms = None

            if (rooms is not None):
                last_room = rooms.text.replace("\n", "")
            try:
                k["room"] = rooms.text.replace("\n", "")
            except:
                k["room"] = last_room

            # Get capacity for 'data-block-id' i
            capacity = allData.find("div", {
                "class": "c-occupancy-icons hprt-occupancy-occupancy-info"})
            k["capacity"] = int(re.findall(r'\d+', capacity.text.replace("\n", ""))[0])

            # Get price for 'data-block-id' i
            price = allData.find("div", {
                "class": "bui-price-display__value prco-text-nowrap-helper prco-inline-block-maker-helper prco-f-font-heading"})
            k["price"] = int(re.findall(r'\d+',price.text.replace("\n", ""))[0])

            # If capacity of 'data-block-id' i corresponds to asked capacity, add price to price list g
            if (k["capacity"] == (adults+children)):
                g.append(k)

            k = {}



        except:
            k["room"] = None
            k["price"] = None

    # return one list l. index 0: Date, Requested Capacity, Amount of rooms, address etc..
    # index 1: List of applying prices
    l = [o,g]
    return (l)


# Define a date range for which prices should be scraped
start_date = date(2023, 5, 10)
end_date = date(2024, 1, 1)    # perhaps date.now()

delta = end_date - start_date   # returns timedelta
time_period = []

# loop creating every date within the given range. For each date will be done one booking.com reservation site request
for i in range(delta.days + 1):
    day = str(start_date + timedelta(days=i))
    time_period.append(day)

results = []

for i in range(0, len(time_period)-1):
    results.append(scrape_prices("https://www.booking.com/hotel/de/b-amp-b-munchen-hbf.de.html?", time_period[i], time_period[i+1], 2,0,1))
    # Printing scraping progress in the console
    print("Elapsed: " + str(int((i+1)*(100/(len(time_period)-1)))) + "%")
    print(start_date+timedelta(days=i))


all_prices = []
# Iterating over all results and getting them into a clean format
for i in range(0,len(results)):
    for j in range(0, len(results[i][1])):
        all_prices.append({'name': results[i][0]['name'],
                     'date': results[i][0]['date'],
                     'adults': results[i][0]['adults'],
                     'children': results[i][0]['children'],
                     'no_rooms': results[i][0]['no_rooms'],
                     'address': results[i][0]['address'],
                     'rating': results[i][0]['rating'],
                     'room': results[i][1][j]['room'],
                     'capacity': results[i][1][j]['capacity'],
                     'price': results[i][1][j]['price']})

# Creating data frame with clean results and storing it in variable df
df = pd.DataFrame(all_prices)
df.to_excel(r'output.xlsx', index=False)
df = df.append(all_prices, ignore_index=True)
