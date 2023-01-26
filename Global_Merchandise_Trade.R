destfile <- "Data/Global_Merchandise_Trade.xlsx"

url = "https://www.cpb.nl/sites/default/files/omnidownload/CPB-World-Trade-Monitor-April-2022.xlsx"

download.file(url, destfile, mode = "wb")
