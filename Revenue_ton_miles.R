destfile <- "Data/Supply_Chain.xlsx"

url = "https://www.cn.ca/-/media/Files/Investors/Investor-Performance-Measures/CN-BU-WEBSUMMARYpv.xlsx"
download.file(url, destfile, mode = "wb")
