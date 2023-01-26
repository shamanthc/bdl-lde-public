destfile <- "Data/Revenue_ton_miles.xlsx"

url = "https://www.cn.ca/-/media/Files/Investors/Investor-Performance-Measures/CN-BU-WEBSUMMARYpv.xlsx"
download.file(url, destfile, mode = "wb")
