
#===============================================================================================================
#                                               Hotel Occupancy Rate
#================================================================================================================

# hotel website link to monthly Occupancy Rate
#==============================================
# First 10 provinces
url_link1 <- "http://www.mtc-currentperformance.com/HotelDataXML.aspx?querytype=1&type=csv&sy=2001&sm=1&ey=2022&em=8&MS=1&GA=1,2,3,4,5,6,7,8,9,10&PR=&PSR="

# next 10 cities
url_link2 <- "http://www.mtc-currentperformance.com/HotelDataXML.aspx?querytype=1&type=csv&sy=2001&sm=1&ey=2022&em=8&MS=1&GA=&PR=1,2,3,4,5,6,&PSR=a0,a1,b0,b1,"

# next 10 cities
url_link3 <- "http://www.mtc-currentperformance.com/HotelDataXML.aspx?querytype=1&type=csv&sy=2001&sm=1&ey=2022&em=8&MS=1&GA=&PR=7,&PSR=b2,b3,b4,b5,b6,c0,c1,c2,c3,"

# next 10 cities
url_link4 <- "http://www.mtc-currentperformance.com/HotelDataXML.aspx?querytype=1&type=csv&sy=2001&sm=1&ey=2022&em=8&MS=1&GA=&PR=8,9,10,11,&PSR=d0,d1,e0,e1,e2,f0,"

#next 10 cities
url_link5 <- "http://www.mtc-currentperformance.com/HotelDataXML.aspx?querytype=1&type=csv&sy=2001&sm=1&ey=2022&em=8&MS=1&GA=&PR=12,13,14,&PSR=f1,g0,g1,h0,h1,h2,i0,"

#last 5 cities
url_link6 <- "http://www.mtc-currentperformance.com/HotelDataXML.aspx?querytype=1&type=csv&sy=2001&sm=1&ey=2022&em=8&MS=1&GA=&PR=15,16,17,18,&PSR=i1,"

# download data
#===============
download.file(url = url_link1, destfile = "Data/Hotel_Occupancy_Rate_monthly1.csv", method='curl')
download.file(url = url_link2, destfile = "Data/Hotel_Occupancy_Rate_monthly2.csv", method='curl')
download.file(url = url_link3, destfile = "Data/Hotel_Occupancy_Rate_monthly3.csv", method='curl')
download.file(url = url_link4, destfile = "Data/Hotel_Occupancy_Rate_monthly4.csv", method='curl')
download.file(url = url_link5, destfile = "Data/Hotel_Occupancy_Rate_monthly5.csv", method='curl')
download.file(url = url_link6, destfile = "Data/Hotel_Occupancy_Rate_monthly6.csv", method='curl')
