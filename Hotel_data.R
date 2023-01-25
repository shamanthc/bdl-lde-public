library(tidyr)
library(dplyr)


#===============================================================================================================
#                                               Hotel Occupancy Rate
#================================================================================================================

# hotel website link yearly for Occupancy Rate
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


dt1 <- read.csv(url(url_link1),header = T, check.names = F)%>%
  select(where(~ any(!is.na(.))))
colnames(dt1)[1] <- "Year"
dt1 <- dt1 %>% 
  tidyr::pivot_longer(!c(Year,Month), names_to = "GEO", values_to = "Value")

dt2 <- read.csv(url(url_link2),header = T, check.names = F)%>%
  select(where(~ any(!is.na(.))))
colnames(dt2)[1] <- "Year"
dt2 <- dt2 %>% 
  tidyr::pivot_longer(!c(Year,Month), names_to = "GEO", values_to = "Value")

dt3 <- read.csv(url(url_link3),header = T, check.names = F)%>%
  select(where(~ any(!is.na(.))))
colnames(dt3)[1] <- "Year"
dt3 <- dt3 %>% 
  tidyr::pivot_longer(!c(Year,Month), names_to = "GEO", values_to = "Value")

dt4 <- read.csv(url(url_link4),header = T, check.names = F)%>%
  select(where(~ any(!is.na(.))))
colnames(dt4)[1] <- "Year"
dt4 <- dt4 %>% 
  tidyr::pivot_longer(!c(Year,Month), names_to = "GEO", values_to = "Value")

dt5 <- read.csv(url(url_link5),header = T, check.names = F)%>%
  select(where(~ any(!is.na(.))))
colnames(dt5)[1] <- "Year"
dt5 <- dt5 %>% 
  tidyr::pivot_longer(!c(Year,Month), names_to = "GEO", values_to = "Value")

dt6 <- read.csv(url(url_link6),header = T, check.names = F)%>%
  select(where(~ any(!is.na(.))))
colnames(dt6)[1] <- "Year"
dt6 <- dt6 %>% 
  tidyr::pivot_longer(!c(Year,Month), names_to = "GEO", values_to = "Value")

Hotel_occupancy_rate <- rbind(dt1, dt2, dt3, dt4, dt5, dt6)%>% dplyr::mutate("Type" = "Hotel Occupancy Rate")




#===============================================================================================================
#                                              Hotel Average Daily Rate
#================================================================================================================

# hotel website link for monthly average daily rate
#=======================================================


# First 10 provinces
url_link1 <- "http://www.mtc-currentperformance.com/HotelDataXML.aspx?querytype=1&type=csv&sy=2001&sm=1&ey=2022&em=8&MS=1&GA=1,2,3,4,5,6,7,8,9,10&PR=&PSR="

# next 10 cities
url_link2 <- "http://www.mtc-currentperformance.com/HotelDataXML.aspx?querytype=1&type=csv&sy=2001&sm=1&ey=2022&em=12&MS=2&GA=&PR=1,2,3,4,5,6,&PSR=a0,a1,b0,b1,"


# next 10 cities
url_link3 <- "http://www.mtc-currentperformance.com/HotelDataXML.aspx?querytype=1&type=csv&sy=2001&sm=1&ey=2022&em=12&MS=2&GA=&PR=7,&PSR=b2,b3,b4,b5,b6,c0,c1,c2,c3,"

# next 10 cities
url_link4 <- "http://www.mtc-currentperformance.com/HotelDataXML.aspx?querytype=1&type=csv&sy=2001&sm=1&ey=2022&em=12&MS=2&GA=&PR=8,9,10,11,&PSR=d0,d1,e0,e1,e2,f0,"

#next 10 cities
url_link5 <- "http://www.mtc-currentperformance.com/HotelDataXML.aspx?querytype=1&type=csv&sy=2001&sm=1&ey=2022&em=12&MS=2&GA=&PR=12,13,14,&PSR=f1,g0,g1,h0,h1,h2,i0,"

#last 5 cities
url_link6 <- "http://www.mtc-currentperformance.com/HotelDataXML.aspx?querytype=1&type=csv&sy=2001&sm=1&ey=2022&em=12&MS=2&GA=&PR=15,16,17,18,&PSR=i1,"


dt1 <- read.csv(url(url_link1),header = T, check.names = F)%>%
  select(where(~ any(!is.na(.))))
colnames(dt1)[1] <- "Year"
dt1 <- dt1 %>% 
  tidyr::pivot_longer(!c(Year,Month), names_to = "GEO", values_to = "Value")

dt2 <- read.csv(url(url_link2),header = T, check.names = F)%>%
  select(where(~ any(!is.na(.))))
colnames(dt2)[1] <- "Year"
dt2 <- dt2 %>% 
  tidyr::pivot_longer(!c(Year,Month), names_to = "GEO", values_to = "Value")

dt3 <- read.csv(url(url_link3),header = T, check.names = F)%>%
  select(where(~ any(!is.na(.))))
colnames(dt3)[1] <- "Year"
dt3 <- dt3 %>% 
  tidyr::pivot_longer(!c(Year,Month), names_to = "GEO", values_to = "Value")

dt4 <- read.csv(url(url_link4),header = T, check.names = F)%>%
  select(where(~ any(!is.na(.))))
colnames(dt4)[1] <- "Year"
dt4 <- dt4 %>% 
  tidyr::pivot_longer(!c(Year,Month), names_to = "GEO", values_to = "Value")

dt5 <- read.csv(url(url_link5),header = T, check.names = F)%>%
  select(where(~ any(!is.na(.))))
colnames(dt5)[1] <- "Year"
dt5 <- dt5 %>% 
  tidyr::pivot_longer(!c(Year,Month), names_to = "GEO", values_to = "Value")

dt6 <- read.csv(url(url_link6),header = T, check.names = F)%>%
  select(where(~ any(!is.na(.))))
colnames(dt6)[1] <- "Year"
dt6 <- dt6 %>% 
  tidyr::pivot_longer(!c(Year,Month), names_to = "GEO", values_to = "Value")

Average_daily_rate <- rbind(dt1, dt2, dt3, dt4, dt5, dt6)%>% dplyr::mutate("Type" = "Average Daily Rate")

#===============================================================================================================
#                                              Hotel Revenue per Available Room
#================================================================================================================

# hotel website link for monthly revenue per available Room
#===========================================================

# First 10 provinces
url_link1 <- "http://www.mtc-currentperformance.com/HotelDataXML.aspx?querytype=1&type=csv&sy=2001&sm=1&ey=2022&em=12&MS=3&GA=1,2,3,4,5,6,7,8,9,10&PR=&PSR="

# next 10 cities
url_link2 <- "http://www.mtc-currentperformance.com/HotelDataXML.aspx?querytype=1&type=csv&sy=2001&sm=1&ey=2022&em=12&MS=3&GA=&PR=1,2,3,4,5,6,&PSR=a0,a1,b0,b1,"


# next 10 cities
url_link3 <- "http://www.mtc-currentperformance.com/HotelDataXML.aspx?querytype=1&type=csv&sy=2001&sm=1&ey=2022&em=12&MS=3&GA=&PR=7,&PSR=b2,b3,b4,b5,b6,c0,c1,c2,c3,"

# next 10 cities
url_link4 <- "http://www.mtc-currentperformance.com/HotelDataXML.aspx?querytype=1&type=csv&sy=2001&sm=1&ey=2022&em=12&MS=3&GA=&PR=8,9,10,11,&PSR=d0,d1,e0,e1,e2,f0,"

#next 10 cities
url_link5 <- "http://www.mtc-currentperformance.com/HotelDataXML.aspx?querytype=1&type=csv&sy=2001&sm=1&ey=2022&em=12&MS=3&GA=&PR=12,13,14,&PSR=f1,g0,g1,h0,h1,h2,i0,"

#last 5 cities
url_link6 <- "http://www.mtc-currentperformance.com/HotelDataXML.aspx?querytype=1&type=csv&sy=2001&sm=1&ey=2022&em=12&MS=3&GA=&PR=15,16,17,18,&PSR=i1,"


dt1 <- read.csv(url(url_link1),header = T, check.names = F)%>%
  select(where(~ any(!is.na(.))))
colnames(dt1)[1] <- "Year"
dt1 <- dt1 %>% 
  tidyr::pivot_longer(!c(Year,Month), names_to = "GEO", values_to = "Value")

dt2 <- read.csv(url(url_link2),header = T, check.names = F)%>%
  select(where(~ any(!is.na(.))))
colnames(dt2)[1] <- "Year"
dt2 <- dt2 %>% 
  tidyr::pivot_longer(!c(Year,Month), names_to = "GEO", values_to = "Value")

dt3 <- read.csv(url(url_link3),header = T, check.names = F)%>%
  select(where(~ any(!is.na(.))))
colnames(dt3)[1] <- "Year"
dt3 <- dt3 %>% 
  tidyr::pivot_longer(!c(Year,Month), names_to = "GEO", values_to = "Value")

dt4 <- read.csv(url(url_link4),header = T, check.names = F)%>%
  select(where(~ any(!is.na(.))))
colnames(dt4)[1] <- "Year"
dt4 <- dt4 %>% 
  tidyr::pivot_longer(!c(Year,Month), names_to = "GEO", values_to = "Value")

dt5 <- read.csv(url(url_link5),header = T, check.names = F)%>%
  select(where(~ any(!is.na(.))))
colnames(dt5)[1] <- "Year"
dt5 <- dt5 %>% 
  tidyr::pivot_longer(!c(Year,Month), names_to = "GEO", values_to = "Value")

dt6 <- read.csv(url(url_link6),header = T, check.names = F)%>%
  select(where(~ any(!is.na(.))))
colnames(dt6)[1] <- "Year"
dt6 <- dt6 %>% 
  tidyr::pivot_longer(!c(Year,Month), names_to = "GEO", values_to = "Value")

Revenue_per_room <- rbind(dt1, dt2, dt3, dt4, dt5, dt6)%>% dplyr::mutate("Type" = "Revenue per Available Room")

# Combine all three datasets
#============================
Hotel_data <- rbind(Hotel_occupancy_rate, Average_daily_rate, Revenue_per_room)

# save data
#==========

write.csv(Hotel_data, file   = paste0("Data/Hotel_data.csv")  , row.names = F)
