library(tidyr)
library(dplyr)


#===============================================================================================================
#                                               Hotel Occupancy Rate
#================================================================================================================

# hotel website link yearly for Occupancy Rate
#==============================================
# First 10 provinces
url_link1 <- "http://www.mtc-currentperformance.com/HotelDataXML.aspx?querytype=1&type=csv&sy=2001&sm=1&ey=2022&em=8&MS=1&GA=1,2,3,4,5,6,7,8,9,10&PR=&PSR="

dt1 <- read.csv(url(url_link1),header = T, check.names = F)%>%
  select(where(~ any(!is.na(.))))

colnames(dt1)[1] <- "Year"

dt1 <- dt1 %>% 
  tidyr::pivot_longer(!c(Year,Month), names_to = "GEO", values_to = "Value")


# save data
#==========

write.csv(dt1, file   = paste0("Data/Hotel_data.csv")  , row.names = F)
