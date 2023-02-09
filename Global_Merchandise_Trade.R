library(dplyr)
library(stringr)
library(janitor)
library(readxl)


destfile <- "Data/Global_Merchandise_Trade.xlsx"

url = "https://www.cpb.nl/sites/default/files/omnidownload/CPB-World-Trade-Monitor-November-2022.xlsx"

download.file(url, destfile, mode = "wb")



data <- read_excel(destfile,skip=1, sheet = 1)
data6 <- filter(data, rowSums(is.na(data)) != ncol(data))
data6 <- data6[,colSums(is.na(data6))<nrow(data6)]

data_t = as.data.frame(t(data6)) #%>% tidyr::unite("V1", V1:V2, remove = TRUE)
data11 = data_t %>% mutate(mycol = coalesce(V1,V2)) 
data12 = data11 %>% relocate(mycol)   %>% select(-c("V1","V2"))
data_n = as.data.frame(t(data12))[1:30,] %>%
  janitor::row_to_names(row_number = 1)

names(data_n) <- str_replace_all(names(data_n), "m", "_")
names(data_n) <- str_replace_all(names(data_n), " ", "_")
names(data_n) <- str_replace_all(names(data_n), ",", "_")
names(data_n) <- str_replace_all(names(data_n), ":", "_")

data_n = data_n[,-3]
colnames(data_n)[2] = "Category"

#data_new = data_n %>%  tidyr::pivot_longer(!c(data_n[1],"Category"), names_to = "Year_Month", values_to = "Volumes")
data_new = data_n %>% 
  tidyr::pivot_longer(!c(colnames(data_n)[1], "Category"), names_to = "Year_Month", values_to = "Volumes")



data_to_use = data_new %>%  dplyr::rename("GEO" = colnames(data_n)[1]) 


data_to_use <- data_to_use %>%
  mutate(Category = substr(Category,1,3))



#Prices



Pdata_n = as.data.frame(t(data12))[31:60,] %>%
  janitor::row_to_names(row_number = 1)
colnames(Pdata_n)[-1] = colnames(data_n)[-1]

names(Pdata_n) <- str_replace_all(names(Pdata_n), "m", "_")
names(Pdata_n) <- str_replace_all(names(Pdata_n), " ", "_")
names(Pdata_n) <- str_replace_all(names(Pdata_n), ",", "_")
names(Pdata_n) <- str_replace_all(names(Pdata_n), ":", "_")
names(Pdata_n) <- str_replace_all(names(Pdata_n), "/", "_")

Pdata_n = Pdata_n[,-3]
colnames(Pdata_n)[2] = "Category"


Pdata_new = Pdata_n %>%  tidyr::pivot_longer(!c(colnames(Pdata_n)[1],"Category"), names_to = "Year_Month", values_to = "Prices")



Pdata_to_use = Pdata_new %>%  dplyr::rename("GEO" = colnames(Pdata_n)[1]) 


Pdata_to_use <- Pdata_to_use %>%
  mutate(Category = substr(Category,1,3))

Final_Data = dplyr::left_join(data_to_use,Pdata_to_use, by = c("GEO","Category","Year_Month"))


Final_Data = Final_Data %>% dplyr::mutate(Category = ifelse(Category == "tgz","World Trades",Category),
                                          Category = ifelse(Category == "mgz","World Imports",Category),
                                          Category = ifelse(Category == "xgz","World Exports",Category))%>%
  mutate(Year_Month = paste0(substr(Year_Month,1,4)," ",substr(Year_Month,6,7)))


Final_Data$Year_Month <- as.Date(as.character(paste(Final_Data$Year_Month, '01')),format="%Y%m%d")
Final_Data = Final_Data %>%  dplyr::rename("Date" = Year_Month)






Final_Data$Prices = as.numeric(Final_Data$Prices)


YearOverYear<-function (x,periodsPerYear){
  if(NROW(x)<=periodsPerYear){
    stop("too few rows")
  }
  else{
    indexes<-1:(NROW(Final_Data$Prices)-12)
    return(c(rep(NA,12),(Final_Data$Prices[indexes+12]-Final_Data$Prices[indexes])/Final_Data$Prices[indexes]))
  }
}


Final_Data = cbind(Final_Data,YoY_Prices=YearOverYear(Final_Data$Prices,12))
Final_Data = cbind(Final_Data,YoY_Vol=YearOverYear(Final_Data$Volumes,12))
Final_Data = Final_Data %>% dplyr::mutate(`Nominal Grwoth` = YoY_Prices+YoY_Vol)


Final_Data = Final_Data %>% dplyr::filter(Date>as.Date("2000-01-01"))


write.csv(Final_Data, file   = paste0("Data/Global_merchandise_trade.csv")  , row.names = F)


