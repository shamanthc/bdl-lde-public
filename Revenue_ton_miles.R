#destfile <- "Data/Revenue_ton_miles.xlsx"
#
#url = "https://www.cn.ca/-/media/Files/Investors/Investor-Performance-Measures/CN-BU-WEBSUMMARYpv.xlsx"
#https://www.cn.ca/-/media/files/investors/investor-performance-measures/cn-bu-websummary.xlsx
#download.file(url, destfile, mode = "wb")

library(readxl)
library(dplyr)
library(janitor)
# Load the stringr package
library(stringr)

destfile <- "Data/Revenue_ton_miles_V2.xlsx"


url = "https://www.cn.ca/-/media/files/investors/investor-performance-measures/cn-bu-websummary.xlsx"

download.file(url, destfile, mode = "wb")

##2019
df_2019 <- read_excel(destfile,
                      sheet = "2019",
                      skip = 1,
                      n_max = 21)
df_2019 <- tail(df_2019,-12)
df_2019 <- t(df_2019)

df_2019 <- janitor::row_to_names(df_2019, row_number = 1)

df_2019 <- as.data.frame(df_2019) 
df_2019 <- tibble::rownames_to_column(df_2019, "Week")%>% dplyr::mutate("Year" = "2019")



df_2019 = df_2019 %>% tidyr::pivot_longer(!c(Year,Week), names_to = "Industry", values_to = "Revenue")


##2020
df_2020 <- read_excel(destfile,
                      sheet = "2020",
                      skip = 2,
                      n_max = 20)
df_2020 <- df_2020[-c(2)]
df_2020 <- tail(df_2020,-12)
df_2020 <- t(df_2020)

df_2020 <- janitor::row_to_names(df_2020, row_number = 1)

df_2020 <- as.data.frame(df_2020)
df_2020 <- tibble::rownames_to_column(df_2020, "Week")%>% dplyr::mutate("Year" = "2020")



df_2020 = df_2020 %>% tidyr::pivot_longer(!c(Year,Week), names_to = "Industry", values_to = "Revenue")

##2021
df_2021 <- read_excel(destfile,
                      sheet = "2021",
                      skip = 1,
                      n_max = 20)
df_2021 <- tail(df_2021,-12)
df_2021 <- t(df_2021)

df_2021 <- janitor::row_to_names(df_2021, row_number = 1)

df_2021 <- as.data.frame(df_2021)
df_2021 <- tibble::rownames_to_column(df_2021, "Week")%>% dplyr::mutate("Year" = "2021")



df_2021 = df_2021 %>% tidyr::pivot_longer(!c(Year,Week), names_to = "Industry", values_to = "Revenue")

##2022
df_2022 <- read_excel(destfile,
                      sheet = "2022",
                      skip = 1,
                      n_max = 20)
df_2022 <- tail(df_2022,-12)
df_2022 <- t(df_2022)

df_2022 <- janitor::row_to_names(df_2022, row_number = 1)

df_2022 <- as.data.frame(df_2022)
df_2022 <- tibble::rownames_to_column(df_2022, "Week")%>% dplyr::mutate("Year" = "2022")



df_2022 = df_2022 %>% tidyr::pivot_longer(!c(Year,Week), names_to = "Industry", values_to = "Revenue")



##2023
df_2023 <- read_excel(destfile,
                      sheet = "2023",
                      skip = 1,
                      n_max = 20)
df_2023 <- tail(df_2023,-12)
df_2023 <- t(df_2023)

df_2023 <- janitor::row_to_names(df_2023, row_number = 1)

df_2023 <- as.data.frame(df_2023)
df_2023 <- tibble::rownames_to_column(df_2023, "Week")%>% dplyr::mutate("Year" = "2023")



df_2023 = df_2023 %>% tidyr::pivot_longer(!c(Year,Week), names_to = "Industry", values_to = "Revenue")





##2024
df_2024 <- read_excel(destfile,
                      sheet = "2024",
                      skip = 1,
                      n_max = 20)
df_2024 <- tail(df_2024,-12)
df_2024 <- t(df_2024)

df_2024 <- janitor::row_to_names(df_2024, row_number = 1)

df_2024 <- as.data.frame(df_2024)
df_2024 <- tibble::rownames_to_column(df_2024, "Week")%>% dplyr::mutate("Year" = "2024")



df_2024 = df_2024 %>% tidyr::pivot_longer(!c(Year,Week), names_to = "Industry", values_to = "Revenue")



Collected = rbind(df_2019,df_2020,df_2021,df_2022,df_2023,df_2024)

Collected <- Collected %>%
  dplyr::mutate(Week = paste0("Week ", Week))

Collected$Industry <-  str_trim(sub("\\\\.*", "", Collected$Industry), side = "right")
Collected$Industry <- ifelse(Collected$Industry == "RTMs","RTMs TOTAL", Collected$Industry)
Collected$Source <- "CN"




###script for downloading data CPKC

destfile <- "Data/Revenue_ton_miles_V2_CPKC.xlsx"


url = "https://s21.q4cdn.com/736796105/files/doc_downloads/key-metrics/weekly/2024/08/19/CPKC-Weekly-RTMs-and-Carloads-2024.xlsx"

download.file(url, destfile, mode = "wb")


##2020
df_2020 <- read_excel(destfile,
                      sheet = "CPKC 2020",
                      skip = 5,
                      n_max = 13)

df_2020 <- tail(df_2020,-1)
df_2020 <- t(df_2020)

df_2020 <- janitor::row_to_names(df_2020, row_number = 1)

df_2020 <- as.data.frame(df_2020)
df_2020 <- tibble::rownames_to_column(df_2020, "Week")%>% dplyr::mutate("Year" = "2020")



df_2020 = df_2020 %>% tidyr::pivot_longer(!c(Year,Week), names_to = "Industry", values_to = "Revenue")

##2021
df_2021 <- read_excel(destfile,
                      sheet = "CPKC 2021",
                      skip = 5,
                      n_max = 13)
df_2021 <- tail(df_2021,-1)
df_2021 <- t(df_2021)

df_2021 <- janitor::row_to_names(df_2021, row_number = 1)

df_2021 <- as.data.frame(df_2021)
df_2021 <- tibble::rownames_to_column(df_2021, "Week")%>% dplyr::mutate("Year" = "2021")



df_2021 = df_2021 %>% tidyr::pivot_longer(!c(Year,Week), names_to = "Industry", values_to = "Revenue")

##2022
df_2022 <- read_excel(destfile,
                      sheet = "CPKC 2022",
                      skip = 5,
                      n_max = 13)
df_2022 <- tail(df_2022,-1)
df_2022 <- t(df_2022)

df_2022 <- janitor::row_to_names(df_2022, row_number = 1)

df_2022 <- as.data.frame(df_2022)
df_2022 <- tibble::rownames_to_column(df_2022, "Week")%>% dplyr::mutate("Year" = "2022")



df_2022 = df_2022 %>% tidyr::pivot_longer(!c(Year,Week), names_to = "Industry", values_to = "Revenue")



##2023
df_2023 <- read_excel(destfile,
                      sheet = "CPKC 2023",
                      skip = 5,
                      n_max = 13)
df_2023 <- tail(df_2023,-1)
df_2023 <- t(df_2023)

df_2023 <- janitor::row_to_names(df_2023, row_number = 1)

df_2023 <- as.data.frame(df_2023)
df_2023 <- tibble::rownames_to_column(df_2023, "Week")%>% dplyr::mutate("Year" = "2023")



df_2023 = df_2023 %>% tidyr::pivot_longer(!c(Year,Week), names_to = "Industry", values_to = "Revenue")





##2024
df_2024 <- read_excel(destfile,
                      sheet = "CPKC 2024",
                      skip = 5,
                      n_max = 13)
df_2024 <- tail(df_2024,-1)
df_2024 <- t(df_2024)

df_2024 <- janitor::row_to_names(df_2024, row_number = 1)

df_2024 <- as.data.frame(df_2024)
df_2024 <- tibble::rownames_to_column(df_2024, "Week")%>% dplyr::mutate("Year" = "2024")



df_2024 = df_2024 %>% tidyr::pivot_longer(!c(Year,Week), names_to = "Industry", values_to = "Revenue")



Collected_CPKC = rbind(df_2020,df_2021,df_2022,df_2023,df_2024)

Collected_CPKC <- Collected_CPKC %>%
  dplyr::mutate(Week = paste0("Week ", Week))

Collected_CPKC$Industry <-  str_trim(sub("\\\\.*", "", Collected_CPKC$Industry), side = "right")
Collected_CPKC$Industry <- ifelse(Collected_CPKC$Industry == "TOTAL REVENUE TON MILES","RTMs TOTAL", 
                                  ifelse(Collected_CPKC$Industry == "ENERGY, CHEMICALS & PLASTICS","PETRO&CHEMICALS",
                                         ifelse(Collected_CPKC$Industry == "METALS, MINERALS & CONSUMER PRODUCTS","METALS&MINERALS",
                                                ifelse(Collected_CPKC$Industry == "GRAIN","GRAIN & FERTS",
                                                       ifelse(Collected_CPKC$Industry == "FERTILIZER & SULPHUR","GRAIN & FERTS",Collected_CPKC$Industry)))))

Collected_CPKC <- Collected_CPKC %>%
  dplyr::group_by(Week,Year,Industry)%>%
  dplyr::summarise(Revenue =sum(as.numeric(Revenue)))

Collected_CPKC$Source <- "CPKC"
Collected_CPKC <- Collected_CPKC[Collected_CPKC$Industry != "POTASH",]
Collected_CPKC <- Collected_CPKC[Collected_CPKC$Week != "Week 53",]

Collected <- Collected[Collected$Year %in% unique(Collected_CPKC$Year),]
Collected$Revenue <- as.numeric(Collected$Revenue)

Collected <- rbind(Collected_CPKC, Collected)


Collected_grouped <- Collected %>%
  dplyr::group_by(Week,Year,Industry)%>%
  dplyr::summarise(Revenue =sum(as.numeric(Revenue)))
Collected_grouped$Source = "Total"

Collected <- rbind(Collected_grouped, Collected)

Collected <- Collected %>%
  dplyr::mutate(weeknum = as.numeric(substr(Week,6,length(Week))))%>%
  arrange(Source,Year,weeknum,Industry)%>%
  dplyr::select(Week,Year,Industry,Revenue,Source)

write.csv(Collected, file   = paste0("Data/Revenue_ton_mile_V2.csv")  , row.names = F)
