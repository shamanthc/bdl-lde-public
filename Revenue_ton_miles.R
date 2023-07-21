#destfile <- "Data/Revenue_ton_miles.xlsx"
#
#url = "https://www.cn.ca/-/media/Files/Investors/Investor-Performance-Measures/CN-BU-WEBSUMMARYpv.xlsx"
#download.file(url, destfile, mode = "wb")

library(readxl)
library(dplyr)
library(janitor)

destfile <- "Data/Revenue_ton_miles_V2.xlsx"


url = "https://www.cn.ca/-/media/Files/Investors/Investor-Performance-Measures/CN-BU-WEBSUMMARYpv.xlsx"

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




Collected = rbind(df_2019,df_2020,df_2021,df_2022,df_2023)

#savepath2 = "C:/Users/dsingh/Files to save"
write.csv(Collected, file   = paste0("Data/Revenue_ton_mile_V2.csv")  , row.names = F)
