library(rvest)
library(tidyr)
library(dplyr)
library(janitor)

webpage <- read_html("https://www.catsa-acsta.gc.ca/en/screened-passenger-data")

tbls <- html_nodes(webpage, "table")

# empty list to add table data to
#=================================
tbls2_ls <- list()

# scrape table
#==============
tbls2_ls$Table1 <- webpage %>%
  html_nodes("table") %>% 
  html_table(fill = TRUE) %>%
  .[[1]]

# data preperation 
#===================
dt <- data.frame(tbls2_ls[[1]])

dt <- dt[-1,]

t_dt = as.data.frame(t(dt))

t_dt <- t_dt %>%
  tidyr::unite("Airports_Year", "2":"3")

dt1 = as.data.frame(t(t_dt))

dt1 <- dt1 %>%
  janitor::row_to_names(row_number = 1)


colnames(dt1)[1] = "Date"
dt1 <- dt1  %>%  tidyr::pivot_longer(!Date, names_to = "Airport_details", values_to = "count")


dt2 <- dt1%>%mutate(month=month(Date), day = day(Date))%>%
  separate(Airport_details, c("Airport", "year"), "_")%>%
  mutate(Date_new = make_date(year, month, day),
         Airport_details = paste0(Airport, "_", year))%>%
  select(date= Date_new, Airport_details, count)


write.csv(dt2, file   = paste0("Data/Screened Passengers.csv")  , row.names = F)
