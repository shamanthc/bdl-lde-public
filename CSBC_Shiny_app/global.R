#==========================================================================================================
# 2022 Q2 table for this question: Is your business in a better overall position today than it was in 2019?
#==========================================================================================================

# Installing and Loading Packages

# required_packages <- c("tidyverse","formattable", "ggplot2","keras","MLmetrics", "Shiny","plotly",
#                        "tidyr","knitr","janitor","data.table","tidyxl","jsonline","tidyquant","lubridate","zoo",
#                        "cansim","hrbrthemes","viridis", "ggplot2", "ggcorr", "openxlsx","zoo", "ggExtra",
#                        "stringr", "statcanR","plyr","moonbook", "sf","rgdal","geojsonio","spdplyr","rmapshaper",
#                        "GGally","corrplot", "ggrepel", "tidytext", "heatmapply","ztable","magrittr","mapcan", "readxl", "stringr")
# need_install <- required_packages[!(required_packages) %in% installed.packages()]
# if(length(need_install)>0){
#   install.packages(need_install)
# }
#
# lapply(required_packages, require, character.only = TRUE)



################################################################################################################################################################
library(dplyr)
library(stringr)
library(cansim)
library(tidyr)
library(tidyverse)
library(readxl)


# setwd("path to app folder")
# getwd()

latest_quarter_table_obstacle <- "table Q3.2022"
last_quarter <- "Q2 2022"

#Basic classifications####
options(encoding = 'UTF-8')


Industries <- c("North American Industry Classification System (NAICS), all industries",
                "Agriculture, forestry, fishing and hunting [11]",
                "Mining, quarrying, and oil and gas extraction [21]",
                "Construction [23]"                                  ,
                "Manufacturing [31-33]"                               ,
                "Wholesale trade [41]"                                 ,
                "Retail trade [44-45]"                                  ,
                "Transportation and warehousing [48-49]"                 ,
                "Information and cultural industries [51]"                ,
                "Finance and insurance [52]"                               ,
                "Real estate and rental and leasing [53]"                   ,
                "Professional, scientific and technical services [54]"       ,
                "Administrative and support, waste management and remediation services [56]"  ,
                "Health care and social assistance [62]"                                       ,
                "Arts, entertainment and recreation [71]"                                       ,
                "Accommodation and food services [72]"                                           ,
                "Other services (except public administration) [81]"            )

Industries_clean <-c(
    "All Industries",
    "Agriculture, forestry, fishing",
    "Mining, quarrying, oil, gas extraction",
    "Construction",
    "Manufacturing",
    "Wholesale trade",
    "Retail trade",
    "Transportation, warehousing",
    "Information and culture",
    "Finance and insurance",
    "Real estate, rental, leasing",
    "Professional services",
    "Administration and support services" ,
    "Health care, social assistance",
    "Arts, entertainment, recreation" ,
    "Accomodation and food services" ,
    "Other services"
  )



Industries2 <- c("North American Industry Classification System (NAICS), all industries",
                 "Agriculture, forestry, fishing and hunting",
                 "Mining, quarrying, and oil and gas extraction",
                 "Construction"                                  ,
                 "Manufacturing"                               ,
                 "Wholesale trade"                                 ,
                 "Retail trade"                                  ,
                 "Transportation and warehousing"                 ,
                 "Information and cultural industries"                ,
                 "Finance and insurance"                               ,
                 "Real estate and rental and leasing"                   ,
                 "Professional, scientific and technical services"       ,
                 "Administrative and support, waste management and remediation services"  ,
                 "Health care and social assistance"                                       ,
                 "Arts, entertainment and recreation"                                       ,
                 "Accommodation and food services"                                           ,
                 "Other services (except public administration)"            )


Employment_size <- c("Business or organization size of employment, all employment sizes"  ,
                     "1 to 4 employees"                                                    ,
                     "5 to 19 employees"                                                   ,
                     "20 to 99 employees"                                                   ,
                     "100 or more employees" )
Employment_size_clean <- c("All employment sizes"  ,
                     "1 to 4 employees"                                                    ,
                     "5 to 19 employees"                                                   ,
                     "20 to 99 employees"                                                   ,
                     "100 or more employees" )




Majority_Ownership <- c("Majority ownership, all ownerships",
                        "Majority ownership, woman",
                        "Majority ownership, First Nations, Metis or Inuit",
                        "Majority ownership, immigrant to Canada",
                        "Majority ownership, person with a disability",
                        "Majority ownership, member of LGBTQ2 community")
Majority_Ownership_clean <- c("All ownerships",
                              "Majority woman",
                              "Majority First Nations, Metis or Inuit",
                              "Majority immigrant to Canada",
                              "Majority person with a disability",
                              "Majority member of LGBTQ2")



Ownership_minority <- c("Ownership by visible minority, all visible minorities",
                        "Ownership by visible minority, South Asian",
                        "Ownership by visible minority, Chinese",
                        "Ownership by visible minority, Black",
                        "Ownership by visible minority, Filipino",
                        "Ownership by visible minority, Latin American",
                        "Ownership by visible minority, Arab",
                        "Ownership by visible minority, Southeast Asian",
                        "Ownership by visible minority, West Asian",
                        "Ownership by visible minority, Korean",
                        "Ownership by visible minority, Japanese",
                        "Ownership by visible minority, other visible minority",
                        "Ownership by visible minority, preferred not to say" )

Ownership_minority_clean <- c("Ownership by all visible minorities",
                              "Ownership by South Asian",
                              "Ownership by Chinese",
                              "Ownership by Black",
                              "Ownership by Filipino",
                              "Ownership by Latin American",
                              "Ownership by Arab",
                              "Ownership by Southeast Asian",
                              "Ownership by West Asian",
                              "Ownership by Korean",
                              "Ownership by Japanese",
                              "Ownership by other visible minority",
                              "Ownership by preferred not to say" )



Business_activity <- c("Business or organization activity in the last 12 months, all business or organization activities",
                       "Exported goods outside of Canada",
                       "Exported services outside of Canada",
                       "Made investments outside of Canada",
                       "Sold goods to businesses in Canada who then resold them outside of Canada",
                       "Imported goods from outside of Canada",
                       "Imported services from outside of Canada",
                       "Relocated any business or organizational activities or employees from another country into Canada",
                       "Engaged in other international business activities",
                       "Business or organization activity, none or other")


Business_activity_clean <- c("All business activities in the last 12 months",
                       "Exported goods outside of Canada",
                       "Exported services outside of Canada",
                       "Made investments outside of Canada",
                       "Sold goods to businesses in Canada who then resold them outside of Canada",
                       "Imported goods from outside of Canada",
                       "Imported services from outside of Canada",
                       "Relocated any business activities or employees from another country into Canada",
                       "Engaged in other international business activities",
                       "Business or organization activity, none or other")


Business_characterisitcs_mapping = data.frame(Business_characteristics = c(Industries, Employment_size,Majority_Ownership,Ownership_minority,Business_activity),
                                              Clean=c(Industries_clean,Employment_size_clean,Majority_Ownership_clean,Ownership_minority_clean,Business_activity_clean))


Geography <- c("Canada", "Newfoundland and Labrador", "Prince Edward Island",  "Nova Scotia",
               "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan", "Alberta", "British Columbia", "Yukon", "Northwest Territories",
               "Nunavut")



round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

Data_cleaning_function <-  function(data){


  names(data) <- str_replace_all(names(data), " ", "_")
  names(data) <- str_replace_all(names(data), ",", "")
  names(data) <- str_replace_all(names(data), "-", "_")
  output <- data %>%  dplyr::ungroup() %>%
    dplyr::select(-c("REF_DATE","DGUID","UOM","UOM_ID","SCALAR_FACTOR","SCALAR_ID","VECTOR",
                     "COORDINATE","val_norm","STATUS","SYMBOL","TERMINATED","DECIMALS","GeoUID",
                     "Hierarchy_for_GEO", "Date")) %>%
    dplyr::select(-contains(c("Hierarchy","Classification")))%>%
    mutate(VALUE=round2(VALUE,0))%>%
    replace_na(list(VALUE = 0))
  return(output)
}



Data_cleaning_function_round1 <-  function(data){
  
  
  names(data) <- str_replace_all(names(data), " ", "_")
  names(data) <- str_replace_all(names(data), ",", "")
  names(data) <- str_replace_all(names(data), "-", "_")
  output <- data %>%  dplyr::ungroup() %>%
    dplyr::select(-c("REF_DATE","DGUID","UOM","UOM_ID","SCALAR_FACTOR","SCALAR_ID","VECTOR",
                     "COORDINATE","val_norm","STATUS","SYMBOL","TERMINATED","DECIMALS","GeoUID",
                     "Hierarchy_for_GEO", "Date")) %>%
    dplyr::select(-contains(c("Hierarchy","Classification")))%>%
    mutate(VALUE=round(VALUE,1))%>%
    replace_na(list(VALUE = 0))
  return(output)
}



Canada_data_filter <- function(data){
  output <- data %>%   dplyr::ungroup() %>%
    dplyr::filter(GEO == "Canada")
  return(output)
}

lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


# list of obstacle
#==================

# Get Q3_2022
Slide12_Q3_22 <- cansim::get_cansim("3310053401") %>% 
  dplyr::rename("Obstacles" =`Obstacles for the business or organization`)
# Get Q2_2022
Slide12_Q2_22 <- cansim::get_cansim("3310050401") %>% 
  dplyr::rename("Obstacles" =`Obstacles for the business or organization`)


Slide12_Q2_22_clean = Data_cleaning_function_round1(Slide12_Q2_22)
Slide12_Q3_22_clean = Data_cleaning_function_round1(Slide12_Q3_22)

Slide12_Q2_22_can = Slide12_Q2_22_clean %>%
  dplyr::filter(Business_characteristics == "North American Industry Classification System (NAICS), all industries",
                GEO == "Canada")
Slide12_Q3_22_can = Slide12_Q3_22_clean %>%
  dplyr::filter(Business_characteristics == "North American Industry Classification System (NAICS), all industries",
                GEO == "Canada")

Required_dataset_12 <- dplyr::left_join(x = Slide12_Q3_22_can, y= Slide12_Q2_22_can, 
                                        by= c("Obstacles","GEO","Business_characteristics"))%>%
  dplyr::mutate(Obstacles = case_when(
    Obstacles == "Cost of personal protective equipment (PPE), additional cleaning or implementing distancing requirements" ~ "Cost of personal protective equipment (PPE)",
    TRUE ~ as.character(Obstacles)
    
  ))



Costs = c("Rising cost of inputs",                                         
          "Cost of insurance",                                              
          "Transportation costs",
          "Rising costs in real estate, leasing or property taxes",
          "Rising interest rates and debt costs")
Labour = c("Shortage of labour force",                                       
           "Recruiting skilled employees",                                  
           "Retaining skilled employees")
Supply_chain = c("Difficulty acquiring inputs, products or supplies domestically", 
                 "Difficulty acquiring inputs, products or supplies from abroad", 
                 "Maintaining inventory levels")
Customer_demand = c("Insufficient demand for goods or services offered",             
                    "Fluctuations in consumer demand",                                
                    "Attracting new or returning customers")
Operations = c("Obtaining financing",                                            
               "Maintaining sufficient cash flow or managing debt")              
Finance = c("Increasing competition")

Inflation = c("Rising inflation")



data_obstacle <- Required_dataset_12 %>%
  dplyr::mutate( Percent_change = VALUE.x -VALUE.y) %>%
  dplyr::rename("VALUE_NOW" = "VALUE.x") %>% 
  dplyr::rename("VALUE_THEN" = "VALUE.y") %>% 
  dplyr::mutate(Categories = "NONE",
                Categories = ifelse(Obstacles %in% Costs, "Costs", Categories),
                Categories = ifelse(Obstacles %in% Labour, "Labour", Categories),
                Categories = ifelse(Obstacles %in% Supply_chain, "Supply chain", Categories),
                Categories = ifelse(Obstacles %in% Customer_demand, "Customer demand", Categories),
                Categories = ifelse(Obstacles %in% Operations, "Operations", Categories),
                Categories = ifelse(Obstacles %in% Finance, "Finance", Categories),
                Categories = ifelse(Obstacles %in% Inflation, "Inflation", Categories),
                Percent_change1 = round2(Percent_change,0), 
                Percent_change = ifelse(is.na(Percent_change1), "NA", as.character(Percent_change1)),
                color_perc1 = ifelse(as.integer(Percent_change1)>0, "red", ifelse(as.integer(Percent_change1)<0, "green",  "none")),
                color_perc = ifelse(Percent_change=="NA", "none", as.character(color_perc1)),
                VALUE_NOW=round2(VALUE_NOW,0)
  )%>% 
  dplyr::filter(Categories!= "NONE", VALUE_NOW>=19)
list_obstacles <- data_obstacle$Obstacles
