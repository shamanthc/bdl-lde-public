
library(dplyr)
library(stringr)
library(ggplot2)
library(cansim)
library(tidyr)
library(tidyverse)
library(directlabels)
library(ggrepel)

source("expected_to_change_server.R")
source("obstacle_server_newQuarter.R")
source("outlook_server.R")
source("talent_server_newQuarter.R")


server <- function(input, output, session) {
  
  updateTabItems(session, "sidebar", "obstacles_newQ")
  
  observeEvent(input$business_characteristics, {
    if (input$business_characteristics == "All industries") {
      choices <-Industries_clean
      updateSelectInput(inputId = "specific_business_characteristics", choices = choices, selected = "All Industries")
    } else if (input$business_characteristics == "All employment sizes") {
      choices <-Employment_size_clean
      updateSelectInput(inputId = "specific_business_characteristics", choices = choices, selected = "All employment sizes")
    } else if (input$business_characteristics == "All ownerships") {
      choices <- Majority_Ownership_clean
      updateSelectInput(inputId = "specific_business_characteristics", choices = choices, selected = "All ownerships")
      
    } else if (input$business_characteristics == "All visible minorities") {
      choices <- Ownership_minority_clean
      updateSelectInput(inputId = "specific_business_characteristics", choices = choices, selected = "Ownership by all visible minorities")
    } else {
      choices <- Business_activity_clean
      updateSelectInput(inputId = "specific_business_characteristics", choices = choices, selected = "All business activities in the last 12 months")
    }
    
   
  })
  
  expected_to_change_server(input, output, session)
  obstacle_server_newQuarter(input, output, session)
  outlook_server(input, output, session)
  talent_server_newQuarter(input, output, session)


}
