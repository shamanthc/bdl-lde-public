
library(dplyr)
library(stringr)
library(ggplot2)
library(cansim)
library(tidyr)
library(tidyverse)
library(directlabels)
library(ggrepel)
library(plotly)
library(writexl)
source("expected_to_change_server.R")
source("obstacle_server_newQuarter.R")
source("outlook_server.R")
source("talent_server_newQuarter.R")


server <- function(input, output, session) {
  
  updateTabItems(session, "sidebar", "obstacles_newQ")
  
  observeEvent(input$business_characteristics, {
    if (input$business_characteristics %in% Industries_clean) {
      business_characteristics_choices <- "All Industries"
    #  specific_business_characteristics_choices <- 
      #updateSelectInput(inputId = "specific_business_characteristics", choices = choices, selected = "All Industries")
    } else if (input$business_characteristics %in% Employment_size_clean ) {
      business_characteristics_choices <-"All employment sizes"
      #updateSelectInput(inputId = "specific_business_characteristics", choices = choices, selected = "All employment sizes")
    } else if (input$business_characteristics %in% Majority_Ownership_clean) {
      business_characteristics_choices <-  "All ownerships"
      #updateSelectInput(inputId = "specific_business_characteristics", choices = choices, selected = "All ownerships")
      
    } else if (input$business_characteristics %in% Ownership_minority_clean) {
      business_characteristics_choices <- "All visible minorities"
      #updateSelectInput(inputId = "specific_business_characteristics", choices = choices, selected = "Ownership by all visible minorities")
    } else {
      business_characteristics_choices <- "All business activities in the last 12 months"
      #updateSelectInput(inputId = "specific_business_characteristics", choices = choices, selected = )
    }
    
   
  })
  
  

  expected_to_change_server(input, output, session)
  obstacle_server_newQuarter(input, output, session)
  outlook_server(input, output, session)
  talent_server_newQuarter(input, output, session)
  
  

}
