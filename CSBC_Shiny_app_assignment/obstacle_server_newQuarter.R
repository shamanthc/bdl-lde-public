
obstacle_server_newQuarter <- function(input, output, session) {
  
  
  # Get Q4_2022
  Slide12_Q4_22 <- cansim::get_cansim("3310060301") %>% 
    dplyr::rename("Obstacles" =`Obstacles for the business or organization`)
  # Get Q3_2022
  Slide12_Q3_22 <- cansim::get_cansim("3310053401") %>% 
    dplyr::rename("Obstacles" =`Obstacles for the business or organization`)
  
  
  
  output$obstacle1_newQ <- renderPlot({
    
    last_quarter <- "2022 Q3"
    
    
    # # Get Q4_2022
    # Slide12_Q4_22 <- cansim::get_cansim("3310060301") %>% 
    #   dplyr::rename("Obstacles" =`Obstacles for the business or organization`)
    # # Get Q3_2022
    # Slide12_Q3_22 <- cansim::get_cansim("3310053401") %>% 
    #   dplyr::rename("Obstacles" =`Obstacles for the business or organization`)
    # 
    
    Slide12_Q3_22_clean = Data_cleaning_function_round1(Slide12_Q3_22)
    Slide12_Q4_22_clean = Data_cleaning_function_round1(Slide12_Q4_22)
    
    Slide12_Q3_22_can = Slide12_Q3_22_clean %>%
      left_join(Business_characterisitcs_mapping, by="Business_characteristics")%>%
      dplyr::filter(Clean == input$business_characteristics,
                    GEO == input$geo)
    Slide12_Q4_22_can = Slide12_Q4_22_clean %>%
      left_join(Business_characterisitcs_mapping, by="Business_characteristics")%>%
      dplyr::filter(Clean == input$business_characteristics,
                    GEO == input$geo)
    
    Required_dataset_12 <- dplyr::left_join(x = Slide12_Q4_22_can, y= Slide12_Q3_22_can, 
                                            by= c("Obstacles","GEO","Business_characteristics")) 
    
    
    Costs = c( "Rising cost of inputs",                                         
               "Cost of insurance",                                              
               "Transportation costs",
               "Rising interest rates and debt costs",
               "Rising costs in real estate, leasing or property taxes",
               "Rising inflation")
    
    Labour = c("Shortage of labour force",                                       
               "Recruiting skilled employees",                                  
               "Retaining skilled employees")
    
    Supply_chain = c("Difficulty acquiring inputs, products or supplies domestically",
                     "Difficulty acquiring inputs, products or supplies from abroad",
                     "Difficulty acquiring inputs, products or supplies from within Canada",
                     "Maintaining inventory levels")
    
    Customer_demand = c("Insufficient demand for goods or services offered",             
                        "Fluctuations in consumer demand",                                
                        "Attracting new or returning customers")
    
    Operations = c("Obtaining financing",                                            
                   "Maintaining sufficient cash flow or managing debt")   
    
    Finance = c("Increasing competition")
    
    
    
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
                    Percent_change1 = round2(Percent_change,0), 
                    Percent_change = ifelse(is.na(Percent_change1), "NA", as.character(Percent_change1)),
                    color_perc1 = ifelse(as.integer(Percent_change1)>0, "red", ifelse(as.integer(Percent_change1)<0, "green",  "#4F4E4E")),
                    color_perc = ifelse(Percent_change=="NA", "#4F4E4E", as.character(color_perc1)),
                    VALUE_NOW=round2(VALUE_NOW,0)
      ) %>% 
      dplyr::filter(Categories!= "NONE")%>%
      arrange(-VALUE_NOW)%>%
      head(10)
    
    nb <- length(unique(data_obstacle$Obstacles))
    
    this_family <- "sans"
    max_value <- as.integer(max(data_obstacle$VALUE_NOW)+0.1*max(data_obstacle$VALUE_NOW))
    
    ggplot(data_obstacle, aes(fill = reorder(Categories, -VALUE_NOW), y = VALUE_NOW,
                              x=reorder(Obstacles, +VALUE_NOW), 
                              label=paste0(VALUE_NOW,"%"))) +
      geom_bar(stat="identity")+
      scale_fill_manual("Categories",values = c("#00B9B4", "#F8A12C", "#FE4812","#4F4E4E", "#0C3163", "#005753","#B8E4F1","#DCDFDA"))+
      geom_text(position = position_dodge(1),hjust=-0.2, color="#4F4E4E")+
      expand_limits(y= c(1, max_value),x= c(1, nb+2))+
      geom_text(aes(label= ifelse(color_perc == "red", paste0("+",Percent_change,"%"), ifelse(Percent_change=="NA", paste0(Percent_change),  paste0(Percent_change,"%"))), color= color_perc),
                hjust=-0.1,  
                size=3, position = position_fill(vjust = -4, reverse=TRUE))+
      annotate(geom="text", label=paste("Change since\n ",  "last survey"), x= nb+1, y=-7.5, color="#4F4E4E",  hjust=0, 
               family="sans", size=3) +
      scale_color_manual(values = c('red' = 'red', 'green' = 'green3', 'none' = '#4F4E4E'), guide = "none")+
      coord_flip()+
      ylab("") + 
      xlab("")+
      labs(title = paste0(input$geo, ": Top 10 business Obstacles expected, next three month"),
           subtitle = "% of respondents, by obstacle",
           caption = paste("*Top 10 obstacles. \n\nSources: Canadian Chamber of Commerce Business Data Lab; Statistics Canada, Canadian Survey on Business Conditions.")
      )+
      theme_minimal()+
      theme(
        axis.text.y = element_text( size=11, margin=margin(r=-10), color= "#4F4E4E", family = this_family),
        axis.text.x = element_blank(),
        
        plot.title    = element_text(size = 14, face="bold", family = this_family, color="#4F4E4E"),
        plot.subtitle = element_text(size = 12, color="gray58", family = this_family),
        
        plot.caption          = element_text(hjust = 0, color="#4F4E4E"),
        plot.title.position   = "plot", 
        plot.caption.position = "plot",
        
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        
        
        legend.text  =element_text(family = this_family, face="bold", color="#4F4E4E") ,
        legend.title = element_text(family = this_family, face="bold", color="#4F4E4E"),
        
        legend.position = c(.9, .1),
        legend.box      ="vertical"
      )
    
  })
  
  
  data_on_obstacle1 <- reactive({
    
    # data on obstacle for Q4 2022 simulation
    #=========================================
    # Slide12_Q4_22 <-cansim::get_cansim("3310060301") %>% 
    #   dplyr::rename("Obstacles" =`Obstacles for the business or organization`)
    # 
    
    Slide12_Q4_22_clean <- Data_cleaning_function_round1(Slide12_Q4_22)
    
    if(business_characteristics_choices == "All Industries"){
      Slide12_Q4_22_can <- Slide12_Q4_22_clean %>%
        dplyr::filter(Obstacles == input$obstacles,
                      Business_characteristics %in% Industries,
                      GEO == input$geo)%>%
        mutate(Business_characteristics = case_when( 
          Business_characteristics == "North American Industry Classification System (NAICS), all industries"       ~ "All Industries",
          Business_characteristics == "Agriculture, forestry, fishing and hunting [11]"                             ~ "Agriculture, forestry, fishing",
          Business_characteristics == "Mining, quarrying, and oil and gas extraction [21]"                          ~ "Mining, oil, gas extraction",
          Business_characteristics == "Construction [23]"                                                           ~ "Construction",
          Business_characteristics == "Manufacturing [31-33]"                                                       ~ "Manufacturing",
          Business_characteristics == "Wholesale trade [41]"                                                        ~ "Wholesale trade",
          Business_characteristics == "Retail trade [44-45]"                                                        ~ "Retail trade",
          Business_characteristics == "Transportation and warehousing [48-49]"                                      ~ "Transportation, warehousing",
          Business_characteristics == "Information and cultural industries [51]"                                    ~ "Information and culture",
          Business_characteristics == "Finance and insurance [52]"                                                  ~ "Finance and insurance",
          Business_characteristics == "Real estate and rental and leasing [53]"                                     ~ "Real estate, rental, leasing",
          Business_characteristics == "Professional, scientific and technical services [54]"                        ~ "Professional services",
          Business_characteristics == "Administrative and support, waste management and remediation services [56]"  ~ "Administration and support services" ,
          Business_characteristics == "Health care and social assistance [62]"                                      ~ "Health care, social assistance",
          Business_characteristics == "Arts, entertainment and recreation [71]"                                     ~ "Arts, entertainment, recreation" ,
          Business_characteristics == "Accommodation and food services [72]"                                        ~ "Accommodation and food services" ,
          Business_characteristics == "Other services (except public administration) [81]"                          ~ "Other services",
          TRUE ~ "Error"))%>%
        arrange(VALUE)
    }else if(business_characteristics_choices == "All employment sizes"){
      
      Slide12_Q4_22_can <- Slide12_Q4_22_clean %>%
        dplyr::filter(Obstacles == input$obstacles,
                      Business_characteristics %in% Employment_size,
                      GEO == input$geo)%>%
        mutate(Business_characteristics = case_when( 
          Business_characteristics == "Business or organization size of employment, all employment sizes"  ~ "All employment sizes",
          TRUE ~ as.character(Business_characteristics)))%>%
        arrange(VALUE)
      
    }else if(business_characteristics_choices == "All ownerships"){
      Slide12_Q4_22_can <- Slide12_Q4_22_clean %>%
        dplyr::filter(Obstacles == input$obstacles,
                      Business_characteristics %in% Majority_Ownership,
                      GEO == input$geo)%>%
        mutate(Business_characteristics = case_when( 
          Business_characteristics == "Majority ownership, all ownerships"  ~ "All ownerships",
          Business_characteristics == "Majority ownership, woman"  ~ "Majority woman",
          Business_characteristics == "Majority ownership, First Nations, Metis or Inuit"  ~ "Majority First Nations, Metis or Inuit",
          Business_characteristics == "Majority ownership, immigrant to Canada"  ~ "Majority immigrant to Canada",
          Business_characteristics == "Majority ownership, person with a disability"  ~ "Majority person with a disability",
          Business_characteristics == "Majority ownership, member of LGBTQ2 community"  ~ "Majority member of LGBTQ2",
          TRUE ~ "Error"))%>%
        arrange(VALUE)
      
      
    }else if(business_characteristics_choices == "All visible minorities"){
      
      Slide12_Q4_22_can <- Slide12_Q4_22_clean %>%
        dplyr::filter(Obstacles == input$obstacles,
                      Business_characteristics %in% Ownership_minority ,
                      GEO == input$geo)%>%
        mutate(Business_characteristics = case_when( 
          Business_characteristics == "Ownership by visible minority, all visible minorities"  ~ "Ownership by all visible minorities",
          Business_characteristics == "Ownership by visible minority, South Asian"  ~ "Ownership by South Asian",
          Business_characteristics == "Ownership by visible minority, Chinese"  ~ "Ownership by Chinese",
          Business_characteristics == "Ownership by visible minority, Black"  ~ "Ownership by Black",
          Business_characteristics == "Ownership by visible minority, Filipino"  ~ "Ownership by Filipino",
          Business_characteristics == "Ownership by visible minority, Latin American"  ~ "Ownership by Latin American",
          Business_characteristics == "Ownership by visible minority, Arab"  ~ "Ownership by Arab",
          Business_characteristics == "Ownership by visible minority, Southeast Asian"  ~ "Ownership by Southeast Asian",
          Business_characteristics == "Ownership by visible minority, West Asian"  ~ "Ownership by West Asian",
          Business_characteristics == "Ownership by visible minority, Korean"  ~ "Ownership by Korean",
          Business_characteristics == "Ownership by visible minority, Japanese"  ~ "Ownership by Japanese",
          Business_characteristics == "Ownership by visible minority, other visible minority"  ~ "Ownership by other visible minority",
          Business_characteristics == "Ownership by visible minority, preferred not to say"  ~ "Ownership by preferred not to say",
          TRUE ~ "Error"))%>%
        arrange(VALUE)
    }else{
      Slide12_Q4_22_can <- Slide12_Q4_22_clean %>%
        dplyr::filter(Obstacles == input$obstacles,
                      Business_characteristics %in% Business_activity  ,
                      GEO == input$geo)%>%
        mutate(Business_characteristics = case_when( 
          Business_characteristics == "Business or organization activity in the last 12 months, all business or organization activities"  ~ "All business activities in the last 12 months",
          Business_characteristics == "Sold goods to businesses in Canada who then resold them outside of Canada"  ~ paste("Sold goods to businesses in Canada who \nthen resold them outside of Canada"),
          Business_characteristics == "Relocated any business or organizational activities or employees from another country into Canada"  ~ paste("Relocated any business activities or \nemployees from another country into Canada"),
          TRUE ~ as.character(Business_characteristics)))%>%
        arrange(VALUE)
      
    }
    
    Slide12_Q4_22_can%>%
      mutate(VALUE=round(VALUE,0))
    
    
  })
  
  
  
  data_on_obstacle2 <- reactive({
    
    # data on obstacle for Q4 2022 simulation
    #=========================================
    
    # Slide12_Q4_22 <-cansim::get_cansim("3310060301") %>% 
    #   dplyr::rename("Obstacles" =`Obstacles for the business or organization`)
    # 
    # 
    
    Slide12_Q4_22_clean <- Data_cleaning_function_round1(Slide12_Q4_22)
    
    if(business_characteristics_choices == "All Industries"){
      Slide12_Q4_22_can <- Slide12_Q4_22_clean %>%
        dplyr::filter(Obstacles == input$obstacles,
                      Business_characteristics %in% Industries)%>%
        mutate(Business_characteristics = case_when( 
          Business_characteristics == "North American Industry Classification System (NAICS), all industries"       ~ "All Industries",
          Business_characteristics == "Agriculture, forestry, fishing and hunting [11]"                             ~ "Agriculture, forestry, fishing",
          Business_characteristics == "Mining, quarrying, and oil and gas extraction [21]"                          ~ "Mining, oil, gas extraction",
          Business_characteristics == "Construction [23]"                                                           ~ "Construction",
          Business_characteristics == "Manufacturing [31-33]"                                                       ~ "Manufacturing",
          Business_characteristics == "Wholesale trade [41]"                                                        ~ "Wholesale trade",
          Business_characteristics == "Retail trade [44-45]"                                                        ~ "Retail trade",
          Business_characteristics == "Transportation and warehousing [48-49]"                                      ~ "Transportation, warehousing",
          Business_characteristics == "Information and cultural industries [51]"                                    ~ "Information and culture",
          Business_characteristics == "Finance and insurance [52]"                                                  ~ "Finance and insurance",
          Business_characteristics == "Real estate and rental and leasing [53]"                                     ~ "Real estate, rental, leasing",
          Business_characteristics == "Professional, scientific and technical services [54]"                        ~ "Professional services",
          Business_characteristics == "Administrative and support, waste management and remediation services [56]"  ~ "Administration and support services" ,
          Business_characteristics == "Health care and social assistance [62]"                                      ~ "Health care, social assistance",
          Business_characteristics == "Arts, entertainment and recreation [71]"                                     ~ "Arts, entertainment, recreation" ,
          Business_characteristics == "Accommodation and food services [72]"                                        ~ "Accommodation and food services" ,
          Business_characteristics == "Other services (except public administration) [81]"                          ~ "Other services",
          TRUE ~ "Error"))%>%
        arrange(VALUE)
    }else if(business_characteristics_choices == "All employment sizes"){
      
      Slide12_Q4_22_can <- Slide12_Q4_22_clean %>%
        dplyr::filter(Obstacles == input$obstacles,
                      Business_characteristics %in% Employment_size)%>%
        mutate(Business_characteristics = case_when( 
          Business_characteristics == "Business or organization size of employment, all employment sizes"  ~ "All employment sizes",
          TRUE ~ as.character(Business_characteristics)))%>%
        arrange(VALUE)
      
    }else if(business_characteristics_choices == "All ownerships"){
      Slide12_Q4_22_can <- Slide12_Q4_22_clean %>%
        dplyr::filter(Obstacles == input$obstacles,
                      Business_characteristics %in% Majority_Ownership)%>%
        mutate(Business_characteristics = case_when( 
          Business_characteristics == "Majority ownership, all ownerships"  ~ "All ownerships",
          Business_characteristics == "Majority ownership, woman"  ~ "Majority woman",
          Business_characteristics == "Majority ownership, First Nations, Metis or Inuit"  ~ "Majority First Nations, Metis or Inuit",
          Business_characteristics == "Majority ownership, immigrant to Canada"  ~ "Majority immigrant to Canada",
          Business_characteristics == "Majority ownership, person with a disability"  ~ "Majority person with a disability",
          Business_characteristics == "Majority ownership, member of LGBTQ2 community"  ~ "Majority member of LGBTQ2",
          TRUE ~ "Error"))%>%
        arrange(VALUE)
      
      
    }else if(business_characteristics_choices == "All visible minorities"){
      
      Slide12_Q4_22_can <- Slide12_Q4_22_clean %>%
        dplyr::filter(Obstacles == input$obstacles,
                      Business_characteristics %in% Ownership_minority)%>%
        mutate(Business_characteristics = case_when( 
          Business_characteristics == "Ownership by visible minority, all visible minorities"  ~ "Ownership by all visible minorities",
          Business_characteristics == "Ownership by visible minority, South Asian"  ~ "Ownership by South Asian",
          Business_characteristics == "Ownership by visible minority, Chinese"  ~ "Ownership by Chinese",
          Business_characteristics == "Ownership by visible minority, Black"  ~ "Ownership by Black",
          Business_characteristics == "Ownership by visible minority, Filipino"  ~ "Ownership by Filipino",
          Business_characteristics == "Ownership by visible minority, Latin American"  ~ "Ownership by Latin American",
          Business_characteristics == "Ownership by visible minority, Arab"  ~ "Ownership by Arab",
          Business_characteristics == "Ownership by visible minority, Southeast Asian"  ~ "Ownership by Southeast Asian",
          Business_characteristics == "Ownership by visible minority, West Asian"  ~ "Ownership by West Asian",
          Business_characteristics == "Ownership by visible minority, Korean"  ~ "Ownership by Korean",
          Business_characteristics == "Ownership by visible minority, Japanese"  ~ "Ownership by Japanese",
          Business_characteristics == "Ownership by visible minority, other visible minority"  ~ "Ownership by other visible minority",
          Business_characteristics == "Ownership by visible minority, preferred not to say"  ~ "Ownership by preferred not to say",
          TRUE ~ "Error"))%>%
        arrange(VALUE)
    }else{
      Slide12_Q4_22_can <- Slide12_Q4_22_clean %>%
        dplyr::filter(Obstacles == input$obstacles,
                      Business_characteristics %in% Business_activity)%>%
        mutate(Business_characteristics = case_when( 
          Business_characteristics == "Business or organization activity in the last 12 months, all business or organization activities"  ~ "All business activities in the last 12 months",
          Business_characteristics == "Sold goods to businesses in Canada who then resold them outside of Canada"  ~ paste("Sold goods to businesses in Canada who \nthen resold them outside of Canada"),
          Business_characteristics == "Relocated any business or organizational activities or employees from another country into Canada"  ~ paste("Relocated any business activities or \nemployees from another country into Canada"),
          TRUE ~ as.character(Business_characteristics)))%>%
        arrange(VALUE)
      
    }
    
    Slide12_Q4_22_can%>%
      mutate(VALUE=round(VALUE,0))
    
    
  })
  
  
  
  
  
  output$obstacle2_newQ <- renderPlot({
    
    thisData <- data_on_obstacle1()%>%
      filter(VALUE>=10)
    #   start_time <- Sys.time()
    #   end_time <- Sys.time()
    # View(start_time - end_time)
    if(business_characteristics_choices == "All Industries"){
      fac_order <- thisData %>%
        filter(
          Business_characteristics != "All Industries") %>%
        arrange(VALUE) %>%
        pull(Business_characteristics)
      
      fac_order <- c(fac_order, "All Industries")
      
      this_face   <- ifelse(fac_order  == "All Industries", "bold", "plain")
      this_color  <- ifelse(fac_order  == "All Industries", "#4F4E4E", "gray25")
      
      business_char <- "by industry"
      
    }else if(business_characteristics_choices == "All employment sizes"){
      fac_order <- c("1 to 4 employees" ,"5 to 19 employees","20 to 99 employees","100 or more employees", "All employment sizes")
      this_face   <- ifelse(fac_order  == "All employment sizes", "bold", "plain")
      this_color  <- ifelse(fac_order  == "All employment sizes", "#4F4E4E", "gray25")
      business_char <- "by employment size"
      
    }else if(business_characteristics_choices == "All ownerships"){
      fac_order <- thisData %>%
        filter(Business_characteristics != "All ownerships") %>%
        arrange(VALUE) %>%
        pull(Business_characteristics)
      
      fac_order <- c(fac_order, "All ownerships")
      
      this_face   <- ifelse(fac_order  == "All ownerships", "bold", "plain")
      this_color  <- ifelse(fac_order  == "All ownerships", "#4F4E4E", "gray25")
      
      business_char <- "by majority owned"
      
    }else if(business_characteristics_choices == "All visible minorities"){
      fac_order <- thisData %>%
        filter(Business_characteristics != "Ownership by all visible minorities") %>%
        arrange(VALUE) %>%
        pull(Business_characteristics)
      
      
      fac_order <- c(fac_order, "Ownership by all visible minorities")                                             
      
      
      this_face   <- ifelse(fac_order  == "Ownership by all visible minorities", "bold", "plain")
      this_color  <- ifelse(fac_order  == "Ownership by all visible minorities", "#4F4E4E", "gray25")
      business_char <- "by visible minority"
      
    }else{
      fac_order <- thisData %>%
        filter( Business_characteristics != "All business activities in the last 12 months") %>%
        arrange(VALUE) %>%
        pull(Business_characteristics)
      
      
      fac_order <- c(fac_order, "All business activities in the last 12 months")                                             
      
      
      this_face   <- ifelse(fac_order  == "All business activities in the last 12 months", "bold", "plain")
      this_color  <- ifelse(fac_order  == "All business activities in the last 12 months", "black", "gray25")
      business_char <- "by all business activities"
      
    }
    
    this_family <- "sans"
    
    max_value <- as.integer(max(thisData$VALUE)+0.1*max(thisData$VALUE))
    
    ggplot(thisData , aes(y=VALUE, x=factor(Business_characteristics, levels = fac_order))) +
      geom_bar(stat="identity", fill = "#00B9B4", width=0.6, colour="#4F4E4E")+
      geom_text(aes(label = paste0(VALUE,"%")), colour = "#4F4E4E", hjust=-0.1)+
      coord_flip()+
      expand_limits(y= c(1, max_value))+
      ylab("") + 
      xlab("")+
      labs(title = paste0(input$geo, ": Expected obstacles, next three months" ),
           subtitle = paste( "% of respondents with",tolower(input$obstacles), "as an obstacle"),
           caption = paste("*",business_characteristics_choices, "by less than 10% of respondents are not reported. \n\nSources: Canadian Chamber of Commerce Business Data Lab; Statistics Canada, Canadian Survey on Business Conditions."))+
      theme_minimal()+
      theme(
        axis.text.y = element_text(face = this_face, size=11,  color= "#4F4E4E", family = this_family),
        axis.text.x = element_blank(),
        
        plot.title    = element_text(size = 14, face="bold", family = this_family, color="#4F4E4E"),
        plot.subtitle = element_text(size = 14, color="gray58", family = this_family),
        
        plot.caption          = element_text(hjust = 0),
        plot.title.position   = "plot", 
        plot.caption.position = "plot",
        
        
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        
        
        legend.text  =element_text(family = this_family, face="bold", color="#4F4E4E") ,
        legend.title = element_text(family = this_family, face="bold", color="#4F4E4E"),
        
        legend.position = c(.9, .1),
        legend.box      ="vertical"
      )
    
  })
  
  
  
  
  output$obstacle3_newQ <- renderPlot({
    
    thisData <- data_on_obstacle2()
    
    if(business_characteristics_choices == "All Industries"){
      thisData <- thisData%>%
        filter(Business_characteristics == "All Industries")%>%
        mutate(fill_color= ifelse(GEO=="Canada", "yes", "no"))
      
      if(input$geo != "Canada"){thisData [thisData $GEO == input$geo,]$fill_color<-  "other"}
      
      business_char <- "all industries"
      
    }else if(business_characteristics_choices == "All employment sizes"){
      thisData <- thisData%>%filter(Business_characteristics == "All employment sizes")%>%
        mutate( fill_color= ifelse(GEO=="Canada", "yes", "no"))
      
      business_char <- "all employment sizes"
      if(input$geo != "Canada"){thisData [thisData $GEO == input$geo,]$fill_color<-  "other"}
      
    }else if(business_characteristics_choices == "All ownerships"){
      thisData <- thisData%>%filter(Business_characteristics == "All ownerships")%>%
        mutate( fill_color= ifelse(GEO=="Canada", "yes", "no"))
      
      business_char <- "all ownerships"
      if(input$geo != "Canada"){thisData [thisData $GEO == input$geo,]$fill_color<-  "other"}
      
    }else if(business_characteristics_choices == "All visible minorities"){
      thisData <- thisData%>%filter(Business_characteristics == "Ownership by all visible minorities")%>%
        mutate( fill_color= ifelse(GEO=="Canada", "yes", "no"))
      
      business_char <- "ownership by all visible minorities"
      if(input$geo != "Canada"){thisData [thisData $GEO == input$geo,]$fill_color<-  "other"}
      
    }else {
      thisData <- thisData%>%filter(Business_characteristics == "All business activities in the last 12 months")%>%
        mutate( fill_color= ifelse(GEO=="Canada", "yes", "no"))
      
      business_char <- "all business activities"
      if(input$geo != "Canada"){thisData [thisData $GEO == input$geo,]$fill_color<-  "other"}
      
    }
    
    
    
    fac_order <- thisData  %>%
      arrange(VALUE) %>%
      pull(GEO)
    
    
    this_face   <- ifelse(fac_order  == "Canada", "bold", "plain")
    this_color  <- ifelse(fac_order  == "Canada", "#4F4E4E", "gray25")
    
    
    this_family <- "sans"
    
    max_value <- as.integer(max(thisData$VALUE)+0.1*max(thisData$VALUE))
    
    ggplot(thisData, aes(y=VALUE, x=factor(GEO, levels = fac_order), fill=fill_color)) +
      geom_bar(stat="identity",  width=0.6, colour="#4F4E4E")+
      geom_text(aes(label = paste0(VALUE,"%")), colour = "#4F4E4E", hjust=-0.1)+
      scale_fill_manual(values = c('no'="#00B9B4", 'yes'="#0C3163", 'other'= "#F8A12C"))+
      coord_flip()+
      expand_limits(y= c(1, max_value))+
      ylab("") + 
      xlab("")+
      labs(title = paste("Over the next three months, which of the following are expected to be obstacles for your business in",input$geo , "?" ),
           subtitle = paste0("% of respondents that identify ",tolower(input$obstacles), " as an obstacle for ", business_char,", by province"),
           caption = paste("Sources: Canadian Chamber of Commerce Business Data Lab; Statistics Canada, Canadian Survey on Business Conditions."))+
      theme_minimal()+
      theme(
        axis.text.y = element_text(face = this_face, size=11,  color= "#4F4E4E", family = this_family),
        axis.text.x = element_blank(),
        
        plot.title    = element_text(size = 14, face="bold", family = this_family, color="#4F4E4E"),
        plot.subtitle = element_text(size = 14, color="gray58", family = this_family),
        
        plot.caption          = element_text(hjust = 0),
        plot.title.position   = "plot", 
        plot.caption.position = "plot",
        
        
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        
        
        legend.text  =element_text(family = this_family, face="bold", color="#4F4E4E") ,
        legend.title = element_text(family = this_family, face="bold", color="#4F4E4E"),
        
        legend.position = "none"
      )
    
  })
  
  
  
  output$obstacle4_newQ <- renderPlotly({
    
    thisData <- data_on_obstacle2()%>%
      mutate(GEO1 = case_when(GEO == "Canada" ~ "CA",
                              GEO == "Ontario"~ "ON",
                              GEO == "Alberta"~ "AB",
                              GEO == "British Columbia" ~ "BC",
                              GEO == "Newfoundland and Labrador" ~ "NL",
                              GEO == "Saskatchewan" ~ "SK",
                              GEO == "Nova Scotia" ~ "NS",
                              GEO == "New Brunswick" ~ "NB",
                              GEO == "Quebec" ~ "QC",
                              GEO == "Yukon" ~ "YT",
                              GEO == "Manitoba" ~ "MA",
                              GEO == "Prince Edward Island" ~ "PEI",
                              GEO == "Nunavut" ~ "NU",
                              GEO == "Northwest Territories" ~ "NT",
      ))
    
    if(business_characteristics_choices == "All Industries"){
      thisData <- thisData%>%
        filter(Business_characteristics == "All Industries")%>%
        mutate(fill_color= ifelse(GEO=="Canada", "yes", "no"))
      
      if(input$geo != "Canada"){thisData [thisData $GEO == input$geo,]$fill_color<-  "other"}
      
      business_char <- "all industries"
      
    }else if(business_characteristics_choices == "All employment sizes"){
      thisData <- thisData%>%filter(Business_characteristics == "All employment sizes")%>%
        mutate( fill_color= ifelse(GEO=="Canada", "yes", "no"))
      
      business_char <- "all employment sizes"
      if(input$geo != "Canada"){thisData [thisData $GEO == input$geo,]$fill_color<-  "other"}
      
    }else if(business_characteristics_choices == "All ownerships"){
      thisData <- thisData%>%filter(Business_characteristics == "All ownerships")%>%
        mutate( fill_color= ifelse(GEO=="Canada", "yes", "no"))
      
      business_char <- "all ownerships"
      if(input$geo != "Canada"){thisData [thisData $GEO == input$geo,]$fill_color<-  "other"}
      
    }else if(business_characteristics_choices == "All visible minorities"){
      thisData <- thisData%>%filter(Business_characteristics == "Ownership by all visible minorities")%>%
        mutate( fill_color= ifelse(GEO=="Canada", "yes", "no"))
      
      business_char <- "ownership by all visible minorities"
      if(input$geo != "Canada"){thisData [thisData $GEO == input$geo,]$fill_color<-  "other"}
      
    }else {
      thisData <- thisData%>%filter(Business_characteristics == "All business activities in the last 12 months")%>%
        mutate( fill_color= ifelse(GEO=="Canada", "yes", "no"))
      
      business_char <- "all business activities"
      if(input$geo != "Canada"){thisData [thisData$GEO == input$geo,]$fill_color<-  "other"}
      
    }
    
    
    
    fac_order <- thisData  %>%
      arrange(VALUE) %>%
      pull(GEO1)
    
    this_face   <- ifelse(fac_order  == "Canada", "bold", "plain")
    this_color  <- ifelse(fac_order  == "Canada", "#4F4E4E", "gray25")
    
    this_family <- "sans"
    
    max_value <- as.integer(max(thisData$VALUE)+0.2*max(thisData$VALUE))
    
    y_intercept_benchmarking <- thisData %>% dplyr::filter(GEO == "Canada")
    
    fig_obs_region <- ggplot(data= thisData %>% dplyr::filter(GEO != "Canada"), aes(y=VALUE, x=factor(GEO1, levels = fac_order), fill=fill_color)) +
      geom_bar(stat="identity",  width=0.6, colour="#4F4E4E")+
      geom_hline(aes(yintercept= y_intercept_benchmarking$VALUE, linetype = 'Canada Average',label = "Benchmark"), color = "red", size = 1) +
      # labs(x = "Sample Date", y = "Sample Value", 
      #      color = "Points", linetype = NULL) +
      theme(panel.grid.major = element_line(color = "darkgrey", 
                                            size = 0.5, linetype = 3)
            # ,
            # panel.background = element_rect(fill = "white", colour = "grey50")
      )+
      guides(fill="none")+
      geom_text(aes(label = paste0(VALUE,"%")), colour = "#4F4E4E", vjust=-0.4)+
      scale_fill_manual(values = c('no'="#00B9B4", 'yes'="#0C3163", 'other'= "#F8A12C"))+
      scale_y_continuous(name="Percentage",limits=c(0,max_value),breaks= seq(0,max_value,by=y_intercept_benchmarking$VALUE))+
      geom_hline(yintercept=0, color = "#4F4E4E", size=1)
    
    fig_obs_region <- ggplotly(fig_obs_region)
    fig_obs_region <- fig_obs_region %>% layout(
      title = list(text = paste0("Regional benchmarking: obstacles by region" ,
                                 '<br>',
                                 '<sup>',
                                 "% of respondents with ",tolower(input$obstacles), " as an obstacle for ",business_char,", by province")),
      xaxis = list(title = paste0("Region",
                                  "<br>",
                                  "Sources: Canadian Chamber of Commerce Business Data Lab; Statistics Canada, Canadian Survey on Business Conditions."),
                   titlefont = list(size = 10)
      ), 
      
      hovermode = FALSE,
      # barmode = 'group',
      showlegend = TRUE,
      legend = list(x = 1, 
                    y = NULL, 
                    orientation = "v", 
                    traceorder = "normal"),
      margin = list(t = 50, b = 40, r = 10, l = 40)
    )
    
    
    fig_obs_region
  })
  
  
  data_on_obstacle3 <- reactive({
    
    Slide13_Q2_22 <- cansim::get_cansim("3310050401") %>%  dplyr::mutate(Period = "Q2 2022", Quarter = "Q2", Year = "2022")
    Slide13_Q1_22 <- cansim::get_cansim("3310046901") %>%  dplyr::mutate(Period = "Q1 2022", Quarter = "Q1", Year = "2022")
    Slide13_Q4_21 <- cansim::get_cansim("3310040001") %>%  dplyr::mutate(Period = "Q4 2021", Quarter = "Q4", Year = "2021")
    Slide13_Q3_21 <- cansim::get_cansim("3310036401") %>%  dplyr::mutate(Period = "Q3 2021", Quarter = "Q3", Year = "2021")
    Slide13_Q2_21 <- cansim::get_cansim("3310033801") %>%  dplyr::mutate(Period = "Q2 2021", Quarter = "Q2", Year = "2021")
    Slide13_Q1_21 <- cansim::get_cansim("3310030801") %>%  dplyr::mutate(Period = "Q1 2021", Quarter = "Q1", Year = "2021")
    
    
    Slide13_Q2_22_clean = Data_cleaning_function(Slide13_Q2_22)
    Slide13_Q1_22_clean = Data_cleaning_function(Slide13_Q1_22)
    Slide13_Q4_21_clean = Data_cleaning_function(Slide13_Q4_21)
    Slide13_Q3_21_clean = Data_cleaning_function(Slide13_Q3_21)
    Slide13_Q2_21_clean = Data_cleaning_function(Slide13_Q2_21)
    Slide13_Q1_21_clean = Data_cleaning_function(Slide13_Q1_21)
    
    
    data_obs <- rbind(Slide13_Q2_22_clean,
                      Slide13_Q1_22_clean,
                      Slide13_Q4_21_clean,
                      Slide13_Q3_21_clean,
                      Slide13_Q2_21_clean,
                      Slide13_Q1_21_clean)%>% 
      dplyr::rename(Obstacles = Obstacles_for_the_business_or_organization)%>%
      dplyr::mutate(Business_characteristics = case_when( 
        Business_characteristics == "North American Industry Classification System (NAICS), all industries"       ~ "All Industries",
        Business_characteristics == "Agriculture, forestry, fishing and hunting [11]"                             ~ "Agriculture, forestry, fishing",
        Business_characteristics == "Mining, quarrying, and oil and gas extraction [21]"                          ~ "Mining, quarrying, oil, gas extraction",
        Business_characteristics == "Construction [23]"                                                           ~ "Construction",
        Business_characteristics == "Manufacturing [31-33]"                                                       ~ "Manufacturing",
        Business_characteristics == "Wholesale trade [41]"                                                        ~ "Wholesale trade",
        Business_characteristics == "Retail trade [44-45]"                                                        ~ "Retail trade",
        Business_characteristics == "Transportation and warehousing [48-49]"                                      ~ "Transportation, warehousing",
        Business_characteristics == "Information and cultural industries [51]"                                    ~ "Information and culture",
        Business_characteristics == "Finance and insurance [52]"                                                  ~ "Finance and insurance",
        Business_characteristics == "Real estate and rental and leasing [53]"                                     ~ "Real estate, rental, leasing",
        Business_characteristics == "Professional, scientific and technical services [54]"                        ~ "Professional services",
        Business_characteristics == "Administrative and support, waste management and remediation services [56]"  ~ "Administration and support services" ,
        Business_characteristics == "Health care and social assistance [62]"                                      ~ "Health care, social assistance",
        Business_characteristics == "Arts, entertainment and recreation [71]"                                     ~ "Arts, entertainment, recreation" ,
        Business_characteristics == "Accommodation and food services [72]"                                        ~ "Accomodation and food services" ,
        Business_characteristics == "Other services (except public administration) [81]"                         ~ "Other services",
        TRUE ~ "Error") )%>% 
      dplyr::filter(Business_characteristics == input$business_characteristics,
                    GEO == input$geo)
    
    
    obstacles_last_quarter <- data_obs %>%
      dplyr::filter(Period == last_quarter, VALUE > 10, !str_detect(Obstacles, "none") )%>%pull(Obstacles)
    
    
    obstacles_timeSeries <- data_obs%>%
      dplyr::filter(Obstacles %in% obstacles_last_quarter)%>%
      dplyr::select(Period, Obstacles)
    
    count_obstacles <- obstacles_timeSeries%>%
      dplyr::group_by(Obstacles) %>%
      dplyr::summarise(count = n())
    
    data_obs <- data_obs%>%
      left_join(count_obstacles, by = "Obstacles")%>%
      filter(#Obstacles %in% input$many_obstacles,
        count %in% input$nb_quarter)
    
    data_obs
    
  })
  
  
  output$obstacle5_newQ <- renderPlot({
    
    thisData <- data_on_obstacle3()
    
    level_order <- c("Q1 2021", "Q2 2021", "Q3 2021", "Q4 2021", "Q1 2022", "Q2 2022")
    this_family <- "sans"
    
    ggplot(thisData, aes(x = factor(Period, level = level_order), y = VALUE,
                         group = Obstacles, colour = Obstacles, label = Obstacles)) +
      geom_line(size=1) +
      geom_point(data=filter(thisData, Period %in% last_quarter))+
      geom_dl(data = subset(thisData, Period == last_quarter),aes(label = Obstacles, colour=Obstacles), method = list(dl.trans(x = x + .2), "last.bumpup"), size=2)+
      xlab("")+
      ylab("")+
      expand_limits(x= c(1, 10))+
      scale_colour_viridis_d()+
      scale_y_continuous(labels = function(x) paste0(x, "%"))+
      labs(title = paste("Over the next three months, which of the following are expected to be obstacles for your business in",input$geo , "?" ),
           subtitle = paste0("% of respondents that identify", tolower(input$obstacles) ,"by obstacle "),
           caption = paste("*",business_characteristics_choices, "by less than 10% of respondents are not reported. \n\nSources:Canadian Chamber of Commerce Business Data Lab, Statistics Canada"))+
      theme(
        panel.background = element_blank(),
        axis.text.y = element_text( size=11, margin=margin(r=+10), color= "black", family = this_family),
        
        plot.title    = element_text(size = 14, face="bold", family = this_family),
        plot.subtitle = element_text(size = 14, color="gray58", family = this_family),
        
        plot.caption          = element_text(hjust = 0, face= "italic"),
        plot.title.position   = "plot", 
        plot.caption.position = "plot",
        
        
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color="grey"),
        
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        
        
        legend.text  = element_text(family = this_family, face="bold") ,
        legend.title = element_text(family = this_family, face="bold"),
        
        legend.position = "none"
        
        
      )
    
  })
  
  
  
  output$obstacle6_newQ <- renderPlot({
    
    thisData <- data_on_obstacle3()%>% arrange(Period)
    
    this_family <- "sans"
    
    data_test <- thisData%>%
      dplyr::filter(Obstacles == input$obstacles)%>%
      unique()%>%
      ungroup()
    
    
    ggplot(data = data_test , 
           aes(x = factor(Quarter), y = VALUE , fill = Year)) +
      geom_bar(stat = "identity", position = position_dodge(preserve = "single"), alpha = 0.85, width=0.9)+
      geom_text(aes(label = paste0(round(VALUE,2),"%")))+
      geom_hline(yintercept=0,  color="black")+
      scale_fill_manual("",values = c("#FDE7C9","#FBCC8D","#FAB65C","#F8A12C" ))+
      xlab("")+
      ylab("")+
      expand_limits(x= c(1, 5))+
      scale_colour_viridis_d()+
      labs(title = paste("Over the next three months, which of the following are expected to be obstacles for your business in",input$geo , "?" ),
           subtitle = paste("% of respondents that identify",tolower(input$obstacles), "as an obstacle, ", input$business_characteristics),
           caption = paste("*",business_characteristics_choices, "by less than 10% of respondents are not reported. \n\nSources:Canadian Chamber of Commerce Business Data Lab, Statistics Canada"))+
      theme(
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        
        plot.title    = element_text(size = 14, face="bold", family = this_family),
        plot.subtitle = element_text(size = 14, color="gray58", family = this_family),
        
        plot.caption          = element_text(hjust = 0, face= "italic"),
        plot.title.position   = "plot", 
        plot.caption.position = "plot",
        
        
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        
        
        legend.text  = element_text(family = this_family, face="bold") ,
        legend.title = element_text(family = this_family, face="bold"),
        
        legend.position = "none"
        
        
      )
    
    
  })
  
  
  
  
  
}

