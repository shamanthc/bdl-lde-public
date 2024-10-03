outlook_server <- function(input, output, session) {
  
  # read tables
  #=============
  # check that outlook options are correctly spelled (i.e.somewhat pessimistic and not soemwhat pessimistic )  
  outlook_Q4_22 <- cansim::get_cansim("33100630")%>%  dplyr::mutate(Quarter = "Q4 2022")
  outlook_Q3_22 <- cansim::get_cansim("33100567")%>%  dplyr::mutate(Quarter = "Q3 2022")
  outlook_Q1_22 <- cansim::get_cansim("33100488")%>%  dplyr::mutate(Quarter = "Q1 2022")
  outlook_Q4_21 <- cansim::get_cansim("33100426")%>%  dplyr::mutate(Quarter = "Q4 2021")
  outlook_Q3_21 <- cansim::get_cansim("33100393")%>%  dplyr::mutate(Quarter = "Q3 2021")
  
  
  
  
  
  data_outlook1 <- reactive({
    
    
    # Search question at StatCan
    #================================
    # Future outlook for the business or organization over the next 12 months, fourth quarter of 2022
    outlook_Q3_2022_table <- "33-10-0630"
    this_quarter <- "Q4 2022"
    
    # # read tables
    # #=============
    # # check that outlook options are correctly spelled (i.e.somewhat pessimistic and not soemwhat pessimistic )  
    # outlook_Q4_22 <- cansim::get_cansim("33100630")%>%  dplyr::mutate(Quarter = "Q4 2022")
    # outlook_Q3_22 <- cansim::get_cansim("33100567")%>%  dplyr::mutate(Quarter = "Q3 2022")
    # outlook_Q1_22 <- cansim::get_cansim("33100488")%>%  dplyr::mutate(Quarter = "Q1 2022")
    # outlook_Q4_21 <- cansim::get_cansim("33100426")%>%  dplyr::mutate(Quarter = "Q4 2021")
    # outlook_Q3_21 <- cansim::get_cansim("33100393")%>%  dplyr::mutate(Quarter = "Q3 2021")
    
    colnames(outlook_Q4_22)= colnames(outlook_Q1_22)
    colnames(outlook_Q3_22)= colnames(outlook_Q1_22)
    
    
    # combine data
    #=======================
    
    data_outlook <- rbind(outlook_Q4_22,
                          outlook_Q3_22,
                          outlook_Q1_22,
                          outlook_Q4_21,
                          outlook_Q3_21)
    
    
    data_outlook1 <- Data_cleaning_function_round1(data_outlook)%>%
      dplyr::rename(Outlook = Future_outlook_of_the_business_or_organization)%>%
      left_join(Business_characterisitcs_mapping, by="Business_characteristics")%>%
      dplyr::filter(GEO==input$geo,
      )%>%
      mutate(Outlook = gsub("Future outlook of the business or organization, ","", as.character(Outlook)),
             Outlook = gsub("Future outlook, ","", as.character(Outlook)),
             Outlook = ifelse(Outlook == "Future outlook of the business or organization is unknown", "Unknown", Outlook),
             Outlook = ifelse(Outlook == "soemwhat pessimistic", "somewhat pessimistic", Outlook),
             Outlook1 = ifelse(Outlook == "very optimistic"| Outlook == "somewhat optimistic", "Optimistic", ifelse(Outlook == "very pessimistic"| Outlook == "somewhat pessimistic","Pessimistic", "Unknown")) 
      )%>%
      group_by(Business_characteristics, Outlook1, Quarter)%>%
      mutate(sum_value = sum(VALUE))
    
    data_outlook1
    
    
  })
  
  
  output$outlook1 <- renderPlot({
    
    # Plot line for combined Optimistic
    #===================================
    #outlook_Q3_2022_table <- "33-10-0567"
    # data with both optimistic and pessimistic
    thisData <- data_outlook1()%>%
      dplyr::select(-Outlook, -VALUE)%>%
      unique()%>%
      dplyr::filter(#Business_characteristics == "All Industries",
        Clean ==input$business_characteristics,
        Outlook1 %in% c("Optimistic", "Pessimistic"))%>% 
      unique()%>%
      mutate(label1 = paste0(Outlook1,", ", round(sum_value),"%"))
    
    
    
    last_quarter = "Q4 2022"
    level_order <- c("Q3 2021", "Q4 2021", "Q1 2022", "Q3 2022", "Q4 2022")
    this_family <- "sans"
    
    
    ggplot(thisData, aes(x = factor(Quarter, level = level_order), y = sum_value,
                         group = Outlook1, colour = Outlook1, label = Outlook1)) +
      geom_line(size=1.2)+
      geom_point(size=2.5)+
      geom_hline(yintercept=0,  color="#4F4E4E")+
      geom_dl(data = subset(thisData, Quarter== last_quarter),aes(label = label1, colour=Outlook1), method = list(dl.trans(x = x + .2), "last.points", cex = 1.2, color="#4F4E4E"))+
      xlab("")+
      ylab("")+
      expand_limits(x= c(1, 6), y=c(60,80))+
      scale_color_manual("", values = c("#00B9B4", "#FE4812"))+ 
      scale_y_continuous(labels = function(x) paste0(x, "%"))+
      labs(title = paste0("Over the next 12 months, what is the future outlook for your business in ", input$geo, "?" ),
           subtitle = paste0("% of respondents"),
           caption = paste0("Note: Optimistic includes 'Somewhat optimistic' and 'Very optimistic'; Pessimistic includes 'Somewhat pessimistic' and 'Very pessimistic'. Responses do not sum up to 100% because\n of 'Unknown' responses. Question not included in Q2 2022.\n\nSources: Canadian Chamber of Commerce Business Data Lab; Statistics Canada, Canadian Survey on Business Conditions."))+
      theme(
        panel.background = element_blank(),
        axis.text.y = element_text( size=11, margin=margin(r=+10), color= "#4F4E4E", family = this_family),
        axis.text.x = element_text( size=11, margin=margin(r=+10), color= "#4F4E4E", family = this_family),
        
        plot.title    = element_text(size = 14, face="bold", family = this_family, color="#4F4E4E"),
        plot.subtitle = element_text(size = 14, color="gray58", family = this_family),
        
        plot.caption          = element_text(hjust = 0, color="#4F4E4E"),
        plot.title.position   = "plot", 
        plot.caption.position = "plot",
        
        
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color="#4F4E4E"),
        
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        
        
        legend.text  = element_text(family = this_family, face="bold", color="#4F4E4E") ,
        legend.title = element_text(family = this_family, face="bold", color="#4F4E4E"),
        
        legend.position = "none"
        
        
      )
    
    
  })
  
  
  
  output$outlook2 <- renderPlot({
    this_quarter <- "Q4 2022"
    
    # outlook Bar chart by top/bottom 3 industries 
    #==============================================
    
    
    # data_outlook2 <- data_outlook1%>%
    data_outlook2 <- data_outlook1()%>%
      dplyr::filter(Quarter == this_quarter,
             Outlook %in% c("very optimistic", "somewhat optimistic"),
             sum_value >0) %>%
      ungroup()%>%
      mutate(this_font_face = case_when(Clean  == "All Industries"~ 2, 
                                        Clean == "All employment sizes"~2,
                                        Clean == "All ownerships" ~ 2,
                                        Clean == "Ownership by all visible minorities" ~ 2,
                                        Clean == "All business activities in the last 12 months"~ 2,
                                        TRUE ~1),
             Outlook = firstup(Outlook))%>%
      dplyr::select(-Outlook1, -sum_value)%>%
      group_by(Business_characteristics)%>%
      mutate(tot=sum(VALUE))%>%
      arrange(desc(tot))
    
    
    
    #====
    if(business_characteristics_choices == "All Industries"){
      data_outlook2 <- data_outlook2 %>%
        dplyr::filter( Business_characteristics %in% Industries)%>%
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
      
      data_outlook2 <- data_outlook2 %>%
        dplyr::filter(Business_characteristics %in% Employment_size)%>%
        mutate(Business_characteristics = case_when( 
          Business_characteristics == "Business or organization size of employment, all employment sizes"  ~ "All employment sizes",
          TRUE ~ as.character(Business_characteristics)))%>%
        arrange(VALUE)
      
    }else if(business_characteristics_choices == "All ownerships"){
      data_outlook2 <- data_outlook2 %>%
        dplyr::filter( Business_characteristics %in% Majority_Ownership)%>%
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
      
      data_outlook2 <- data_outlook2 %>%
        dplyr::filter(Business_characteristics %in% Ownership_minority)%>%
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
      data_outlook2 <- data_outlook2 %>%
        dplyr::filter(Business_characteristics %in% Business_activity)%>%
        mutate(Business_characteristics = case_when( 
          Business_characteristics == "Business or organization activity in the last 12 months, all business or organization activities"  ~ "All business activities in the last 12 months",
          Business_characteristics == "Sold goods to businesses in Canada who then resold them outside of Canada"  ~ paste("Sold goods to businesses in Canada who \nthen resold them outside of Canada"),
          Business_characteristics == "Relocated any business or organizational activities or employees from another country into Canada"  ~ paste("Relocated any business activities or \nemployees from another country into Canada"),
          TRUE ~ as.character(Business_characteristics)))%>%
        arrange(VALUE)
      
    }
    #====
    
    
    if(business_characteristics_choices == "All Industries"){
      fac_order <- data_outlook2 %>%
        filter(Business_characteristics != "All Industries") %>%
        arrange(tot) %>% 
        dplyr::select(Business_characteristics)%>%unique()%>%
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
      fac_order <- data_outlook2 %>%
        filter(Business_characteristics != "All ownerships") %>%
        arrange(tot) %>% dplyr::select(Business_characteristics)%>%unique()%>%
        pull(Business_characteristics)
      
      
      fac_order <- c(fac_order, "All ownerships")
      
      this_face   <- ifelse(fac_order  == "All ownerships", "bold", "plain")
      this_color  <- ifelse(fac_order  == "All ownerships", "#4F4E4E", "gray25")
      
      business_char <- "by majority owned"
      
    }else if(business_characteristics_choices == "All visible minorities"){
      fac_order <- data_outlook2 %>%
        filter(Business_characteristics != "Ownership by all visible minorities") %>%
        arrange(tot) %>% dplyr::select(Business_characteristics)%>%unique()%>%
        pull(Business_characteristics)
      
      
      fac_order <- c(fac_order, "Ownership by all visible minorities")                                             
      
      
      this_face   <- ifelse(fac_order  == "Ownership by all visible minorities", "bold", "plain")
      this_color  <- ifelse(fac_order  == "Ownership by all visible minorities", "#4F4E4E", "gray25")
      business_char <- "by visible minority"
      
    }else{
      fac_order <- data_outlook2 %>%
        filter( Business_characteristics != "All business activities in the last 12 months") %>%
        arrange(tot) %>% dplyr::select(Business_characteristics)%>%unique()%>%
        pull(Business_characteristics)
      
      
      fac_order <- c(fac_order, "All business activities in the last 12 months")                                             
      
      
      this_face   <- ifelse(fac_order  == "All business activities in the last 12 months", "bold", "plain")
      this_color  <- ifelse(fac_order  == "All business activities in the last 12 months", "black", "gray25")
      business_char <- "by all business activities"
      
    }
    
    
    data_outlook2 <- data_outlook2%>% filter(Business_characteristics %in% fac_order)
    
    
    data_total <- data_outlook2 %>% group_by(Business_characteristics)%>%mutate(tot=sum(VALUE))%>%
      dplyr::select(Business_characteristics, tot)%>%
      unique()%>%filter(Business_characteristics %in% fac_order)%>%
      arrange(desc(tot))
    
    this_family <- "sans"
    
    ggplot(data_outlook2, aes(fill = Outlook, y = VALUE,
                              x=factor(Business_characteristics, levels = fac_order) )) +
      geom_bar(stat="identity", width=0.6, position="stack")+
      
      geom_text(aes(label=paste0(round(VALUE),"%")), position = position_stack(vjust = 0.5), size=5, color="white", fontface=2) +
      geom_text(aes(x=factor(Business_characteristics, levels = fac_order), y= tot + 3.5, fontface= rev(this_face), label = paste0(round(tot), "%"), fill = NULL),size=5, data = data_total) +
      scale_fill_manual("",values = c("Very optimistic"="#00a09b" ,"Somewhat optimistic"="#00d3cd"))+
      expand_limits(x= c(-0.5, 7), y =c(0, 110 ))+
      scale_color_manual(values = c('red' = 'red', 'green' = 'green3', 'black' = 'black'), guide = "none")+
      coord_flip()+
      ylab("") + 
      xlab("")+
      labs(title = paste0("Over the next 12 months, what is the future outlook for your business in ", input$geo, "?" ),
           subtitle = paste0("% of respondents, by top to bottom most optimistic industries"),
           caption = paste0("Sources: Canadian Chamber of Commerce Business Data Lab; Statistics Canada, Canadian Survey on Business Conditions."))+  
      theme_minimal()+
      theme(
        axis.text.y = element_text(face = this_face, size=11, margin=margin(r=-10), color= "black", family = this_family),
        axis.text.x = element_blank(),
        
        plot.title    = element_text(size = 14, face="bold", family = this_family),
        plot.subtitle = element_text(size = 14, color="gray58", family = this_family),
        
        plot.caption          = element_text(hjust = 0),
        plot.title.position   = "plot", 
        plot.caption.position = "plot",
        
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        
        
        legend.text  =element_text(family = this_family, face="bold") ,
        legend.title = element_text(family = this_family, face="bold"),
        
        legend.position =c(0.4, 0) ,
        legend.direction = "horizontal"
      ) #+
    # annotate("text", x=5, y=100, label= paste("Most Optimistic\n(top 3)"), colour = "#00B9B4", fontface = "bold")+
    # annotate("text", x=2, y=100, label= paste("Least Optimistic\n(bottom 3)"), colour = "#FE4812", fontface = "bold" )
    
    
    
    
  })
  
  
  
  
}