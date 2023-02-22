talent_server_newQuarter <- function(input, output, session) {
  
  
  output$talent1_newQ <- renderPlot({ 
    
    Slide19_Q4_22 <- cansim::get_cansim("3310060301")
    Slide19_Q3_22 <- cansim::get_cansim("3310053401")
    Slide19_Q2_22 <- cansim::get_cansim("3310050401")
    Slide19_Q1_22 <- cansim::get_cansim("3310046901")
    
    Slide19_Q4_22_clean = Data_cleaning_function(Slide19_Q4_22)%>%  dplyr::mutate(Quarter = "Q4 2022")
    Slide19_Q3_22_clean = Data_cleaning_function(Slide19_Q3_22)%>%  dplyr::mutate(Quarter = "Q3 2022")
    Slide19_Q2_22_clean = Data_cleaning_function(Slide19_Q2_22)%>%  dplyr::mutate(Quarter = "Q2 2022")
    Slide19_Q1_22_clean = Data_cleaning_function(Slide19_Q1_22)%>%  dplyr::mutate(Quarter = "Q1 2022")
    
    Required_dataset_19_plot1 <- rbind(Slide19_Q4_22_clean,
                                       Slide19_Q3_22_clean,
                                       Slide19_Q2_22_clean,
                                       Slide19_Q1_22_clean 
                                       )%>%  
      left_join(Business_characterisitcs_mapping, by = "Business_characteristics")%>%
      dplyr::filter( Clean == input$specific_business_characteristics,
                    Obstacles_for_the_business_or_organization %in% c("Recruiting skilled employees","Retaining skilled employees",
                                                                      "Shortage of labour force"),
                    GEO == input$geo)%>%
      mutate(font_size=ifelse(Quarter == "Q4 2022", "bold", "plain"))%>%
      rename(Obstacles = Obstacles_for_the_business_or_organization)
    
    
    
    level_order <- c(  "Q1 2022", "Q2 2022", "Q3 2022", "Q4 2022")
    
    this_family <- "sans"
    
    
    ggplot(data = Required_dataset_19_plot1 , 
           aes(x = factor(Quarter, level = level_order), y = VALUE , fill = Quarter)) +
      geom_bar(stat = "identity", position = position_dodge(), alpha = 0.85, width=1)+
      geom_hline(yintercept=0,  color="black")+
      scale_fill_manual("",values = c("#FDE7C9","#FBCC8D","#FAB65C","#F8A12C" , "green"),labels=level_order, breaks=level_order)+
      geom_text(aes(label = paste0(round(VALUE,2),"%"), fontface=font_size), colour = "black",
                position = position_stack(vjust = 1.05, reverse = TRUE))+
      ylab("") + xlab("")+
      labs(title = paste0("Over the next three months, which are expected to be obstacles for your business in ", input$geo, "?"),
           subtitle = "% of respondents, by labour obstacle")+
      theme(
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        
        plot.title    = element_text(size = 14, face="bold", family = this_family),
        plot.subtitle = element_text(size = 14, color="gray58", family = this_family),
        
        plot.caption          = element_text(hjust = 0, face= "italic"),
        plot.title.position   = "plot", 
        plot.caption.position = "plot",
        
        
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        
        
        legend.text =element_text(family = this_family, face="bold") ,
        legend.title = element_text(family = this_family, face="bold"),
        
        strip.placement = "outside",  
        strip.background = element_blank(),
        strip.text.x = element_text(face="bold"),
        
        panel.spacing = unit(6, "lines"),
        
        legend.position = "bottom"
        
        
      )+
      facet_wrap(~Obstacles, nrow = 1, strip.position = "bottom")
    
  })
  

  
  
  output$talent3_newQ <- renderPlot({
    
    
    
    # Q3 2022 table
    #==============
    
    Q4_2022_tableNumber <- "33100603"
    tableNumber <- "33-10-0534"
    
    Slide22_Q4_22 <- cansim::get_cansim(Q4_2022_tableNumber)
    
    Slide22_Q4_22_clean = Data_cleaning_function(Slide22_Q4_22)
    
    # Similar plot as report
    #=======================
    
    
    required_table <- Slide22_Q4_22_clean %>%  
      dplyr::filter(Business_characteristics %in% Industries,
                    GEO == input$geo) %>%  
      dplyr::filter(grepl("skilled|labour", Obstacles_for_the_business_or_organization)) %>% 
      dplyr::mutate(VALUE = tidyr::replace_na(VALUE,0),
                    Business_characteristics = case_when( 
                      Business_characteristics == "North American Industry Classification System (NAICS), all industries"       ~ "All Industries*",
                      Business_characteristics == "Agriculture, forestry, fishing and hunting [11]"                             ~ "Agriculture, forestry, fishing",
                      Business_characteristics == "Mining, quarrying, and oil and gas extraction [21]"                          ~ "Mining, oil and gas",
                      Business_characteristics == "Construction [23]"                                                           ~ "Construction",
                      Business_characteristics == "Manufacturing [31-33]"                                                       ~ "Manufacturing",
                      Business_characteristics == "Wholesale trade [41]"                                                        ~ "Wholesale trade",
                      Business_characteristics == "Retail trade [44-45]"                                                        ~ "Retail trade",
                      Business_characteristics == "Transportation and warehousing [48-49]"                                      ~ "Transportation, warehousing",
                      Business_characteristics == "Information and cultural industries [51]"                                    ~ "Information, culture",
                      Business_characteristics == "Finance and insurance [52]"                                                  ~ "Finance, insurance",
                      Business_characteristics == "Real estate and rental and leasing [53]"                                     ~ "Real estate",
                      Business_characteristics == "Professional, scientific and technical services [54]"                        ~ "Professional services",
                      Business_characteristics == "Administrative and support, waste management and remediation services [56]"  ~ "Administrative services" ,
                      Business_characteristics == "Health care and social assistance [62]"                                      ~ "Health care, social assistance",
                      Business_characteristics == "Arts, entertainment and recreation [71]"                                     ~ "Arts, entertainment, recreation" ,
                      Business_characteristics == "Accommodation and food services [72]"                                        ~ "Accommodation, food services" ,
                      Business_characteristics == "Other services (except public administration) [81]"                          ~ "Other services",
                      TRUE ~ "Error") )%>%
      rename(Obstacles = Obstacles_for_the_business_or_organization)%>%
      select(-"GEO")%>%
      group_by(Business_characteristics)%>%
      mutate(order = mean(VALUE),
             bin = ifelse(order>41 | order < 24, "yes", "no"),
             font_size=ifelse(Business_characteristics  == 'All Industries*', "bold","plain"))%>%
      arrange(order)%>%
      ungroup()
    
    # Order business characteristics by the average of obstacles
    #============================================================
    fac_order <- required_table  %>%
      dplyr::filter(Business_characteristics != "All Industries*") %>% 
      pull(Business_characteristics)
    
    
    fac_order <- unique(c(fac_order, "All Industries*"))
    this_face   <- ifelse(fac_order  == "All Industries*", "bold", "plain")
    this_family <- "sans"
    
    min_value <- min(required_table$VALUE)
    max_value <- max(required_table$VALUE)
    avr <- mean(min_value, max_value)
    
    ggplot(required_table, aes(x = factor(Obstacles, levels=c("Shortage of labour force",'Recruiting skilled employees',"Retaining skilled employees")), 
                                                         y = factor(Business_characteristics, levels = fac_order))) +
      geom_tile(aes(fill = VALUE), colour = "white") +
      geom_text(aes(label = paste0(VALUE, "%"), fontface=font_size), color = "black", size=4) + 
      scale_x_discrete(position = "top") +
      scale_fill_gradientn(colours = c("#FE4812","yellow", "green3"), 
                           breaks=c(min_value, 30, max_value),
                           labels=c("Lower % of businesses","","Highest % of businesses"),
                           values = scales::rescale(c(100, 35, 0)))+
      theme_minimal()+
      theme(
        axis.text.y = ggtext::element_markdown(face = this_face, size=11, margin=margin(r=-10), color= "black", family = this_family),
        axis.text.x = element_text( size=11, color="black"),
        
        plot.title    = element_text(size = 14, face="bold", family = this_family),
        plot.subtitle = element_text(size = 14, color="gray58", family = this_family),
        
        plot.caption          = element_text(hjust = 0),
        plot.title.position   = "plot", 
        plot.caption.position = "plot",
        
        
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        
        legend.text =element_text(family = this_family, face="bold", color="black", size=9) ,
        legend.title = element_blank(),
        
        legend.key.height = unit(0.3, 'cm'), #change legend key height
        legend.key.width = unit(3, 'cm'), #change legend key width
        legend.position ="bottom" 
      )+
      labs(y ="", 
           x="",
           title= paste0("Over the next three months, which of the following are expected to be obstacles for your business", input$geo, "?"),
           subtitle="% of respondents, by industry",
           caption = paste0("*Note: Industies sorted in descending order based on the average across the three responses. \n\nSources: Canadian Chamber of Commerce Business Data Lab; Statistics Canada, Canadian Survey on Business Conditions (Table: ",tableNumber, ")."))
    
  })
  
 
  
  
  output$talent5_newQ <- renderPlot({
    
    
    Slide_25 = get_cansim("3310056601") # Q3 2022 data
    Slide_25_clean = Data_cleaning_function(Slide_25)%>%
      rename(workArrangement = "Work_arrangements_anticipated_in_the_next_three_months")%>%
      dplyr::filter(Business_characteristics %in% Industries,
                    GEO == input$geo)
    
    
    dt1 <- Slide_25_clean %>%
      mutate(workArrangement = case_when(
        workArrangement %in% c("Average percentage of workforce anticipated to work on-site most hours",
                               "Average percentage of workforce anticipated to work the same amount of hours on-site and remotely",
                               "Average percentage of workforce anticipated to work remotely most hours")~ "Hybird work",
        workArrangement == "Average percentage of workforce anticipated to work on-site exclusively" ~ "Work exclusively on-site" ,
        TRUE ~ "Work remotely exclusively"),
        Business_characteristics = case_when( 
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
          Business_characteristics == "Other services (except public administration) [81]"                          ~ "Other services",
          TRUE ~ "Error"))%>% 
      filter(Business_characteristics != "Error")%>%
      group_by(GEO, Business_characteristics, workArrangement)%>%
      mutate(VALUE = sum(VALUE))%>%
      unique()
    
    
    
    # 
    data1_sum <- dt1 %>%
      dplyr::group_by(Business_characteristics)%>% 
      dplyr::mutate(VALUE=sum(VALUE))%>%slice(n())
    
    #
    fac_order <- dt1 %>%
      filter(workArrangement == "Work remotely exclusively",
             Business_characteristics != "All Industries") %>%
      arrange(VALUE) %>%
      pull(Business_characteristics)
    
    fac_order <- c(fac_order, "All Industries")
    
    this_face   <- ifelse(fac_order  == "All Industries", "bold", "plain")
    this_color  <- ifelse(fac_order  == "All Industries", "black", "gray25")
    this_family <- "sans"
    
    thisLevel <- c("Work remotely exclusively", "Hybird work", "Work exclusively on-site" )
    thisLevels1 <- thisLevel[!thisLevel %in% "Work remotely exclusively"]
    thisLevels1 <- c("Work remotely exclusively", thisLevels1)
    
    
    ggplot(dt1, aes(fill = factor(workArrangement, levels= thisLevels1),
                    y=VALUE, (x=factor(Business_characteristics, levels = fac_order)))) +
      geom_bar(position=position_fill(reverse = TRUE), stat="identity", width=0.8)+
      scale_fill_manual("", values = c("#FE4812","#00B9B4","#F8A12C"))+
      geom_text(aes(label = paste0(VALUE,"%")), position = position_fill(vjust = 0.6, reverse=TRUE), colour = "white", size=4, fontface =2)+
      #geom_text(data=data1_sum, aes(label=paste0(VALUE,"%")),hjust=0.5, colour="grey", size=3, position = position_fill(vjust = -0.05, reverse=TRUE))+
      coord_flip()+
      ylab("") + 
      xlab("")+
      labs(title = paste0("Over the next three months, what percentage of the employees of your business is anticipated to do each of the following for ",input$geo , "?"),
           subtitle = "% of respondents, by industry")+
      theme_minimal()+
      theme(
        axis.text.y = element_text(face = this_face, size=11,  color= "black", family = this_family),
        axis.text.x = element_blank(),
        
        plot.title    = element_text(size = 14, face="bold", family = this_family),
        plot.subtitle = element_text(size = 14, color="gray58", family = this_family),
        
        plot.caption          = element_text(hjust = 0, face= "italic"),
        plot.title.position   = "plot", 
        plot.caption.position =  "plot",
        
        
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        
        legend.text     =element_text(family = this_family),
        legend.position ="bottom" ,
        legend.box      ="horizontal"
      )
    
    
    
    
    
    
    
  })
}