expected_to_change_server <- function(input, output, session) {
  
  output$change1<- renderPlot({
    
    # Business or organization expectations over the next three months, fourth quarter of 2022
    # Question: Over the next three months, how are the following expected to change for your business?
    
    last_quarter <- "Q4 2022"
    last_quarter_expecation_table <- "33100602"
    
    # read tables
    #=============
    
    Slide37_Q4_22 <- cansim::get_cansim(last_quarter_expecation_table)%>%  dplyr::mutate(Quarter = "Q4 2022")
    Slide37_Q3_22 <- cansim::get_cansim("33100533")%>%  dplyr::mutate(Quarter = "Q3 2022")
    Slide37_Q2_22 <- cansim::get_cansim("33100503")%>%  dplyr::mutate(Quarter = "Q2 2022")
    Slide37_Q1_22 <- cansim::get_cansim("33100468")%>%  dplyr::mutate(Quarter = "Q1 2022")
    
    
    # plot without Q3 2021
    #=======================
    
    data_expected_change <- rbind(Slide37_Q4_22,
                                  Slide37_Q3_22,
                                  Slide37_Q2_22,
                                  Slide37_Q1_22 
    )
    
    
    data_expected_change <- Data_cleaning_function_round1(data_expected_change)
    
    colnames(data_expected_change)
    
    data_expected_change <- data_expected_change%>%  
      dplyr::rename(Business_information = Business_or_organization_information  ,
                    Expected_change = Expected_change_over_the_next_three_months )%>%
      mutate(Expected_change=gsub("Expected change over the next three months, ","", as.character(Expected_change)),
             Business_information1 = gsub("Business or organization information, ","", as.character(Business_information)),
             font_size=ifelse(Quarter == last_quarter , "bold","plain"),
             Business_information = case_when(Business_information1 == "number of employees" ~ "Number of employees",
                                              Business_information1 == "vacant positions" ~ "Vacant positions",
                                              Business_information1 == "sales of goods and services offered by the business or organization" ~ "Sales",
                                              Business_information1 == "selling price of goods and services offered by the business or organization" ~ "Selling price",
                                              Business_information1 == "demand for goods and services offered by the business or organization"  ~ "Demand",
                                              Business_information1 == "imports" ~ "Imports",
                                              Business_information1 == "exports" ~ "Exports",
                                              Business_information1 == "operating income" ~ "Operating income",
                                              Business_information1 == "operating expenses" ~ "Operating expenses",
                                              Business_information1 == "profitability" ~ "Profitability",
                                              Business_information1 == "cash reserves" ~ "Cash reverves",
                                              Business_information1 == "capital expenditures" ~ "Capital expenditures",
                                              Business_information1 == "training expenditures"  ~ "Training expenditures",
                                              Business_information1 == "marketing and advertising budget"  ~ "Marketing budget",
                                              Business_information1 == "expenditures in research and development"  ~ "Research expenditures",
                                              TRUE ~ "Error"))%>%
      filter(Expected_change %in% c("increase","decrease"))%>%
      left_join(Business_characterisitcs_mapping, by="Business_characteristics")
    
    
    if(input$business_characteristics == "All industries"){

      data_expected_change<- data_expected_change%>%
        dplyr::filter(Clean == input$specific_business_characteristics)
      
      
    }else if(input$business_characteristics == "All employment sizes"){
      
      data_expected_change<- data_expected_change%>%
        dplyr::filter(Clean == input$specific_business_characteristics)
      
    }else if(input$business_characteristics == "All ownerships"){
      data_expected_change<- data_expected_change%>%
        dplyr::filter(Clean == input$specific_business_characteristics)
      
      
    }else if(input$business_characteristics == "All visible minorities"){
      
      data_expected_change <- data_expected_change%>%  
        dplyr::filter(Clean == input$specific_business_characteristics)
      
    }else{
      data_expected_change <- data_expected_change%>% 
        dplyr::filter(Clean == input$specific_business_characteristics)
      
    }
    
    
    plot1 <- data_expected_change %>% 
      tidyr::pivot_wider(names_from = Expected_change, values_from = VALUE)%>%
      dplyr::mutate(VALUE = round(increase - decrease))%>%
      filter(Business_information %in% c("Sales","Selling price","Operating expenses","Profitability","Capital expenditures","Number of employees"))
    
    
    
    level_order <- c( "Q1 2022", "Q2 2022", "Q3 2022", "Q4 2022")
    
    this_family <- "sans"
    
    
      plot_canada <- plot1 %>% filter(GEO=="Canada")
      plot1<- plot1 %>% filter(GEO==input$geo)
      
    
      
      ggplot(data = transform(plot1, Business_information= factor(Business_information, levels=c("Sales","Selling price","Operating expenses","Profitability","Capital expenditures","Number of employees")))  , 
             aes(x = factor(Quarter, level = level_order), y = VALUE , fill = Quarter)) +
        geom_bar(stat = "identity", position = position_dodge(), alpha = 0.85, width=1)+
        geom_point(data= transform(plot_canada, Business_information= factor(Business_information, levels=c("Sales","Selling price","Operating expenses","Profitability","Capital expenditures","Number of employees"))),aes(x = factor(Quarter, level = level_order), y = VALUE ), shape=21, fill="#FE4812",color="#FE4812" ,size=2)+
        geom_hline(yintercept=0,  color="#4F4E4E")+
        # with contrast
        #===============
      scale_fill_manual("",values = c("#06fff8","#00d3cd","#00a09b", "#006d6a" ),labels=level_order, breaks=level_order)+ # turquoise color  - ordered least to most pigmentation with contrast
        geom_text(aes(y=VALUE+2*sign(VALUE),label=paste0(round(VALUE,2),"%"), fontface=font_size), size=3, color="#4F4E4E")+
        ylab("") + xlab("")+
        labs(title = paste0(input$geo, ": Expected change, next three months") ,
             subtitle = "Balance of opinion, percentage points*",
             caption = paste("Sources: Canadian Chamber of Commerce Business Data Lab; Statistics Canada, Canadian Survey on Business Conditions."))+
        theme(
          panel.background = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          panel.spacing = unit(2, "lines"),
          
          plot.title    = element_text(size = 14, face="bold", family = this_family, color="#4F4E4E"),
          plot.subtitle = element_text(size = 14, color="gray58", family = this_family),
          
          plot.caption          = element_text(hjust = 0, color="#4F4E4E"),
          plot.title.position   = "plot", 
          plot.caption.position = "plot",
          
          
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          
          
          legend.text =element_text(family = this_family, face="bold", color="#4F4E4E") ,
          legend.title = element_text(family = this_family, face="bold", color="#4F4E4E"),
          
          strip.placement = "outside",  
          strip.background = element_blank(),
          strip.text.x = element_text(face="bold", color="#4F4E4E"),
          
          legend.position = "bottom")+
      facet_wrap(~Business_information, nrow = 1, strip.position = "bottom")
    
  })

  
  
  observeEvent(input$geo, {
    if (input$geo == "Canada") {
      
      output$cityControls <- renderUI({
        plotOutput("change1", height = 400, width= 900)
      })
    }else{
      
      output$cityControls <- renderUI({
                 plotOutput("change1", height = 600, width= 900)
      })
      
    }
    
    
    
  })
  
  
  
}