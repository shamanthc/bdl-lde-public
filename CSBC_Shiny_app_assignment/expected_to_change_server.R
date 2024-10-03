expected_to_change_server <- function(input, output, session) {
  
  last_quarter_expecation_table <- "33100602"

  
  # read tables
  #=============
  
  Slide37_Q4_22 <- cansim::get_cansim(last_quarter_expecation_table)%>%  dplyr::mutate(Quarter = "Q4 2022")
  Slide37_Q3_22 <- cansim::get_cansim("33100533")%>%  dplyr::mutate(Quarter = "Q3 2022")
  Slide37_Q2_22 <- cansim::get_cansim("33100503")%>%  dplyr::mutate(Quarter = "Q2 2022")
  Slide37_Q1_22 <- cansim::get_cansim("33100468")%>%  dplyr::mutate(Quarter = "Q1 2022")
  
  
  
  
  
  
  
  data_download_expec <- reactive({ 
    
    # Business or organization expectations over the next three months, fourth quarter of 2022
    # Question: Over the next three months, how are the following expected to change for your business?
    
    last_quarter <- "Q4 2022"
    last_quarter_expecation_table <- "33100602"
    # 
    # # read tables
    # #=============
    # 
    # Slide37_Q4_22 <- cansim::get_cansim(last_quarter_expecation_table)%>%  dplyr::mutate(Quarter = "Q4 2022")
    # Slide37_Q3_22 <- cansim::get_cansim("33100533")%>%  dplyr::mutate(Quarter = "Q3 2022")
    # Slide37_Q2_22 <- cansim::get_cansim("33100503")%>%  dplyr::mutate(Quarter = "Q2 2022")
    # Slide37_Q1_22 <- cansim::get_cansim("33100468")%>%  dplyr::mutate(Quarter = "Q1 2022")
    # 
    # 
    
    
    
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
    
    data_expected_change <- data_expected_change%>% 
      dplyr::filter(Clean == input$business_characteristics)
    
    
    
    
    plot1 <- data_expected_change %>% 
      tidyr::pivot_wider(names_from = Expected_change, values_from = VALUE)%>%
      dplyr::mutate(VALUE = round(increase - decrease))%>%
      filter(Business_information %in% c("Sales","Selling price","Operating expenses","Profitability","Capital expenditures","Number of employees"))
    
    
    
    return(as.data.frame(plot1))
  })
  
  
  output$change1<- renderPlotly({
    
    plot1 <- data_download_expec()
    level_order <- c( "Q1 2022", "Q2 2022", "Q3 2022", "Q4 2022")
    
    this_family <- "sans"
    
    minimum_value_graph <- round(((min(plot1$VALUE)*1.2)/10),0)*10
    maximum_value_graph <- round(((max(plot1$VALUE)*1.2)/10),0)*10
    
    
    fig_expec <-  ggplot(plot1, aes(x = Business_information, y = VALUE)) +
      geom_col(aes(fill = Quarter),
               data = . %>% filter(GEO == input$geo), width = 0.8,
               position = position_dodge(width = 0.8)) +
      
      # geom_point(aes(group = Quarter),
      #          data = . %>% filter(GEO  == "Canada"), fill = "red", width = 0.1,
      #          position = position_dodge(width = 0.8))+
      geom_col(aes(group = Quarter),
               data = . %>% filter(GEO  == "Canada"), fill = "red", width = 0.1,
               position = position_dodge(width = 0.8))+
      scale_fill_manual("",values = c("#06fff8","#00d3cd","#00a09b", "#006d6a" ),labels=level_order, breaks=level_order)+ # turquoise color  - ordered least to most pigmentation with contrast
      scale_y_continuous(breaks = seq(minimum_value_graph, maximum_value_graph, by=10))
    
    fig_expec <- ggplotly(fig_expec)
    fig_expec <- fig_expec %>% layout(
      title = paste0(input$geo, ": Expected change, next three months" ,
                     '<br>',
                     '<sup>',
                     "Balance of opinion, percentage points*"),
      
      xaxis = list(title = paste0("Business Information",
                                  "<br>",
                                  "Sources: Canadian Chamber of Commerce Business Data Lab; Statistics Canada, Canadian Survey on Business Conditions."),
                   titlefont = list(size = 12)
      ), 
      yaxis = list(title = "Percentage", 
                   ticklen = 8, 
                   tickcolor = "#EEEEEE"
                     , 
                   #rangemode = "tozero",
                   #range = c(-100, 100), 
                   tick0 = 0, 
                   dtick = 10, 
                   tickfont = list(size = 11)
      ),
      hovermode = 'x',
      # barmode = 'group',
      showlegend = TRUE,
      legend = list(x = 1, 
                    y = 1, 
                    orientation = "v", 
                    traceorder = "normal"),
      margin = list(t = 50, b = 40, r = 10, l = 40)
    )
    
    fig_expec
    
  })
  
  
  # Downloadable csv of selected dataset ----
  output$downloaddata_expec <- downloadHandler(
    
    filename = function() {
      paste0("CSBC",input$geo, ".csv")
    },
    content = function(file) {
      plot_data <-   data_download_expec()
      plot_data <- plot_data %>% dplyr:: filter(GEO %in% c("Canada", input$geo))
      plot_data <- unique(plot_data)
      write_csv(plot_data, file)
    }
    
  )
  
    
  
  observeEvent(input$geo, {
    if (input$geo == "Canada") {
      
      output$cityControls <- renderUI({
        fluidRow(plotlyOutput("change1", height = 600, width= 1100),
                 div(fluidRow(downloadButton("downloaddata_expec", "Download data"))))
        
      })
    }else{
      
      output$cityControls <- renderUI({
        fluidRow(plotlyOutput("change1", height = 600, width= 1100),
                 div(fluidRow(downloadButton("downloaddata_expec", "Download data"))))
      })
      
    }
  })
  
  
  
}