library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(tidyverse)
library(DT)


sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebar",
    # recurring survey questions
    #============================
    menuItem("Business expectations", tabName = "expectation", icon = icon("dashboard")),
    
    menuItem("Business obstacles",   tabName = "obstacles_newQ", icon = icon("dashboard")),
 
    menuItem("Business outlook",     tabName = "outlook", icon = icon("dashboard")),
    
    menuItem("Competition for talent",  tabName = "talents_newQ", icon = icon("dashboard"))
  )
)


body <- dashboardBody(
  
  includeCSS("www/chamber_theme.css"),
  fluidRow(column(4, selectInput("geo", "Select province or territory", choices = Geography)),
           column(4,  selectInput("business_characteristics", "Select industry", 
                                  selected= "All industries",
                                  choices = c("All industries", "All employment sizes", "All ownerships", "All visible minorities", "All business activities")) ),
           column(4,selectInput("specific_business_characteristics", "Select specific business characteristic", choices = NULL,selected = "All Industries") )),

  tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Canadian Survey on Business Conditions - Q4 2022 </span>\');
      })
     ')),
  tabItems(
  tabItem(tabName = "obstacles",
          fluidRow(
            box(width=12, plotOutput("obstacle1")),
            box(width=12, plotOutput("obstacle2"))
          )
  ),
  tabItem(tabName = "obstacles_newQ",

          fluidRow(
            column(11,box(width=12, plotOutput("obstacle1_newQ")))),
          fluidRow(
            column(1, ""),
            column(6, selectInput("obstacles", "Investigate obstacles by industry", choices = list_obstacles)),
            column(5, "")),
          fluidRow(
            column(11, box(width=12, plotOutput("obstacle2_newQ"))),
            column(11, box(width=12, plotOutput("obstacle4_newQ")))
          )
  ),
  tabItem(tabName = "outlook",
          fluidRow(
            column(2,""),
            column(8,""),
            column(2,"")
          ),
          fluidRow(
            box(plotOutput("outlook1"), width=12),
            box(plotOutput("outlook2"), width=12)
           )
  ),
  tabItem(tabName = "expectation",
          uiOutput("cityControls")
  ),
  tabItem(tabName = "labourShortages",
          h2("Labour shortages")
  ),

    tabItem(tabName = "recovery",
            fluidRow(
              column(2,""),
              column(8,
                     radioButtons(
                       "sort", "Sort businesses by --->", 
                       choices = c("Better position", "Same position", "Worse position", 
                                   "Unknown"), 
                       selected = "Better position", 
                       inline = TRUE)),
              column(2, ""),
              box( plotOutput("plot1"), width=12)
             ),
            fluidRow(
              box(plotOutput("plot2"), width=6),
              box(plotOutput("plot3"), width=6))
            
    ),
    
    tabItem(tabName = "debt",
            fluidRow(
              column(2,""),
              column(8,
                     radioButtons(
                       "sort_debt", "Sort by debt tolerance --->", 
                       choices = c("Cannot take on more debt", "Can take on more debt", "Unknown"), 
                       selected = "Cannot take on more debt", 
                       inline = TRUE)),
              column(2, ""), box(width=12,plotOutput("plot_debt3"))),
            fluidRow(
              box(width=6, plotOutput("plot_debt1")),
              box(width=6, plotOutput("plot_debt2"))
                       )
    ),
    tabItem(tabName = "pressures",
            fluidRow(
              box(width=12, plotOutput("pressure1")),
              box(width=12, plotOutput("pressure2"))
            )
    ),
  
  tabItem(tabName = "talents_newQ",
          fluidRow(
            column(12, box(width=12, plotOutput("talent1_newQ")))
          )
  ),
    tabItem(tabName = "challenges",
            h2("Supply chain challenges tab content")
    ),
    tabItem(tabName = "digital",
            h2("Digital tab content")
    ),
    tabItem(tabName = "strategy",
            h2("Business strategy tab content"))
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = tags$img(src='Logo.PNG', height = '60', width ='200')),
  sidebar,
  body
)
