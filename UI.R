library(ggvis)
library(shiny)
library(DT)
library(shinythemes)

currmonth <- format(Sys.Date(),"%B %Y")


format(Sys.Date(),"%B %Y")


shinyUI(fluidPage(theme=shinytheme("flatly"),
  
  titlePanel("Monthly Transaction Detail Report"),
  downloadButton("downloadData", 'Download All Data'),
  
  
  
  mainPanel(tabsetPanel(type="tab",
                        tabPanel("MTD Summary",
                                 column(width=12,
                                        selectInput("Month1",
                                                           "Month Select",
                                                          choices=levels(TRAN$Month),
                                                    selected="June 2016",
                                                
                                                   #        selected=currmonth,                                                    selectize=T,
                                                    multiple=T)
                                 ),
                                 DT::dataTableOutput("table1")
                                       
      
                        ),
                        tabPanel("Daily Tracking",
                                 column(width=6,
                                        selectInput("Month2",
                                                    "Month Select",
                                                    choices=levels(TRAN$Month),
                                                    selected = "June 2016",
                                                 #   selected=currmonth,
                                                    selectize=T,
                                                    multiple=T)
                                        ),
                                 column(width=6,
                                        selectInput("Office1",
                                                    "Office Select",
                                                    choices=levels(TRAN$PostedClientCode),
                                                    selected="Knoxville",
                                                    selectize=T,
                                                    multiple=T
                                                    )),
                                 DT::dataTableOutput("table2")
                                 
                                 
                                 
                                 
                                 ),
                        tabPanel("Dollars Graph",
                                 fluidRow(column(width=12,
                                        selectInput("officina",
                                                    "Office Select",
                                                    choices=levels(Summary$PostedClientCode),
                                                    selected="Knoxville",
                                                    selectize=T,
                                                    multiple=T
                                        ))),
                                 fluidRow(column(width=12,
                                 ggvisOutput("theplot"),
                                 uiOutput("plotcontrol")
                                 )))
                    
  ),
  textOutput("counter")
  )))

