#==========================================================================
# Topic : Shiny
# Date : 2020. 12. 27.
# Author : Jeong Wook Lee
#==========================================================================


#==========================================================================
# Load packages
#==========================================================================

sapply(c('dplyr','shiny','shinydashboard','ggplot2', 'shinycssloaders', 'DT'), require, character.only = T)


#==========================================================================
# Claim UI
#==========================================================================


ui = dashboardPage(
    dashboardHeader(
        title = "Flex Board Analysis"
    ),
    
    dashboardSidebar(
        
        # Search bar
        sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
        
        sidebarMenu(style = "position: fixed; overflow: visible;",
                    
                    # 1. Caution bar
                    menuItem('Caution', tabName = 'Caution', icon = icon('chalkboard-teacher') ),
                    
                    # 2. Load bar
                    menuItem('Load and View Data', tabName = 'data', icon = icon('save')
                             
                             # menuSubItem("Load", tabName = "data_load", icon = icon('save')),
                             # menuSubItem("Plot", tabName = "data_plot", icon = icon('table'))
                    ),
                    
                    
                    # 3. Partition bar
                    menuItem('Partition', tabName = 'Partition', icon = icon('table') ),
                    
                    
                    # Visualizing data bar
                    menuItem('Visualizing data', tabName = 'visual', icon = icon('chalkboard-teacher') )
                    
                    
                    
                    
        )
    ),
    
    dashboardBody(
        
        
        # Body 
        tabItems(
            
            # 1. Caution Body
            tabItem(tabName = 'Caution',
                    
                    h4('1. All variables should be numeric except for target variable'),
                    h4('2. It is allowed to binary target varialbe'),
                    h4('3. All rows including "NA" are removed in all analysis '),
                    h4('4. Oversampling : Smote -> Tomek'),
                    h4('5. For Tomek Sample, Lift Chart is not showed')
            ),
            
            # 2. Load Body
            tabItem(tabName = 'data',
                    h3("Loading File"),
                    
                    
                    
                    fluidRow(
                        
                        box(width=3, uiOutput("choose_file")),
                        
                        tabBox(width=5, 
                               tabPanel("CSV",
                                        fileInput("file1", "Choose CSV File",  
                                                  multiple = FALSE,
                                                  accept = c("text/csv",
                                                             "text/comma-separated-values,text/plain",
                                                             ".csv")),
                                        
                                        checkboxInput("header", "Header", TRUE),
                                        
                                        actionButton(inputId = "LoadFile.CSV",   
                                                     label="Submit",
                                                     color="primary",
                                                     style="bordered",
                                                     block=TRUE) 
                               ),
                               
                               tabPanel("Xlsx",
                                        fileInput("file", "Choose Xlsx File", accept = c(".xlsx")),
                                        uiOutput("choose_Y1"),     
                                        numericInput("SR", "Start Row", 1, 1, 10, 1),  
                                        actionButton(inputId = "LoadFile",
                                                     label="Submit",
                                                     color="primary",
                                                     style="bordered",
                                                     block=TRUE)
                               )
                        ),
                        
                        # Table output
                        mainPanel(
                            tabsetPanel(type="tabs",  
                                        tabPanel("Table", 
                                                 dataTableOutput("DT0"), 
                                                 icon=icon("server")),
                                        
                                        navbarMenu("Summary",icon=icon("archive"),  
                                                   tabPanel("Type1", verbatimTextOutput("check.str0")),
                                                   tabPanel("Type2", htmlOutput("check.str1")) )))
                    )
            ),
            
            # 3. Partition Body
            
            tabItem(tabName = 'Partition',
                    
                    fluidRow(
                        tabBox(
                            tabPanel("Step 1. Variable",
                                     uiOutput("choose_Y2"),
                                     uiOutput("choose_Y3")),
                            tabPanel("Step 2. SMOTE",
                                     numericInput("NK",   "Number of Neighbour", 3, 1, 10, 1),
                                     numericInput("PO",   "Perc.over", 250, 1, 5000, 1),
                                     numericInput("PC",   "Perc.under", 160, 1, 5000, 1),
                                     numericInput("Seed", "Seed Value", 10, 1, 5000, 1)),
                            
                            tabPanel("Step 3. Proportion",
                                     sliderInput("PB",    "Prob.", 0, 1, 0.66))),
                        
                        mainPanel(
                            tabsetPanel(type = "tabs",
                                        tabPanel("Variable", icon=icon("archive"), verbatimTextOutput("response"),
                                                 verbatimTextOutput("check.str2"),
                                                 verbatimTextOutput("check.str3")),
                                        
                                        navbarMenu("Sample", icon=icon("box"),
                                                   tabPanel("Training Data Without Oversampling", dataTableOutput("Trafile"),
                                                            verbatimTextOutput("check.str4")),
                                                   
                                                   tabPanel("Training Data With Oversampling",    dataTableOutput("Trafile2"),
                                                            verbatimTextOutput("response1"), 
                                                            verbatimTextOutput("check.str5")),
                                                   
                                                   tabPanel("Test Data",                          dataTableOutput("Tedfile"),
                                                            verbatimTextOutput("check.str6"))
                                        ),
                                        
                                        navbarMenu("Tomek Sample", icon=icon("braille"),
                                                   tabPanel("Training Data",                      dataTableOutput("Trafile1"),
                                                            verbatimTextOutput("check.str7")),
                                                   
                                                   tabPanel("Test Data",                          dataTableOutput("Tedfile1"),
                                                            verbatimTextOutput("check.str8")))
                            )))
                    
            ),
            
            # 3. Plot Body
            tabItem(
                tabName = 'data_plot',
                uiOutput('draw_colname'),
                actionButton('draw','Draw Plot'),
                plotOutput('plot_selected')
            )
        )
        
        
        
    )
    
)

