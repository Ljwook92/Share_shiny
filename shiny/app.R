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




# 2. Load Body server -----------------------------------------------------

server = function(input,output){
    
    Load.Sheet <- reactive({    
        
        req(input$file)  
        
        inFile    <- input$file
        
        SheetName <- excel_sheets(path=inFile$datapath)
        
    })
    
    # Choose_file
    output$choose_file <- renderUI({
        
        radioButtons("choose_file", "File Type", c("CSV", "XLSX"), "CSV", inline=TRUE)  
        
    })
    
    # Choose_Y1
    output$choose_Y1 <- renderUI({
        
        selectInput("SN", "Sheet Name", Load.Sheet())       
        
    })
    
    # Load.Orig
    Load.Orig <- reactive({             
        
        
        if(input$choose_file=="CSV"){
            
            validate(
                need(try(input$LoadFile.CSV), "")   
            )
            
            inFile   <- isolate(input$file1)
            
            CSV.DATA <- read.csv(inFile$datapath, header = input$header)
            
            DATA     <- CSV.DATA
            
        }else if(input$choose_file=="XLSX"){
            
            validate(
                need(try(input$LoadFile), "") 
            )
            
            inFile     <- isolate(input$file)                     
            excel.data <- read_excel(inFile$datapath, sheet=isolate(input$SN), skip=isolate(input$SR-1))  
            rename     <- make.names(names(excel.data), unique=TRUE)
            colnames(excel.data) <- rename
            
            DATA       <- excel.data
        }
        
    })
    
    # DT0
    output$DT0 <- renderDataTable({
        
        datatable(Load.Orig())
        
    })
    
    # Check.str0
    output$check.str0 <- renderPrint({
        
        my.skim <- skim_with(numeric = sfl(hist = NULL), character = sfl(length))
        my.skim(Load.Orig()) 
        
    })
    
    # Check.str1
    output$check.str1 <- renderUI({
        
        DATA    <- Load.Orig()
        Profile <- print( dfSummary(DATA), omit.headings = FALSE, method="render" )  
        Profile
        
    })
    
    
    
    # 3. Partition server ---------------------------------------------------------------
    
    output$choose_Y2 <- renderUI({ 
        selectInput("RV", "Target", c("", names(Load.Orig())))  
    })
    
    output$choose_Y3 <- renderUI({
        req(input$RV)
        
        DATA <- data.frame( Load.Orig() )
        DATA %<>% dplyr::select(input$RV, everything()) %>% dplyr::select(-1)     
        checkboxGroupInput("CVT", "Predictor (Convert Type)", names(DATA), selected = "")  
    })
    
    Response <- reactive({
        req(input$RV)      
        
        DATA <- data.frame(  Load.Orig() )                  
        DATA %<>% dplyr::select(input$RV, everything())       
        
        if ( is.numeric(DATA[,1]) == TRUE ) {         
            DATA[,1] <- DATA[,1]
        }
        else { DATA[,1] <- ifelse( DATA[,1]==unique( DATA[,1] )[1], 0, 1 ) }
        
        DATA %<>% mutate_if(is.character, as.numeric) %>%      
            na.omit()
        DATA[,1] <- as.factor(DATA[,1]) 
        
        ori.freq.y <- table(DATA[,1])
        ori.prob.y <- prop.table(ori.freq.y)
        
        # SMOTE
        set.seed(input$Seed)
        DATA1 <- DATA %>% 
            mutate( factor.y = as.factor(ifelse(DATA[,1]==1,"Y","N")) ) %>%     
            dplyr::select(-1)                                                   
        smote <- SMOTE(factor.y~., data=DATA1, k=input$NK, perc.over=input$PO, perc.under=input$PC) 
        
        s.freq.y <- table(smote$factor.y)
        s.prob.y <- prop.table(s.freq.y)
        
        # Tomek Data
        set.seed(input$Seed)
        smote %<>% mutate( num.y = ifelse(factor.y=="Y",1,0) ) %>%      
            dplyr::select(-factor.y)                                             
        
        smote.y <- smote$num.y
        smote.x <- dplyr::select(smote, -num.y)
        Tomek   <- ubTomek(smote.x, smote.y)
        
        t.freq.y <- table(Tomek$Y)
        t.prob.y <- prop.table(t.freq.y)
        
        Tomek.DATA        <- data.frame( y=Tomek$Y, Tomek$X  )
        names(Tomek.DATA) <- c(paste0(input$RV,""), names(Tomek$X))            
        Tomek.DATA[,1]    <- as.factor(Tomek.DATA[,1])                        
        
        table.y <- cbind(
            rbind( round(ori.freq.y), round(ori.prob.y, 2) ),
            rbind( round(s.freq.y),   round(s.prob.y, 2) ),
            rbind( round(t.freq.y),   round(t.prob.y, 2) )
        )
        
        table.y <- data.frame(table.y) 
        colnames(table.y) <- c("Original", "", "SMOTE", "", "Tomek", "")
        rownames(table.y) <- c("Freq.", "Prob.")
        
        list(Null=sum( is.na(DATA) ), table.y=table.y, DATA=DATA, Tomek.DATA=Tomek.DATA)
    })
    
    # SMOTE에서는 factor형이 안 먹히므로 SMOTE -> Tomek 수행 후 원하는 변수들 factor로 변환 
    Convert.DATA <- reactive({
        DATA       <- Response()$DATA 
        Tomek.DATA <- Response()$Tomek.DATA
        
        # After SMOTE, in addition to 0 and 1 for factor(0 and 1), other numeric values are made, 
        # So, it is needed to handle -> round
        if (length(input$CVT)==1) {
            DATA[,input$CVT]       <- as.factor( round(DATA[,input$CVT]) )
            Tomek.DATA[,input$CVT] <- as.factor( round(Tomek.DATA[,input$CVT]) )
        }
        else {
            DATA[,input$CVT]       %<>% mutate_if(is.numeric, round) %>%
                mutate_if(is.numeric, as.factor)
            Tomek.DATA[,input$CVT] %<>% mutate_if(is.numeric, round) %>%
                mutate_if(is.numeric, as.factor)             
        }   
        
        if ( is.numeric(DATA[,1]) == TRUE ) {         
            DATA[,1] <- DATA[,1]
        }
        else { DATA[,1] <- ifelse( DATA[,1]==unique( DATA[,1] )[1], 0, 1 ) }
        DATA[,1] <- as.factor(DATA[,1])  # For Evaluation (Prediction)
        
        list( DATA=DATA, Tomek.DATA=Tomek.DATA )
    })
    
    # Check Response Var. (Original, SMOTE, and Tomek)
    output$response <- renderPrint({
        list(
            MissingValue= Response()$Null,
            Target      = Response()$table.y
            # DATA        = head( Response()$DATA ),
            # Tomek.DATA  = head( Response()$Tomek.DATA )
        )
    })
    
    # input$CVT을 실행했을 때 Original data와 Oversampling data의 유형 확인 
    output$check.str2 <- renderPrint({
        Original.Data <- Convert.DATA()$DATA[,-1]        # Exclude Response Var.
        my.skim <- skim_with(numeric = sfl(hist = NULL), character = sfl(length))
        my.skim(Original.Data) 
    })
    
    output$check.str3 <- renderPrint({
        Tomek.Data <- Convert.DATA()$Tomek.DATA[,-1]     # Exclude Response Var.
        my.skim <- skim_with(numeric = sfl(hist = NULL), character = sfl(length))
        my.skim(Tomek.Data) 
    })
    
    Partition <- reactive({
        DATA <- Convert.DATA()$DATA
        
        ## Partition for Original DATA
        set.seed(input$Seed)
        ind.sample <- sample(c(1:dim(DATA)[1]), dim(DATA)[1]*input$PB)
        trd.sample <- DATA[ind.sample, ] 
        ted.sample <- DATA[-ind.sample, ]
        
        ## Training Data With SMOTE and Tomek
        trd.sample %<>% mutate_if(is.factor, as.character) %>%  # Factor -> Char. -> Num. (Char.변환을 생략하면 정상 변환이 안됨)
            mutate_if(is.character, as.numeric)      
        trd.sample[,1] <- as.factor(trd.sample[,1])  
        
        ori.freq.y <- table(trd.sample[,1])
        ori.prob.y <- prop.table(ori.freq.y)
        
        # SMOTE
        set.seed(input$Seed)
        DATA1 <- trd.sample %>% 
            mutate( factor.y = as.factor(ifelse(trd.sample[,1]==1,"Y","N")) ) %>%      # SMOTE는 종속변수의 형태가 문자형만!
            dplyr::select(-1)                                                    # factor.y가 SMOTE대상이 되므로 원래 종속변수 제거 
        smote <- SMOTE(factor.y~., data=DATA1, k=input$NK, perc.over=input$PO, perc.under=input$PC)  # k=5 :근처 5개의 근접이웃 적용
        
        s.freq.y <- table(smote$factor.y)
        s.prob.y <- prop.table(s.freq.y)
        
        # Tomek Data
        set.seed(input$Seed)
        smote %<>% mutate( num.y = ifelse(factor.y=="Y",1,0) ) %>%      # Tomek : 종속변수와 독립변수의 형태가 수치형!
            dplyr::select(-factor.y)                                             # num.y가 Tomek대상이 되므로 smote에서 생성된 factor.y 제거
        
        smote.y <- smote$num.y
        smote.x <- dplyr::select(smote, -num.y)
        Tomek   <- ubTomek(smote.x, smote.y)
        
        t.freq.y <- table(Tomek$Y)
        t.prob.y <- prop.table(t.freq.y)
        
        trd.Tomek <- data.frame( y=Tomek$Y, Tomek$X  )
        names(trd.Tomek ) <- c(paste0(input$RV,""), names(Tomek$X))            # 추후 분석에서 원래 종속 변수이름 사용해야 하므로..
        trd.Tomek[,1]    <- as.factor(trd.Tomek[,1])                           # For Evaluation (Prediction) : Tomek과정에서 기존의 종속변수가 삭제되고 똑같은 종속변수가 새로 만들어졌으므로.. 
        
        table.y <- cbind(
            rbind( round(ori.freq.y), round(ori.prob.y, 2) ),
            rbind( round(s.freq.y),   round(s.prob.y, 2) ),
            rbind( round(t.freq.y),   round(t.prob.y, 2) )
        )
        
        table.y <- data.frame(table.y) 
        colnames(table.y) <- c("Original", "", "SMOTE", "", "Tomek", "")
        rownames(table.y) <- c("Freq.", "Prob.")
        
        ## SMOTE에서는 facctor형이 안 먹히므로 SMOTE -> Tomek 수행 후 원하는 변수들 factor로 변환
        if (length(input$CVT)==1) {
            trd.Tomek[,input$CVT] <- as.factor( round(trd.Tomek[,input$CVT]) )
            trd.sample[,input$CVT] <- as.factor( round(trd.Tomek[,input$CVT]) )          
        }
        else {
            trd.Tomek[,input$CVT]  %<>% mutate_if(is.numeric, round) %>% mutate_if(is.numeric, as.factor) 
            trd.sample[,input$CVT] %<>% mutate_if(is.numeric, round) %>% mutate_if(is.numeric, as.factor)
        }   
        
        ## Partition for Tomek DATA
        set.seed(input$Seed)
        Tomek.DATA <- Convert.DATA()$Tomek.DATA
        y <- unlist( dplyr::select(Tomek.DATA, input$RV) )         # list형으로 인식! -> unlist해줘야 함.
        set.seed(input$Seed)
        ind.CDP <- createDataPartition(y, p=input$PB, list=T)      # p=0.8 : 80%를 Traning data로 분할
        trd.CDP <- Tomek.DATA[ind.CDP$Resample1,]
        ted.CDP <- Tomek.DATA[-ind.CDP$Resample1,]
        
        list(trd.sample=trd.sample, trdTomek.sample=trd.Tomek, table.y=table.y, ted.sample=ted.sample,
             trd.CDP=trd.CDP, ted.CDP=ted.CDP)
    })
    
    # Training Data : Sample
    output$Trafile <- renderDataTable({
        datatable(Partition()$trd.sample)
    })
    
    output$check.str4 <- renderPrint({
        my.skim <- skim_with(numeric = sfl(hist = NULL), character = sfl(length))
        my.skim(Partition()$trd.sample) 
    })
    
    # Training Data : Sample After Tomek
    output$Trafile2 <- renderDataTable({
        datatable(Partition()$trdTomek.sample)
    })
    
    # Check Response Var. of Sample After Tomek (Original, SMOTE, and Tomek)
    output$response1 <- renderPrint({
        Partition()$table.y
    })
    
    output$check.str5 <- renderPrint({
        my.skim <- skim_with(numeric = sfl(hist = NULL), character = sfl(length))
        my.skim(Partition()$trdTomek.sample) 
    })
    
    # Test Data : Smaple
    output$Tedfile <- renderDataTable({
        datatable(Partition()$ted.sample)
    })
    
    output$check.str6 <- renderPrint({
        my.skim <- skim_with(numeric = sfl(hist = NULL), character = sfl(length))
        my.skim(Partition()$ted.sample)
    })
    
    # Training Data : Tomek.Sample (createDataPartion)
    output$Trafile1 <- renderDataTable({
        datatable(Partition()$trd.CDP)
    })
    
    output$check.str7 <- renderPrint({
        my.skim <- skim_with(numeric = sfl(hist = NULL), character = sfl(length))
        my.skim(Partition()$trd.CDP) 
    })
    
    # Test Data : Tomek.Sample (createDataPartion)
    output$Tedfile1 <- renderDataTable({
        datatable(Partition()$ted.CDP)
    })
    
    output$check.str8 <- renderPrint({
        my.skim <- skim_with(numeric = sfl(hist = NULL), character = sfl(length))
        my.skim(Partition()$ted.CDP) 
    })
    
    
    Partition.Var <- reactive({
        if (input$TOP=="Sample Without Oversampling") { 
            trd.DATA <- Partition()$trd.sample 
            ted.DATA <- Partition()$ted.sample 
        }
        else if (input$TOP=="Sample With Oversampling") { 
            trd.DATA <- Partition()$trdTomek.sample 
            ted.DATA <- Partition()$ted.sample 
        }
        else {
            trd.DATA <- Partition()$trd.CDP 
            ted.DATA <- Partition()$ted.CDP
        }
        
        ted.y <- unlist( dplyr::select(ted.DATA, input$RV) ) 
        
        list(trd.DATA=trd.DATA, ted.DATA=ted.DATA, ted.y=ted.y)
    })
    
    
    
    
    
    
}


shinyApp(ui,server)