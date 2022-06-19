library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(dplyr)
library(DT)
library(tidyverse)
library(magrittr)
library(shinycssloaders)
library(ggplot2)
library(plotly)
library(corrplot)
library(caret)
library(stargazer)
library(janitor)
library(scales)
library(mice)

setwd("C:\\Users\\josep\\OneDrive\\Documents\\MBA\\MGMT 590\\Final Dataset- Shiny app\\Final Dataset")

universities <- read_csv("Most-Recent-Cohorts-Institution.csv")
fieldofstudy <- read_csv("FieldOfStudyData.csv")
CWUR <- read_csv("CWUR.csv")

Comb_data <- read_csv("Comb_data.csv")

universities$LATITUDE <- as.numeric(universities$LATITUDE)
universities$LONGITUDE <- as.numeric(universities$LONGITUDE)
universities$ADM_RATE_ALL <- as.numeric(universities$ADM_RATE_ALL)
universities$TUITIONFEE_IN <- as.numeric(universities$TUITIONFEE_IN)
universities$TUITIONFEE_OUT <- as.numeric(universities$TUITIONFEE_OUT)
universities$AGE_ENTRY <- as.numeric(universities$AGE_ENTRY)
universities$FEMALE <- as.numeric(universities$FEMALE)
universities$MARRIED <- as.numeric(universities$MARRIED)
universities$PCT_ASIAN <- as.numeric(universities$PCT_ASIAN)
universities$PCT_WHITE <- as.numeric(universities$PCT_WHITE)
universities$PCT_BLACK <- as.numeric(universities$PCT_BLACK)
universities$PCT_HISPANIC <- as.numeric(universities$PCT_HISPANIC)

universities <- na.omit(universities)

CWUR[is.na(CWUR)] <- 0

df <- merge(x=universities,y=fieldofstudy,by= c("OPEID6", "OPEID6"), all.x=FALSE, all.y=FALSE)

cluster1 <- Comb_data %>% select(world_rank, quality_of_education, research_performance, quality_of_faculty, SAT_AVG)
cluster1$SAT_AVG <- as.numeric(cluster1$SAT_AVG)
imputedValues <- mice(data=cluster1
                      , seed=2016
                      , method="cart" 
                      , m=1           
                      , maxit = 1     
)
cluster1 <- mice::complete(imputedValues,1)
cluster1 <- scale(cluster1)
cluster1 <- data.frame(scale(cluster1))

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title="Find your University"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Home", tabName = "Home"),
                menuItem("Locate your University", tabName = "find"),
                menuItem("Compare Universities", tabName = "school2"),
                menuItem("Find Similar Universities", tabName = "cluster"),
                menuItem("Contribution to Rank of University", tabName = "pred")
    )),
  
  dashboardBody(
    mainPanel(
      tabItems(
        tabItem(tabName = "Home",
                fluidRow(
                  h2("Welcome to University Selection Dashboard"),
                  fluidRow(
                    box(width = 15 , h4("This dashboard will help you compare different university's in the USA.")))
                )),
        
        tabItem(tabName = "find",
                fluidRow(
                  box(width = 4,
                      selectInput("state",label = "Choose the State", choices = c("ALL",unique(df$STABBR)),
                                  selected = "ALL")
                  ),
                  box(width =  4,
                      selectInput("univ_name", label = "Choose the University", choices = c("ALL",unique(sort(df$INSTNM.y))),
                                  selected = "ALL")
                  ),
                  
                  box(width =  4,
                      selectInput("credit", label = "Choose the credit course", c("Bachelor's Degree"=3, "Master's Degree"=5, "Graduation/Certification Certificate"=8,
                                                                                  "Doctoral Degree"=6, "Associate's Degree"=2, "First Professional Degree"=7, "Undergraduate Certificate or Diploma"=1,
                                                                                  "Post-baccalaureate Certificate"=4),
                                  selected = "ALL")
                  )),
                fluidRow(
                  box(width = 6,
                      sliderInput("out_tuition_range", label="Out State - Tuition Range", 
                                  min = 1000, max = 62000, 
                                  value=c(min(df[['TUITIONFEE_OUT']]), max(df[['TUITIONFEE_OUT']])),
                                  sep="", step=5000)
                  ),
                  box(width = 6,
                      sliderInput("in_tuition_range", label="In State - Tuition Range", 
                                  min = 1000, max = 62000,
                                  value= c(min(df[['TUITIONFEE_IN']]), max(df[['TUITIONFEE_IN']])),
                                  sep ="", step=5000)
                  )),
                
                fluidRow(
                  box(width = 12,
                      leafletOutput(outputId = "mymap"))),
                fluidRow(
                  box(width = 12,
                      dataTableOutput(outputId = "summary_table")))
        ),
        
        tabItem(tabName = "school2",
                h2(HTML("Compare Universities <br/><br/>")),
                fluidRow(
                  column(width=6,selectInput("univ1",label = "Choose the 1st University", choices = c("",unique(sort(Comb_data$institution))),
                                             selected = NULL,multiple = F)),
                  column(width=6,selectInput("univ2", label = "Choose the 2nd University", choices = c("",unique(sort(Comb_data$institution))),
                                             selected = NULL,multiple = F))),
                fluidRow(column(width = 12,dataTableOutput('compare')))),
        
        tabItem(tabName = "cluster", 
                fluidRow(
                  box(width = 6,
                      selectInput("var1", "Variable 1", choices = names(cluster1))
                  ),
                  box(width = 6,
                      selectInput("var2", "Variable 2", choices = names(cluster1),
                                  selected = "SAT_AVG")
                  )),
                fluidRow(
                  box(
                    width = 7,
                    selectInput("cu_school", "School", choices = sort(Comb_data$institution))
                  ),
                  box(
                    width =5,
                    sliderInput('num_k', "Pick number of Clusters", value = 8, min = 4, max = 10)
                  )),
                fluidRow(
                  box(width = 12,
                      dataTableOutput("clu_table")
                  )),
                fluidRow(
                  box(width = 12,
                      plotOutput("clu_plot"))
                )),
        
        ####### Prediction ########
        tabItem(tabName = "pred", 
                fluidPage(
                  fluidRow(h2("See How features contribute to Rank of the university")),
                  fluidRow(box(
                    checkboxGroupInput(
                      "SelectX",
                      label = "Select variables:",
                      choices = c("quality_of_education","national_rank", "quality_of_faculty","alumni_employment","research_performance")
                      ),
                    solidHeader = TRUE,
                    width = 6,
                    status = "primary",
                    title = "X variable"
                  ),
                  box(
                    selectInput("SelectY", label = "Select variable to predict:", choices = c("world_rank")),
                    solidHeader = TRUE,
                    width = 6,
                    status = "primary",
                    title = "Y variable"
                  ))),
                fluidRow(dataTableOutput("model_results")))
    ))))

# Define server logic required to draw a map
server <- function(input, output, session) {
  ### Find school ###
  data_input <- reactive({df %>%
      filter(TUITIONFEE_OUT>= input$out_tuition_range[1]) %>%
      filter(TUITIONFEE_OUT<= input$out_tuition_range[2]) %>%
      filter(TUITIONFEE_IN>= input$in_tuition_range[1]) %>%
      filter(TUITIONFEE_IN<= input$in_tuition_range[2]) %>%
      filter(CREDLEV == input$credit) %>%
      filter(if(input$univ_name!="ALL")INSTNM.y==input$univ_name else TRUE) %>%
      filter(if(input$state!="ALL")STABBR==input$state else TRUE) %>%
      distinct(INSTNM.y,CITY,STABBR,ZIP,INSTURL,ADM_RATE_ALL,TUITIONFEE_OUT,AGE_ENTRY,PCT_HISPANIC,PCT_BLACK,PCT_WHITE,PCT_ASIAN,.keep_all= TRUE)%>%
      select(INSTNM.y,CITY,STABBR,ZIP,INSTURL,ADM_RATE_ALL,TUITIONFEE_OUT,AGE_ENTRY,PCT_HISPANIC,PCT_BLACK,PCT_WHITE,PCT_ASIAN,LONGITUDE,LATITUDE, CREDDESC)%>%
      group_by(INSTNM.y)
  }) 
  data_input_ordered <- reactive({data_input()[order(match(data_input()$STABBR)),]
  })
  labels <- reactive({
    paste("<p>", data_input_ordered()$STABBR,"</p>",sep="")
  })
  output$mymap <- renderLeaflet({data_input() %>% leaflet() %>%
      addProviderTiles(('OpenStreetMap')) %>%
      addCircleMarkers(radius=0.001,
                       color='blue',
                       popup=~paste0(INSTNM.y,
                                     "<br/>",
                                     CITY))})
  output$summary_table <- renderDataTable(data_input())
  
  ### compare ###
  output$compare <- renderDataTable({ 
    validate(need(input$univ1 != "", 'Please Select the 1st University'))
    validate(need(input$univ2 != "", 'Please Select the 2nd University'))
    validate(need(input$univ1 != input$univ2, 'Please Select Different Universities'))
    df_1<-Comb_data %>% 
      filter(institution==input$univ1) %>% 
      mutate(PCT_BLACK=paste0(substr(PCT_BLACK,1,5),"%"),
             PCT_WHITE=paste0(substr(PCT_WHITE,1,5),"%"),
             PCT_ASIAN=paste0(substr(PCT_ASIAN,1,5),"%"),
             PCT_HISPANIC=paste0(substr(PCT_HISPANIC,1,5),"%"),
             FEMALE=percent(as.numeric(FEMALE),2),
             ADM_RATE=percent(as.numeric(ADM_RATE_ALL),2),
             TUITIONFEE_IN=dollar(TUITIONFEE_IN),
             TUITIONFEE_OUT=dollar(TUITIONFEE_OUT)) %>%
      select(`World Rank`=world_rank,
             `Quality of Education`=quality_of_education,
             `Alumni Employment`=alumni_employment,
             `Faculty Rank`=quality_of_faculty,
             `Research Performance`=research_performance,
             `Admission Rate`=ADM_RATE,
             `Avg SAT Score`=SAT_AVG,
             `In-State Tuition Fee`=TUITIONFEE_IN,
             `Out-STate Tuition Fee`=TUITIONFEE_OUT,
             `Female Percentage`=FEMALE,
             `Percentage of White Race`=PCT_WHITE,
             `Percentage of Black Race`=PCT_BLACK,
             `Percentage of Hispanic`=PCT_HISPANIC,
             `Percentage of Asian`=PCT_ASIAN) %>%
      t() %>% 
      data.frame() %>% 
      rownames_to_column() %>%
      row_to_names(row_number = 1)
    names(df_1)<-c("Features",input$univ1)
    
    df_2<-Comb_data %>% 
      filter(institution==input$univ2) %>% 
      mutate(PCT_BLACK=paste0(substr(PCT_BLACK,1,5),"%"),
             PCT_WHITE=paste0(substr(PCT_WHITE,1,5),"%"),
             PCT_ASIAN=paste0(substr(PCT_ASIAN,1,5),"%"),
             PCT_HISPANIC=paste0(substr(PCT_HISPANIC,1,5),"%"),
             FEMALE=percent(as.numeric(FEMALE),2),
             ADM_RATE=percent(as.numeric(ADM_RATE_ALL),2),
             TUITIONFEE_IN=dollar(TUITIONFEE_IN),
             TUITIONFEE_OUT=dollar(TUITIONFEE_OUT)) %>%
      select(`World Rank`=world_rank,
             `Quality of Education`=quality_of_education,
             `Alumni Employment`=alumni_employment,
             `Faculty Rank`=quality_of_faculty,
             `Research Performance`=research_performance,
             `Admission Rate`=ADM_RATE,
             `Avg SAT Score`=SAT_AVG,
             `In-State Tuition Fee`=TUITIONFEE_IN,
             `Out-STate Tuition Fee`=TUITIONFEE_OUT,
             `Female Percentage`=FEMALE,
             `Percentage of White Race`=PCT_WHITE,
             `Percentage of Black Race`=PCT_BLACK,
             `Percentage of Hispanic`=PCT_HISPANIC,
             `Percentage of Asian`=PCT_ASIAN) %>%
      t() %>% 
      data.frame() %>% 
      rownames_to_column() %>%
      row_to_names(row_number = 1)
    names(df_2)<-c("Features",input$univ2)
    
    df_compare<-df_1 %>%
      inner_join(df_2)
    
    row.names(df_compare)<-NULL
    df_compare
  },rownames=FALSE)
  
  ### cluster ###
  
  selectedData <- reactive({
    cluster1[, c(input$var1, input$var2)]
  })
  cluster2 <- reactive({
    kmeans(selectedData(), centers = input$num_k)
  })
  cluster_table <- reactive({
    data.frame(Cluster = cluster2()$cluster, Institution = Comb_data$institution)
  })
  cluster_num <- reactive({
    subset[cluster_table()[cluster_table()$Institution == input$clu_school, "Cluster"]]
  })
  output$clu_table <- renderDataTable({
    x<-cluster_table() %>% filter(Institution == input$cu_school)
    cluster_table() %>% filter(Cluster %in% x$Cluster)
  })
  output$clu_plot <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(), col = cluster2()$cluster, pch = 20, cex = 3)
    points(cluster2()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  ### prediction ###
  output$model_results<-renderDataTable({
    x<-CWUR %>%
      select(input$SelectX,input$SelectY)
    model<-lm(world_rank ~ ., data=x)
    matrix_coef <- summary(model)$coefficients  # Extract coefficients in matrix
    y<-data.frame(matrix_coef) %>% rownames_to_column() %>% 
      select(Feature=rowname,
             `Regression Coefficient`=Estimate)
    #names(y)<-c("Feature","Regression Coefficient")
    y

  },rownames=FALSE)
  
  }

# Run the application 
shinyApp(ui = ui, server = server)

