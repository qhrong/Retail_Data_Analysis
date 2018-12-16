library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(sp)
library(maps)
library(rgeos)
library(maptools)
library(leaflet)
library(magrittr)
library(lubridate)
library(tidyverse)
library(wordcloud)
library(wordcloud2)
library(tidytext)
library(tm)
library(cluster)
library(plotly)
library(benford.analysis)

data_retail <- read.csv("new_data.csv")
#Data process for mapping
levels(data_retail$Country)[levels(data_retail$Country)=="United Kingdom"] <- "UK"
levels(data_retail$Country)[levels(data_retail$Country)=="EIRE"] <- "Ireland"
levels(data_retail$Country)[levels(data_retail$Country)=="RSA"] <- "South Africa"


# Define UI for application 
ui <- dashboardPage(
  skin = "black",
  
  # Application title
  dashboardHeader(title="Retail Data Analysis"),
  
  # Define sidebars
  dashboardSidebar(
    sidebarMenu(
      
      id = "sidebarmenu",
      #The first sidebar, "welcome" and data source
      menuItem("Welcome", tabName = "Welcome", icon = icon("home", lib = "glyphicon")),
      #The second sidebar, world map and region map
      menuItem("Sales Map", tabName = "Map", icon = icon("map-marker", lib = "glyphicon")),
      #The third sidebar, time trend
      menuItem("Time Trend", tabName = "Time", icon=icon("calendar",'ib="glyphicon')),
      #The forth sidebar, text mining 
      menuItem("Text Mining", tabName = "Text", icon = icon("font", lib = "glyphicon")),
      #The fifth sidebar, RFM Analysis
      menuItem("Marketing Analysis", 
               tabName = "Marketing", icon = icon("search", lib = "glyphicon"),
               menuItem("RFM Analysis",
                        tabName = "RFM",
                        icon = icon("shopping-cart", lib = "glyphicon")
               ),
               menuItem("Clustering",
                        tabName = "Clustering",
                        icon = icon("tasks", lib = "glyphicon"))),
      #The sixth sidebar, other EDA
      menuItem("Distribution Test", tabName = "Distribution", icon = icon("file", lib = "glyphicon"))
    )
  ),
  
  # Define sidebar's contents  
  dashboardBody(
    tabItems(
      #First tab
      tabItem(
        tabName = "Welcome",
        fluidRow(column(6,
                        box(solidHeader = TRUE, status = "success",width=12,
                            title = "Overview",
                            h4("In this Shiny app, users can be directed to:", style = "font-family: 'Helvetica',"),
                            h5("  1) Sales Map"),
                            h5("  2) Time Trend"),
                            h5("  3) Items Description"),
                            h5("  4) Marketing Analysis: RFM and Clustering"),
                            h5("  5) Other EDA")),
                        box(solidHeader = TRUE, status = "success",width=12,
                            title = "About the Data",
                            h4("Description: This is a transnational data set which contains all the transactions occurring between 01/12/2010 and 09/12/2011 for a UK-based and registered non-store online retail. The company mainly sells unique all-occasion gifts. Many customers of the company are wholesalers.",
                               style = "font-family: 'Helvetica',"),
                            h4("Acknowledgement: Per the UCI Machine Learning Repository, this data was made available by Dr Daqing Chen, Director: Public Analytics group. chend '@' lsbu.ac.uk, School of Engineering, London South Bank University, London SE1 0AA, UK.",
                               style = "font-family: 'Helvetica',")
                        )),
                 column(6,
                        widgetUserBox(
                          title = "Qianhui Rong",
                          subtitle = "Developer",
                          type = 2, width = 12,
                          src = "https://sg.fiverrcdn.com/photos/114254141/original/641be0449baef4f6d1fe5fd6ab1f97712789ec66.png",
                          color = "yellow",
                          "MSSP Student @ Boston University"),
                        div(style="display: inline-block",
                            img(src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQSJnmBdGdZK_a23oXEYnKX-EDDM9hAO-BS6CdGr5Js7nOCGSSL", 
                                height=300, width=400)))
        )
      ), #End of first tab definition
      
      #Second Tab
      tabItem(
        tabName = "Map",
        fluidRow(
          column(12,
                 box(title = "Worldwide Total Sale Amount Group By Country",width = NULL, solidHeader = TRUE,
                     leafletOutput("worldmap", width = "100%", height = 400)), #Leaflet output 
                 selectInput("select1","Select a Country:", choices = unique(data_retail$Country),
                             # c("Australia","Austria","Bahrain","Belgium",
                             #                                          "Brazil","Canada","Cyprus","Czech Republic",
                             #                                          "Denmark","Ireland","Finland","France",
                             #                                          "Germany","Greece","Hong Kong","Iceland",
                             #                                          "Israel","Italy","Japan","Lebanon",
                             #                                          "Lithuania","Malta","Netherlands","Norway",
                             #                                          "Poland","Portugal","South Africa","Saudi Arabia",
                             #                                          "Singapore","Spain","Sweden","Switzerland",
                             #                                          "United Arab Emirates","UK" ,"USA"),
                             selected = "UK"),
                 box(title = "Data of Selected Country",tableOutput("table1"))
          ))
      ), #End of second tab definition
      
      #Third Tab
      tabItem(
        tabName = "Time",
        fluidPage(
          fluidRow(
            column(12,
                   boxPlus(title = "General Trend Plot",footer = "This plot shows an increasing trend starting from Sept.2011.",
                           plotOutput("generalplot")),
                   box(plotOutput("timeplot"),title = "Sales amount in time scale"),
                   selectInput("timeselect",label = "Choose a time scale for plotting",
                                choices = c("Month","Day","Date","Hour"),multiple = FALSE))
          )
        )
      ), #End of third tab definition
      
      #Forth Tab
      tabItem(
        tabName = "Text",
        fluidRow(
          column(6,
                 sliderInput("freq",
                             "Minimum Frequency:",
                             min = 10,  max = 50, value = 15),
                 sliderInput("max",
                             "Maximum Number of Words:",
                             min = 1,  max = 300,  value = 100))), 
        mainPanel(
          plotOutput("wordcloud"))
      ), #End of forth tab definition
      
      #Fifth Tab
      tabItem(
        tabName = "RFM",
        fluidRow(
          column(12,
                 box(plotOutput("Rplot"),title="Recency Plot"),
                 box(plotOutput("Fplot"),title="Frequency Plot"),
                 box(plotOutput("Mplot"),title="Monetary Plot")))),
      
      tabItem(
        tabName = "Clustering",
        fluidRow(
          column(12,
                 plotlyOutput("KMeanplot"),
                 sliderInput("kmean","Number of Groups for KMeans:", min=2,max=10,value = 2)
          ))
      ), #End of fifth tab definition
      
      #Sixth Tab
      tabItem(
        tabName = "Distribution",
        fluidPage(fluidRow(
          column(12,
                 box(title="Benford Analysis for Unit Price",plotOutput("btfplot")),
                 box(title="Benford Analysis for Quantity",plotOutput("btfplot2"))))
        ),
        mainPanel(
          h4("We can see from Benford Analysis plots that both unit price and quantity don't follow Benford Distribution and the difference is large.")
        )
      ) #End of sixth tab definition 
      
    )
  )
)#End of UI design



# Define server logic required to draw a histogram
server <- function(input,output){
  
  #Worldmap for the second tab 
  output$worldmap <- renderLeaflet({
    
    #Load Worldmap Data
    world <- maps::map("world", fill=TRUE, plot=FALSE)
    world_map <- map2SpatialPolygons(world, sub(":.*$", "", world$names))
    world_map <- SpatialPolygonsDataFrame(world_map,
                                          data.frame(country=names(world_map), 
                                                     stringsAsFactors=FALSE), 
                                          FALSE)
    
    data_retail %>% select(Country,amount_spend) %>% group_by(Country) %>% 
      summarise(total=sum(amount_spend)) -> data_twocol
    target <- subset(world_map, country %in% data_twocol$Country)
    data_twocol <- as.data.frame(data_twocol)
    colnames(data_twocol)[1] <- "country"
    data_map<-sp::merge(target,data_twocol,
                        by="country",sort=FALSE,
                        duplicateGeoms =TRUE,all.x=FALSE)
    pal <- colorNumeric(
      palette = "RdYlBu",
      domain = data_map$total
    )
    
    leaflet(data_map) %>% 
      addTiles() %>% 
      addPolygons(data=data_map,
                  fillColor = ~pal(data_map$total),
                  weight = 0.5,
                  opacity = 2,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.5) %>% 
      addLegend(pal = pal, values = data_map$total, 
                opacity = 0.7, 
                title = "Total Sales Amount by Country",
                labFormat = labelFormat(prefix = "₤"),
                position = "bottomright") %>% 
      fitBounds(-168, 80, 168, -65)   
  }) #End of worldmap for second tab
  
  #Datatable for the second tab 
  output$table1 <- renderTable({
    tabledata <- data_retail[data_retail$Country == input$select1,]
    out_tbl <- matrix(c(sum(tabledata$amount_spend),round(length(unique(tabledata$CustomerID)))),nrow = 1)
    out_tbl <- as.data.frame(out_tbl)
    colnames(out_tbl) <- c("Total Sales","Customer Number")
    rownames(out_tbl) <- c(input$select1)
    out_tbl
  }) #End of datatable for the second tab
  
  #General trend plot for the third tab
  output$generalplot <- renderPlot({
    data_retail %>%
      group_by(Date) %>%
      summarise(revenue = sum(amount_spend)) %>%
      ggplot(aes(x=as.Date(Date),y=revenue))+geom_line()+
      geom_smooth(method = 'auto', se = FALSE)+
      labs(x = 'Date', y = 'Revenue', title = 'Revenue vs. Date')
  })
  
  #Time scale plot for the third tab
  output$timeplot <- renderPlot({
    if(input$timeselect=="Month"){
      data_retail %>%
        group_by(Month) %>%
        summarise(revenue = sum(amount_spend)) %>%
        ggplot(aes(x=Month,y=revenue,fill=Month))+geom_col(alpha=0.8)+
        labs(x = 'Month', y = 'Revenue', title = 'Revenue vs. Month')+
        scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12"))+
        guides(fill=FALSE)
    }
    if(input$timeselect=="Day"){
      data_retail %>%
        group_by(Day) %>%
        summarise(revenue = sum(amount_spend)) %>%
        ggplot(aes(x=Day,y=revenue,fill=Day))+geom_col(alpha=0.8)+
        labs(x = 'Day in the Week', y = 'Revenue', title = 'Revenue vs. Day')+
        scale_x_discrete(limits=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))+ 
        guides(fill=FALSE)
    }
    if(input$timeselect=="Date"){
      data_retail %>%
        group_by(DateofMonth) %>%
        summarise(revenue = sum(amount_spend)) %>%
        ggplot(aes(x=DateofMonth,y=revenue,fill=DateofMonth))+geom_col(alpha=0.8)+
        labs(x = 'Date in the Month', y = 'Revenue', title = 'Revenue vs. Date')+
        guides(fill=FALSE)
    }
    if(input$timeselect=="Hour"){
      data_retail %>%
        group_by(Hour) %>%
        summarise(revenue = sum(amount_spend)) %>%
        ggplot(aes(x=Hour,y=revenue,fill=Hour))+geom_col(alpha=0.8)+
        labs(x = 'Hour in the Day', y = 'Revenue', title = 'Revenue vs. Hour')+
        scale_x_discrete(limits=c("6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"))+ 
        guides(fill=FALSE)
    }
  }) #End of plots for the third tab
  
  
  #Wordclouds for the forth tab
  output$wordcloud <- renderPlot({
    
    #Untoken description
    item_desc <- data_retail[,4]
    names(item_desc) <- c("text")
    item_desc <- as.data.frame(item_desc)
    item_desc$text <- as.character(item_desc$item_desc) 
    item_desc$text <- removeNumbers(item_desc$text) #remove numbers 
    item_desc <- unnest_tokens(tbl = item_desc, input = text,output = word)
    
    item_desc %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = input$max,
                     min.freq = input$freq,
                     random.order=FALSE, rot.per=0.35,
                     colors=brewer.pal(8, "Dark2")))
  })  #End of wordcloud for the forth tab
  
  #Plots for the fifth tab
  RFM <- data_retail %>% 
    group_by(CustomerID) %>% 
    summarise(Recency=as.numeric(as.Date("2012-01-01")-max(as.Date(Date))),
              Frequency=n_distinct(InvoiceNo), 
              Monetary= round(sum(amount_spend)/n_distinct(InvoiceNo),2))
  
  #Rplot
  output$Rplot <- renderPlot({
    RFM_1 <- ggplot(RFM)+aes(x=Recency)+geom_histogram(binwidth = 5)
    RFM_1
  })
  #Fplot
  output$Fplot <- renderPlot({
    RFM_2 <- ggplot(RFM)+aes(x=Frequency)+geom_histogram(binwidth = 5)
    RFM_2
  })  
  #Mplot
  output$Mplot <- renderPlot({
    RFM_3 <- ggplot(RFM)+aes(x=Monetary)+geom_histogram(binwidth = 10)+coord_cartesian(xlim = c(0,10000)) #Zoom into 0~10000 to have a closer look.
    RFM_3
  })   
  #KMeans
  output$KMeanplot <- renderPlotly({
    RFM$Recency <- log(RFM$Recency+1)
    RFM$Frequency <- log(RFM$Frequency+1)
    RFM$Monetary <- log(RFM$Monetary+1)
    km <- kmeans(x=select(RFM,Recency,Frequency,Monetary),input$kmean)
    RFM %<>% mutate(cluster=as.factor(km$cluster))
    plot_ly(RFM, x = ~Recency, y = ~Frequency, z = ~Monetary, color = ~cluster,size =1) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'Recency'),
                          yaxis = list(title = 'Frequency'),
                          zaxis = list(title = 'Monetary')))
  }) #End of plots for the fifth tab
  
  #Distribution plot for the sixth tab
  output$btfplot <- renderPlot({
    bfd.Price <- benford(data_retail$UnitPrice)
    plot(bfd.Price)
  })
  
  output$btfplot2 <- renderPlot({
    bfd.Quant <- benford(data_retail$Quantity)
    plot(bfd.Quant)
  }) #End of plots for the sixth tab
  
}

# Run the application 
shinyApp(ui = ui, server = server)
