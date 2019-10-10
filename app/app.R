#setwd("~/Desktop/fall2019-proj2--sec2-grp1/output")
#load("data.RData")
load("uniquedata.RData")
#clean <- read.csv("clean.csv")
#install.packages("leaflet.extras")
#install.packages("shinythemes")
#install.packages("plyr")
library(plyr)
library(shiny)
library(shinythemes)
library(leaflet.extras)
library(ggmap)
library(dplyr)
library(leaflet)
library(tigris)
library(RJSONIO)
library(RCurl)
library(tidyverse)
library(maptools)

borough_list<-c("Brooklyn","Manhattan","Queens","Bronx","Staten Island")
cuisine_list<-c("Pizza","Italian","Bakery","Caribbean","Japanese","American","Chinese","Café","Spanish","Latin","Mexican")
cuisine_list1<-c("Pizza","Italian","Bakery","Caribbean","Japanese","American","Chinese","Café","Spanish","Latin","Mexican")
cusine = c("American","Chinese" ,"Café","Pizza","Latin","Mexican" ,"Italian","Caribbean","Japanese","Bakery","Spanish" )  
boro = c("Brooklyn","Manhattan","Queens","Bronx","Staten Island")

ui <- bootstrapPage(theme = shinytheme("cyborg"),
                    navbarPage(title="New York City Restaurants",
                               tabPanel("Zipcode Map Visualization",
                                        div(class="outer",includeCSS("style_1.css"),
                                            
                                            tags$style(type = "text/css", ".outer {position: fixed; top: 50px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                            
                                            tags$head(tags$style(HTML('#controls {background-color: rgba(0,0,0,0.45);}'))),
                                            
                                            leafletOutput("heat_map", width = "100%", height = "100%"),
                                            absolutePanel(top = 80, left = 10,
                                                          
                                                          
                                                          checkboxGroupInput("restaurant_choice", "Restaurant Choice:",
                                                                             c("Chinese" = '1',
                                                                               "Japanese" = '2',
                                                                               "Mexican" = '3',
                                                                               "Pizza" = '4',
                                                                               "Bakery" = '5'
                                                                             )),
                                                          draggable = TRUE
                                            ),
                                            absolutePanel(top = 10, right = 10,
                                                          
                                                          
                                                          checkboxGroupInput("avoid_choice", "Type of Violation:",
                                                                             c("Rat" = '6',
                                                                               "Noxious Gas" = '7'
                                                                               )),
                                                          draggable = TRUE)
                                        )),
                               
                               
                               tabPanel("Borough Map Visualization",
                                        div(class="outer",includeCSS("style_1.css"),
                                            
                                            tags$style(type = "text/css", ".outer {position: fixed; top: 50px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                            
                                            tags$head(tags$style(HTML('#controls {background-color: rgba(0,0,0,0.45);}'))),
                                            
                                            leafletOutput("map", width = "100%", height = "100%"),
                                            absolutePanel(top = 10, left = 10,
                                                          
                                                          
                                                          selectInput("boro", "Borough",
                                                                      choices = boro
                                                          ),
                                                          selectInput("cusine", "Cusine",
                                                                      choices = cusine
                                                          ), draggable = TRUE
                                            )
                                        )),
                               
                               
                               tabPanel("Score Comparison", titlePanel("Score Distribution"),
                                        # Sidebar layout with input and output definitions ----
                                        sidebarLayout(
                                          # Sidebar panel for inputs ----
                                          sidebarPanel(
                                            selectInput('borough1', 'region', 
                                                        borough_list, selected='Brooklyn'),
                                            selectInput('cuisine1', 'cuisine type', 
                                                        cuisine_list1, selected='Pizza')),
                                          # Main panel for displaying outputs ----
                                          mainPanel(
                                            plotOutput(outputId = "plots4", height = "300"),
                                            h6("Notes: lower score indicates better inspection results"),
                                            verbatimTextOutput("Summary1"))),
                                        hr(),
                                        sidebarLayout(
                                          # Sidebar panel for inputs ----
                                          sidebarPanel(
                                            selectInput('borough2', 'region', 
                                                        borough_list, selected='Brooklyn'),
                                            selectInput('cuisine2', 'cuisine type', 
                                                        cuisine_list1, selected='Pizza')),
                                          # Main panel for displaying outputs ----
                                          mainPanel(
                                            plotOutput(outputId = "plots5", height = "300"),
                                            h6("Notes: lower score indicates better inspection results"),
                                            verbatimTextOutput("Summary2")
                                          ))
                               ),
                               
                               tabPanel("Restaurant recommendation",
                                        titlePanel("Top Recommended Restaurants"),
                                        # Sidebar layout with input and output definitions ----
                                        sidebarLayout(
                                          # Sidebar panel for inputs ----
                                          sidebarPanel(
                                            selectInput('borough', 'region', 
                                                        borough_list, selected='Brooklyn')),
                                          # Main panel for displaying outputs ----
                                          mainPanel(
                                            plotOutput(outputId = "plots1", height = "300"))),
                                        hr(),
                                        sidebarLayout(
                                          # Sidebar panel for inputs ----
                                          sidebarPanel(
                                            selectInput('cuisine', 'cuisine type', 
                                                        cuisine_list1, selected='Pizza')),
                                          # Main panel for displaying outputs ----
                                          mainPanel(
                                            plotOutput(outputId = "plots2", height = "300"),
                                            h6("notes: lower score indicates better inspection results"),
                                            plotOutput(outputId = "plots3", height = "300"))),
                                        hr(),
                                        sidebarLayout(
                                          # Sidebar panel for inputs ----
                                          sidebarPanel(
                                            numericInput("top", "Top restaurants to view:", 5)),
                                          # Main panel for displaying outputs ----
                                          mainPanel(
                                            h4("Top restaurants"),
                                            tableOutput("view")))),
                               
                               
                               tabPanel("Contact", div(class="outer",includeCSS("style_1.css"),
                                                       
                                                       tags$style(type = "text/css", ".outer {position: fixed; top: 50px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                                       
                                                       tags$head(tags$style(HTML('#controls {background-color: rgba(0,0,0,0.45);}'))),
                                                       
                                                       plotOutput("plot_nyc")
                               )))
)




server <- shinyServer(
  function(input, output, session){
    
    
    ################################################################################################
    data = uniquedata[,c("DBA","BORO","ZIPCODE","CUISINE.DESCRIPTION","SCORE","Longitude","Latitude")]
    cusine = c("American","Chinese" ,"Café","Pizza","Latin","Mexican" ,"Italian","Caribbean","Japanese","Bakery","Spanish" )  
    boro = c("Brooklyn","Manhattan","Queens","Bronx","Staten Island")
    
    
    data = data%>%filter(ZIPCODE>0)%>%
      filter(!is.na(Longitude ))%>% 
      filter(! is.na(Latitude))
    
    d = reactive({data[which((data$BORO == input$boro)&(data$CUISINE.DESCRIPTION == input$cusine)),]})
    
    d1 = reactive({ddply(d(),~ZIPCODE,summarise,avg = mean(SCORE))})
    #d1 = reactive({
    
    #d1$ZIPCODE = d1()$ZIPCODE
    #d1$avg = d1()$`mean(SCORE)`
    #})
    
    d3 = reactive({d1()[order(d1()$avg),]})
    
    d4 = reactive({left_join(d3(),d())})
    #c = reactive({d4()$avg})
    pal = reactive(colorNumeric( palette = "Blues",domain = d4()$avg))
    #pal = colorNumeric( palette = "Blues",domain = c(9:17))
    ###############################################################################################
    
    
    nycborough <- geojsonio::geojson_read("Borough_Boundaries.geojson", what = "sp")
    nyczipcode <- geojsonio::geojson_read("nyc_zipcode.geojson", what = "sp")
    
    uniquedata_1 <- uniquedata %>%
      dplyr::group_by(ZIPCODE) %>%
      dplyr::mutate(AVG_ZIP = mean(SCORE))  
    
    uniquedata_2 <- uniquedata %>%
      dplyr::group_by(BORO) %>%
      dplyr::mutate(AVG_BORO = mean(SCORE))
    
    uniquedata_merge <- geo_join(nyczipcode, uniquedata_1, by_sp = 'postalCode', by_df = 'ZIPCODE')
    uniquedata_merge_2 <- geo_join(nycborough, uniquedata_2, by_sp = 'boro_name', by_df = 'BORO')
    
    
    #palette of average_zip score layer
    pal_10 <- colorNumeric(palette = "Blues", domain = uniquedata_merge$AVG_ZIP)
    
    
    #palette of average_borough score layer
    pal_20 <- colorNumeric(palette = "Blues", domain = uniquedata_merge_2$AVG_BORO)
    
    uniquedata <- uniquedata[na.omit(uniquedata$Latitude) & na.omit(uniquedata$Longitude),]
    Chinese_A <- uniquedata[uniquedata$CUISINE.DESCRIPTION == "Chinese" & uniquedata$GRADE == "A" & uniquedata$SCORE < 3 & uniquedata$SCORE > 0,]
    Japanese_A <- uniquedata[uniquedata$CUISINE.DESCRIPTION == "Japanese" & uniquedata$GRADE == "A" & uniquedata$SCORE < 3 & uniquedata$SCORE > 0,]
    Mexican_A <- uniquedata[uniquedata$CUISINE.DESCRIPTION == "Mexican" & uniquedata$GRADE == "A" & uniquedata$SCORE < 3 & uniquedata$SCORE > 0,]
    Pizza_A <- uniquedata[uniquedata$CUISINE.DESCRIPTION == "Pizza" & uniquedata$GRADE == "A" & uniquedata$SCORE < 3 & uniquedata$SCORE > 0,]
    Bakery_A <- uniquedata[uniquedata$CUISINE.DESCRIPTION == "Bakery" & uniquedata$GRADE == "A" & uniquedata$SCORE < 3 & uniquedata$SCORE > 0,]
    uniquedata_bad <- uniquedata[uniquedata$GRADE == 'C' & uniquedata$SCORE > 30 & uniquedata$VIOLATION.CODE == "04L",]
    uniquedata_bad_2 <- uniquedata[uniquedata$VIOLATION.CODE == "05B",]
    
    #icon definition
    rat_icon <- makeIcon(iconUrl = "rat.png",
                         iconWidth = 18, iconHeight = 18)
    noxious_icon <- makeIcon(iconUrl = "noxious.png",
                         iconWidth = 18, iconHeight = 18)
    
    Chinese_A$on <- 1
    Japanese_A$on <- 1
    Mexican_A$on <- 1
    Pizza_A$on <- 1
    Bakery_A$on <- 1
    uniquedata_bad$on <- 1
    uniquedata_bad_2$on <- 1
    
    borough_list<-c("Brooklyn","Manhattan","Queens","Bronx","Staten Island")
    cuisine_list<-c("Pizza","Italian","Bakery","Caribbean","Japanese","American","Chinese","Café/Coffee/Tea","Spanish","Latin (Cuban, Dominican, Puerto Rican, South & Central American)","Mexican")
    cuisine_list1<-c("Pizza","Italian","Bakery","Caribbean","Japanese","American","Chinese","Café","Spanish","Latin","Mexican")
    
    output$plots4 <- renderPlot({  
      data12<-uniquedata[which((uniquedata$BORO==input$borough1)&(uniquedata$CUISINE.DESCRIPTION==input$cuisine1)),]
      ggplot(data=data12,aes(x=SCORE)) +
        geom_histogram(aes(y=..density..),fill="deepskyblue") +
        geom_density(col="black")+
        xlab("score") +
        ylab("density") +
        ggtitle("Distribution of scores in selected borough and cuisine type")
    })
    output$Summary1<-renderPrint({
      summary(uniquedata[which((uniquedata$BORO==input$borough1)&(uniquedata$CUISINE.DESCRIPTION==input$cuisine1)),]$SCORE)
    })
    output$plots5 <- renderPlot({  
      data22<-uniquedata[which((uniquedata$BORO==input$borough2)&(uniquedata$CUISINE.DESCRIPTION==input$cuisine2)),]
      ggplot(data=data22,aes(x=SCORE)) +
        geom_histogram(aes(y=..density..),fill="lightgoldenrod1") +
        geom_density(col="black")+
        xlab("score") +
        ylab("density") +
        ggtitle("Distribution of scores in selected borough and cuisine type")
    })
    output$Summary2<-renderPrint({
      summary(uniquedata[which((uniquedata$BORO==input$borough2)&(uniquedata$CUISINE.DESCRIPTION==input$cuisine2)),]$SCORE)
    })
    output$plots1 <- renderPlot({
      count<-c(unname(table(uniquedata$BORO)))
      data1<-data.frame(borough_list,count)
      ggplot(data=data1,aes(x=borough_list,y=count,fill=factor(ifelse(borough_list==input$borough,"Selected","Others")))) +
        geom_bar(stat="identity") +
        scale_fill_manual(name = "borough", values=c("deepskyblue","dodgerblue3")) +
        xlab("borough") +
        ylab("the numbers of restaurants") +
        ggtitle("Restaurants numbers in each borough")
    })
    output$plots2 <- renderPlot({  
      count1<-c(unname(table(uniquedata$CUISINE.DESCRIPTION)))
      data2<-data.frame(cuisine_list1,count1)
      ggplot(data=data2,aes(x=cuisine_list1,y=count1,                       fill=factor(ifelse(cuisine_list1==input$cuisine,"Selected","Others")))) +
        geom_bar(stat="identity") +
        scale_fill_manual(name = "cuisine", values=c("lightgoldenrod1","goldenrod1")) +
        xlab("cuisine type") +
        ylab("the numbers of restaurants") +
        ggtitle("Restaurants numbers of each cuisine type")
    })
    output$plots3 <- renderPlot({  
      data11<-uniquedata[which((uniquedata$BORO==input$borough)&(uniquedata$CUISINE.DESCRIPTION==input$cuisine)),]
      ggplot(data=data11,aes(x=SCORE)) +
        geom_histogram(aes(y=..density..),fill="darkolivegreen3") +
        geom_density(col="black")+
        xlab("score") +
        ylab("density") +
        ggtitle("Distribution of scores in selected borough and cuisine type")
    })
    output$view <- renderTable({
      topdata<-uniquedata[which((uniquedata$BORO==input$borough)&(uniquedata$CUISINE.DESCRIPTION==input$cuisine)),]
      topdata$INSPECTION.DATE<-as.Date(topdata$INSPECTION.DATE,"%m/%d/%Y")
      sortdata<-topdata[order(topdata$INSPECTION.DATE, decreasing = TRUE),c(1,3,4,5,6,8,11)]
      sortdata<-sortdata[!duplicated(sortdata[1:4]),]
      sortdata<-sortdata[order(sortdata$SCORE),-c(6,7)]
      head(sortdata, n = input$top)
    })
    

    
    output$plot_nyc <- renderImage({
      
      filename <- normalizePath(file.path('nyc.jpg'))
      
      
      list(src = filename, width = 1600, height = 875)
    }, deleteFile = FALSE)
  
    
    icon_redefined <- pulseIcons(color = 'pink', heartbeat = 0.6, iconSize = 10)
    
    
    nycounties <- geojsonio::geojson_read("Borough_Boundaries.geojson",
                                          what = "sp")
    
    pal_2 <- colorFactor(palette = "RdBu", levels = levels(nycounties$boro_name))
    
    # split_data <- uniquedata[which((uniquedata$BORO == input$borough10) & (uniquedata$CUISINE.DESCRIPTION == input$restaurant10)), ] 
    
  #  output$borough.heat.map <- renderLeaflet({
  #    withProgress(message = 'Please be patient...',
                   
    output$heat_map <- renderLeaflet({
      first_plot <- leaflet(uniquedata_merge) %>%
        addTiles() %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap, group = "zipcode") %>%
        #   addProviderTiles("Stamen.Watercolor", group = "borough") %>%
        
        
        
        addPolygons(layerId = ~postalCode,
                    stroke = T,
                    weight = 1,
                    fillOpacity = 0.9,
                    color = ~pal_10(AVG_ZIP),
                    highlightOptions = highlightOptions(color = "blue1", opacity = 0.9, weight = 4, fillOpacity = 0.9, bringToFront = TRUE, sendToBack = TRUE),
                    label = sprintf("Borough: <strong>%s</strong><br/>Zip Code: <strong>%s</strong><br/>Average Score: <strong>%g<sup></sup></strong>",
                                    as.character(uniquedata_merge$borough), as.character(uniquedata_merge$postalCode), uniquedata_merge$AVG_ZIP
                    ) %>% lapply(htmltools::HTML),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")
        ) %>%
        
       
        addPulseMarkers(lng = ~Chinese_A[Chinese_A$on == ifelse((1 %in% input$restaurant_choice),1,0),]$Longitude,
                   lat = ~Chinese_A[Chinese_A$on == ifelse((1 %in% input$restaurant_choice),1,0),]$Latitude,
                   icon = icon_redefined, label = ~Chinese_A$DBA)%>%
        
        
        addPulseMarkers(lng = ~Japanese_A[Japanese_A$on == ifelse((2 %in% input$restaurant_choice),1,0),]$Longitude,
                   lat = ~Japanese_A[Japanese_A$on == ifelse((2 %in% input$restaurant_choice),1,0),]$Latitude,
                   icon = icon_redefined, label = ~Japanese_A$DBA)%>%
        
        addPulseMarkers(lng = ~Mexican_A[Mexican_A$on == ifelse((3 %in% input$restaurant_choice),1,0),]$Longitude,
                   lat = ~Mexican_A[Mexican_A$on == ifelse((3 %in% input$restaurant_choice),1,0),]$Latitude,
                   icon = icon_redefined, label = ~Mexican_A$DBA)%>%
        
        addPulseMarkers(lng = ~Pizza_A[Pizza_A$on == ifelse((4 %in% input$restaurant_choice),1,0),]$Longitude,
                   lat = ~Pizza_A[Pizza_A$on == ifelse((4 %in% input$restaurant_choice),1,0),]$Latitude,
                   icon = icon_redefined, label = ~Pizza_A$DBA)%>%
        
        addPulseMarkers(lng = ~Bakery_A[Bakery_A$on == ifelse((5 %in% input$restaurant_choice),1,0),]$Longitude,
                   lat = ~Bakery_A[Bakery_A$on == ifelse((5 %in% input$restaurant_choice),1,0),]$Latitude,
                   icon = icon_redefined, label = ~Bakery_A$DBA)%>%
      
        addMarkers(lng = ~uniquedata_bad[uniquedata_bad$on == ifelse((6 %in% input$avoid_choice), 1, 0),]$Longitude,
                   lat = ~uniquedata_bad[uniquedata_bad$on == ifelse((6 %in% input$avoid_choice), 1, 0),]$Latitude,
                   icon = rat_icon, label = paste(uniquedata_bad$BUILDING, uniquedata_bad$STREET, sep = " ")) %>%
        
        addMarkers(lng = ~uniquedata_bad_2[uniquedata_bad_2$on == ifelse((7 %in% input$avoid_choice), 1, 0),]$Longitude,
                   lat = ~uniquedata_bad_2[uniquedata_bad_2$on == ifelse((7 %in% input$avoid_choice), 1, 0),]$Latitude,
                   icon = noxious_icon, label = paste(uniquedata_bad_2$BUILDING, uniquedata_bad_2$STREET, sep = " ")) %>%
  
        setView(lng = -74.0103095, lat = 40.71446219, zoom = 11) %>%
        addLegend("bottomleft", pal = pal_10, values = na.omit(uniquedata_merge$AVG_ZIP), title = "AVG Score by Zip.") %>%
        addMiniMap(width = 100, height = 100)

      first_plot  
    })
    

    load("uniquedata.RData")
    data = uniquedata[,c("DBA","BORO","ZIPCODE","CUISINE.DESCRIPTION","SCORE","Longitude","Latitude")]
    cusine = c("American","Chinese" ,"Café","Pizza","Latin","Mexican" ,"Italian","Caribbean","Japanese","Bakery","Spanish" )  
    boro = c("Brooklyn","Manhattan","Queens","Bronx","Staten Island")
    
    
    data = data%>%filter(ZIPCODE>0)%>%
      filter(!is.na(Longitude ))%>% 
      filter(! is.na(Latitude))
    
    
    
    d = reactive({data[which((data$BORO == input$boro)&(data$CUISINE.DESCRIPTION == input$cusine)),]})
    
    d1 = reactive({ddply(d(),~ZIPCODE,summarise,avg = mean(SCORE))})
    #d1 = reactive({
    
    #d1$ZIPCODE = d1()$ZIPCODE
    #d1$avg = d1()$`mean(SCORE)`
    #})
    
    d3 = reactive({d1()[order(d1()$avg),]})
    
    d4 = reactive({left_join(d3(),d())})
    #c = reactive({d4()$avg})
    pal = reactive(colorQuantile(palette = "Blues",domain = d()$SCORE))
    #pal = colorNumeric( palette = "Blues",domain = c(9:17))
    output$map <- renderLeaflet({
      
      leaflet(d())%>%
        addTiles()%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(lng = ~Longitude,
                         lat = ~Latitude,
                         popup = ~paste0('<strong>',"Restaurant:",'</strong>',DBA,"<br/>",'<strong>',"Score:",'</strong>',round(d()$SCORE,2))
                         %>% lapply(htmltools::HTML),
                         #color = ~reactive({pal()(avg)}),
                         color = ~pal()(SCORE),
                         radius =4,stroke = TRUE,fillOpacity = 0.1,weight =5)%>%
        addLegend("bottomright",title = "average scores",pal = pal(),values = ~SCORE,opacity = 0.7)})
    
  }
)




shinyApp(ui, server)
