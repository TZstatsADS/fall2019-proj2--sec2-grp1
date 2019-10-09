library(tidyverse)
library(ggmap)
library(plyr)
library(leaflet)
library(shiny)
load("~/Desktop/fall2019-proj2--sec2-grp1/output/uniquedata.RData")
data = uniquedata[,c("DBA","BORO","ZIPCODE","CUISINE.DESCRIPTION","SCORE","Longitude","Latitude")]
cusine = c("American","Chinese" ,"Café","Pizza","Latin","Mexican" ,"Italian","Caribbean","Japanese","Bakery","Spanish" )  
boro = c("Brooklyn","Manhattan","Queens","Bronx","Staten Island")


data = data%>%filter(ZIPCODE>0)%>%
    filter(!is.na(Longitude ))%>% 
    filter(! is.na(Latitude))

ui = navbarPage(strong("New York City Restaurants"),
                tabPanel("sub-boro"),
                sidebarLayout(
                    sidebarPanel(
                        selectInput("boro","Borough",choices =boro ),
                        selectInput("cusine","Cusine",choices = cusine )
                    ),
                    mainPanel(leafletOutput("map"))
                ))



server <- function(input,output){
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
    output$map <- renderLeaflet({
        
        leaflet(d4())%>%
            addTiles()%>%
            addCircleMarkers(lng = ~Longitude,
                             lat = ~Latitude,
                             popup = ~paste0(DBA,"<br/>",round(d4()$avg,2)),
                             #color = ~reactive({pal()(avg)}),
                             color = ~pal()(avg),
                             radius =4,stroke = TRUE,fillOpacity = 0.1,weight =5)%>%
            addLegend("bottomright",title = "average scores",pal = pal(),values = ~avg,opacity = 0.7)})
}

shinyApp(ui = ui,server = server)