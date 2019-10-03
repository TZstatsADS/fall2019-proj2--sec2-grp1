setwd("../output")
load('data.RData')

library(shiny)
borough_list<-c("Brooklyn","Manhattan","Queens","Bronx","Staten Island")
cuisine_list<-c("Pizza","Italian","Bakery","Caribbean","Japanese","American","Chinese","Café/Coffee/Tea","Spanish","Latin (Cuban, Dominican, Puerto Rican, South & Central American)","Mexican")
cuisine_list1<-c("Pizza","Italian","Bakery","Caribbean","Japanese","American","Chinese","Café","Spanish","Latin","Mexican")



ui <- navbarPage(strong("New York City Restaurants"),
                 tabPanel("Overview",
                          titlePanel("Top 5 Recommended Restaurants"),
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
                              uiOutput('nearest'))),
                          hr(),
                          sidebarLayout(
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              numericInput("top", "Top restaurants to view:", 5)),
                            # Main panel for displaying outputs ----
                            mainPanel(
                              h4("Top restaurants"),
                              tableOutput("view"))
                          )))



library(ggplot2)
server <- function(input, output) {
  output$plots1 <- renderPlot({
    count<-c(unname(table(data$BORO)))
    data1<-data.frame(borough_list,count)
    ggplot(data=data1,aes(x=borough_list,y=count,                       fill=factor(ifelse(borough_list==input$borough,"Selected","Others")))) +
      geom_bar(stat="identity") +
      scale_fill_manual(name = "borough", values=c("deepskyblue","dodgerblue3")) +
      xlab("borough") +
      ylab("the numbers of restaurants") +
      ggtitle("Restaurants numbers in each borough")
  })
  output$plots2 <- renderPlot({  
    count1<-c(unname(table(data$CUISINE.DESCRIPTION)))
    data2<-data.frame(cuisine_list1,count1)
    ggplot(data=data2,aes(x=cuisine_list1,y=count1,                       fill=factor(ifelse(cuisine_list1==input$cuisine,"Selected","Others")))) +
      geom_bar(stat="identity") +
      scale_fill_manual(name = "cuisine", values=c("lightgoldenrod1","goldenrod1")) +
      xlab("cuisine type") +
      ylab("the numbers of restaurants") +
      ggtitle("Restaurants numbers of each cuisine type")
  })
  output$view <- renderTable({
    topdata<-data[which((data$BORO==input$borough)&(data$CUISINE.DESCRIPTION==input$cuisine)),]
    sortdata<-topdata[order(topdata$SCORE,topdata$GRADE),c(1,3,4,5,6)]
    sortdata<-sortdata[!duplicated(sortdata[1:4]),]
    head(sortdata, n = input$top)
  })
}




shinyApp(ui,server)
