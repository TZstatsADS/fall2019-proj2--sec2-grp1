
load('uniquedata.RData')

library(shiny)
borough_list<-c("Brooklyn","Manhattan","Queens","Bronx","Staten Island")
cuisine_list<-c("Pizza","Italian","Bakery","Caribbean","Japanese","American","Chinese","Café/Coffee/Tea","Spanish","Latin (Cuban, Dominican, Puerto Rican, South & Central American)","Mexican")
cuisine_list1<-c("Pizza","Italian","Bakery","Caribbean","Japanese","American","Chinese","Café","Spanish","Latin","Mexican")



ui <- navbarPage(strong("New York City Restaurants"),
                 tabPanel("Score Comparison",
                          titlePanel("Score Distribution"),
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
                              h4("Summary"),
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
                              h4("Summary"),
                              verbatimTextOutput("Summary2")
                              ))
                            ),
                 tabPanel("Restaurant recommendation",
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
                              plotOutput(outputId = "plots3", height = "300"))),
                          hr(),
                          sidebarLayout(
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              numericInput("top", "Top restaurants to view:", 5)),
                            # Main panel for displaying outputs ----
                            mainPanel(
                              h4("Top restaurants"),
                              tableOutput("view")))
                          ))



library(ggplot2)
server <- function(input, output) {
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
    ggplot(data=data2,aes(x=cuisine_list1,y=count1,fill=factor(ifelse(cuisine_list1==input$cuisine,"Selected","Others")))) +
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
}




shinyApp(ui,server)
