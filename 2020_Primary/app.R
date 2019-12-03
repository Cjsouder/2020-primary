#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinythemes)
library(tidyverse)

# Read in rds file with cleaned up data

clean_data <- readRDS("data.rds")
   

# I defined input choices for factors that represent media type and outcomes outside of my UI.

factors <- c("Mean Online Media Mentions" = "stories", 
             "Mean Cable News Mentions" = "clips")

outcomes <- c("Mean Polling Percentage" = "percent", 
              "Mean Share Price" = "price")

ui <-

fluidPage(
    
    # I chose a white and blue theme and titled my app
    
    theme = shinytheme("cerulean"),
    
    titlePanel("What Effect Does Media Mentions Have On 2020 Democratic Presidential Primary Polls and Betting?"),
    
    #Created a sidebar that allows the user to select media type and outcome variable. 
  
    sidebarLayout(
        sidebarPanel(
            selectInput("Media",
                        "Select Media Type:",
                        choices = factors,
                        selected = "Mean Online Media Mentions"),
        
        selectInput("outcome",
                    "Select an outcome variable:",
                    choices = outcomes,
                    selected = "Average Polling Percentage")),
        
        # I created About and Panel tabs in the main panel
        
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("About", htmlOutput("about")),
                        tabPanel("Data", 
                                 plotOutput("plot"),
                                 h3("Results"),
                                 p("The data confirmed my hypothesis. 
                                   Overall, as average media and online mentions increases, 
                                   poll percentage and betting price also increases."))
        )
    )))


#I used reactives for x and y to get the desired input

server <- function(input, output) {
    
    x_label <- reactive({
        req(input$Media)
        if(input$Media == "stories"){
            x_label <- "Mean Online Media Mentions"
        } else if(input$Media == "clips"){
            x_label <- "Mean Cable News Mentions"
        }})
    
    y_label <- reactive({
        req(input$outcome)
        if(input$outcome == "percent"){
            y_label <- "Average Polling Percentage"
        } else if(input$outcome == "price"){
            y_label <- "Mean Share Price"
        }})
    
    #I pasted strings of text describing the project

    
    output$about <- renderUI({
        
        str1 <- paste("Background")
        str2 <- paste("There are currently over a dozen candidates running for the 2020 Democratic nomination 
        for president of the United States. In the early stages of the nomination process, there has been lots of 
        contention over the role of the mainstream media and it's role in shaping public opinion and ultimately the 
        outcome of the primary. The mainstream media has long been victim to attacks from politicians
                      and their supporters for coverage that is seen as unfair. Recently, the media
                      has been accused of picking a favorite candidate and centering their political 
                      coverage around that candidate. This begs the question: How does media coverage 
                      effect a candidates performance? Does modern political media coverage even effect voters?")
        str3 <- paste("Data")
        str4 <- paste("This project explores this with cable mentions data, online mentions data, and polling data 
                      from FiveThirtyEight and betting data from PredictIt. The cable mentions data contains the mentions 
                      of currently declared major candidates for the 2020 Democratic primary since December 30, 2018 across 
                      CNN, Fox News, and MSNBC. Coverage is measured by splitting daily news footage into 15-second clips and
                      finding the clips that contain a mention of the candidates' first and last names. The online mentions data 
                      contains the mentions of currently declared major candidates in online news stories. The polling data is 
                      measured by aggregating national polling from several credible polling agencies on the major candidates. 
                      The betting data represents the latest 'Yes' price offered for 24 declared and undeclared presidential candidates. 
                      This data allows for the evaluation of the 2020 media coverage by comparing the coverage to public opinion 
                      through the polling and betting data.")
        str5 <- paste("Hypotheses")
        str6 <- paste("I hypothesized that there will be a positive correlation between both types of
                      media coverage and betting and polling outcomes.")
        
        HTML(paste(h3(str1), p(str2), h3(str3), p(str4), h3(str5), p(str6)))
    })
    
    #I piped the clean data into a ggplot of outcome vs. media for the data tab. 
    #I decided to not have a title because I felt that the title for the shinyapp
    #describes the graphic sufficiently.
    
    output$plot <- renderPlot({
        
        clean_data %>% 
            ggplot(aes_string(x = input$Media, y = input$outcome)) + 
            geom_point() +
            labs(x = x_label(),
                 y = y_label()) +
        geom_smooth(method = "lm", se = FALSE)
        
    })
    
}

#Use shinyApp function to create shiny app with ui and server

shinyApp(ui, server)


#I followed the path of Morgan Townsend's project in the fall of 2018 to figure
#out how to use reactives and how to create a sidebar