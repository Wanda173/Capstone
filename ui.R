#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Capstone Project for Data Science
# Wanda Ken 07/07/2021

library(shiny)
library(ggplot2)
library(tokenizers)
library(ggplot2)
library(tm)
library(DT)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Next Word Prediction App"),

    sidebarLayout(
        
        # Sidebar with an input and a submit button
        sidebarPanel(
            h1(textInput("inputText", "Enter at least one word:"), style = "font-size:20px;"),
            submitButton("Submit"),
            br(),br(),br(),hr(),
            
            h6(textOutput("render"), style = "font-size:10px;")
        ),
        
        mainPanel(
            tabsetPanel(type = "tabs", 
                        tabPanel("Plot", br(), plotOutput("distPlot"),
                                               br(),hr(),
                                               h1(strong(textOutput("dispText")), style = "font-size:30px;")
                                 ),
                        tabPanel("Data", br(), h2(textOutput("tabText"), style = "font-size:30px;"),br(),
                                               dataTableOutput("mytable")
                                 ),
                        tabPanel("About", 
                                 h5(htmlOutput("dispAbout"))
                                          
                                )
                       )
                  ),
        
    )
))
