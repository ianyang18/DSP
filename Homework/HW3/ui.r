library(shiny)
library(ggvis)

dataset <- c("set1","set2","set3","set4","set5")
target <- c("Female","Male")
query <- list("F1"="F1","AUC"="AUC")

shinyUI(fluidPage(
    title = "Visualization",
    titlePanel("Data Visualization"),
    hr(),
    fluidRow(
        column(2,
            h4("Dataset Explorer"),
            selectInput("set",'Set',dataset,selected=NULL),
            selectInput("target",'Target',target),
            checkboxGroupInput("query",'Queries',query,selected=c("Sensitivity","Specificity"))
            #numericInput("m","Method Number",0,0,10)
        ),
        column(6,
            h4("View Data"),
            #textOutput("text"),
            ggvisOutput("plot")
        ),
        column(4,
            h4("Summary Data"),
            tableOutput("table")
        )
    )
))
