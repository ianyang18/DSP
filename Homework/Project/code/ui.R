library(shiny)
library(ggvis)

knnp <- c("1", "2", "3", "4", "5", "6")
kfold <- c("1", "2", "3", "4", "5", "10")
var <- c("Customer Subtype", "Number of houses", "Avg size household", "Avg age", "Customer main type", 
  "Roman catholic", "Protestant", "Other religion", "No religion", "Married", "Living together",
  "Other relation", "Singles", "Household without children", "Household with children",
  "High level education", "Medium level education", "Lower level education", "High status",
  "Entrepreneur", "Farmer", "Middle management", "Skilled labourers", "Unskilled labourers",
  "Social class A", "Social class B1", "Social class B2", "Social class C", "Social class D",
  "Rented house", "Home owners", "1 car", "2 cars", "No car", "National Health Service",
  "Private health insurance", "Income < 30.000", "Income 30-45.000", "Income 45-75.000",
  "Income 75-122.000", "Income >123.000", "Average income", "Purchasing power class", 
  "Contribution private third party insurance", "Contribution third party insurance (firms)",
  "Contribution third party insurane (agriculture)", "Contribution car policies", 
  "Contribution delivery van policies", "Contribution motorcycle/scooter policies",
  "Contribution lorry policies", "Contribution trailer policies", "Contribution tractor policies",
  "Contribution agricultural machines policies", "Contribution moped policies",
  "Contribution life insurances", "Contribution private accident insurance policies",
  "Contribution family accidents insurance policies", "Contribution disability insurance policies",
  "Contribution fire policies", "Contribution surfboard policies", "Contribution boat policies",
  "Contribution bicycle policies", "Contribution property insurance policies",
  "Contribution social security insurance policies", "Number of private third party insurance",
  "Number of third party insurance (firms)", "Number of third party insurane (agriculture)", 
  "Number of car policies", "Number of delivery van policies", "Number of motorcycle/scooter policies",
  "Number of lorry policies", "Number of trailer policies", "Number of tractor policies",
  "Number of agricultural machines policies", "Number of moped policies", "Number of life insurances",
  "Number of private accident insurance policies", "Number of family accidents insurance policies",
  "Number of disability insurance policies", "Number of fire policies", "Number of surfboard policies",
  "Number of boat policies", "Number of bicycle policies", "Number of property insurance policies",
  "Number of social security insurance policies", "Number of mobile home policies")

shinyUI(fluidPage(
    title = "Visualization",
    titlePanel("Data Visualization"),
    hr(),
    fluidRow(
        column(3,
            h4("Dataset Explorer"),
            selectInput("xcol", 'Attribute', var, selected= "Avg size household"),
            hr(),
            h4("Model Parameter"),
            sliderInput("knnp", 'KNN-K', 1, 10, 3, step= 1),
            selectInput("kfold", 'k-fold CV', kfold, selected= 2)

            #checkboxGroupInput("query",'Queries',query,selected=c("Sensitivity","Specificity"))
            #numericInput("m","Method Number",0,0,10)
        ),
        column(8,
            tabsetPanel(
              tabPanel("Data Summary", tableOutput("table")),
              tabPanel("Data Plot", plotOutput("dataPlot")),
              tabPanel("Model Plot", ggvisOutput("modelPlot")),
              tabPanel("Test data Summary", textOutput("result"))
              #tabPanel("k-fold CV error", tableOutput("errTable")),
            )
        )
    )
))
