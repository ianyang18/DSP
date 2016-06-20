install.packages(c("shiny", "ggvis", "class"), dep = T)
libs = c("shiny", "ggvis", "class")
sapply(libs, require, character.only = TRUE)
#library(ElemStatLearn)

getwd()
setwd('..')
data.path <- paste(getwd(), "data", sep="/")
train.path <- paste(data.path, "ticdata2000.txt", sep="/")
test.path <- paste(data.path, "ticeval2000.txt", sep="/")
gt.path <- paste(data.path, "tictgts2000.txt", sep="/")

train.data <- read.table(train.path)
test.data <- read.table(test.path)
gt <- read.table(gt.path)

colnames(train.data) <- c("Customer Subtype", "Number of houses", "Avg size household", "Avg age", "Customer main type", 
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

# Function: calculate the accuracy 
# Input: table, type
# Output: accuracy
accuracy <- function(t, l) {
  TN <- 0
  N <- 0
  for(i in l) {
    for(j in l) {
      if(i == j) TN <- TN + t[i, j]
      N <- N + t[i, j]
    }
  }
  return(TN/N)
}

shinyServer(function(input, output, session) {
  dataInput <- reactive({
    # Spliting data into k-fold by marking each data to several groups
    l <- 1:input$kfold
    train.data$id <- sample(1:input$kfold, nrow(train.data), replace=TRUE)
    kfold.hit <- rep.int(0, input$knnp)
         
    for(kp in 1:input$knnp) {
      kp.hit <- rep.int(0, input$kfold)
      for(i in 1:input$kfold) {
        training <- subset(train.data, id %in% l[-i])
        validation <- subset(train.data, id %in% c(i))
          
        data_pred <- knn(train = training[, 1:85], test = validation[, 1:85], cl = training[, 86], k = kp)
        resultFrame <- data.frame(target = data_pred, predict = validation[, 86])
        t <- table(resultFrame$target, resultFrame$predict)
        a <- accuracy(t, levels(factor(validation[, 86])))
        kp.hit[i] <- a
        print(kp.hit)
      }
      avg.kp.hit <- mean(kp.hit)
      kfold.hit[kp] <- avg.kp.hit
    }
    data.frame("k"=c(1:input$knnp), "avgError"=kfold.hit)
  })
    
  output$table <- renderTable({
    prop.table(table(train.data[, c(input$xcol, "Number of mobile home policies")]))
  })
  
  output$result <- renderText({
    kfold.hit <- dataInput()[, "avgError"]
    kp <- which(kfold.hit == max(kfold.hit))
    data_pred <- knn(train = train.data[, 1:85], test = test.data[, 1:85], cl = train.data[, 86], k = kp)
    resultFrame <- data.frame(target = data_pred, predict = gt[, 1])
    t <- table(resultFrame$target, resultFrame$predict)
    a <- accuracy(t, levels(factor(train.data[, 86])))
    
    gt_sample <- sample(0:1, nrow(gt), replace=TRUE) 
    random.t <- table(gt_sample, resultFrame$predict)
    random.a <- accuracy(random.t, levels(factor(train.data[, 86])))
    
    paste("The accuracy of the test data is: ", a)
  })
  
  output$dataPlot <- renderPlot({
    #table(train.data[, input$xcol])
    #barplot(table(train.data[, input$xcol]))
    barplot(prop.table(table(train.data[, input$xcol])),
            border = "dark blue",
            xlab = input$xcol,
            ylab = paste("Proportion of ", input$xcol)
            )
  })
    
  dataInput %>%
  ggvis(~k, ~avgError, key:=~k) %>%   
  layer_points(size := 50, size.hover := 200,
               fillOpacity := 0.2, fillOpacity.hover := 0.5,
               stroke = ~avgError, key := ~k) %>%
  add_axis("x", title="KNN-N", title_offset=100) %>%
  add_axis("y", title="Accuracy", title_offset=100) %>%
  add_tooltip(function(data){
      paste0("KNN-K: ", data$k, "<br>", "Accuracy: ", round(data$avgError, digits=3))
  }, "hover") %>%
  bind_shiny("modelPlot")
})
