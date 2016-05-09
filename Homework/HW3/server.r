library(shiny)
library(ggvis)
source("hw2_104753024_Chun-Yao_Yang.R")
shinyServer(function(input, output,session) {
    dataInput <- reactive({
        path<-paste(getwd(),"data",sep="/")
        dataset<-paste(path,input$set,sep="/")
        names<-f1<-ss<-sc<-sc1<-auc<-c()
        #inputFile<-sample(list.files(dataset),input$m)
        for(file in list.files(dataset)) {
            name<-gsub(".csv","",basename(file))
            d<-read.csv(paste(dataset,file,sep="/"),header=T,sep=",",stringsAsFactors=FALSE)
            resultframe<-data.frame(target=d$reference,pred=d$prediction,score=d$pred.score)
            t<-table(resultframe$target,resultframe$pred)
            cm<-CM(t,tolower(input$target))
            eval<-prediction(resultframe$score,resultframe$target)
            names<-c(names,name)
            for(q in input$query){
                if(q=="F1"){f1<-c(f1,F1(cm))}
                #else if(q=="Sensitivity"){ss<-c(ss,SS(cm))}
                #else if(q=="Specificity"){sc<-c(sc,SC(cm))}
                else if(q=="AUC"){auc<-c(auc,AUC(eval))}
            }
            sc<-c(sc,SC(cm))
            ss<-c(ss,SS(cm))
        }
        outData<-data.frame(Methods=names,Sensitivity=ss,Specificity=sc,stringsAsFactors=FALSE)
        for(q in input$query){
            if(q=="F1"){outData["F1"]=f1}
            else if(q=="AUC"){outData["AUC"]=auc}
            else if(q=="Significance"){outData["Significance"]=sf}
            #else if(q=="Sensitivity"){outData["Sensitivity"]=ss}
            #else if(q=="Specificity"){outData["Specificity"]=sc}
        }
        outData
    })
    output$table <- renderTable({
        #dataInput()[sample(nrow(dataInput()),input$m),]
        if(length(input$query)){
            index<-sapply(dataInput()[,c("Sensitivity","Specificity",input$query)],function(x) which.max(x))
            dataInput<-rbind(dataInput(),c("highest",dataInput()[,c("Methods")][index]))
        }
        else{
            index<-sapply(dataInput()[,c("Sensitivity","Specificity")],function(x) which.max(x))
            dataInput<-rbind(dataInput(),c("highest",dataInput()[,c("Methods")][index]))
        }
    })
    output$text <- renderText({
    })
    dataInput %>%
    ggvis(~Specificity,~Sensitivity, key:=~Methods) %>%   
    layer_points() %>%
    add_axis("x", title="Specificity", title_offset=50) %>%
    add_axis("y", title="Sensitivity", title_offset=50) %>%
    add_tooltip(function(data){
        paste0("Method: ", data$Methods, "<br>", "Specificity: ", data$Specificity, "<br>", "Sensitivity: ", data$Sensitivity)
    }, "hover") %>%
    bind_shiny("plot")
})
