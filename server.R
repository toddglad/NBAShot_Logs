
library(shiny)
library(dplyr)
library(caret)
library(arm)
library(lme4)
library(MASS)
library(leaps)
library(ggplot2)
shot_logs<-read.csv("./Data/shot_logs.csv") # Read in Data


shot_logs <- predict(preProcess(shot_logs, method=c("medianImpute")),shot_logs) #Impute missing values


shot_logs%>%
  filter(shot_logs$TOUCH_TIME <= 24 & shot_logs$TOUCH_TIME > 0) -> shot_logs #Get rid of value that don't make sense


log_fxn <-summary(glm(FGM ~  SHOT_CLOCK + TOUCH_TIME + LOCATION +  CLOSE_DEF_DIST + SHOT_DIST,
                     family = binomial,
                     data = shot_logs)) # Initialize logisitc regression

shinyServer(
  function(input, output) {
      output$log_fxn <- renderPrint({ #Print Summary of Logistic regression
       log_fxn
      })

   out1 <- reactive({ # Take coefficients of summary and find the percentage
        if(input$in1 == "Home"){
        percentage <-invlogit(coef(log_fxn)[1]+1*coef(log_fxn)["LOCATIONH","Estimate"] 
                 +input$defenderdistance*coef(log_fxn)["CLOSE_DEF_DIST","Estimate"]+
                   input$distancefromhoop*coef(log_fxn)["SHOT_DIST","Estimate"] + 
                   input$shotclock*coef(log_fxn)["SHOT_CLOCK","Estimate"]
                 + input$touchtime*coef(log_fxn)["TOUCH_TIME","Estimate"])
        }
        else{
        percentage <-invlogit(coef(log_fxn)[1]+0*coef(log_fxn)["LOCATIONH","Estimate"] +
                     input$defenderdistance*coef(log_fxn)["CLOSE_DEF_DIST","Estimate"]+
                     input$distancefromhoop*coef(log_fxn)["SHOT_DIST","Estimate"]+ 
                     input$shotclock*coef(log_fxn)["SHOT_CLOCK","Estimate"]+
                     input$touchtime*coef(log_fxn)["TOUCH_TIME","Estimate"])
        }
      percentage
      })
    output$out1 <- reactive({out1()})
    output$barPlot <- renderPlot({barplot(out1(), ylab = "Percentage", ylim = c(0,1))})
    output$table <- renderDataTable(shot_logs, options = list(pageLength = 10))
    output$plotshotclk <- renderPlot({ggplot(shot_logs, aes(x = shot_logs$SHOT_CLOCK, y = shot_logs$FGM))+
        labs(title = "Shot Clock vs. Field Goal Make", x = "Shot Clock", y = "Field Goal Made")+
        geom_bar(stat = "identity")+
        scale_y_continuous(limits = c(0, 1000))})
    output$plottoucht <- renderPlot({ggplot(shot_logs, aes(x = shot_logs$TOUCH_TIME, y = shot_logs$FGM))+
        labs(title = "Touch Time vs Field Goal Make", x = "Touch Time", y = "Field Goal Made")+
        geom_bar(stat = "identity")+
        scale_y_continuous(limits = c(0, 6000))+
        scale_x_continuous(limits = c(0, 25))})
    output$plotloc <- renderPlot({ggplot(shot_logs, aes(x = shot_logs$LOCATION, y = shot_logs$FGM))+
        labs(title = "Location vs Field Goal Make", x = "Location", y = "Field Goal Made")+
        geom_bar(stat = "identity")})
    output$plotclosestdefdist <- renderPlot({ggplot(shot_logs, aes(x = shot_logs$CLOSE_DEF_DIST, y = shot_logs$FGM))+
        labs(title = "Closest Defender Distance vs Field Goal Make", x = "Closest Defender Distance", y = "Field Goal Made")+
        geom_bar(stat = "identity")+
        scale_x_continuous(limits = c(0, 25))})
    output$plothoopdist <- renderPlot({ggplot(shot_logs, aes(x = shot_logs$SHOT_DIST, y = shot_logs$FGM))+
        labs(title = "Distance from Hoop vs Field Goal Make", x = "Distance from Hoop", y = "Field Goal Made")+
        geom_bar(stat = "identity")+
        scale_x_continuous(limits = c(0, 40))})
    
    
    
    b <- shot_logs%>%
      group_by(player_name)%>%
      filter(sum(FGM) > 350)
    
    output$plotoplayer <- renderPlot({
      ggplot(b, aes(x = b$player_name, y = b$FGM))+
        labs(title = "Player Name vs. Field Goal Make", x = "Player Name", y = "Total Field Goals Made")+
        geom_bar(stat = "identity")+
        scale_y_continuous(limits = c(0, 600))+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
})
    }
)