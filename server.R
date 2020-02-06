library(shiny);#require(scales)

### Parte donde se procesa la data
options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output,session) {
  
  
  ### Carga de la data
  
  # Data <- reactive({
  # 
  #   ingreso <- prueba[prueba$finmes >= as.Date('2015-01-01') &
  #                       prueba$finmes <= as.Date('2017-09-30'),]
  # 
  #   if(input$areain == 'Todos'){
  #     datos <- ingreso
  #   } else {
  #     datos <- ingreso[as.character(ingreso$esp_egrpa)==input$areain,]
  #   }
  # 
  #   return(datos)
  # 
  #     })
  
  

  output$plotbox <- renderPlotly({
    
#    datos <- Data()
  
    
      if(input$areain == 'Todos'){
        datos <- prueba[,c("difer","esp_egrpa")]
        datos$esp_egrpa <- "Todos"
      } else {
        datos <- prueba[as.character(prueba$esp_egrpa)==input$areain,c("difer","esp_egrpa")]
      }
    rm(prueba)
    gc()
      
    num <-  as.numeric(quantile(datos$difer,0.99))
    
    datos$estadia <- ifelse(datos$difer>num,
                           num, datos$difer)
    
    datos$area <- as.character(datos$esp_egrpa)
    
    plot_ly(datos, x= ~area ,
            y = ~estadia,
            color = ~area, type = "box")
    
  })
  
  
  output$summary <- renderPrint({
    if(input$areain == 'Todos'){
      datos <- prueba[,c("difer","esp_egrpa")]
      datos$esp_egrpa <- "Todos"
    } else {
      datos <- prueba[as.character(prueba$esp_egrpa)==input$areain,c("difer","esp_egrpa")]
    }
    rm(prueba)
    gc()
    
    num <-  as.numeric(quantile(datos$difer,0.99))
  
    datos$estadia <- ifelse(datos$difer>num,
                            num, datos$difer)
    
    summary(datos$estadia)
    
    })
  
  
  output$ingresos <- renderPrint({
    if(input$areain == 'Todos'){
      datos <- prueba[prueba$finmes=='2017-09-30',c("difer","esp_egrpa")]
      datos$esp_egrpa <- "Todos"
    } else {
      datos <- prueba[as.character(prueba$esp_egrpa)==input$areain &
                        prueba$finmes=='2017-09-30',c("difer","esp_egrpa")]
    }
    rm(prueba)
    gc()
    
    datos$count <- 1
    
    sum(datos$count)
    

  })
  
  
    Sertrain <- reactive({
    
    if(input$areain == 'Todos'){
      sbux.tsf <- sbux.ts
    } else {
      datos <- prueba[as.character(prueba$esp_egrpa)==input$areain &
                        prueba$difer < 60 ,]
    
    #rm(prueba)
    gc()
    
    #num <-  as.numeric(quantile(datos$difer,0.99))
    datos$count <- 1
    
    ingresosT<- aggregate(count ~ finmes, data = datos, sum)
    
    sbux.tsf = ts(data=ingresosT$count, frequency = 12,              
                 start=c(2013,1), end=c(2016,12)) 
    
    
    }
    return(sbux.tsf)
    
  })

    Sertrain2 <- reactive({
      
      if(input$areain == 'Todos'){
        sbux.tsf <- sbux.ts
      } else {
        datos <- prueba[as.character(prueba$esp_egrpa)==input$areain &
                          prueba$difer < 60 ,]
        
        #rm(prueba)
        gc()
        
        #num <-  as.numeric(quantile(datos$difer,0.99))
        datos$count <- 1
        
        ingresosT<- aggregate(count ~ finmes, data = datos, sum)
        
        sbux.tsf2 = ts(data=ingresosT$count, frequency = 12,              
                      start=c(2013,1), end=c(2017,9)) 
        
        
      }
      return(sbux.tsf2)
      
    })
    
    
  output$estacion <- renderPrint({
    
    if(input$areain == 'Todos'){
      lineas <- sbux.ts
    } else {
      
      lineas <- Sertrain()
      
    }
    
    adf.test(lineas)
    
  })
  
  
  
  output$seasonplot <- renderPlot({
    
    if(input$areain == 'Todos'){
      datos <- sbux.ts
    } else {
      datos <- Sertrain()
    }
    
    seasonplot(datos, 12, col=rainbow(5), year.labels=TRUE, main="Grafico Estacional")
    
  })

  output$arimatot <- renderPlot({
    
    if(input$areain == 'Todos'){
      datos <- sbux.ts
      sbux2 <-sbux.ts2
          } else {
      datos <- Sertrain()
      sbux2 <- Sertrain2()
    }
    
    
    fit <- auto.arima(datos)
    
    plot(forecast(fit,h=9))
    lines(sbux2,col='black', yaxt='n', ann=FALSE)
    
    
  })
  
  
    
  
  output$exposer <- renderPlot({
    
    if(input$areain == 'Todos'){
      datos <- sbux.ts
      sbux2 <-sbux.ts2
          } else {
      datos <- Sertrain()
      sbux2 <- Sertrain2()
    }
    
    infofore <- HoltWinters(datos)
    
    pronos <- predict(infofore,n.ahead = 9)
    
    
    plot(sbux2,col='black',main="Holt-Winters filtering",ylab="Observed/fited")
    lines(pronos,col='red')
    
    
    #par(new=TRUE) infofore, , ann=FALSE
    #plot(sbux.ts2,col='red')
    #regresion <- lm(datos$count~datos$finmes)
    #coef(regresion)
    
       
  })
  
  output$redneu <- renderPlot({
    
    if(input$areain == 'Todos'){
      datos <- sbux.ts
      sbux2 <-sbux.ts2
    } else {
      datos <- Sertrain()
      sbux2 <- Sertrain2()
    }
    
    fitnn <- nnetar(datos, lambda=0.5)
    
    fcast <- forecast(fitnn, PI=TRUE, h=9)
    plot(fcast)
    lines(sbux2,col='black', yaxt='n', ann=FALSE)
    #    mlp.fit <- mlp(datos)
    #    plot(mlp.fit)
    #    print(mlp.fit)
    
    
        
  })

  ################# residuos  ########################################
  
  output$arimares <- renderPlot({
    
    if(input$areain == 'Todos'){
      datos <- sbux.ts
    } else {
      datos <- Sertrain()
    }
    
    
    fit <- auto.arima(datos)
    
    checkresiduals(fit)
    
    
  })
  

  
  output$expores <- renderPlot({
    
    if(input$areain == 'Todos'){
      datos <- sbux.ts
    } else {
      datos <- Sertrain()
    }
    
    #regresion <- lm(datos$count~datos$finmes)
    #coef(regresion)
    
    infofore <- HoltWinters(datos)
    
    checkresiduals(infofore)
    
  })
  
  output$redneures <- renderPlot({
    
    if(input$areain == 'Todos'){
      datos <- sbux.ts
    } else {
      datos <- Sertrain()
    }
    
    #    mlp.fit <- mlp(datos)
    #    plot(mlp.fit)
    #    print(mlp.fit)
    
    fitnn <- nnetar(datos, lambda=0.5)
    
  checkresiduals(fitnn$residuals)
    
  })
  
  
  ##############  
  
  output$arimaerr <- renderPrint({
    
    if(input$areain == 'Todos'){
      datos <- sbux.ts
      datos2 <- sbux.ts2
    } else {
      datos <- Sertrain()
      datos2 <- Sertrain2()
    }
    
    
    fit <- auto.arima(datos)
    
    pred <- forecast(fit,h=9)
    
    RMSE <- sqrt(mean((pred$mean-datos2[time(datos2) >= 2017 ])^2))
    
    MAE <- mean(abs(pred$mean-datos2[time(datos2) >= 2017 ]))
    
    #Avg(Abs(Actual-Forecast)/Abs(Actual)) *100
    MAPE <- 100*mean(abs(pred$mean-datos2[time(datos2) >= 2017 ])/abs(pred$mean))
    
    return(list(RMSE= RMSE,MAE=MAE,MAPE=MAPE))
    
        
  })
  
  
  
  
  output$expoerr <- renderPrint({
    
    if(input$areain == 'Todos'){
      datos <- sbux.ts
      datos2 <- sbux.ts2
      } else {
      datos <- Sertrain()
      datos2 <- Sertrain2()
    }
    
    #regresion <- lm(datos$count~datos$finmes)
    #coef(regresion)
    
    infofore <- HoltWinters(datos)
    
    pronos <- predict(infofore,n.ahead = 9)
    
    RMSE <- sqrt(mean((pronos-datos2[time(datos2) >= 2017 ])^2))
    
    MAE <- mean(abs(pronos-datos2[time(datos2) >= 2017 ]))
    
    #Avg(Abs(Actual-Forecast)/Abs(Actual)) *100
    MAPE <- 100*mean(abs(pronos-datos2[time(datos2) >= 2017 ])/abs(pronos))
    
    return(list(RMSE= RMSE,MAE=MAE,MAPE=MAPE))
    
  })
  
  output$redneuerr <- renderPrint({
    
    if(input$areain == 'Todos'){
      datos <- sbux.ts
      datos2 <- sbux.ts2
      } else {
      datos <- Sertrain()
      datos2 <- Sertrain2()
    }
    

    fitnn <- nnetar(datos, lambda=0.5)
    
    fcast <- forecast(fitnn, PI=TRUE, h=9)
    #autoplot(fcast)
    
    #Sqrt(Avg(Power(Actual -Forecast)))
    
    RMSE <- sqrt(mean((fcast$mean-datos2[time(datos2) >= 2017 ])^2))
    
    MAE <- mean(abs(fcast$mean-datos2[time(datos2) >= 2017 ]))
    
    #Avg(Abs(Actual-Forecast)/Abs(Actual)) *100
    MAPE <- 100*mean(abs(fcast$mean-datos2[time(datos2) >= 2017 ])/abs(fcast$mean))
  
    return(list(RMSE= RMSE,MAE=MAE,MAPE=MAPE))
    
    })
  
  
  #############  modelos ############################
  
  output$arimamod <- renderPrint({
    
    if(input$areain == 'Todos'){
      datos <- sbux.ts
    } else {
      datos <- Sertrain()
    }
    
    fit <- auto.arima(datos)
    
    list(Modelo=fit,
    pvalue=
    Box.test(fit$residuals,lag=1))
    
  })
  
  
  
  
  output$expomod <- renderPrint({
    
    if(input$areain == 'Todos'){
      datos <- sbux.ts
    } else {
      datos <- Sertrain()
    }
    
    #regresion <- lm(datos$count~datos$finmes)
    #coef(regresion)
    
    HoltWinters(datos)
    
    
  })
  
  output$redneumod <- renderPrint({
    
    if(input$areain == 'Todos'){
      datos <- sbux.ts
    } else {
      datos <- Sertrain()
    }
    
    
    nnetar(datos, lambda=NULL)
    
    
  })
  
  
  output$proy <- renderDataTable({
    
    if(input$areain == 'Todos'){
      datos <- sbux.ts
    } else {
      datos <- Sertrain()
    }
    
    
    fit <- auto.arima(datos)
    pred <- forecast(fit,h=9)
    
    infofore <- HoltWinters(datos)
    
    pronos <- predict(infofore,n.ahead = 9)
  
    fitnn <- nnetar(datos, lambda=0.5)
    
    fcast2 <- forecast(fitnn, PI=TRUE, h=9)
    
   proyec <- data.frame(ARMA= pred$fit,  
                           HolTW = pronos[1], 
                          real= sbux.ts )
      
   return(proyec)
    
  })
  
  
  output$proy2 <- renderPrint({
    
    if(input$areain == 'Todos'){
      datos <- sbux.ts
    } else {
      datos <- Sertrain()
    }
    
    
    fitnn <- nnetar(datos, lambda=0.5)
    
    fcast2 <- forecast(fitnn, PI=TRUE, h=9)
    
    return(fcast2)
    
  })
  
  
  ##### filtros #####################################
  
  # output$filt <- renderUI({
  #   d <- Data2()
  #   x<- names(d)
  #   selectInput('carac', 'Escoja una caracteristica',
  #               choices = x ) #,selected = "Ninguno")
  # })
  # 
  # output$filt2 <- renderUI({
  #   d <- Data2()
  #   x<- d[,1]
  #   selectInput('per', 'Escoja una persona',
  #               choices = x ) #,selected = "Ninguno")
  # })
  


  
  
  
})