##################################################
###### 2 .- Carga de paquetes ###########

packages <- c('ggplot2','plotly','reshape2','reshape','plyr','shiny',
              'data.table','DT','stringr','knitr','rmarkdown','OneR','shinythemes',
              'tidyverse','xts','tseries','foreign','lubridate','forecast',
              'tidyverse','timetk','readxl','tidyquant','scales','sweep',
              'zoo','broom','tibble','stringr','highcharter','knitr','caret')

for (package in packages) {
  if (!(require(package, character.only=T, quietly=T))) {
    install.packages(package)
    library(package, character.only=T)
  }
}


rm(package,packages)




#load("~/Maestria/TFM/TFMproj/egresos_Tot.Rdata")

#plot_ly(x = ~ingresosT2$finmes, y = ~ingresosT2$count, mode = 'lines+markers')

#load("~/Maestria/TFM/TFMproj/train.Rdata")
load("~/Maestria/TFM/TFMproj/test.Rdata")

load("~/Maestria/TFM/TFMproj/serietottrain.Rdata")

load("~/Maestria/TFM/TFMproj/serietotval.Rdata")


prueba<-test
rm(test)

gc()

estareas <-function (base, area) {
  
  igresos <- base[base$esp_egrpa ==area,]
  
  num <- as.numeric(quantile(igresos$difer,0.99))
  
  igresos <- igresos[  igresos$difer <= num,]
  
  igresos$count <- 1
  
  rm(igresos)
  gc()
  
  ingresosT<- aggregate(count ~ finmes+esp_egrpa, data = train, sum)
  
  return(ingresosT) 
}







