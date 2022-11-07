

rm(list = ls())
setwd("C:/Users/Villagran/Desktop/datavoz/28.922 Talento digital/pretest")

# LECTURA DE BB.DD DE DATOS
library(openxlsx)
library(dplyr)
library(tidyverse)
library(stringr)
library(haven)
library(labelled)



SALTOS<-read.xlsx("SALTOS.xlsx")

data1<- read_sav("datos_DS2231_PT_02_11_2022.sav")


secuencia<-function(objeto){
  n<-length(objeto)
  seq(objeto[[1]],objeto[[n]])
}



salida<-SALTOS$salida
criterio.salto<-SALTOS$criterios.salto
llegada1<-SALTOS$llegada1
llegada2<-SALTOS$llegada2

data1<-as.data.frame(sapply(data1, as.numeric))


resultados2<-data.frame(data1$REGISTRO)


for (i in 1:length(salida)){
  
  ifelse ((str_detect(criterio.salto[i],",")), 
          rango<- criterio.salto[i] %>% str_split(",") %>% 
            unlist() %>% as.numeric(),
          
          rango<-criterio.salto[i] %>% str_split(":") %>%
            unlist() %>% as.numeric() %>% secuencia
  )
  
  coordenada1<-grep(paste("^",salida[i],"$",sep=""), names(data1))
  coordenada2<-grep(paste("^",llegada1[i],"$",sep=""), names(data1))
  coordenada3<-grep(paste("^",llegada2[i],"$",sep=""), names(data1))
  
  variable1<-as.integer(unlist(data1[,coordenada1]))
  
  s.check<-(variable1 %in%  rango)*1
  
  
  ll.check1<-(!is.na(data1[,coordenada2]) & data1[,coordenada2] != "")*1
  ll.check2<-(!is.na(data1[,coordenada3]) & data1[,coordenada3] != "")*1
  
  revision<-paste0(s.check,ll.check1, ll.check2)
  resultados2<-cbind(revision,resultados2)
}


# Creacion de nombres de columnas, reordenamiento de dataframe y etiquetaje

nombres.2<-c()


for (i in 1:length(llegada1)){
  x1<-paste0(salida[i],"(",criterio.salto[i],")",":",llegada1[i],";",llegada2[i])
  nombres.2<-append(nombres.2,x1)
}


# Reordenar base de datos y colocacion de etiquetas
resultados2<-rev(resultados2)

etiquetas<-  append("id", nombres.2)

colnames(resultados2)<-etiquetas

# resumen

resumen.s2<-as.data.frame(nombres.2)

saltos.p<-resultados2[2:ncol(resultados2)]



saltos.p[saltos.p=="000"]<-NA
saltos.p[saltos.p=="101"]<-1
saltos.p[saltos.p=="111"]<-0
saltos.p[saltos.p=="100"]<-0
saltos.p[saltos.p=="011"]<-NA
saltos.p[saltos.p=="001"]<-NA
saltos.p[saltos.p=="010"]<-NA
saltos.p[saltos.p=="110"]<-0

###

for (i in 1:ncol(saltos.p)){
  saltos.p[i]<-as.numeric(unlist(saltos.p[i]))
}

valores<-unname(round(colMeans(saltos.p[1:ncol(saltos.p)], na.rm = T),2)*100)


resumen.s2$'% de saltos correctos'<-valores

# Recodificacisn sugerida de las observaciones

resultados2<-resultados2 %>% 
  dplyr::mutate_at(c(2:ncol(resultados2)), recode, '000'='sin salto', 
                   '101'='s. correcto','111'='error', '100'='error',
                   '011'='sin salto','001'='sin salto','010'='sin salto',
                   '110'='error')

