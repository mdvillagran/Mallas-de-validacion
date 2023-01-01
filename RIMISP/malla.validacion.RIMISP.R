

rm(list = ls())
setwd("C:/Users/Villagran/Desktop/datavoz/21.10.2022 RIMISP/AVANCE")



# PAQUETES
library(openxlsx)
library(dplyr)
library(tidyverse)
library(haven)
library(labelled)
library(sjmisc)



data1<- read_sav("datos_DS2240_VF_12_12_2022.sav")


################################################################################
############################## EVALUACION DE NA ################################
################################################################################

####### Variables obligatorias

obligatorias<-names(data1)[14:93]

variables.condicionales<-c("PUEBLOS", "PUEBLOS_O","A5",
                           "A6B","A6","A7","A8", "B9_1",
                           "B9_2","B9_3","B9_4","B9_5","B9_6",
                           "B9_O", "D2","D3","D4","D5","D6",
                           "D7_1","D7_2","D7_3","D7_4","D7_5", "D7_6",
                           "D7_7","D8",  "D9")

obligatorias<-setdiff(obligatorias, variables.condicionales)


#######

resultadosNA<-data.frame(data1$REGISTRO)


for (i in obligatorias){
  coordenada<-grep(paste0("^",i,"$"), names(data1))
  columna<-((!is.na(data1[,coordenada]) & as.vector(data1[[coordenada]]) %nin% "")*1)
  resultadosNA<-cbind(assign("i",columna),resultadosNA)
}


# Reordenamiento del data frame de salida y etiquetaje de sus columnas

resultadosNA<-rev(resultadosNA)

rotulos<-  append("id.", obligatorias)

colnames(resultadosNA)<-rotulos


# Reporte resumen (completitud total variables)

valores<-unname(round(colMeans(resultadosNA[2:ncol(resultadosNA)]),2)*100)


resumen.n<-as.data.frame(obligatorias)
resumen.n$'% de completitud'<-valores

# Recodificacion sugerida de las observaciones

resultadosNA<-resultadosNA %>% 
  dplyr::mutate_at(c(2:ncol(resultadosNA)), recode, '0'='NA', '1'='Dato')

#####


################################################################################
############################## EVALUACION DE SALTOS ############################
################################################################################


#####
# Creacion de data frame con los id, donde guardaremos los resultados del bucle

#

secuencia<-function(objeto){
  seq(objeto[[1]],objeto[[length(objeto)]])
}

#


### VARIABLES DE SALTO


data1$A5.b<-ifelse(is.na(data1$A5),0,data1$A5)


data1$A4.b<-(data1$A4>(data1$A5.b+1))*1

data1$b<-ifelse(data1$B1==1 | data1$B2==1 | 
                  data1$B3==1 | data1$B4==1 | 
                  data1$B5==1 | data1$B6==1 | 
                  data1$B7==1 | data1$B8==1,1,NA)

variables.b<-names(data1) %>% str_subset(pattern = "B9")


data1$B9_7<-ifelse(data1$B9_O=="-",NA,1)

data1$B9<-ifelse(data1$B9_1==1 | data1$B9_2==1 | 
                  data1$B9_3==1 | data1$B9_4==1 | 
                  data1$B9_5==1 | data1$B9_6==1 | 
                  data1$B9_7==1 ,1,NA)



# fundir varias variables






salida<-c("A4","A4.b", "b")
criterio.salto<-c("2:15","1","1")
llegada<-c("A5","A7","B9")

# Creación de data frame
resultadosS<-data.frame(data1$REGISTRO)



for (i in 1:length(salida)){
  
  ifelse ((str_detect(criterio.salto[i],",")), 
          rango<- criterio.salto[i] %>% str_split(",") %>% 
            unlist() %>% as.numeric(),
          
          rango<-criterio.salto[i] %>% str_split(":") %>%
            unlist() %>% as.numeric() %>% secuencia
  )
  
  coordenada1<-grep(paste("^",salida[i],"$",sep=""), names(data1))
  coordenada2<-grep(paste("^",llegada[i],"$",sep=""), names(data1))
  
  variable1<-as.integer(unlist(data1[,coordenada1]))
  
  s.check<-(variable1 %in%  rango)*1
  

  ll.check<-(!is.na(data1[coordenada2])& as.vector(data1[[coordenada2]]) %nin% "")*1

  revision<-paste0(s.check, ll.check)
  resultadosS<-cbind(revision,resultadosS)
}


# Creacion de nombres de columnas, reordenamiento de dataframe y etiquetaje

nombres.u<-c()


for (i in 1:length(llegada)){
  x1<-paste0(salida[i]," (",criterio.salto[i],")","-> ",llegada[i])
  nombres.u<-append(nombres.u,x1)
}


# Reordenar base de datos y colocacion de etiquetas
resultadosS<-rev(resultadosS)

etiquetas<-  append("id", nombres.u)

colnames(resultadosS)<-etiquetas

# resumen

resumen.s<-as.data.frame(nombres.u)

saltos.p<-resultadosS[2:ncol(resultadosS)]



saltos.p[saltos.p=="00"]<-NA
saltos.p[saltos.p=="11"]<-1
saltos.p[saltos.p=="10"]<-0
saltos.p[saltos.p=="01"]<-0


for (i in 1:ncol(saltos.p)){
  saltos.p[i]<-as.numeric(unlist(saltos.p[i]))
}

valores<-unname(round(colMeans(saltos.p[1:ncol(saltos.p)], na.rm = T),2)*100)


resumen.s$'% de saltos correctos'<-valores

# Recodificacisn sugerida de las observaciones

resultadosS<-resultadosS %>% 
  dplyr::mutate_at(c(2:ncol(resultadosS)), recode, '00'='sin salto', 
                   '11'='s. correcto','01'='s. con otro valor', '10'='s. NA llegada')







################################################################################

################################################################################
###################### REVISION DE PREGUNTAS CON SALTO 2 #######################
################################################################################

################################################################################


salida<-c("D1_1","D6")

criterio.salto<-c("1","0")

llegada1<-c("D2","D7_1")

llegada2<-c("D6","D10")


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
  
  
  ll.check1<-(!is.na(data1[,coordenada2])  & as.vector(data1[[coordenada2]]) %nin% "")*1
  ll.check2<-(!is.na(data1[,coordenada3]) & as.vector(data1[[coordenada3]]) %nin% "")*1
  
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
  dplyr::mutate_at(c(2:ncol(resultadosS)), recode, '000'='sin salto', 
                   '101'='s. correcto','111'='error', '100'='error',
                   '011'='sin salto','001'='sin salto','010'='sin salto',
                   '110'='error')










############## Tablas de frecuencias generales  ##########################

distribuciones<-sapply(data1, sjmisc::frq)




listado <- list("resumen NA variable" = resumen.n, 
                "NA Desagregado" = resultadosNA,
                "resumen SALTO 1" = resumen.s,
                "SALTO 1 Desagregado" = resultadosS)


write.xlsx(listado, file = "malla.de.validacion.CENISMA.xlsx", rowNames = TRUE)



write.xlsx(distribuciones, file = "frecuencias.CENISMA.xlsx", rowNames = TRUE)
