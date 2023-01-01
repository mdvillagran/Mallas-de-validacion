

rm(list = ls())
setwd("C:/Users/Villagran/Desktop/datavoz/12.10.2022.CENISMA/bbdd/bbdd final")

# PAQUETES
library(openxlsx)
library(dplyr)
library(tidyverse)
library(haven)
library(labelled)


# LECTURA DE BB.DD DE DATOS
# SALTOS<-read.xlsx("SALTOS.xlsx")

data1<- read_sav("cenisma.sav")


################################################################################
############################## EVALUACION DE NA ################################
################################################################################

####### Variables obligatorias

obligatorias<-names(data1)
data1<-subset(data1, is.na(IDEN_RECHAZO))

#######

resultadosNA<-data.frame(data1$FOLIO)

`%notin%` <- Negate(`%in%`)

for (i in obligatorias){
  coordenada<-grep(paste0("^",i,"$"), names(data1))
  columna<-((!is.na(data1[,coordenada]) & as.vector(data1[[coordenada]]) %notin% "")*1)
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

# Recodificacisn sugerida de las observaciones

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
  n<-length(objeto)
  seq(objeto[[1]],objeto[[n]])
}

#


salida<-c("P36")
criterio.salto<-c("1")
llegada<-c("P37")

# Creación de data frame
resultadosS<-data.frame(data1$FOLIO)



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
  

  ll.check<-(!is.na(data1[coordenada2]))*1

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


############## Tablas de frecuencias generales  ##########################

distribuciones<-sapply(data1, sjmisc::frq)




listado <- list("resumen NA variable" = resumen.n, 
                "NA Desagregado" = resultadosNA,
                "resumen SALTO 1" = resumen.s,
                "SALTO 1 Desagregado" = resultadosS)


write.xlsx(listado, file = "malla.de.validacion.CENISMA.xlsx", rowNames = TRUE)



write.xlsx(distribuciones, file = "frecuencias.CENISMA.xlsx", rowNames = TRUE)
