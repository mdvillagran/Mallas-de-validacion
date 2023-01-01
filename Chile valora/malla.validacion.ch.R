


# PAQUETES
library(openxlsx)
library(dplyr)
library(tidyverse)
library(haven)
library(labelled)



rm(list = ls())
setwd("C:/Users/Villagran/Desktop/datavoz/22.9.22 Chile Valora/mallas de validacion")

# BBDD

data1<-haven::read_sav("datos_DS2234_VF_01_12_2022.sav")

################################################################################
############################## EVALUACION DE NA ################################
################################################################################

####### Variables obligatorias

names(data1)

obligatorias<-c("A1","A2","A3","A4","A5", "B0_1", "B0_2", "B0_3","B0_4",
                "B1_1","B1_2","B1_3","B1_4","B1_5","B1_6",
                "B2_1","B2_2","B2_3","B2_4","B2_5","B2_6","B2_7","B2_8",
                "B3",
                "C1_1","C1_2","C1_3","C1_4","C1_5",
                "C2",
                "D1_1","D1_2","D1_3","D1_4","D1_5",
                "D2",
                "D3",
                "D5_1","D5_2","D5_3","D5_4","D5_5","D5_6","D5_7","D5_8","D5_9",
                "D7B",
                "D8",
                "G1",
                "G6",
                "H1_1","H1_2","H1_3",
                "H2",
                "H3",
                "I1",
                "I2",
                "I3")

setdiff(obligatorias, names(data1))

#######

resultadosNA<-data.frame(data1$REGISTRO)

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




######################## OBLIGATORIAS NO CERTIFICADOS ##########################

no.certificados<-subset(data1, F4==2)

obligatorias<-c("D7")

setdiff(obligatorias, names(no.certificados))

#######

resultadosNA.NC<-data.frame(no.certificados$REGISTRO)

`%notin%` <- Negate(`%in%`)

for (i in obligatorias){
  coordenada<-grep(paste0("^",i,"$"), names(no.certificados))
  columna<-((!is.na(no.certificados[,coordenada]) & as.vector(no.certificados[[coordenada]]) %notin% "")*1)
  resultadosNA.NC<-cbind(assign("i",columna),resultadosNA.NC)
}


# Reordenamiento del data frame de salida y etiquetaje de sus columnas

resultadosNA.NC<-rev(resultadosNA.NC)

rotulos<-  append("id.", obligatorias)

colnames(resultadosNA.NC)<-rotulos


# Reporte resumen (completitud total variables)

valores<-unname(round(colMeans(resultadosNA.NC[2:ncol(resultadosNA.NC)]),2)*100)


resumen.n.NC<-as.data.frame(obligatorias)
resumen.n.NC$'% de completitud'<-valores

# Recodificacisn sugerida de las observaciones

resultadosNA.NC<-resultadosNA.NC %>% 
  dplyr::mutate_at(c(2:ncol(resultadosNA.NC)), recode, '0'='NA', '1'='Dato')




######################## OBLIGATORIAS CERTIFICADOS #############################

certificados<-subset(data1, F4==1)

obligatorias<-c("E1_1","E1_2","E1_3","E1_4",
                "E2", 
                "E4_1", "E4_2", "E4_3","E4_4",
                "E6_1","E6_2","E6_3","E6_4","E6_5","E6_6")

setdiff(obligatorias, names(certificados))

#######

resultadosNA.C<-data.frame(certificados$REGISTRO)

`%notin%` <- Negate(`%in%`)



for (i in obligatorias){
  coordenada<-grep(paste0("^",i,"$"), names(certificados))
  columna<-((!is.na(certificados[,coordenada]) & as.vector(certificados[[coordenada]]) %notin% "")*1)
  resultadosNA.C<-cbind(assign("i",columna),resultadosNA.C)
}


# Reordenamiento del data frame de salida y etiquetaje de sus columnas

resultadosNA.C<-rev(resultadosNA.C)

rotulos<-  append("id.", obligatorias)

colnames(resultadosNA.C)<-rotulos


# Reporte resumen (completitud total variables)

valores<-unname(round(colMeans(resultadosNA.C[2:ncol(resultadosNA.C)]),2)*100)


resumen.n.C<-as.data.frame(obligatorias)
resumen.n.C$'% de completitud'<-valores

# Recodificacisn sugerida de las observaciones

resultadosNA.C<-resultadosNA.C %>% 
  dplyr::mutate_at(c(2:ncol(resultadosNA.C)), recode, '0'='NA', '1'='Dato')





################################################################################
############################## EVALUACION DE SALTOS ############################
################################################################################

### bbdd completa

a<-grep("D4_1H", names(data1))
b<-grep("D4_5M", names(data1))


data1$DA<-rowSums(data1[a:b], na.rm = T)
data1$DA[data1$DA==0]<-NA

data1$D5A<-data1$D5_6+data1$D5_7+data1$D5_8+data1$D5_9


#####
# Creacion de data frame con los id, donde guardaremos los resultados del bucle

#

secuencia<-function(objeto){
  n<-length(objeto)
  seq(objeto[[1]],objeto[[n]])
}

#


salida<-c("B3","B3","C2","D3","D5A","D8","G1","G1","G3","G1")
criterio.salto<-c("1","1","1","0:280", "0","1","1","1","1","1")
llegada<-c("B4","B5","C3","DA","D6","D9","G2","G3","G4","G5" )

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
  
  s.check<-(variable1 %in%  rango )*1
  

  ll.check<-(!is.na(data1[coordenada2]) & as.vector(data1[[coordenada2]]) %notin% "-")*1

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




###################### Saltos en subset certificados ##########################

salida<-c("E2","E1_1")
criterio.salto<-c("1:7","1")
llegada<-c("E3", "E5M" )

# Creación de data frame
resultadosS.C<-data.frame(certificados$REGISTRO)



for (i in 1:length(salida)){
  
  ifelse ((str_detect(criterio.salto[i],",")), 
          rango<- criterio.salto[i] %>% str_split(",") %>% 
            unlist() %>% as.numeric(),
          
          rango<-criterio.salto[i] %>% str_split(":") %>%
            unlist() %>% as.numeric() %>% secuencia
  )
  
  coordenada1<-grep(paste("^",salida[i],"$",sep=""), names(certificados))
  coordenada2<-grep(paste("^",llegada[i],"$",sep=""), names(certificados))
  
  variable1<-as.integer(unlist(certificados[,coordenada1]))
  
  s.check<-(variable1 %in%  rango )*1
  
  
  ll.check<-(!is.na(certificados[coordenada2]) & as.vector(certificados[[coordenada2]]) %notin% "-")*1
  
  revision<-paste0(s.check, ll.check)
  resultadosS.C<-cbind(revision,resultadosS.C)
}


# Creacion de nombres de columnas, reordenamiento de dataframe y etiquetaje

nombres.u<-c()


for (i in 1:length(llegada)){
  x1<-paste0(salida[i]," (",criterio.salto[i],")","-> ",llegada[i])
  nombres.u<-append(nombres.u,x1)
}


# Reordenar base de datos y colocacion de etiquetas
resultadosS.C<-rev(resultadosS.C)

etiquetas<-  append("id", nombres.u)

colnames(resultadosS.C)<-etiquetas

# resumen

resumen.s.C<-as.data.frame(nombres.u)

saltos.p<-resultadosS.C[2:ncol(resultadosS.C)]



saltos.p[saltos.p=="00"]<-NA
saltos.p[saltos.p=="11"]<-1
saltos.p[saltos.p=="10"]<-0
saltos.p[saltos.p=="01"]<-0


for (i in 1:ncol(saltos.p)){
  saltos.p[i]<-as.numeric(unlist(saltos.p[i]))
}

valores<-unname(round(colMeans(saltos.p[1:ncol(saltos.p)], na.rm = T),2)*100)


resumen.s.C$'% de saltos correctos'<-valores

# Recodificacisn sugerida de las observaciones

resultadosS.C<-resultadosS.C %>% 
  dplyr::mutate_at(c(2:ncol(resultadosS.C)), recode, '00'='sin salto', 
                   '11'='s. correcto','01'='s. con otro valor', '10'='s. NA llegada')




################# # #
################# # #
################# # #
##### EXPORTACION # #
################# # #
################# # #
################# # #

names <- list('NA.BBDD' = resultadosNA, 'NA.RESUMEN.BBDD' = resumen.n,
              "NA.NCERT.BBDD" = resultadosNA.NC,"NA.RESUMEN.NO.CERT." =resumen.n.NC,
              "NA.CERT.BBDD" = resultadosNA.C,"NA.RESUMEN.CERT." =resumen.n.C,
              "SALTOS.BBDD"= resultadosS,"RESUMEN.SALTOS.BBDD"= resumen.s,
              "SALTOS.CERTIFICADOS"=resultadosS.C, "RESUMEN.SALTOS.CERT"=resumen.s.C)


write.xlsx(names, file = 'MALLA.DE.VALIDACION.CHILE.VALORA.xlsx')
