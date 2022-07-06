
rm(list = ls())
setwd("C:/Users/villagran/Desktop/datavoz/Las salinas/malla.v")

# LECTURA DE BB.DD DE DATOS
library(openxlsx)
library(dplyr)
library(plyr)
library(tidyverse)
library(stringr)
library(haven)
library(labelled)





################################################################################
######################## check de base de datos Salinas ########################
################################################################################


data<- read_sav("DS2219_ Las_Salinas-jun_-22_2022_05_07_15_10.sav")

data<- data %>% filter(Status %in% c("Approved", "Requires Approval"))

parte1<-data[1:37]
parte2<-data[1007:ncol(data)]

data1<-cbind(parte1, parte2)

parte1<-NULL
parte2<-NULL


### Transformar variables dobles a numéricas


nombres<-as.data.frame(look_for(data1)[[2]])
nombres$tipo<-unname(look_for(data1)[[4]])

nombres <- nombres %>% filter (! tipo %in% c("chr"))
colnames(nombres)[1]<-"variables.base"


posicion<-c()

for (i in nombres$variables.base){
  coordenada<-grep(paste0("^",i,"$"), names(data1))
  data1[coordenada]<-as.numeric(unlist(data1[coordenada]))
  }


############### CONTEO DE AVANCE POR ENCUESTAS POR UMP Y SECTOR ################

sectores<-read.xlsx("muestrafinal_LasSalinas.xlsx")

sectores<-sectores %>% select(Sector, UMP_ORIGINAL)



ump<-data1 %>% select(ump)

ump$encuestas=1

conteo1<-aggregate(encuestas~ump, ump,sum)

colnames(sectores)[2]<-"ump"

encuestas.ump.sector<-merge(sectores, conteo1, all.x = T)

encuestas.ump.sector$encuestas[with(encuestas.ump.sector, is.na(encuestas) )] <-0


encuestas.ump.sector = encuestas.ump.sector [ , c(2,1,3)]

write.xlsx(encuestas.ump.sector, file = "avance.de.encuestas.xlsx", row.names = TRUE)


###############################################################################
###############################################################################
# # # # # # # # # # # # #  checkeo de incidencias # # # # # # # # # # # # # # #
################################################################################
################################################################################

incidencias<-data %>%  select(ends_with("Codigo_disposicion"))

objeto1<-(gather(incidencias)[2])

sjmisc::frq(objeto1) # 110 encuestas finalizadas


################################################################################
################### Aplicacion de loop y creacion de planillas #################
################################################################################

################################################################################

################################################################################
######################## check de preguntas obligatorias #######################
################################################################################

                # identidicadores
obligatorias<-c("SbjNum","Srvyr","comuna",".", "p2",
                # Preguntas 2:5
    "T_p2_1", "T_p2_2", "T_p2_3", "T_p2_4","T_p2_5","T_p2_6", "T_p2_7", "T_p2_8",
"T_p2_9","T_p2_10", "p3","T_p4_1","T_p4_2", "T_p4_3", "p5_1","p5_2",
                # Pregunta 7,12,19,23,24
"p7","p12","p19","T_p23_1","T_p23_2","T_p23_3","T_p23_4","T_p23_5","T_p23_6",
"T_p23_7","T_p23_8","T_p23_9","T_p23_10","T_p23_11","T_p23_12","T_p23_13",
"p24","p26_1", 
                # Pregunta 28,30,31,40
"p28_1","p30","p31", "p32","p40",
                # Pregunta 42, 43, 44, 45, 46
"p42","p43","p44","p45","p46","p47_O1","p48_O1",
                #preguntas 49,50,51,52,53,54
"p49","p50","p51","p52","p53","p54")





# Evaluación de las variables seleccionadas y casos NA mediante el bucle



#######

resultadosNA<-data.frame(data1$SbjNum)

for (i in obligatorias){
  coordenada<-grep(paste0("^",i,"$"), names(data1))
  columna<-((!is.na(data1[,coordenada]) & data1[,coordenada] != "")*1)
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

################################################################################
###################### REVISION DE PREGUNTAS CON SALTO 1 #######################
################################################################################

################################################################################

# Creación de vectores SALIDAS, LLEGADAS y CRITERIO DE SALTO

# salto p3(10) -> p3_1


# p5_1(12)->p6
# p5_2(12)->p6

data1 <- mutate(data1, p5.6 = case_when(
  p5_1 == 12 | p5_2 == 12 ~ 1
  
))



# p14_O1(6)->p15
# p14_O2(6)->p15
# p14_O3(6)->p15


data1 <- mutate(data1, p14.15 = case_when(
  p14_O1 == 6 | p14_O2 == 6 | p14_O3 == 6 ~ 1
  
))


# p17_1(6)->p18
# p17_2(6)->p18

data1 <- mutate(data1, p17.18 = case_when(
  p17_1 == 6 | p17_2 == 6 ~ 1
  
))



 
# p20_O1(6)->p21
# p20_O2(6)->p21
# p20_O3(6)->p21
# p20_O4(6)->p21
# p20_O5(6)->p21
# p20_O6(6)->p21
# p20_O7(6)->p21
# p20_O8(6)->p21

data1 <- mutate(data1, p20.21 = case_when(
  p20_O1 == 6 | p17_2 == 6 ~ 1
  
))




# p26_1(10)->p27
# p26_2(10)->p27


data1 <- mutate(data1, p26.27 = case_when(
  p26_1 == 10 | p26_2 == 10 ~ 1
  
))



# p28_1(11)->p29
# p28_2(11)->p29

data1 <- mutate(data1, p28.29 = case_when(
  p28_1 == 11 | p28_2 == 11 ~ 1
  
))



# p36_O1(10)->p37
# p36_O2(10)->p37
# p36_O3(10)->p37
# p36_O4(10)->p37
# p36_O5(10)->p37
# p36_O6(10)->p37
# p36_O7(10)->p37
# p36_O8(10)->p37
# p36_O9(10)->p37
# p36_O10(10)->p37
# p36_O11(10)->p37
# p36_O12(10)->p37

data1 <- mutate(data1, p36.37 = case_when(
  p36_O1 == 10 | p36_O2 == 10 |
    p36_O3 == 10 | p36_O4 == 10 |
     p36_O5 == 10 | p36_O6 == 10 |
       p36_O7 == 10 | p36_O8 == 10 |
         p36_O9 == 10 | p36_O10 == 10 |
          p36_O11 == 10 | p36_O12 == 10
    ~ 1
  ))




# p38_1(12)->p39
# p38_2(12)->p39

data1 <- mutate(data1, p38.39 = case_when(
  p38_1 == 12 | p38_2 == 12 ~ 1
  
))




salida<-c("p7","p32","p33","p5.6","p14.15","p17.18","p20.21","p26.27","p28.29",
          "p36.37","p38.39")

criterio.salto<-c("1,88,99","1","2","12","6","6","6","10","11","10","12")

llegada<-c("p8","p33","p34","p6","p15","p18","p21","p27","p29","p37","p39")


################################################################################


#####
# Creacion de data frame con los id, donde guardaremos los resultados del bucle

resultadosS<-data.frame(data1$SbjNum)

#

secuencia<-function(objeto){
  n<-length(objeto)
  seq(objeto[[1]],objeto[[n]])
}

#

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
  
  
  ll.check<-(!is.na(data1[,coordenada2]) & data1[,coordenada2] != "")*1

  revision<-paste0(s.check,ll.check)
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


salida<-c("p7","p12","p19","p22_1","p22_2", "p24", "p26_1","p28_1","p33","p35")

criterio.salto<-c("2,88,99","2,88,99","2,88,99","9,88,99",
                  "9,88,99","2,88,99","9,88,99","10,88,99","1,88,99","2,88,99")

llegada1<-c("p8","p14_O1","p20_O1","p22_2","p22_3","p25","p26_2","p28_2","p34","p36_O1")

llegada2<-c("p19","p19","p22_1","T_p23_1","T_p23_1","p26_1","p28_1","p30","p36_O1",
            "p40")


resultados2<-data.frame(data1$SbjNum)


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
saltos.p[saltos.p=="010"]<-0
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



 # Exportacion de los resultados a excel único
 
 
 listado <- list("resumen NA variable" = resumen.n, 
                 "NA Desagregado" = resultadosNA,
                 "resumen SALTO 1" = resumen.s,
                 "SALTO 1 Desagregado" = resultadosS,
                 "resumen SALTO 2" = resumen.s2,
                 "SALTO 2 Desagregado" = resultados2)
 
 
 write.xlsx(listado, file = "malla.validacion.salinas.xlsx", row.names = TRUE)
 
 
 