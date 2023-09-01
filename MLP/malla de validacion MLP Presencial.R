
library(haven)
library(dplyr)
library(sjmisc)
library(stringr)



rm(list = ls())
setwd("C:/Users/Villagran/Desktop/datavoz/15.3.2023.Minera los pelambres/BBDD")


bbdd<-read_spss("choapa.sav")



# P1 Conoce o ha oído hablar de minera los pelambres 
# (Na = 0, es un comportamiento correcto de la pregunta)

sum(is.na(bbdd$p1))


# Salto correcto pregunta P1 a P2 (Valor 0, es correcto)

(sum((bbdd$p1!=1)))-sum(is.na(bbdd$p2))


################################################################################
############# Modulo 1 | Conocimiento y evaluación general #####################
################################################################################

# Preguntas que si bien tienen un salto previo (Solo puede ser respondida por "conoce a MLP" Sí,
# No debe tener NA salvo este salto)

modulo.1.p7<-bbdd %>% select(SbjNum,p2, p3_1, p3_2,p4_1,p4_1,p4_2,p4_3,p4_4,p4_5, p4_6, p4_7,
                          p4_8,p4_9,p4_10,p4_11,p5,p7_1,p7_2,p7_3,p7_4,p19_2,p19_3,
                          p19_4,p19_5,p21,p22,p23, p24_1,p24_2,p25_1,p25_2,p26_1,
                          p26_2,p27,p35_1,p35_3,p35_4,p35_5,p41)


# Dejamos sólo las personas que cononcen MLP
modulo.1.p7<- modulo.1.p7[!is.na(modulo.1.p7$p2),]



### Revisamos completitud de los datos

resultadosNA1<-data.frame(modulo.1.p7$SbjNum)



for (i in setdiff(names(modulo.1.p7), "SbjNum")){
  coordenada<-grep(paste0("^",i,"$"), names(modulo.1.p7))
  columna<-((!is.na(modulo.1.p7[,coordenada]) & as.vector(modulo.1.p7[[coordenada]]) %nin% "")*1)
  resultadosNA1<-cbind(assign("i",columna),resultadosNA1)
}


# Reordenamiento del data frame de salida y etiquetaje de sus columnas

resultadosNA1<-rev(resultadosNA1)

rotulos<-  append("id", setdiff(names(modulo.1.p7), "SbjNum"))

colnames(resultadosNA1)<-rotulos


# Reporte resumen (completitud total variables grupo 1)

valores<-unname(round(colMeans(resultadosNA1[2:ncol(resultadosNA1)]),2)*100)


resumen.n1<-as.data.frame(setdiff(names(modulo.1.p7), "SbjNum"))
colnames(resumen.n1)[1]<-"variables"
resumen.n1$'% de completitud'<-valores


################################################################################
###################### Completitud de variables grupo 2 ########################

modulo.4.p53<-bbdd %>% select(SbjNum,p14_1,p14_2, p14_3, p15_1,p15_2,p15_3,p16,
                             p32,p33,p34,p37_1,p37_2,p37_3,p37_4,p37_5,p37_6,
                             p37_7,p37_8,p37_9,p37_10,p39_1,p39_2,p39_3,p39_4,
                             p39_5,p39_6,p39_7,p39_8,p43, p43_edad,p44,p45,
                             p46,p48,p50, p51, p52_1,p52_2,p52_3,p52_4,p53)



### Revisamos completitud de los datos

resultadosNA1<-data.frame(modulo.4.p53$SbjNum)



for (i in setdiff(names(modulo.4.p53), "SbjNum")){
  coordenada<-grep(paste0("^",i,"$"), names(modulo.4.p53))
  columna<-((!is.na(modulo.4.p53[,coordenada]) & as.vector(modulo.4.p53[[coordenada]]) %nin% "")*1)
  resultadosNA1<-cbind(assign("i",columna),resultadosNA1)
}


# Reordenamiento del data frame de salida y etiquetaje de sus columnas

resultadosNA1<-rev(resultadosNA1)

rotulos<-  append("id", setdiff(names(modulo.4.p53), "SbjNum"))

colnames(resultadosNA1)<-rotulos


# Reporte resumen (completitud total variables grupo 1)

valores<-unname(round(colMeans(resultadosNA1[2:ncol(resultadosNA1)]),2)*100)


resumen.n2<-as.data.frame(setdiff(names(modulo.4.p53), "SbjNum"))
colnames(resumen.n2)[1]<-"variables"
resumen.n2$'% de completitud'<-valores

################################################################################
################################# Saltos tipo 1 ################################
################################################################################

bbdd$p8_si<-ifelse(bbdd$p8_1==1 |bbdd$p8_2==1|bbdd$p8_3==1,1,0)

bbdd$p8_si.1.3<-ifelse(bbdd$p8_1==1 |bbdd$p8_3==1,1,0)

bbdd$p.salamanca.11<-ifelse(bbdd$comuna_=="SALAMANCA"&(bbdd$p11==1|bbdd$p11==2),1,0)

bbdd$p.salamanca.illa.1<-ifelse((bbdd$comuna_=="SALAMANCA"|bbdd$comuna_=="ILLAPEL")&
                                  bbdd$p1==1,1,0)

bbdd$vilos<-ifelse((bbdd$comuna_=="LOS VILOS")&
                                  bbdd$p1==1,1,0)

bbdd$p1.p35<-ifelse(bbdd$p1==1 &(bbdd$p35_1==1|bbdd$p35_3==1|
                                   bbdd$p35_4==1|bbdd$p35_5==1),1,0)

bbdd$p39.p40<-ifelse(bbdd$p39_4<5 |bbdd$p39_5<5,1,0)

bbdd$p41.p42<-ifelse(bbdd$p1==1 & bbdd$p41<5,1,0)

bbdd$p46.p47<-ifelse(bbdd$p46>1 ,1,0)

salida<-c("p8_si","p8_si.1.3","p10", "p.salamanca.11","p.salamanca.11",
          "p19_2","p19_3","p19_4","p19_5", "p.salamanca.illa.1",
          "p.salamanca.illa.1", "vilos", "vilos", "p1.p35","p37_1",
          "p37_2","p37_3","p37_4","p37_5","p37_6",
          "p37_7","p37_8","p37_9","p37_10","p39.p40",
          "p41.p42", "p46.p47","p46.p47")

criterio.salto<-c("1","1","1","1","1",
                  "1","1","1","1","1",
                  "1","1","1","1","1",
                  "1","1","1","1","1",
                  "1","1","1","1","1",
                  "1","1","1")

llegada<-c("p9","p10","p11","p12","p13",
           "p20_2","p20_3","p20_4","p20_5","p28",
           "p29","p30","p31","p36","p38_1"
           ,"p38_2","p38_3","p38_4","p38_5","p38_6",
           "p38_7","p38_8","p38_9","p38_10", "p40",
           "p42","p47","p49")




# Creacion de data frame con los id, donde guardaremos los resultados del bucle

resultadosS<-data.frame(bbdd$SbjNum)

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
  
  coordenada1<-grep(paste("^",salida[i],"$",sep=""), names( bbdd))
  coordenada2<-grep(paste("^",llegada[i],"$",sep=""), names( bbdd))
  
  variable1<-as.integer(unlist( bbdd[,coordenada1]))
  
  s.check<-(variable1 %in%  rango)*1
  
  
  ll.check<-(!is.na( bbdd[,coordenada2]) & as.vector(bbdd[[coordenada2]]) %nin% "")*1
  
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


################################################################################
###################### REVISION DE PREGUNTAS CON SALTO 2 #######################
################################################################################

################################################################################

bbdd$canela<-ifelse(str_detect(bbdd$comuna_,"CANELA"),1,0)



salida<-c("p5","canela","canela","p16","p16")

criterio.salto<-c("1,2,88,99","1","1", "2,88,99", "2,88,99")

llegada1<-c("p6_1","p8_1","p8_2","p17","p18")

llegada2<-c("p7_1","p14_1","p14_1","p32","p32")



resultados2<-data.frame( bbdd$SbjNum)


for (i in 1:length(salida)){
  
  ifelse ((str_detect(criterio.salto[i],",")), 
          rango<- criterio.salto[i] %>% str_split(",") %>% 
            unlist() %>% as.numeric(),
          
          rango<-criterio.salto[i] %>% str_split(":") %>%
            unlist() %>% as.numeric() %>% secuencia
  )
  
  coordenada1<-grep(paste("^",salida[i],"$",sep=""), names( bbdd))
  coordenada2<-grep(paste("^",llegada1[i],"$",sep=""), names( bbdd))
  coordenada3<-grep(paste("^",llegada2[i],"$",sep=""), names( bbdd))
  
  variable1<-as.integer(unlist( bbdd[,coordenada1]))
  
  s.check<-(variable1 %in%  rango)*1


  ll.check1<-(!is.na( bbdd[,coordenada2]) & as.vector(bbdd[[coordenada2]]) %nin% "")*1

  ll.check2<-(!is.na( bbdd[,coordenada3]) &  as.vector(bbdd[[coordenada3]]) %nin% "")*1

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


############################# Ouputs ###########################################




listado <- list("NAs1" = resumen.n1, 
                "NAs2" =resumen.n2,
                "Saltos1"=resumen.s,
                "Saltos2"=resumen.s2)

hoy<-format( Sys.Date(), format="%d %B %Y")

openxlsx::write.xlsx(listado,  file = paste0("Malla de validacion ",hoy,".xlsx"), 
                     rowNames = F)

