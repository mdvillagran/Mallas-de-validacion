
library(haven)
library(dplyr)
library(sjmisc)
library(stringr)



rm(list = ls())
setwd("C:/Users/Villagran/Desktop/datavoz/8.8.2023 ACHS/bbdd")


bbdd<-read_spss("datos_DS2330_VF_31_08_2023.sav")



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

obligatorias<-bbdd %>% dplyr::select(P1,P2_1,P2_2,P2_3,P2_4,P2_5,P2_6,P2_7, P3,
                                     P5_1,P5_2,P5_3,P5_4,P5_5,P5_6,P5_7,P5_8,
                                     P6_1,P6_2,P6_3,P6_4,P6_5,P6_6,P6_7, P6B_1_1,
                                     P6B_1_2,P6B_1_3,P6B_1_4,P6B_1_5,P6B_1_6,
                                     P6B_1_8, P6B_1_9, P7_1, P7_2,P7_3,P7_4,P7_5,
                                     P8_1,P8_2,P8_3,P8_4,P8_5,P8_6,P8_7,P8_8,P8_9,
                                     P9_1,P9_2,P9_3,P9_4,P9_5, P9_6,P11_1,P11_2,P11_3
                                     ,P11_4,P11_5,P11_6,P11_7,P11_8,P11_9,P11_10,P12,
                                     P14,P15, DE1,DE2,DE3,DE4,DE5, ACEPTA)


### Revisamos completitud de los datos

resultadosNA1<-bbdd %>% dplyr::select(REGISTRO)



for (i in names(obligatorias)){
  coordenada<-grep(paste0("^",i,"$"), names(obligatorias))
  columna<-((!is.na(obligatorias[,coordenada]) & as.vector(obligatorias[[coordenada]]) %nin% "")*1)
  resultadosNA1<-cbind(assign("i",columna),resultadosNA1)
}


# Reordenamiento del data frame de salida y etiquetaje de sus columnas

resultadosNA1<-rev(resultadosNA1)

rotulos<-  append("id", setdiff(names(obligatorias), "SbjNum"))

colnames(resultadosNA1)<-rotulos


# Reporte resumen (completitud total variables grupo 1)

valores<-unname(round(colMeans(resultadosNA1[2:ncol(resultadosNA1)]),2)*100)


resumen.n1<-as.data.frame(setdiff(names(obligatorias), "SbjNum"))
colnames(resumen.n1)[1]<-"variables"
resumen.n1$'% de completitud'<-valores




################################################################################
################################# Saltos tipo 1 ################################
################################################################################

bbdd$p2.a<-ifelse(bbdd$P2_2 ==1 | bbdd$P2_3 ==1|bbdd$P2_4 ==1|bbdd$P2_5 ==1
                  ,1,0)

# check<-bbdd %>% dplyr::select(p2.a, P2_2, P2_3,P2_4, P2_5)

bbdd$p2.b<-ifelse(bbdd$P2_3 ==1|bbdd$P2_4 ==1|bbdd$P2_5 ==1
                  ,1,0)

# check<-bbdd %>% dplyr::select(p2.a, P2_1)



salida<-c("P3","p2.a","p2.b","DE5")

criterio.salto<-c("2:5","1","1","1")

llegada<-c("P4","P7_6","P7_7","DE6")

# Creacion de data frame con los id, donde guardaremos los resultados del bucle

resultadosS<-data.frame(bbdd$REGISTRO)

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


# Recodificacisn sugerida de las observaciones

resultadosS<-resultadosS %>% 
  dplyr::mutate_at(c(2:ncol(resultadosS)), recode, '00'='', 
                   '11'='','01'='s. con otro valor', '10'='s. NA llegada')

# ID  de las encuestas que presentan algún problema

Encuestas.con.problemas <- resultadosS %>%
  filter_at(vars(2:ncol(resultadosS)), any_vars(. %in% c('s. con otro valor',
                                                         's. NA llegada')))


# Extracción de variables implicadas en el salto en la bbdd filtrando por las
# encuestas con problemas

output.de.chequeo<-bbdd %>% dplyr::select(REGISTRO, P2_2,P2_3,P2_4,P2_5,P3,P4, P7_6,
                                   P7_7) %>% filter(REGISTRO %in% Encuestas.con.problemas$id )

documento<-list("Encuestas con problemas"=Encuestas.con.problemas,
                "BBDD Filtrada" = output.de.chequeo)


hoy<-format( Sys.Date(), format="%d %B %Y")


openxlsx::write.xlsx(documento, file = paste0("Malla de validacion Achs ",hoy,".xlsx"), 
                     rowNames = TRUE)


################################################################################
