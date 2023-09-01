
library(haven)
library(dplyr)
library(sjmisc)
library(stringr)



rm(list = ls())
setwd("C:/Users/Villagran/Desktop/datavoz/20.3.2023 Violencia social Gerber/BBDD")



bbdd<-read_spss("BBDD17.7.sav")



##



# bbdd2<-bbdd2 %>% dplyr::mutate(C4_NNA = coalesce(!!! syms (c("C4_NNA_O1","C4_NNA_O2","C4_NNA_O3"))))

variables.obligatorias<-c("b1","b2","c1_1","c1_2","c1_3","c1_4","c2_1","c2_2","c3","c4_1",
                          "c4_2","c5_1","c5_2","c6_1","c6_2","c7_1","c7_2","d1_2",
                          "e0","e1_1","e1_2","e1_3","e2","e3","e7","e13","e14",
                          "e17","e18_1","e18_2",
                          "e18_3","e19_1","e19_2",
                          "e19_3","e20"
                        )



################################################################################
############################ Variables obligatorias ############################
################################################################################




#######

resultadosNA<-data.frame(bbdd$SbjNum)


for (i in variables.obligatorias){
  coordenada<-grep(paste0("^",i,"$"), names(bbdd))
  columna<-((!is.na(bbdd[,coordenada]) & as.vector(bbdd[[coordenada]]) %nin% "")*1)
  resultadosNA<-cbind(assign("i",columna),resultadosNA)
}


# Reordenamiento del data frame de salida y etiquetaje de sus columnas

resultadosNA<-rev(resultadosNA)

rotulos<-  append("id.", variables.obligatorias)

colnames(resultadosNA)<-rotulos


# Reporte resumen (completitud total variables)

valores<-unname(round(colMeans(resultadosNA[2:ncol(resultadosNA)]),2)*100)


resumen.n<-as.data.frame(variables.obligatorias)
resumen.n$'% de completitud'<-valores

# Recodificacisn sugerida de las observaciones

resultadosNA<-resultadosNA %>% 
  dplyr::mutate_at(c(2:ncol(resultadosNA)), recode, '0'='NA', '1'='Dato')

################################################################################
################################################################################
######################            SALTOS              ##########################
################################################################################

bbdd$ingreso<-ifelse(bbdd$e8_1 %in% c(88,99) | 
                       bbdd$e8_2 %in% c(88,99) | 
                       bbdd$e8_3 %in% c(88,99) | 
                       bbdd$e8_4 %in% c(88,99) | 
                       bbdd$e8_5 %in% c(88,99) | 
                       bbdd$e8_6 %in% c(88,99) | 
                       bbdd$e8_7 %in% c(88,99) , 1, NA)


bbdd$e1005<-ifelse(!is.na(bbdd$e9) & 
                     !is.na(bbdd$e10) & 
                     !is.na(bbdd$e11) & 
                     !is.na(bbdd$e12), 1, NA)

chech<-bbdd %>% dplyr::select(e8_1,e8_2,e8_3,e8_4, e8_5,
                              e8_6,e8_7, ingreso)

salida<-c("d1_2", "ingreso","e7","e7","e7","e7","e7","e7","e7")
criterio.salto<-c(2,1,1,2,3,4,5,6,7)
llegada<-c("d2","e1005","e8_1","e8_2","e8_3","e8_4","e8_5","e8_6", "e8_7")

# Creación de data frame
resultadosS<-data.frame(bbdd$SbjNum)

secuencia<-function(objeto){
  n<-length(objeto)
  seq(objeto[[1]],objeto[[n]])
}


for (i in 1:length(salida)){
  
  ifelse ((str_detect(criterio.salto[i],",")), 
          rango<- criterio.salto[i] %>% str_split(",") %>% 
            unlist() %>% as.numeric(),
          
          rango<-criterio.salto[i] %>% str_split(":") %>%
            unlist() %>% as.numeric() %>% secuencia
  )
  
  coordenada1<-grep(paste("^",salida[i],"$",sep=""), names(bbdd))
  coordenada2<-grep(paste("^",llegada[i],"$",sep=""), names(bbdd))
  
  variable1<-as.integer(unlist(bbdd[,coordenada1]))
  
  s.check<-(variable1 %in%  rango)*1
  
  
  ll.check<-(!is.na(bbdd[coordenada2])& as.vector(bbdd[[coordenada2]]) %nin% "")*1
  
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




################################################################################
################################################################################
######################            Consistencia             #####################
################################################################################


bbdd$anho.nacimiento<-ifelse(bbdd$e1_3==88|bbdd$e1_3==99,1,0)

bbdd$educacion.i<-ifelse(((bbdd$e2==1|bbdd$e2==2|bbdd$e2==3)&
                           (bbdd$e4==1|bbdd$e4==2))|
                           ((bbdd$e5==1|bbdd$e5==2|bbdd$e5==3)&
                              (bbdd$e6==1|bbdd$e6==2)),
                         
                         1,0)

bbdd$pdi.carabinero<-ifelse((bbdd$e18_1==1& bbdd$e18_2==1),1,0)

bbdd$carabineros.sinamigos<-ifelse((bbdd$e18_1==1& bbdd$e19_3==1),1,0)

bbdd$pdi.sinamigos<-ifelse((bbdd$e18_2==1& bbdd$e19_3==1),1,0)


consistencia<-bbdd %>% dplyr::filter( anho.nacimiento==1 |
                                       educacion.i ==1| pdi.carabinero==1|
                                        carabineros.sinamigos==1|
                                        pdi.sinamigos==1)



consistencia<-haven::as_factor(consistencia)

consistencia<-consistencia %>% dplyr::select(anho.nacimiento,
                                             educacion.i,
                                             pdi.carabinero,
                                             carabineros.sinamigos,
                                             pdi.sinamigos,
                                             SbjNum,b1,b2,c1_1                       
                                             ,c1_2,c1_3,c1_4                        
                                             ,c2_1,c2_2,c3                          
                                             ,c4_1,c4_2,c5_1                        
                                             ,c5_2,c6_1,c6_2                        
                                             ,c7_1,c7_2    )






listado <- list("resumen NA variable" = resumen.n, 
                "NA Desagregado" = resultadosNA,
                "resumen saltos" = resumen.s, 
                "saltos Desagregado" = resultadosS,
                "consistencia" = consistencia)

openxlsx::write.xlsx(listado, file = "malla.de.validacion.MG.xlsx", rowNames = TRUE)


###


