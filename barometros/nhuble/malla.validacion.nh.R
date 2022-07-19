
rm(list = ls())
setwd("C:/Users/villagran/Desktop/datavoz/Barometros/mallas.v/ll")

# LECTURA DE BB.DD DE DATOS
library(openxlsx)
library(dplyr)
library(tidyverse)
library(stringr)
library(haven)
library(labelled)



################################################################################
########################## check de base de datos Baro   #######################
################################################################################


data<- read_sav("Barometro.sav")

data <- data %>% filter(region==16)
data<- data %>% filter(Status %in% c("Approved", "Requires Approval"))

parte1<-data[1:33]
parte2<-data[1000:ncol(data)]

data1<-cbind(parte1, parte2)
data1<-data1 %>% select(-(LL1:OH5)) # eliminamos preguntas de regionales que no son de Nhuble
data1<-data1 %>% select(-(LR1:LR5_3))

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


# FUNDIR VARIAS VARIABLES (DE INGRESOS) AL MISMO TIEMPO

a<-grep("S12_tramo_1", names(data1))
b<-grep("S12_tramo_7", names(data1))

variables<-names(data1[a:b])

data1<- data1 %>% mutate(S12_tramo_1 = coalesce(!!! syms(variables)))



####################### Detalle de preguntas por modulo ########################

# 1 MODULO IDENTIDAD, PERTENENCIA TERRITORIAL Y MOVILIDAD

# m1<-c("p1_a","p1_b","p1_c", "p2", "p3", "p3_1")
# 
# # 2 MODULO CONOCIMIENTO Y REPRESENTACIÓN DE LA REGIÓN 
# 
# m2<-c("p5_1","p5_2","p5_3","p5_4","p5_5","p5_6","p5_7","p5_8",
# "p5_9","p5_10","p5_11","p6","p7" )
# 
# # 3 MODULO EVALUACIÓN DE LA REGIÓN (DEFINICIÓN DE PROBLEMAS Y PRIORIDADES REGIONALES) 
# 
# m3<-c("p8","p9","p10_1","p10_2","p10_3","p10_4","p10_5","p10_6","p10_7","p10_8",
#       "p10_9","p10_10","p10_11","p10_12","p11_1","p11_2","p11_3",
#       "p11_4","p11_5","p11_6","p11_7","p11_8","p11_9","p11_10","p11_11","p11_12")
# 
# # 4 MODULO DISPOSICIÓN A LA PARTICIPACIÓN EN EL ESPACIO PÚBLICO Y EN EL PROCESO DE DESCENTRALIZACIÓN
# 
# m4<-c("p12_1","p12_2","p12_3","p12_4","p13_1","p13_2","p13_3","p13_4","p13_5",
#       "p13_6","p13_7","p13_8","p13_9","p13_10","p13_11","P14","p14_1")
# 
# 
# # 5 MODULO PERCEPCIÓN DE CONFLICTO SOCIOAMBIENTAL
# 
# m5<-c("p16_1","p16_2","p16_3","p16_4","p16_5","p16_6","p16_7","p17_1","p17_2","p17_3",
#       "p17_4","p17_5","p17_6","p17_7")
# 
# # 6 MODULO CONSUMO DE MEDIOS  
# 
# m6<-c("P18_a","p18_b","p18_c")
# 
# # 7 OBSTÁCULOS Y HERRAMIENTAS PARA EL DESARROLLO REGIONAL 
# 
# m7<-c("p19_a","p19_b","p19_c","p20_a","p21_a","p21_b","p22_a","p22_b","p23_1","p23_2",
#       "p23_3","p23_4","p23_5","p23_6","p23_7","p23_8","p23_9","p23_10",
#       "p23_11","p23_12")
# 
# # 8 DESCENTRALIZACIÓN  
# 
# m8<-c("p26_1","p26_2","p26_3","p26_4","p26_5","p26_6","p26_7","p26_8",
# "p27_1","p27_2","p27_3","p27_4","p27_5","p27_6","p27_7","p27_8",
# "p27_9","p28","p29","p30_a","p30_b","p31")
# 
# # 9 EFECTOS DE LA PANDEMIA EN LA REGIÓN
# 
# m9<-c("p32","p33_a","p33_b","p33_c")
# 
# # 10 PROCESO CONSTITUYENTE
# 
# m10<-c("p35","p36","p37")
# 
# # 11 IDENTIFICACIÓN POLÍTICA
# 
# m11<-c("p39","p40","p41")
# 
# # 12 PREGUNTAS REGIONALES
# 
# m12<-c("NU1","NU2","NU3","NU4","NU5")
# 
# 
# # 13 CARACTERIZACIÓN DEL ENTREVISTADO
# 
# m13<-c("telefono_contacto","S1_sexo","S2_edad","S3_nacionalidad","S4_nivel_estudio",
#        "S5_estudios_sostenedor","S6_actividad_principal","S7_actividad_jefeHogar",
#        "S8_ocupacion","S9_pueblo_originario", "S11_personas_hogar","S12_tramo_1")
# 
# 


################################################################################

################################################################################
################### Aplicacion de loop y creacion de planillas #################
################################################################################

################################################################################

################################################################################
######################## check de preguntas obligatorias #######################
################################################################################

                # modulo 1
obligatorias<-c("p1_a","p1_b","p1_c", "p2",
                # modulo 2
        "p5_1","p5_2","p5_3","p5_4","p5_5","p5_6","p5_7","p5_8",
      "p5_9","p5_10","p5_11","p6","p7",
                # modulo 3 
"p8","p9","p10_1","p10_2","p10_3","p10_4","p10_5","p10_6","p10_7","p10_8",
      "p10_9","p10_10","p10_11","p10_12","p11_1","p11_2","p11_3",
      "p11_4","p11_5","p11_6","p11_7","p11_8","p11_9","p11_10","p11_11","p11_12",
                # modulo 4
"p12_1","p12_2","p12_3","p12_4","p13_1","p13_2","p13_3","p13_4","p13_5",
      "p13_6","p13_7","p13_8","p13_9","p13_10","p13_11",
                # modulo 5
"p16_1","p16_2","p16_3","p16_4","p16_5","p16_6","p16_7",
                # modulo 6
"P18_a",
                # modulo 7
"p19_a","p19_b","p19_c","p20_a","p21_a","p22_a","p23_1","p23_2",
      "p23_3","p23_4","p23_5","p23_6","p23_7","p23_8","p23_9","p23_10",
      "p23_11","p23_12",
                # modulo 8
"p26_1","p26_2","p26_3","p26_4","p26_5","p26_6","p26_7","p26_8",
      "p27_1","p27_2","p27_3","p27_4","p27_5","p27_6","p27_7","p27_8",
      "p27_9","p28","p29","p30_a","p31",
                # modulo 9
"p32","p33_a",
                # modulo 10
"p35","p37",
                # modulo 11
"p41", 
                # modulo 12
"NU1","NU2","NU3","NU4","NU5",
                # modulo 13
"telefono_contacto","S1_sexo","S2_edad","S3_nacionalidad","S4_nivel_estudio",
       "S5_estudios_sostenedor","S6_actividad_principal","S7_actividad_jefeHogar",
       "S8_ocupacion","S9_pueblo_originario", "S11_personas_hogar","S12_tramo_1")





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
# salto p14(2,88,99) -> p14_1
# salto s: S_A (1) -> S_B


salida<-c("p3","P14","S_A")

criterio.salto<-c("10","1","1")

llegada<-c("p3_1","p14_1", "S_B")





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



# salto p2(2) ->p3(NA) -> p4
# salto p3(1:7)->(p3_1)->p4
# salto p35(3) -> p36 (NA )-> p37 dato. esta última es respondida por todos




salida<-c("p2","p3","p35")

criterio.salto<-c("2","1:7","3")

llegada1<-c("p3","p3_1","p36")

llegada2<-c("p4","p4","p37")


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



 # Exportacion de los resultados a excel único
 
 
 listado <- list("resumen NA variable" = resumen.n, 
                 "NA Desagregado" = resultadosNA,
                 "resumen SALTO 1" = resumen.s,
                 "SALTO 1 Desagregado" = resultadosS,
                 "resumen SALTO 2" = resumen.s2,
                 "SALTO 2 Desagregado" = resultados2)
 
 
 write.xlsx(listado, file = "malla.de.validacion.R16.xlsx", row.names = TRUE)
 
 
 