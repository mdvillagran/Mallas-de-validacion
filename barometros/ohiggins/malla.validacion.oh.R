
rm(list = ls())
setwd("C:/Users/villagran/Desktop/datavoz/Barometros/mallas.v/ll")

# LECTURA DE BB.DD DE DATOS
library(openxlsx)
library(dplyr)
library(tidyverse)
library(stringr)
library(haven)



################################################################################
########################## check de base de datos Baro   #######################
################################################################################


data<- read_sav("Barometro.sav")

data <- data %>% filter(region==10)
data<- data %>% filter(Status %in% c("Approved", "Requires Approval"))

parte1<-data[1:33]
parte2<-data[1000:ncol(data)]

data1<-cbind(parte1, parte2)
data1<-data1 %>% select(-(OH1_1:LR5_3)) # eliminamos preguntas de regionales que no son de los lagos


######################## UNION DE VARIABLES DE LA BASE #########################


# fusion1<-data[,c("M2_14","M2_P14_OTRA","M2_9","M2_9_OTRA","M2_11","M2_11_OTRA",
#               "M2_12","M2_12_OTRA","M3_6","M3_6_NUM","M3_16A","M3_16A_1",
#               "M3_16B","M3_16B_1","MAINSTAT","MAINSTAT_OTRO","NAT_RELIG",
#               "NAT_RELIG_OTRA","NAT_PRTY","NAT_PRTY_OTRA","NAT_ETHN1",
#               "NAT_ETHN_2","MARITAL","MARITAL_OTRA","F_BORN","F_BORN_OTRA",
#               "M_BORN","M_BORN_OTRA","INM_1","INM_1_OTRA_COMUNA","INM_1",
#               "INM_1_OTRO_PAIS")]

data[data=="-"]<-NA

data$M2_14[data$M2_14==4]<-NA
data$M2_9[data$M2_9==4]<-NA
data$M2_11[data$M2_11==4]<-NA
data$M2_12[data$M2_12==4]<-NA
data$M3_6[data$M3_6==1]<-NA
data$M3_16A[data$M3_16A==1]<-NA
data$M3_16B[data$M3_16B==1]<-NA
data$MAINSTAT[data$MAINSTAT==10]<-NA
data$NAT_RELIG[data$NAT_RELIG==8]<-NA
data$NAT_PRTY[data$NAT_PRTY==14]<-NA
data$NAT_ETHN1[data$NAT_ETHN1==11]<-NA
data$MARITAL[data$MARITAL==9]<-NA
data$F_BORN[data$F_BORN==11]<-NA
data$INM_1[data$M_BORN==2]<-NA
data$INM_1[data$M_BORN==3]<-NA


fusion<- read.xlsx("fusion.xlsx") # input donde tenemos una columna las variables
# originales y en otra, las duplicadas 


fusion$codigo<-paste0("data<-data %>% mutate (", fusion$original,
                    "= coalesce (", fusion$original,
                    ",", fusion$duplicada, "))")

for (i in fusion$codigo){
  print(i)
}

# FUSISN DE VARIABLES

### ALGUNAS VARIABLES SON DE DIFERENTES CLASES, CUANDO ESO OCURRE SE ESTANDARIZAN
### ANTES DE LA FUSION



data<-data %>% mutate (M2_9= coalesce (M2_9,M2_9_OTRA))
data<-data %>% mutate (M2_11= coalesce (M2_11,M2_11_OTRA))
data<-data %>% mutate (M2_12= coalesce (M2_12,M2_12_OTRA))

data$M3_6<-as.factor(data$M3_6)
data<-data %>% mutate (M3_6= coalesce (M3_6,M3_6_NUM))

data<-data %>% mutate (M3_16A= coalesce (M3_16A,M3_16A_1))
data<-data %>% mutate (M3_16B= coalesce (M3_16B,M3_16B_1))

data$MAINSTAT<-as.factor(data$MAINSTAT)
data<-data %>% mutate (MAINSTAT= coalesce (MAINSTAT,MAINSTAT_OTRO))

data$NAT_RELIG<-as.factor(data$NAT_RELIG)
data<-data %>% mutate (NAT_RELIG= coalesce (NAT_RELIG,NAT_RELIG_OTRA))



######################### PREGUNTAS OBLIGATORIAS ###############################

# 1 MODULO IDENTIDAD, PERTENENCIA TERRITORIAL Y MOVILIDAD

# pregunta 1: p1_a,p1_b,p1_c
# pregunta 2: p2
# pregunta 5: p5_1:p5_11

# pregunta 6: p6
# pregunta 7: p7
# pregunta 8: p8
# pregunta 9: p9
# pregunta 10: p10_1: p10_12
# pregunta 11: p11_1: p11_12
# pregunta 12: p12_1: p12_4
# pregunta 13: p13_1: p13_11
# pregunta 14: p14
# pregunta 14.1: p14_1
# pregunta 16: p16_1: p16_7
# pregunta 17: p17_1: p17_7
# pregunta 18: p18_a: p18_c
# pregunta 19: p19_a: p19_c
# pregunta 20: p20_a: p20_c
# pregunta 21: p21_a: p21_c
# pregunta 22: p22_a: p22_b
# pregunta 23: p23_1: p23_12
# pregunta 26: p26_1: p26_8
# pregunta 27: p27_1: p27_9
# pregunta 28: p28
# pregunta 29: p29
# pregunta 30: p30_a: p30_c
# pregunta 31: p31
# pregunta 32: p32
# pregunta 33: p33_a: p33_c
# pregunta 35: p35
# pregunta 37: p37
# pregunta r1: LL1
# pregunta r2: LL2_1 : LL2_4
# pregunta r3: LL3
# pregunta r4: LL4
# pregunta tel: telefono_contacto
# pregunta s1: S1_sexo
# pregunta s2: S2_edad
# pregunta s3: S3_nacionalidad
# pregunta s4: S4_nivel_estudio
# pregunta s5: S5_estudios_sostenedor
# pregunta s6: S6_actividad_principal
# pregunta s7: S7_actividad_jefeHogar
# pregunta s8: S8_ocupacion
# pregunta s9: S9_pueblo_originario
# pregunta s11: S11_personas_hogar


# FUNDIR VARIABLES DE INGRESOS

S12_tramo_1
S12_tramo_2
S12_tramo_3
S12_tramo_4
S12_tramo_5
S12_tramo_6
S12_tramo_7

# pregunta s gse: S_GSE
# pregunta s: S_A









########################### PREGUNTAS CON SALTO ################################

# salto p3(10) -> p3_1
# salto p2(2) -> p5_1
# salto p14(2,88,99) -> p16_1


############################# Salto tipo 2 #####################################

# salto p35() -> p36 NA -> p37 dato. esta última es respondida por todos



# 1 MODULO IDENTIDAD, PERTENENCIA TERRITORIAL Y MOVILIDAD

m1<-c("p1_a","p1_b","p1_c", "p2", "p3", "p3_1")

# 2 MODULO CONOCIMIENTO Y REPRESENTACIÓN DE LA REGIÓN 

m2<-c("p5_1","p5_2","p5_3","p5_4","p5_5","p5_6","p5_7","p5_8",
"p5_9","p5_10","p5_11","p6","p7" )

# 3 MODULO EVALUACIÓN DE LA REGIÓN (DEFINICIÓN DE PROBLEMAS Y PRIORIDADES REGIONALES) 

m3<-c("p8","p9","p10_1","p10_2","p10_3","p10_4","p10_5","p10_6","p10_7","p10_8",
      "p10_9","p10_10","p10_11","p10_12","p11_1","p11_2","p11_3",
      "p11_4","p11_5","p11_6","p11_7","p11_8","p11_9","p11_10","p11_11","p11_12")

# 4 MODULO DISPOSICIÓN A LA PARTICIPACIÓN EN EL ESPACIO PÚBLICO Y EN EL PROCESO DE DESCENTRALIZACIÓN

m4<-c("p12_1","p12_2","p12_3","p12_4","p13_1","p13_2","p13_3","p13_4","p13_5",
      "p13_6","p13_7","p13_8","p13_9","p13_10","p13_11","P14","p14_1")


# 5 MODULO PERCEPCIÓN DE CONFLICTO SOCIOAMBIENTAL

m5<-c("p16_1","p16_2","p16_3","p16_4","p16_5","p16_6","p16_7","p17_1","p17_2","p17_3",
      "p17_4","p17_5","p17_6","p17_7")



# 6 MODULO CONSUMO DE MEDIOS  

m6<-c("P18_a","p18_b","p18_c")

# 7 OBSTÁCULOS Y HERRAMIENTAS PARA EL DESARROLLO REGIONAL 

m7<-c("p20_a","p20_b","p21_a","p21_b","p22_a","p22_b","p23_1","p23_2",
      "p23_3","p23_4","p23_5","p23_6","p23_7","p23_8","p23_9","p23_10",
      "p23_11","p23_12")

# 8 DESCENTRALIZACIÓN  

m8<-c("p26_1","p26_2","p26_3","p26_4","p26_5","p26_6","p26_7","p26_8",
"p27_1","p27_2","p27_3","p27_4","p27_5","p27_6","p27_7","p27_8",
"p27_9","p28","p29","p30_a","p30_b","p31")

# 9 EFECTOS DE LA PANDEMIA EN LA REGIÓN

m9<-c("p32","p33_a","p33_b","p33_c")

# 10 PROCESO CONSTITUYENTE

m10<-c("p39","p40","p41")

# 11 IDENTIFICACIÓN POLÍTICA

m11<-c("p35","p36","p37")

# 12 PREGUNTAS REGIONALES

m11<-c("LL1","T_LL2_1","T_LL2_2","T_LL2_3","T_LL2_4","LL3","LL4")


# 13 CARACTERIZACIÓN DEL ENTREVISTADO

m11<-c("telefono_contacto","S1_sexo","S2_edad","S3_nacionalidad","S4_nivel_estudio",
       "S5_estudios_sostenedor","S6_actividad_principal","S7_actividad_jefeHogar",
       "S8_ocupacion","S9_pueblo_originario", "S11_personas_hogar")

telefono_contacto
S1_sexo
S2_edad
S3_nacionalidad
S4_nivel_estudio
S5_estudios_sostenedor
S6_actividad_principal
S7_actividad_jefeHogar
S8_ocupacion
S9_pueblo_originario
S11_personas_hogar


# DESCONTAMOS LAS PREGUNTAS CON SALTO DE LAS VARIABLES A REVISAR

preguntas.salto<-fusion$original
preguntas.salto<-append(preguntas.salto,fusion$duplicada)
preguntas.salto<-append(preguntas.salto,m2)
preguntas.salto<-append(preguntas.salto,m3)
preguntas.salto<-append(preguntas.salto,m4)
preguntas.salto<-append(preguntas.salto,m6)

names2<-setdiff(names(data), preguntas.salto)

preguntas.o<-data %>% select(all_of(names2))

################################################################################

################################################################################
################### Aplicacion de loop y creacion de planilla ##################
################################################################################

################################################################################

################################################################################
######################## check de preguntas obligatorias #######################
################################################################################


obligatorias<-names(preguntas.o)

resultadosNA<-data.frame(preguntas.o$ENTREVISTADOR)

seleccion<-c("EDAD","REGISTRO")

# Evaluacisn de las variables seleccionadas y casos NA mediante for

for (i in obligatorias){
  coordenada<-grep(paste0("^",i,"$"), names(preguntas.o))
  columna<-((is.na(preguntas.o[,coordenada]))*1)
  resultadosNA<-cbind(assign("i",columna),resultadosNA)
}


# Reordenamiento del data frame de salida y etiquetaje de sus columnas

resultadosNA<-rev(resultadosNA)

rotulos<-  append("id.", obligatorias)

colnames(resultadosNA)<-rotulos

# Recodificacisn sugerida de las observaciones

resultadosNA<-resultadosNA %>% 
  dplyr::mutate_at(c(2:ncol(resultadosNA)), recode, '1'='NA', '0'='Con Dato')

# Exportacisn de los resultados en excel

write.xlsx(resultadosNA, file = "Malla.Validacion.NA.xlsx", 
           sheetName="Sheet1", col.names=TRUE, row.names=FALSE, append=FALSE)  


################################################################################

################################################################################
######################## REVISISN DE PREGUNTAS CON SALTO #######################
################################################################################

################################################################################

# Creacisn de vectores SALIDAS, LLEGADAS y CRITERIO DE SALTO

i.s.ll<- read.xlsx("salto.llegada.xlsx") # imput donde tenemos una columna para el
# origen del salto, y otra para su llegada

salida<-c(i.s.ll$salto)

llegada<-c(i.s.ll$llegada)

# pendiente, el criterio de salto no ha sido actualizado a la bbdd original

criterio.salto<-c(i.s.ll$criterio.salto)
criterio.salto<- str_replace(criterio.salto, "\\.", ",")



################################################################################


# Extraemos las variables de mnteres del data.frame

names3<-append(salida,llegada)

setdiff(names3,names(data))

preguntas.s<-data %>% select(all_of(names3))


# Creacion de data frame con los id, donde guardaremos los resultados del bucle

resultadosS<-data.frame(data$ENTREVISTADOR)

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

  coordenada1<-grep(paste("^",salida[i],"$",sep=""), names(preguntas.s))
  coordenada2<-grep(paste("^",llegada[i],"$",sep=""), names(preguntas.s))
 
  variable1<-as.integer(unlist(ejemplo[,coordenada1]))
  
  s.check<-(variable1 %in%  rango)*1
  ll.check<-(!is.na(preguntas.s[,coordenada2]))*1 

  revision<-paste0(s.check,ll.check)
  resultadosS<-cbind(revision,resultadosS)
}


# Creacion de nombres de columnas, reordenamiento de dataframe y etiquetaje

nombres.u<-c()


for (i in 1:length(llegada)){
  x1<-paste0(salida[i],"-",llegada[i], " (",criterio.salto[i],")")
  nombres.u<-append(nombres.u,x1)
}


# Reordenar base de datos de revisisn y aqadir etiquetas
resultadosS<-rev(resultadosS)

etiquetas<-  append("id", nombres.u)

colnames(resultadosS)<-etiquetas


# Recodificacisn sugerida de las observaciones

resultadosS<-resultadosS %>% 
  dplyr::mutate_at(c(2:ncol(resultadosS)), recode, '00'='sin salto', 
                   '11'='s. correcto','01'='s. con otro valor', '10'='s. NA llegada')


# Exportacisn de los resultados en excel
# 
 write.xlsx(resultadosS, file = "Malla.Validacion.SALTO.xlsx",
           sheetName="Sheet1", col.names=TRUE, row.names=FALSE, append=FALSE)

####
 
 