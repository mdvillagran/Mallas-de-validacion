
rm(list = ls())
setwd("C:/Users/villagran/Desktop/scribs/datavoz/mallas de validacion/CEP87")


# LECTURA DE BB.DD DE DATOS
library(openxlsx)
library(dplyr)
library(tidyverse)
library(stringr)
library(haven)


###-> CADA UNA CORRESPONDE A UNA ROTACION DETERMINADA (ROTACIONES), DESCRITA EN: _1
# P15_1
# P15_2
# P15_3
# P15_4

###-> SALTOS DESDE M2_4

#ALTERNATIVA 1 A: M2_6A, M2_6AA
#ALTERNATIVA 2 A: M2_6B, M2_6BB
#ALTERNATIVA 3 A: M2_6C, M2_6CC
#ALTERNATIVA 88 Y 99 A: M2_14

###-> COALESCE M2_14 Y M2_P14_OTRA

###-> COALESCE M2_9 Y M2_9_OTRA ; M2_10 Y M2_10_OTRA 
###-> COALESCE M2_11 Y M2_11_OTRA ; M2_12 Y M2_12_OTRA

###-> SALTOS DESDE M2_8

# ALTERNATIVA 1 A: M2_13
# ALTERNATIVA 2 A: M2_9, M2_10, M2_11, M2_12
# ALTERNATIVA 88 Y 99 A: M2_13 

###-> COALESCE M3_6 (SE DEBE RECODIFICAR LOS 1 EN NA) Y M3_6_NUM

###-> SALTOS DESDE M3_8

# ALTERNATIVA 1 A: P8B_MESES, M3_9
# ALTERNATIVA 2 A: 

###-> SALTO DESDE M3_FILTRO

# ALTERNATIVA 1 A: M3_16A,M3_16A_1,M3_16B,M3_16B_1,M3_17,M3_18_1,M3_18_2,
# M3_18_3,M3_18_4,M3_18_5,M3_18_6,M3_19

# ALTERNATIVA 2, 88 Y 99 A: M3_20_1

###-> COALESCE M3_16A Y M3_16A_1 ; M3_16B Y M3_16B_1 !! SE DEBEN CODIFICAR PRIMERO
# LOS 1 COMO "NA" EN LA VARIABLE BASE (EJEMPLO M3_16A), REALIZAR LA FUNCISN Y 
# LUEGO VOLVER A CODIFICAR LOS ESPACIOS VACMOS "" COMO NAs

###-> SALTO DESDE M4_1
# ALTERNATIVA 1 A: M4_2
# ALTERNATIVA 2, 88 Y 99 A: M4_2B

###-> SALTO DESDE M4_3
# ALTERNATIVA 1 A: M4_4A
# ALTERNATIVA 2, 88 Y 99 A: M4_4B


###-> SALTO DESDE EDAD

# ALTERNATIVA 1 A: EDAD_EXACTA
# ALTERNATIVA 99 A: NAC

###

###-> SALTO DESDE NAC
# ALTERNATIVA 1 A:

###-> SALTO DESDE NIVELEDUC
# ALTERNATIVAS 1 A 11 A: EDUCYRS
# ALTERNATIVA 99 A: WRK


###-> SALTO DESDE WRK
# ALTERNATIVA 1 A: WRKHRS
# ALTERNATIVA 2 A: EMPREL
# ALTERNATIVA 3 Y 8 A: MAINSTAT
# ALTERNATIVA 9 A: WRKHRS

###-> WRKHRS
# ALTERNATIVA 1 A: WRKHRS_TEX
# ALTERNATIVA 8 Y 9 A: EMPREL

###-> SALTO DESDE WRKSUP
# ALTERNATIVA 1 Y 9 A: NSUP
# ALTERNATIVA 2 Y 8 A: TYPORG1 

###-> SALTO DESDE NSUP
# ALTERNATIVA 1 A: NSUP_TEXT
# ALTERNATIVA 8 Y 9: TYPORG1

###-> SALTO DE ISCO08_1
# ALTERNATIVA 1: ISCO08_1_TEX
# ALTERNATIVA 8 Y 9 : ISCO08_2

###-> SALTO DE ISCO08_2
# ALTERNATIVA 1: ISCO08_2_TEXT
# ALTERNATIVA 8 Y 9 : MAINSTAT

###-> COALESCE MAINSTAT , MAINSTAT_OTRO: TRANSFORMAR PROMERO VALOR "10" EN
# MAINSTAT EN NA

###-> SALTO DESDE PARTLIV
# ALTERNATIVA 1 Y 2 A: SPWORK
# ALTERNATIVA 3 Y 8 A: PARTLIV
# ALTERNATIVA 9 : SPWORK

###-> SALTO DESDE SPWORK
# ALTERNATIVA 1 A: SPWRKHRS
# ALTERNATIVA 2 A: SPEMPREL
# ALTERNATIVA 3 Y 8 A: SPMAINST
# ALTERNATIVA 9 : SPWRKHRS

###-> SALTO DESDE SPWRKHRS
# ALTERNATIVA 1 A: SPWRKHRS_TEXT
# ALTERNATIVA 8 Y 9 A: SPEMPREL

###-> SALTO DESDE SPISCO08_1
# ALTERNATIVA 1 A: SPISCO08_1_TEXT
# ALTERNATIVA 8 Y 9 A: SPISCO08_2


###-> SALTO DESDE SPISCO08_2
# ALTERNATIVA 1 A: SPISCO08_2_TEX 
# ALTERNATIVA 8 Y 9 A: SPMAINST

###-> COALESCE NAT_RELIG , NAT_RELIG_OTRA = SE DEBE RECODIFICAR PRIMERO 8 COMO NA
###-> COALESCE NAT_PRTY , NAT_PRTY_OTRA = SE DEBE RECODIFICAR PRIMERO 14 COMO NA
###-> COALESCE NAT_ETHN1 , NAT_ETHN_2 = SE DEBE RECODIFICAR PRIMERO 11 COMO NA

###-> SALTO DESDE SPISCO08_2
# ALTERNATIVA 1 A: SPISCO08_2_TEX 
# ALTERNATIVA 8 Y 9 A: SPMAINST


###-> SALTO DESDE HOMPOP
# ALTERNATIVA 1 A: HHOMPOP_OTRA
# ALTERNATIVA 88 Y 99 A: LLENA UN 88 O 99 EN EL CAMPO DE LA VARIABLE


###-> SALTO DESDE HHADULT
# ALTERNATIVA 1 A: HHADULT_OTRA
# ALTERNATIVA 88 Y 99 A: LLENA UN 88 O 99 EN EL CAMPO DE LA VARIABLE


###-> SALTO DESDE HHCHILDR
# ALTERNATIVA 1 A: HHCHILDR_OTRA
# ALTERNATIVA 88 Y 99 A: LLENA UN 88 O 99 EN EL CAMPO DE LA VARIABLE

###-> SALTO DESDE HHTODD
# ALTERNATIVA 1 A: HHTODD_OTRA
# ALTERNATIVA 88 Y 99 A: LLENA UN 88 O 99 EN EL CAMPO DE LA VARIABLE

###-> COALESCE MARITAL , MARITAL_OTRA = SE DEBE RECODIFICAR PRIMERO 9 COMO NA
###-> COALESCE F_BORN , F_BORN_OTRA = SE DEBE RECODIFICAR PRIMERO 11 COMO NA
###-> COALESCE M_BORN , M_BORN_OTRA = SE DEBE RECODIFICAR PRIMERO 11 COMO NA
###-> COALESCE INM_1 , INM_1_OTRA_COMUNA, INM_1_OTRO_PAIS = SE DEBE RECODIFICAR 
# PRIMERO 2 Y 3 COMO NA

###-> SALTO DESDE INM_1_OTRO_PAIS
# CUALQUIER REGISTRO: INM_2

###-> SALTO DESDE INM_2
# ALTERNATIVA 1 A: INM_2_OTRA
# ALTERNATIVA 88 Y 99 A: SEG_3

###-> SALTO DESDE SEG3 
# ALTERNATIVA 6 A:  TELFIJO
# TODAS LAS DEMAS RESPUESTAS A: SEG_4


################################################################################
################################### EJEMPLO ####################################
################################################################################
################### Seguimiento de NAs en variables obligatorias ###############
################################################################################


ejemplo <- data.frame(id = 1:10,
                   x1 = 10:19,                      # Ejemplo
                   x2 = 20:29,
                   x3 = c(NA,NA,NA,1,NA,1,99,1,NA,NA),
                   x4 = c(NA,NA,NA,"BLANCO",NA,"BLANCO",99,"ROJO",NA,NA))

# Creacisn de vector con el nombre de las variables/columnas que deben estar completas

seleccion<-c("x1","x3")

# Generacisn de data frame vacmo con la variable identificador de casos de la encuesta
# Aqum guardaremos los resultados de la revisisn

resultadosNA<-data.frame(ejemplo$id)

# Evaluacisn de las variables seleccionadas y casos NA mediante for

for (i in seleccion){
  # identifica posicisn de la variable/columna en el data frame:
  coordenada<-grep(paste("^",i,"$", sep=""), names(ejemplo))
  # revisa si hay NA en ese columna para cada fila del data frame. * 1, devuelve 1 o 0
  columna<-((is.na(ejemplo[,coordenada]))*1)
  # va pegando los resultados en el data frame vacmo creado previamente
  resultadosNA<-cbind(assign("i",columna),resultadosNA)
  }


# Reordenamiento del data frame de salida y etiquetaje de sus columnas

resultadosNA<-rev(resultadosNA)

rotulos<-  append("id.", seleccion)

colnames(resultadosNA)<-rotulos

# Recodificacisn sugerida de las observaciones

resultadosNA<-resultadosNA %>% 
  dplyr::mutate_at(c(2:ncol(resultadosNA)), recode, '1'='NA', '0'='Con Dato')

# Exportacisn de los resultados en excel

# write.xlsx(resultadosNA, file = "Malla.Validacion.NA.xlsx", 
#            sheetName="Sheet1", col.names=TRUE, row.names=FALSE, append=FALSE)  


################################################################################
############################## Seguimiento de Saltos ###########################
################################################################################

# Creacisn de vectores de SALIDA, LLEGADA y CRITERIOS DE SALTO

salida<-c("x2","x2","x2")

llegada<-c("x3","x3","x3")

criterio.salto<-c("25","23.27","23:27")

# Creacisn de data frame con los id, donde guardaremos los resultados del bucle

resultadosS<-data.frame(ejemplo$id)

# Creamos una funcisn que servira para traducir en una secuencia de nzmeros imputs
# como "1 3" o "9". Para ambos casos devolverma "1 2 3"  y "9" respectivamente

secuencia<-function(objeto){
  n<-length(objeto)
  seq(objeto[[1]],objeto[[n]])
}


# Sobre todo al importar desde excel en ocasiones las comas son lemdas como puntos
# por precaucisn realizamos una transformacisn para esos casos

criterio.salto<- str_replace(criterio.salto, "\\.", ",")


#

for (i in 1:length(salida)){
  
  # 1ra parte, traducir en un rango de nzmeros imputs de caracteres como por ejemplo
  # "2,5" en los nzmeros "2 5". imputs como "1:3", en rangos "1 2 3". Y unidades
  # como "4", en el nzmero al que corresponden 
  
  ifelse ((str_detect(criterio.salto[i],",")), 
          rango<- criterio.salto[i] %>% str_split(",") %>% 
            unlist() %>% as.numeric(),
          
          rango<-criterio.salto[i] %>% str_split(":") %>%
            unlist() %>% as.numeric() %>% secuencia
  )
  
  # Identifica la posicisn en el data frame, de la variable/columna de salto
  coordenada1<-grep(paste("^",salida[i],"$",sep=""), names(ejemplo))
  # Identifica la posicisn en el data frame, de la variable/columna de llegada
  coordenada2<-grep(paste("^",llegada[i],"$",sep=""), names(ejemplo))
  
  # Chequea si la variable de salto tiene obervaciones que cumplen la condicion 
  # de filtro. La variable de BBDD debe ser de tipo numirica o integer para
  # ello, nos aseguramos de eso incluyendo las funciones as.integer y unlist
  variable1<-as.integer(unlist(ejemplo[,coordenada1]))
  
  s.check<-(variable1 %in%  rango)*1
  
  #Chequea si la variable de llegada tiene obervaciones
  ll.check<-(!is.na(ejemplo[,coordenada2]))*1 
  
  # Se pegan los resultados anteriores, que son agregados al a otro data frame
  # creado previamente, donde se registraran los resultados para cada individuo
  revision<-paste0(s.check,ll.check)
  resultadosS<-cbind(revision,resultadosS)
}


# Creacisn de nombres de columnas, reordenamiento de dataframe y etiquetaje

nombres.u<-c()

# Creamos el nombre de las columnas del data frame de revisisn, pegando los nombres
# de las variables y salto llegada que correspondan, mas el criterio de salto 

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



# Comparcisn de observaciones y resultados

contraste<-ejemplo %>% select(id,x2,x3)

contraste<-merge(contraste,resultadosS, all.x = T)



# Exportacisn de los resultados en excel
# 
# write.xlsx(resultadosS, file = "Malla.Validacion.SALTO.xlsx", 
#            sheetName="Sheet1", col.names=TRUE, row.names=FALSE, append=FALSE)  



################################################################################

################# VARIABLES DE INICIO Y TIRMINO DE LOS MODULOS #################

# 1 MODULO BASICO / M1_1A , M1_16

# 2 MODULO PROCESO CONSTITUYENTE /  M2_1 , M2_13

# 3 MODULO GENERO / M3_1_1 , M3_28_5

# 4 MODULO DE ELECCIONES PRESIDENCIALES / M4_1 , M4_6

# 5 MODULO COYUNTURA / M5_1 , M5_7

# 6 MODULO SOCIOECONOMICO / SEXO  


################################################################################
########################## check de base de datos CEP 87 #######################
################################################################################


data<- read.xlsx("Base CEP-87 del 03-05-2022.xlsx", sheet = "BBDD")


data<-data[,c("ENTREVISTADOR","REGISTRO","DURACION", "UMP","AREA","REGION", "TOTAL_HOGAR",
              "TOTAL_HOMBRES", "EDAD1_H", "TOTAL_MUJER", "EDAD1_M", "ROTACIONES",
              "M1_1A","M1_1B","M1_1C","M1_2","M1_3","M1_4","M1_5","M1_6","M1_7",
              "M1_8","M1_9","M1_11", "M1_12A","M1_12B","M1_12C","M1_12D","M1_12E",
              "M1_12F","M1_12G","M1_12H","M1_12I","M1_12J","M1_12K","M1_12L",
              "M1_12M","M1_12N","M1_12P","M1_12Q","M1_13_1","M1_13_2","M1_13_3",
              "M1_13_4","M1_13_5","M1_13_6","M1_13_7","M1_13_8","M1_13_9",
              "M1_13_10","M1_13_11","M1_13_12","M1_13_13","M1_13_14","M1_13_15",
              "M1_13_16","M1_13_17","M1_13_18","M1_13_19","M1_13_20","M1_14",
              "P15_1","P15_2","P15_3","P15_4","M1_16","M2_1","M2_2","M2_3","M2_4",
              "M2_6A","M2_6AA","M2_6B","M2_6BB","M2_6C","M2_6CC","M2_14",
              "M2_P14_OTRA","M2_5_1","M2_5_2","M2_5_3","M2_7","M2_8", "M2_9", 
              "M2_9_OTRA", "M2_10", "M2_10_OTRA", "M2_11", "M2_11_OTRA",
              "M2_12", "M2_12_OTRA", "M2_13","M3_1_1","M3_1_2","M3_1_3","M3_1_4",
              "M3_1_5","M3_1_6", "M3_2A","M3_2B","M3_3A","M3_3B","M3_4_1",
              "M3_4_2","M3_4_3","M3_4_4","M3_5_1","M3_5_2","M3_5_3","M3_5_4",
              "M3_6", "M3_6_NUM","M3_7_1","M3_7_2","M3_7_3","M3_7_4","M3_7_5",
              "M3_7_6","M3_8","P8B_MESES", "M3_9", "M3_10A","M3_10B","M3_11",
              "M3_12","M3_13","M3_14","M3_15A","M2_15A_1","M3_15B","M3_15B_1",
              "M3_FILTRO","M3_16A","M3_16A_1","M3_16B","M3_16B_1","M3_17",
              "M3_18_1","M3_18_2","M3_18_3","M3_18_4","M3_18_5","M3_18_6",
              "M3_19","M3_20_1","M3_20_2","M3_20_3","M3_20_4", "M3_20_2","M3_20_3",
              "M3_20_4","M3_21","M3_22A","M3_22B","M3_23","M3_24","M3_25","M3_26",
              "M3_27_1","M3_27_2","M3_27_3","M3_28_1","M3_28_2","M3_28_3","M3_28_4",
              "M3_28_5", "M4_1","M4_2","M4_2B","M4_3","M4_4A","M4_4B","M4_5","M4_6",
              "M4_5","M5_1","M5_6","M5_7","SEXO","EDAD","EDAD_EXACTA","NAC_DIA","NAC_MES",
              "NAC_ANO", "EST",	"NIVELEDUC", "EDUCYRS", "EDUCYRS_TEX", "WRK","WRKHRS",
              "WRKHRS_TEX","EMPREL","WRKSUP","NSUP","NSUP_TEX","TYPORG1","TYPORG2",
              "ISCO08_1","ISCO08_1_TEX","ISCO08_2","ISCO08_2_TEX","MAINSTAT",
              "MAINSTAT_OTRO","PARTLIV","SPWORK","SPWRKHRS","SPWRKHRS_TEXT",
              "SPEMPREL","SPWRKSUP","SPISCO08_1","SPISCO08_1_TEX","SPISCO08_2",
              "SPISCO08_2_TEX","SPMAINST","SPMAINST_OTRA","UNIO","NAT_RELIG",
              "NAT_RELIG_OTRA","ATTEND","TOPBOT","NAT_PRTY","NAT_PRTY_OTRA",
              "NAT_ETHN1","NAT_ETHN_2","ING_1","ING_2", "NHIJ", "HOMPOP",
              "HHOMPOP_OTRA","HHADULT","HHADULT_OTRA","HHCHILDR",
              "HHCHILDR_OTRA","HHTODD","HHTODD_OTRA","NAT_RINC","NAT_INC",
              "MARITAL","MARITAL_OTRA","F_BORN","F_BORN_OTRA","M_BORN","M_BORN_OTRA",
              "INM_1","INM_1_OTRA_COMUNA","INM_1_OTRO_PAIS","INM_2","INM_2_OTRA",
              "SEG_3","SEG_4", "ACEPTA", "CEL", "C3","M2_4","TELFIJO", "NAC",
              "SPISCO08_1_TEX","SPISCO08_2_TEX")]


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

data$NAT_PRTY<-as.factor(data$NAT_PRTY)
data<-data %>% mutate (NAT_PRTY= coalesce (NAT_PRTY,NAT_PRTY_OTRA))

data$NAT_ETHN1<-as.factor(data$NAT_ETHN1)
data<-data %>% mutate (NAT_ETHN1= coalesce (NAT_ETHN1,NAT_ETHN_2))

data$MARITAL<-as.factor(data$MARITAL)
data<-data %>% mutate (MARITAL= coalesce (MARITAL,MARITAL_OTRA))

data$F_BORN<-as.factor(data$F_BORN)
data<-data %>% mutate (F_BORN= coalesce (F_BORN,F_BORN_OTRA))

data$M_BORN<-as.factor(data$M_BORN)
data<-data %>% mutate (M_BORN= coalesce (M_BORN,M_BORN_OTRA))

data$INM_1<-as.factor(data$INM_1)
data<-data %>% mutate (INM_1= coalesce (INM_1,INM_1_OTRA_COMUNA))
data<-data %>% mutate (INM_1= coalesce (INM_1,INM_1_OTRO_PAIS))


############################# PREGUNTAS OBLIGATORIAS ###########################



# VARIABLES (CON SALTO) QUE DEBEN EXCLUIRSE

# 1 MODULO BASICO / M1_1A , M1_16 # deben escluirse:

# NINGUNA

# 2 MODULO PROCESO CONSTITUYENTE /  M2_1 , M2_13

m2<-c("M2_6A","M2_6AA","M2_6B","M2_6BB","M2_9","M2_9_OTRA","M2_10","M2_10_OTRA",
"M2_11","M2_11_OTRA","M2_12","M2_12_OTRA")

# 3 MODULO GENERO / M3_1_1 , M3_28_5

m3<-c("M3_8","M3_8B","P8B_MESES","M3_9","M2_15A_1","M3_15B_1","M3_16A","M3_16A_1",
"M3_16B","M3_16B_1","M3_17","M3_18_1","M3_18_2","M3_18_3","M3_18_4","M3_18_5",
"M3_18_6","M3_19")

# 4 MODULO DE ELECCIONES PRESIDENCIALES / M4_1 , M4_6

m4<-c("M4_2","M4_2B","M4_4A","M4_4B")

# 5 MODULO COYUNTURA / M5_1 , M5_7

# NINGUNA

# 6 MODULO SOCIOECONOMICO / SEXO

m6<-c("EDUCYRS","EDUCYRS_TEX","WRKHRS","WRKHRS_TEX","EMPREL","WRKSUP","NSUP",
"NSUP_TEX","TYPORG1","TYPORG2","ISCO08_1","ISCO08_1_TEX","ISCO08_2","ISCO08_2_TEX",
"MAINSTAT_OTRO","SPWORK","SPWRKHRS","SPWRKHRS_TEXT","SPWORK","SPWRKHRS","SPWRKHRS_TEXT",
"HORAINI_DEMO_7","SPEMPREL","SPWRKSUP","SPISCO08_1","SPISCO08_1_TEX","SPISCO08_2",
"SPISCO08_2_TEX","SPMAINST","SPMAINST_OTRA","NAT_RELIG_OTRA","NAT_PRTY_OTRA",
"HHOMPOP_OTRA","HHADULT","HHADULT_OTRA","HHCHILDR","HHCHILDR_OTRA","HHTODD",
"HHTODD_OTRA","CANT_RESP","SUMA_DEF","CONTROL_SUMA","MARITAL_OTRA","F_BORN_OTRA",
"M_BORN_OTRA","INM_1_OTRA_COMUNA","INM_1_OTRO_PAIS","INM_2","INM_2_OTRA","SEG_4",
"C4","INFO_CONT","B","DS_P50")


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
################### Aplicacisn de loop y creacisn de planilla ##################
################################################################################

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
######################## REVISISN DE PREGUNTAS CON FILTRO ######################
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


# Creacisn de data frame con los id, donde guardaremos los resultados del bucle

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


# Creacisn de nombres de columnas, reordenamiento de dataframe y etiquetaje

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
 
 #############################################################################
                                  "SENAPESCA"
 #############################################################################
 
 data<-read_sav("datos_DS2113_SERNAPESCA_PT_18_08_2021.sav")
 
 data<-data[,c("REGISTRO", "F1","F2")]
 
 # Creacisn de data frame con los id, donde guardaremos los resultados del bucle
 
 resultadosS<-data.frame(data$REGISTRO)
 
 
 salida<-c("F1","F2")
 
 criterio.salto<-c("1:2","1:2")
 
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
   
   coordenada1<-grep(paste("^",salida[i],"$",sep=""), names(data))
   
   variable1<-as.numeric(unlist(data[,coordenada1]))
   na.check<-(is.na(variable1))*1
   rango.check<-(variable1 %in%  rango)*1
    
   revision<-paste0(na.check,rango.check)
   resultadosS<-cbind(revision,resultadosS)
 }
 
  
 
 # Creacisn de nombres de columnas, reordenamiento de dataframe y etiquetaje
 
 nombres.u<-c()
 
 
 for (i in 1:length(salida)){
   x1<-paste0(salida[i], " (",criterio.salto[i],")")
   nombres.u<-append(nombres.u,x1)
 }
 
 
 # Reordenar base de datos de revisisn y aqadir etiquetas
 resultadosS<-rev(resultadosS)
 
 etiquetas<-  append("id", nombres.u)
 
 colnames(resultadosS)<-etiquetas
 
 
 # Recodificacisn sugerida de las observaciones
 
 resultadosS<-resultadosS %>% 
   dplyr::mutate_at(c(2:ncol(resultadosS)), recode, '00'='Fuera de rango',
                    '01'='cumple criterio', '10'='NA')
 
 
 # Exportacisn de los resultados en excel
 # 
 write.xlsx(resultadosS, file = "Malla.Validacion.SALTO.xlsx",
            sheetName="Sheet1", col.names=TRUE, row.names=FALSE, append=FALSE)
 
 ####
 
 
 
 

