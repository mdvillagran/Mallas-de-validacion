
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
 
 