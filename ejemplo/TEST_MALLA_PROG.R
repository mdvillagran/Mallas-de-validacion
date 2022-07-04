

setwd("C:/Users/Villagran/Desktop/scribs/datavoz/mallas de validacion/ejemplo")

# CARGAR DATA -------------------------------------------------------------

library(haven)
library(openxlsx)
library(dplyr)
library(tidyverse)
library(stringr)
#data_enut          <- read_sav("BBDD/BASE_USUARIO_corregida.sav")
data_enut_alterada <- read_sav("BASE_USUARIO_alterada.sav")

data<-data_enut_alterada[,c("id_persona","f11_1_1","f12_1_1" )]

# EJEMPLO JOAO ------------------------------------------------------------

# ERROR 1
data_errorpro   <- data %>% mutate(ERRORPRO_1  = case_when(f11_1_1 == 1 & is.na(f12_1_1) ~ 1, TRUE ~ 0))
data_ERRORPRO_1 <- data_errorpro %>% filter(ERRORPRO_1 == 1)
data_ERRORPRO_1 <- data_ERRORPRO_1 %>% select(id_vivienda,id_hogar,id_persona,ERRORPRO_1,f11_1_1,f12_1_1)
View(data_ERRORPRO_1)

# ERROR 2
data_errorpro   <- data_errorpro %>% mutate(ERRORPRO_2  = case_when(f11_1_1 == 2 & !is.na(f12_1_1) ~ 1, TRUE ~ 0))
data_ERRORPRO_2 <- data_errorpro %>% filter(ERRORPRO_2 == 1)
data_ERRORPRO_2 <- data_ERRORPRO_2 %>% select(id_vivienda,id_hogar,id_persona,ERRORPRO_2,f11_1_1,f12_1_1)
View(data_ERRORPRO_2)

# TOTAL ERRORES
data_errorpro    <-  data_errorpro %>% select(starts_with("ERRORPRO_"))
reporte_errorpro <-  tibble(variable = data_errorpro %>% colnames)
reporte_errorpro <-  reporte_errorpro %>% mutate(errores = data_errorpro %>% colSums(na.rm = TRUE))
View(reporte_errorpro)




# EJEMPLO MARIO -----------------------------------------------------------
# creacion de Id
data_enut_alterada <- data_enut_alterada %>% 
  mutate(Id = paste0(id_vivienda, id_hogar, n_linea_h, id_persona, n_linea_p))


# cargamos nuestro excel con las variables que contengan saltos/filtros
library(readxl)
salto_llegada <- read_xlsx("BBDD/ERRORPROG_EJ.xlsx")


# Creacion de dos vectores SALIDAS Y LLEGADAS
salida<-c("f11_1_1")
llegada<-c("f12_1_1")
criterio.salto<-("1")


criterio.salto<- str_replace(criterio.salto, "\\.", ",")


################################################################################


# Creacisn de data frame con los id, donde guardaremos los resultados del bucle

resultadosS<-data.frame(data_enut_alterada$id_persona)

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
  
  coordenada1<-grep(paste("^",salida[i],"$",sep=""), names(data_enut_alterada))
  coordenada2<-grep(paste("^",llegada[i],"$",sep=""), names(data_enut_alterada))
  
  variable1<-as.integer(unlist(data_enut_alterada[,coordenada1]))
  
  s.check<-(variable1 %in%  rango)*1
  ll.check<-(!is.na(data_enut_alterada[,coordenada2]))*1 
  
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


resultadosS<-resultadosS %>% 
  dplyr::mutate_at(c(2:ncol(resultadosS)), recode, '00'='sin salto', 
                   '11'='s. correcto','01'='s. con otro valor', '10'='s. NA llegada')


sjmisc::frq(resultadosS$`f11_1_1-f12_1_1 (1)`)

# Recodificacion sugerida de las observaciones



# ej<-ej %>% 
#   dplyr::mutate_at(c(2:ncol(ej)), recode, 
#             '00'='Identicas', '11'='Identicas',
#             '01'='NA Llegada', '10'='NA Salida')

# openxlsx::write.xlsx(ej, "pruebamalla.xlsx")

# COMPROBAR CANTIDAD
sjmisc::frq(ej$`f11_1_1-f12_1_1`)



