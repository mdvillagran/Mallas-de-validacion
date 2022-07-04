#Malla de validación
homologadas <- read_sav ("220505 homologadas CEP-DESUC.sav")

#aca se ponen todas las variables 

data_perdidos_c1 <- homologadas %>% mutate(ERRORMISSING_M2_8	=	case_when(M2_8	=	is.na(M2_8) ~ 1, TRUE ~ 0))	
data_perdidos_c1 <- data_perdidos_c1 %>% mutate(ERRORMISSING_M2_9	=	case_when(M2_9	=	is.na(M2_9) ~ 1, TRUE ~ 0))	
data_perdidos_c1 <- data_perdidos_c1 %>% mutate(ERRORMISSING_M4_2	=	case_when(M4_2	=	is.na(M4_2) ~ 1, TRUE ~ 0))	
data_perdidos_c1 <- data_perdidos_c1 %>% mutate(ERRORMISSING_SD_6	=	case_when(SD_6	=	is.na(SD_6) ~ 1, TRUE ~ 0))	
data_perdidos_c1 <- data_perdidos_c1 %>% mutate(ERRORMISSING_SD_9	=	case_when(SD_9	=	is.na(SD_9) ~ 1, TRUE ~ 0))	

#Reporte casos perdidos
#Aca se hace el reporte de casos 
data_perdidos_c1 <-  data_perdidos_c1 %>% select(starts_with("ERRORMISSING_"))
reporte_errormissing_c1 <-  tibble(variable = data_perdidos_c1 %>% colnames)
reporte_errormissing_c1 <-  reporte_errormissing_c1 %>% mutate(errores = data_perdidos_c1 %>% colSums(na.rm = TRUE))
reporte_errormissing_c1

####Casos con errores
#aca filtra todos los casos con errores
reporte_errormissing_c1 %>% filter(errores > 0) #Hay casos perdidos/


####ERRORES PROGRAMACION C1####
#Acá se ponen los saltos/filtros
#M2_8 y M2_9 tenían como filtro M2_7 

data_errorpro_c1  <- homologadas %>% mutate(ERRORPRO_1  = case_when(M2_7== 1& is.na(M2_8)~ 1, TRUE ~ 0))
data_ERRORPROC1_1 <- data_errorpro_c1 %>% filter(ERRORPRO_1 == 1)
data_ERRORPROC1_1 <- data_ERRORPROC1_1 %>% select(FOLIOUC, REGISTRO, M2_7, M2_8, ERRORPRO_1)
data_ERRORPROC1_1

data_errorpro_c1  <- data_errorpro_c1 %>% mutate(ERRORPRO_1  = case_when(M2_7 %in% c(2,88,99) & is.na(M2_9) ~ 1, TRUE ~ 0))
data_ERRORPROC1_2 <- data_errorpro_c1 %>% filter(ERRORPRO_1 == 1)
data_ERRORPROC1_2 <- data_ERRORPROC1_2 %>% select(FOLIOUC, REGISTRO, M2_7, M2_9, ERRORPRO_1)
data_ERRORPROC1_2

####Casos con errores. Selecciona subjnum (id) de los casos con errores 
reporte_errorpro_c1 <-data_errorpro_c1 %>% filter(ERRORPRO_1 == 1)

view(reporte_errorpro_c1)




