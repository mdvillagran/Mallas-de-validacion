library(dplyr)
library(expss)
library(magrittr)
library(purrr)
library(tidyr)
library(haven)
library(stringr)
library(openxlsx)

####CARGAR BBDD

base <- read_sav("Estudios 2022/Pluralismo/Avances/datos_DS2216_25_04_2022_16_06.sav")

####TIEMPOS DE DURACIÓN
base <- base %>% mutate(dur_error = case_when(DURACION < (mean(DURACION)-sd(DURACION)) ~ "Duración anómala (corta)",
                                              DURACION > (mean(DURACION)+sd(DURACION)) ~ "Duración anómala (larga)",
                                              TRUE ~ "Duración en rango"))
tab_duracion <- base %>% select(ID_CONTACTO,ENTREVISTADOR,DURACION,dur_error) %>% as_factor()

tab_duracion %>% group_by(ENTREVISTADOR) %>% count(dur_error) %>%
  pivot_wider(names_from = dur_error, values_from = n) -> tab_duracion_enc

####VALORES PERDIDOS
#MÓDULO SCREENING
data_perdidos <- base %>% mutate(ERRORMISSING_A0 = case_when(A0=is.na(A0) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_A1 = case_when(A1=is.na(A1) ~ 1, TRUE ~ 0))

#MÓDULO SOCIODEMOGRAFICO
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P1 = case_when(P1=is.na(P1) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P2 = case_when(P2=is.na(P2) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P3 = case_when(P3=is.na(P3) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P4 = case_when(P4=is.na(P4) ~ 1, TRUE ~ 0))

#MODULO CONFIANZA Y CONSUMO MEDIOS
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P5_1 = case_when(P5_1=is.na(P5_1) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P5_2 = case_when(P5_2=is.na(P5_2) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P5_3 = case_when(P5_3=is.na(P5_3) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P5_4 = case_when(P5_4=is.na(P5_4) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P5_5 = case_when(P5_5=is.na(P5_5) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P5_6 = case_when(P5_6=is.na(P5_6) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P5_7 = case_when(P5_7=is.na(P5_7) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P5_8 = case_when(P5_8=is.na(P5_8) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P6_1 = case_when(P6_1=is.na(P6_1) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P6_2 = case_when(P6_2=is.na(P6_2) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P6_3 = case_when(P6_3=is.na(P6_3) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P6_4 = case_when(P6_4=is.na(P6_4) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P6_5 = case_when(P6_5=is.na(P6_5) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P6_6 = case_when(P6_6=is.na(P6_6) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P6_7 = case_when(P6_7=is.na(P6_7) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P6_8 = case_when(P6_8=is.na(P6_8) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P7_1 = case_when(P7_1=is.na(P7_1) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P7_2 = case_when(P7_2=is.na(P7_2) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P7_3 = case_when(P7_3=is.na(P7_3) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P7_4 = case_when(P7_4=is.na(P7_4) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P7_5 = case_when(P7_5=is.na(P7_5) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P7_6 = case_when(P7_6=is.na(P7_6) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P7_7 = case_when(P7_7=is.na(P7_7) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P8_1 = case_when(P8_1=is.na(P8_1) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P8_2 = case_when(P8_2=is.na(P8_2) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P8_3 = case_when(P8_3=is.na(P8_3) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P8_4 = case_when(P8_4=is.na(P8_4) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P8_5 = case_when(P8_5=is.na(P8_5) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P9_1 = case_when(P9_1=is.na(P9_1) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P9_2 = case_when(P9_2=is.na(P9_2) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P10_1 = case_when(P10_1=is.na(P10_1) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P10_2 = case_when(P10_2=is.na(P10_2) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P11_1 = case_when(P11_1=is.na(P11_1) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P11_2 = case_when(P11_2=is.na(P11_2) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P11_3 = case_when(P11_3=is.na(P11_3) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P11_4 = case_when(P11_4=is.na(P11_4) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P11_5 = case_when(P11_5=is.na(P11_5) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P11_6 = case_when(P11_6=is.na(P11_6) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P11_7 = case_when(P11_7=is.na(P11_7) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P12_1 = case_when(P12_1=is.na(P12_1) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P12_2 = case_when(P12_2=is.na(P12_2) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P12_3 = case_when(P12_3=is.na(P12_3) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P12_4 = case_when(P12_4=is.na(P12_4) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P13_1 = case_when(P13_1=is.na(P13_1) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P13_2 = case_when(P13_2=is.na(P13_2) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P14_1 = case_when(P14_1=is.na(P14_1) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P14_2 = case_when(P14_2=is.na(P14_2) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P14_3 = case_when(P14_3=is.na(P14_3) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P14_4 = case_when(P14_4=is.na(P14_4) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P15_1 = case_when(P15_1=is.na(P15_1) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P15_2 = case_when(P15_2=is.na(P15_2) ~ 1, TRUE ~ 0))

#MODULO VARIABLES DE CONTROL
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P16 = case_when(P16=is.na(P16) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P17_1 = case_when(P17_1=is.na(P17_1) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P17_2 = case_when(P17_2=is.na(P17_2) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P17_3 = case_when(P17_3=is.na(P17_3) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P17_4 = case_when(P17_4=is.na(P17_4) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P17_5 = case_when(P17_5=is.na(P17_5) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P18_1 = case_when(P18_1=is.na(P18_1) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P18_2 = case_when(P18_2=is.na(P18_2) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P19 = case_when(P19=is.na(P19) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P20 = case_when(P20=is.na(P20) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_P21 = case_when(P21=is.na(P21) ~ 1, TRUE ~ 0))
data_perdidos <- data_perdidos %>% mutate(ERRORMISSING_FUTURO = case_when(FUTURO=is.na(FUTURO) ~ 1, TRUE ~ 0))

data_perdidos_1 <-  data_perdidos %>% select(starts_with("ERRORMISSING_"))
reporte_errormissing <-  tibble(variable = data_perdidos_1 %>% colnames)
reporte_errormissing <-  reporte_errormissing %>% mutate(errores = data_perdidos_1 %>% colSums(na.rm = TRUE))
reporte_errormissing %>% filter(errores > 0) #Ver variables con casos perdidos

####ERRORES PROGRAMACION####
#Responde que si quiere continuar y no da nombre ni correo electronico
base <- base %>% mutate(Formato_correo = str_detect(CORREO, "@")) 
data_errorpro  <- base %>% mutate(ERRORPRO_1  = case_when(FUTURO == 1 & (NOMBRE == "-" | Formato_correo == FALSE) ~ 1, TRUE ~ 0))
data_ERRORPRO_1 <- data_errorpro %>% filter(ERRORPRO_1 == 1)
data_ERRORPRO_1 <- data_ERRORPRO_1 %>% select(ID_CONTACTO,ENTREVISTADOR,ERRORPRO_1,FUTURO,NOMBRE,CORREO)
data_ERRORPRO_1 <- data_ERRORPRO_1 %>% as_factor()

#Reporte por encuestador
tab_futuro <- data_errorpro %>% select(ID_CONTACTO,ENTREVISTADOR,FUTURO,ERRORPRO_1) %>% as_factor()

tab_futuro %>% group_by(ENTREVISTADOR) %>% count(FUTURO) %>%
  pivot_wider(names_from = FUTURO, values_from = n)

tab_futuro %>% group_by(ENTREVISTADOR) %>% count(ERRORPRO_1) %>%
  pivot_wider(names_from = ERRORPRO_1, values_from = n)


#Responde que no quiere continuar y da nombre ni correo electronico
data_errorpro  <- data_errorpro %>% mutate(ERRORPRO_2  = case_when(FUTURO == 2 & (NOMBRE != "-" | Formato_correo == TRUE) ~ 1, TRUE ~ 0))
data_ERRORPRO_2 <- data_errorpro %>% filter(ERRORPRO_2 == 1)
data_ERRORPRO_2 <- data_ERRORPRO_2 %>% select(ID_CONTACTO,ENTREVISTADOR,ERRORPRO_2,FUTURO,NOMBRE,CORREO)
data_ERRORPRO_2

data_errorpro_1  <-  data_errorpro %>% select(starts_with("ERRORPRO_"))
reporte_errorpro <-  tibble(variable = data_errorpro_1 %>% colnames)
reporte_errorpro <-  reporte_errorpro %>% mutate(errores = data_errorpro_1 %>% colSums(na.rm = TRUE))


####GUARDAR EXCEL
list_hojas <- list(tab_duracion,tab_duracion_enc,reporte_errormissing,data_ERRORPRO_1)

write.xlsx(list_hojas,"Estudios 2022/Pluralismo/Avances/Reporte_consistencia_PluralismoUAI_26042022.xlsx",
           sheetName = c("Duración","Duración por encuestador","R. Valores perdidos", "Casos sin correo"))
