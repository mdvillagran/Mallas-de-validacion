
library(haven)
library(dplyr)
library(sjmisc)



rm(list = ls())
setwd("C:/Users/Villagran/Desktop/datavoz/9.1.2023. Proyecto SJM Encuesta a niños migrantes/avance/bbdd 21.3")


bbdd<-read_spss("Servicio-Jesuita-a-Migrantes-2023-ene-23_2023_21_03_09_12.sav")

bbdd2<-bbdd %>% dplyr:: select(!ends_with("_D"))

# Fundir preguntas 

#F1
F1<-bbdd2 %>% dplyr:: select(ends_with("F1"))
F1<-F1 %>% dplyr::mutate(F1.merge = coalesce(!!! syms (names(F1))))

#F2
F2<-bbdd2 %>% dplyr:: select(ends_with("F2"))
F2<-F2 %>% dplyr::mutate(F2.merge = coalesce(!!! syms (names(F2))))

#F3
F3<-bbdd2 %>% dplyr:: select(ends_with("F3"))
F3<-F3 %>% dplyr::mutate(F3.merge = coalesce(!!! syms (names(F3))))


# F4
F4<-bbdd2 %>% dplyr:: select(ends_with("F4"))
F4<-F4 %>% dplyr::mutate(F4.merge = coalesce(!!! syms (names(F4))))

## Preguntas de la encuesta

bbdd2<- bbdd2 %>% dplyr::select("SbjNum","F5","A1":"D21_NNA")

bbdd2$F1<-F1$F1.merge
bbdd2$F2<-F2$F2.merge
bbdd2$F3<-F3$F3.merge
bbdd2$F4<-F4$F4.merge

bbdd2<-bbdd2 %>% dplyr::mutate(A6 = coalesce(!!! syms (c("A6_O1","A6_O2","A6_O3","A6_O4",
                                                       "A6_O5","A6_O6"))))

bbdd2<-bbdd2 %>% dplyr::mutate(A7 = coalesce(!!! syms (c("A7_O1","A7_O2","A7_O3","A7_O4",
                                                         "A7_O5","A7_O6", "A7_O7"))))

bbdd2<-bbdd2 %>% dplyr::mutate(A8 = coalesce(!!! syms (c("A8_O1","A8_O2","A8_O3","A8_O4",
                                                         "A8_O5","A8_O6", "A8_O7", "A8_O8"
                                                         ,"A8_O9","A8_O10","A8_O11","A8_O12"
                                                         ,"A8_O13","A8_O14","A8_O15","A8_O16"
                                                         ,"A8_O17","A8_O18","A8_O19"))))

bbdd2<-bbdd2 %>% dplyr::mutate(C2 = coalesce(!!! syms (c("C2_O1","C2_O2","C2_O3","C2_O4",
                                                         "C2_O5","C2_O6","C2_O7"))))

bbdd2<-bbdd2 %>% dplyr::mutate(C6 = coalesce(!!! syms (c("C6_O1","C6_O2","C6_O3","C6_O4",
                                                         "C6_O5","C6_O6"))))

bbdd2<-bbdd2 %>% dplyr::mutate(B2_NNA = coalesce(!!! syms (c("B2_NNA_O1","B2_NNA_O2","B2_NNA_O3","B2_NNA_O4",
                                                             "B2_NNA_O5","B2_NNA_O6","B2_NNA_O7","B2_NNA_O8",
                                                             "B2_NNA_O9","B2_NNA_O10"))))

bbdd2<-bbdd2 %>% dplyr::mutate(B3_NNA = coalesce(!!! syms (c("B3_NNA_O1","B3_NNA_O2","B3_NNA_O3","B3_NNA_O4",
                                                             "B3_NNA_O5","B3_NNA_O6","B3_NNA_O7","B3_NNA_O8",
                                                             "B3_NNA_O9","B3_NNA_O10"))))

bbdd2<-bbdd2 %>% dplyr::mutate(C4_NNA = coalesce(!!! syms (c("C4_NNA_O1","C4_NNA_O2","C4_NNA_O3"))))

variables.obligatorias<-c(# Preguntas de filtro
                          "F1", "F2", "F3","F4","F5",
                          # Adultos
                          "A1", "A2","A3", "A4", "A5","A6","A7", "A8","A9", 
                          "B1_MES","B1_ANIO","B2_1","B3_1","C1_MES","C1_ANIO", "C2",
                          "C3","C4","C5_1","C5_2","C5_3","C5_4", "C5_5" ,"C5_6", "C5_7",
                          "C5_8" ,"C5_9" ,"C6","C7","C8" ,"C9", "C10", "C11_MES",
                          "C11_ANIO","C12","C13_1","C13_2" , "C13_3", "C13_4", "C13_5", 
                          "C13_6" ,"C13_7", "C13_8","C13_9" , "C13_10","D1","D2",
                          "D3" , "D4" , "D5", "D6",
                          # NNA
                          "A1_NNA" , "A2_NNA" ,"A3_NNA","A4_NNA" ,"B1_NNA", "B2_NNA",
                          "B3_NNA","B4_NNA_2","B4_NNA_3" ,"B4_NNA_5","B4_NNA_6",
                          "B4_NNA_7","B4_NNA_8", "C1_NNA" ,"C1B_NNA", "C2_NNA_1" ,"C2_NNA_2",
                          "C2_NNA_3" ,"C2_NNA_4","C2_NNA_5" ,"C2_NNA_6", "C2_NNA_7",
                          "C2_NNA_8","C2_NNA_9","C2_NNA_10","C2_NNA_11","C2_NNA_12","C2_NNA_13",
                          "T_C3_NNA_1_1", "T_C3_NNA_1_2", "T_C3_NNA_1_3", "T_C3_NNA_1_4", "T_C3_NNA_1_5",
                          "T_C3_NNA_2_1" ,"T_C3_NNA_2_2" ,"T_C3_NNA_2_3", "T_C3_NNA_2_4" ,"T_C3_NNA_2_5",
                          "T_C3_NNA_3_1", "T_C3_NNA_3_2","T_C3_NNA_3_3","T_C3_NNA_3_4" ,"T_C3_NNA_3_5",
                          "T_C3_NNA_4_1" ,"T_C3_NNA_4_2", "T_C3_NNA_4_3", "T_C3_NNA_4_4",
                          "T_C3_NNA_4_5",
                          "D1_NNA","D2_NNA","D3_NNA","D4_NNA","D5_NNA","D6_NNA","D7_NNA",
                          "D8_NNA","D9_NNA","D10_NNA","D11_NNA","D12_NNA","D13_NNA","D14_NNA",
                          "D15_NNA","D16_NNA","D17_NNA","D18_NNA","D19_NNA","D20_NNA","D21_NNA" 
                        )


################################################################################
############################ Variables obligatorias ############################
################################################################################




#######

resultadosNA<-data.frame(bbdd2$SbjNum)


for (i in variables.obligatorias){
  coordenada<-grep(paste0("^",i,"$"), names(bbdd2))
  columna<-((!is.na(bbdd2[,coordenada]) & as.vector(bbdd2[[coordenada]]) %nin% "")*1)
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

#####


listado <- list("resumen NA variable" = resumen.n, 
                "NA Desagregado" = resultadosNA)

write.xlsx(listado, file = "malla.de.validacion.CENISMA.xlsx", rowNames = TRUE)
