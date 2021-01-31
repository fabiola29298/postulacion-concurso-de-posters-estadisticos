library(here)
library(tidyverse)
library(rio)
source(here("01_R", "funciones.R"))

## Importar base de datos
datos2019_crudo <- import(here("002_eh", "Persona", "EH2019_Persona.sav"))

## Implementa la funcion para usar las etiquetas de spss y convertir a factores
datos2019 <- map(datos2019_crudo, relabel_spss_variable) %>% as_tibble()

# Datos 2019 --------------------------------------------------------------

datos2019 <- datos2019 %>%
  unite(fecha_nac, c(s02a_04a, s02a_04b, s02a_04c), sep = "/") %>% 
  rename(sexo = s02a_02,
         edad = s02a_03,
         area = 
         )

# export(datos2019, "eh2019.xlsx")
datos2019 <- datos2019 %>% rename(educacion_telefono_celular = s05d_17,
                                  educacion_telefono_celular_meses = s05d_18,
                                  educacion_computadora_meses_a = s05d_19a,
                                  educacion_computadora_meses_b = s05d_19b,
                                  educacion_internet_meses_a = s05d_20a,
                                  educacion_internet_meses_b = s05d_20b,
                                  educacion_internet_lugar_a = s05d_21a,
                                  educacion_internet_lugar_b = s05d_21b,
                                  educacion_internet_lugar_e = s05d_21e,
                                  educacion_internet_actividades_a = s05d_22a,
                                  educacion_internet_actividades_b = s05d_22b,
                                  educacion_internet_actividades_c = s05d_22c,
                                  educacion_internet_actividades_d = s05d_22d,
                                  educacion_internet_actividades_e = s05d_22e,
                                  educacion_internet_actividades_f = s05d_22f,
                                  educacion_internet_actividades_g = s05d_22g,
                                  educacion_internet_actividades_h = s05d_22h,
                                  educacion_internet_actividades_i = s05d_22i,
                                  educacion_internet_actividades_j = s05d_22j,
                                  educacion_internet_actividades_k = s05d_22k,
                                  educacion_internet_actividades_l = s05d_22l,
                     ) 

export(datos2019, "eh2019.xlsx")
 
datos2019 %>% count(telefono_celular)
summary(datos2019$telefono_celular)

datos2019 <- datos2019 %>% 
  mutate(edad_cat = cut_number(edad, 10))


datos2019 %>% 
  ggplot(aes(as.factor(sexo), edad)) +
  geom_violin()

datos2019 %>% 
  filter(enfermedad_infecciosa_1 != "8. Ninguna?") %>% 
  barras_grupo(enfermedad_infecciosa_1, edad_cat)

datos2019 %>%
  filter(between(edad, 12, 19)) %>%  
  # filter(between(edad, 10, 19)) %>% 
  barras_grupo(s05d_21a, sexo)

#Visualizacion  

install.packages("ggplot2")
library(ggplot2)

 