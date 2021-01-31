## abrir el paquete tidyverse

install.packages("plyr")
install.packages("dplyr") 
install.packages("tidyverse") 
 
## ver base de datos  
library(tidyverse)
library(readxl)
base_datos <- read_excel("ambiente-desarrollo/R/rLadies/17eneroPlots/base_vivienda_persona.xlsx")

View(base_datos)
## Filtrar edad (fijarse limites que pone encuesta)

base_menores <- base_datos %>% #(control, shift, m) 
  filter(edad >5 & edad <19)

base_menores <- base_menores %>% 
  select(depto, edad, sexo, telefono_celular, etnia_pertenece, tipo_etnia,
         telefono_celular_meses, computadora_meses
         internet, internet_meses, internet_lugar, alfabetismo, matematica,
         internet_actividades, starts_with("curso"))

base_menores <-  base_menores %>% 
  filter(!is.na(curso_max_instruccion))
View(base_menores)

## explorar edad
base_menores %>% 
  count(edad)

base_menores %>% 
  ggplot(aes(edad)) +
  geom_histogram()

base_menores <- base_menores %>% 
  mutate(mayor_doce = ifelse(edad <= 12, "Entre 6 - 12", "Entre 13 - 18"))
## explorar sexo

base_menores %>% 
  count(edad, sexo)

## explorar sexo y educacion

base_menores %>% 
  count(sexo, curso_max_instruccion)

base_menores %>%
  ggplot(aes(x = curso_max_instruccion, fill = sexo))



## explorar internet meses
tabla_cant_tienen_internet <- base_menores %>% 
  count(internet_meses)

tabla_cant_tienen_internet %>% 
  ggplot(aes(internet_meses)) +
  geom_histogram()

## explorar tiene celular
  base_menores %>% 
  count(telefono_celular)

## explorar uso de pc
  base_menores %>% 
    count(computadora_meses)

### agrupando por curso
tabla_curso_sexo <- base_menores %>% 
  group_by(curso_max_instruccion) %>% 
  count(sexo) %>% 
  mutate(porcentaje = 100*n/sum(n))

tabla_curso_sexo %>% 
  ggplot(aes(x = porcentaje, y = curso_max_instruccion, fill = sexo)) +
  geom_col()

## agrupando por sexo

tabla_sexo_curso <- base_menores %>% 
  group_by(sexo) %>% 
  count(curso_max_instruccion) %>% 
  mutate(porcentaje = 100*n/sum(n))

tabla_sexo_curso %>% 
  ggplot(aes(x = porcentaje, y = sexo, fill = curso_max_instruccion)) +
  geom_col()

## agrupando sexo vs alfabetismo
tabla_sexo_alfabetismo <- base_menores %>% 
  group_by(sexo) %>% 
  count(alfabetismo) %>% 
  mutate(porcentaje = 100*n/sum(n))

tabla_sexo_alfabetismo %>% 
  ggplot(aes(x = porcentaje, y = sexo, fill = alfabetismo)) +
  geom_col()

tabla_sexo_alfabetismo %>% 
  ggplot(aes(x = porcentaje, y = alfabetismo, fill = sexo)) +
  geom_col()


## agrupando computadora_meses vs telefono
tabla_pc_telefono <- base_menores %>% 
  group_by(sexo) %>% 
  count(telefono_celular, computadora_meses) %>% 
  mutate(porcentaje = 100*n/sum(n))

view(tabla_pc_telefono)

ggplot(data=tabla_pc_telefono, aes(x=sexo, y=telefono_celular)) + 
  geom_bar(stat="identity", position="stack")

ggplot(tabla_pc_telefono,aes(x='',y=porcentaje, fill=telefono_celular))+
  geom_bar(stat = "identity",color="white")+
  coord_polar(theta="y")

tabla_pc_telefono %>% 
  ggplot(aes(x = porcentaje, y = sexo, fill = telefono_celular)) +
  geom_col()

tabla_pc_telefono %>% 
  ggplot(aes(x = porcentaje, y = telefono_celular, fill = sexo)) +
  geom_col()

ggplot(data=tabla_pc_telefono, aes(x=reorder(computadora_meses, telefono_celular), y=telefono_celular, fill=computadora_meses)) + 
  geom_bar(stat="identity", position="dodge")

plot(x = banco$education, main = "Gráfica 1",
     xlab = "Sexo", ylab = "Frecuencia", 
     col = c("royalblue", "seagreen", "purple", "grey"))

## agrupando sexo vs telefono
tabla_sexo_telefono <- base_menores %>% 
  group_by(sexo) %>% 
  count(telefono_celular) %>% 
  mutate(porcentaje = 100*n/sum(n))

tabla_sexo_telefono %>% 
  ggplot(aes(x = porcentaje, y = sexo, fill = telefono_celular)) +
  geom_col()

tabla_sexo_telefono %>% 
  ggplot(aes(x = porcentaje, y = telefono_celular, fill = sexo)) +
  geom_col()

ggplot(data=tabla_sexo_telefono, aes(x=sexo, y=telefono_celular)) + 
  geom_bar(stat="identity", position="stack")

## agrupando depto vs internet
tabla_depto_internet <- base_menores %>% 
  group_by(depto, sexo) %>% 
  count(internet) %>% 
  mutate(porcentaje = 100*n/sum(n))

view(tabla_depto_internet)
## agrupando area vs internet
tabla_area_internet <- base_datos %>% 
  group_by(area) %>% 
  count(sexo,internet) %>% 
  
  mutate(porcentaje = 100*n/sum(n))

view(tabla_area_internet)
## agrupando sexo vs internet
tabla_sexo_internet <- base_menores %>% 
  group_by(sexo) %>% 
  count(internet) %>% 
  mutate(porcentaje = 100*n/sum(n))

view(tabla_sexo_internet)

tabla_sexo_internet %>% 
  ggplot(aes(x = porcentaje, y = sexo, fill = internet)) +
  geom_col()

tabla_sexo_internet %>% 
  ggplot(aes(x = porcentaje, y = internet, fill = sexo)) +
  geom_col()

## agrupando sexo vs procedencia del internet
tabla_sexo_procedencia_internet <- base_menores %>% 
  group_by(internet) %>% 
  count(internet_lugar) 

view(tabla_sexo_procedencia_internet)
## agrupando sexo vs en que se usa el internet
tabla_sexo_actividades_internet <- base_menores %>% 
  group_by(sexo) %>% 
  count(internet_actividades) 

view(tabla_sexo_actividades_internet)
 
## explorando las tics

# Tiene el hogar acceso al servicio de internet en su vivienda?
base_menores %>% 
  count(internet) %>% 
  mutate(porcentaje = 100*n/sum(n))

base_menores %>% 
  count(sexo, mayor_doce, internet)

base_menores %>% 
  group_by(sexo, mayor_doce) %>% 
  count(internet)  %>% 
  mutate(porcentaje = 100*n/sum(n))

## 
base_menores %>% 
  group_by(sexo, mayor_doce) %>% 
  count(telefono_celular)  %>% 
  mutate(porcentaje = 100*n/sum(n))

base_menores %>% 
  group_by(sexo, mayor_doce) %>% 
  count(internet_meses)  %>% 
  mutate(porcentaje = 100*n/sum(n))

tabla_sexo_computadora_meses <- base_menores %>% 
  group_by(sexo, mayor_doce) %>% 
  count(computadora_meses)  %>% 
  mutate(porcentaje = 100*n/sum(n))


tabla_sexo_computadora_meses %>% 
  ggplot(aes(y= computadora_meses, x = porcentaje,
             fill = mayor_doce,
             group = sexo)) +
  geom_col()


base_menores %>% 
group_by(depto) %>% count(internet_meses) %>% 
  mutate(porcentaje = 100*n/sum(n)) %>%
  ungroup() %>% 
  filter(internet_meses == "Si") %>% 
  # mutate(depto = fct_reorder(as_factor(depto, porcentaje))) %>% 
  ggplot(aes(y = depto, x = porcentaje, fill = internet_meses)) + 
  geom_col()



 