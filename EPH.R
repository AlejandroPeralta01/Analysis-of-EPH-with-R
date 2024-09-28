install.packages('tidyverse')
install.packages('eph')
install.packages('dplyr')


library(ggplot2)
library(dplyr)
library(tidyverse)
library(eph)

individual <- get_microdata(year = 2024, trimester = 1, type = "individual")


# Creamos diccionarios para su posterior reemplazo

diccionario_regiones <- c("1" = "Gran Buenos Aires", 
                          "40" = "Noroeste", 
                          "41" = "Noreste", 
                          "42" = "Cuyo", 
                          "43" = "Pampeana",
                          "44" = "Patagonia " )

diccionario_nivel_ed <- c("2" = "Primario Completo",
                          "3" = "Primario Completo",
                          "4" = "Secundario Completo", 
                          "5" = "Secundario Completo",
                          "6" = "Superior Universitario Completo")

# filtramos por nivel educativo y eliminamos posibles nulos

fill_r <- individual %>%
  filter(NIVEL_ED %in% c(2, 3, 4, 5, 6), !is.na(REGION), !is.na(NIVEL_ED)) %>%
  
  mutate(REGION = recode(as.character(REGION), !!!diccionario_regiones)) %>%
  mutate(NIVEL_ED = recode(as.character(NIVEL_ED), !!!diccionario_nivel_ed))

fill_r <- fill_r %>%
  group_by(REGION, NIVEL_ED) %>%
  summarise(CANTIDAD = n()) %>%
  mutate(PROPORCION = CANTIDAD / sum(CANTIDAD))


ggplot(fill_r, aes(x = REGION, y = PROPORCION, fill = as.factor(NIVEL_ED))) + 
  geom_bar(stat = "identity", position = "fill") +  # Usar 'fill' para normalizar a 100%
  geom_text(aes(label = scales::percent(PROPORCION, accuracy = 1)),  # Mostrar porcentaje dentro de cada barra
            position = position_fill(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent_format()) +  # Mostrar eje y en porcentaje
  labs(title = "Proporción del Nivel Educativo por Región",
       x = "Región",
       y = "Porcentaje",
       fill = "Nivel Educativo") +
  theme_minimal()  





