# ESTADÍSTICA PARA PRÁCTICA IV TALLER

# Instala paquetes si es necesario
install.packages("tidyverse")  # Incluye ggplot2, dplyr, etc.
install.packages("readr")

# Cargar librerías
library(tidyverse)

# Leer el archivo CSV
datos <- read_csv("C:/Users/HOME/Pictures/CICLO ESTRAL/Medidas extra/DATOS_CITOLOGÍA.csv", locale = locale(encoding = "ISO-8859-1"))

# Verificar los datos
head(datos)

# Convertir la columna 'CONDICIÓN' y 'FASE IDENTIFI.' en factores
datos$CONDICIÓN <- factor(datos$CONDICIÓN, levels = c("CONTROL", "EXPERIMENTAL"))
datos$`FASE IDENTIFI.` <- factor(datos$`FASE IDENTIFI.`)

# Verificar estructura de los datos
str(datos)

# Cargar librería dplyr
library(dplyr)

##########################################################################################################
# ESTADÍSTICA PARA CELULAS NUCELADAS

# Instalar y cargar el paquete car si no lo tienes
install.packages("car")
library(car)

# Prueba de Levene para homogeneidad de varianzas
leveneTest(`Proporción de células nucleadas (%)` ~ `FASE IDENTIFI.`, data = datos)

# ANOVA para la proporción de células nucleadas entre fases
# Guardar el modelo ANOVA en una variable
anova_modelo <- aov(`Proporción de células nucleadas (%)` ~ `FASE IDENTIFI.`, data = datos)

# Verificar el modelo
summary(anova_modelo)

# Ejecutar TukeyHSD
tukey_nucleadas <- TukeyHSD(anova_modelo)
print(tukey_nucleadas)
