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

# Convertir la columna 'Condición' en un factor
datos$Condición <- factor(datos$CONDICIÓN, levels = c(1, 2), labels = c("Control", "Experimental"))

# Verificar estructura de los datos
str(datos)

# Cargar librería dplyr
library(dplyr)


##########################################################################################################
# ESTADÍSTICA PARA CELULAS NUCELADAS

# Estadísticas descriptivas para la proporción de células nucleadas por condición
descriptivas_nucleadas <- datos %>%
  group_by(CONDICIÓN) %>%
  summarise(
    media = mean(`Proporción de células nucleadas (%)`, na.rm = TRUE),
    mediana = median(`Proporción de células nucleadas (%)`, na.rm = TRUE),
    sd = sd(`Proporción de células nucleadas (%)`, na.rm = TRUE),
    minimo = min(`Proporción de células nucleadas (%)`, na.rm = TRUE),
    maximo = max(`Proporción de células nucleadas (%)`, na.rm = TRUE)
  )

# Mostrar resultados
descriptivas_nucleadas

####################
# Prueba de normalidad para proporción de células nucleadas en Control
shapiro.test(datos$`Proporción de células nucleadas (%)`[datos$CONDICIÓN == "CONTROL"])

# Prueba de normalidad para proporción de células nucleadas en Experimental
shapiro.test(datos$`Proporción de células nucleadas (%)`[datos$CONDICIÓN == "EXPERIMENTAL"])
####################

# RESULTADOS:
# PARA CONTROL: W = 0.97416, p-value = 0.9012
# PARA EXPERIMENTAL: W = 0.91439, p-value = 0.4944

# Dado que los datos parecen ser normales
# Prueba t para comparar la proporción de células nucleadas entre control y experimental
t.test(`Proporción de células nucleadas (%)` ~ CONDICIÓN, data = datos)

# Cargar la librería ggplot2 para gráficas
library(ggplot2)

# Boxplot para la proporción de células nucleadas
ggplot(datos, aes(x = CONDICIÓN, y = `Proporción de células nucleadas (%)`, fill = CONDICIÓN)) +
  geom_boxplot() +
  labs(title = "Comparación respecto a las células nucleadas",
       x = "Condición",
       y = "Proporción de Células Nucleadas (%)") +
  theme_minimal()


##########################################################################################################


##########################################################################################################
# ESTADÍSTICA PARA CELULAS CORNIFICADAS

# Estadísticas descriptivas para la proporción de células cornificadas por condición
descriptivas_cornificadas <- datos %>%
  group_by(CONDICIÓN) %>%
  summarise(
    media = mean(`Proporción de células cornificadas (%)`, na.rm = TRUE),
    mediana = median(`Proporción de células cornificadas (%)`, na.rm = TRUE),
    sd = sd(`Proporción de células cornificadas (%)`, na.rm = TRUE),
    minimo = min(`Proporción de células cornificadas (%)`, na.rm = TRUE),
    maximo = max(`Proporción de células cornificadas (%)`, na.rm = TRUE)
  )

# Mostrar resultados
print(descriptivas_cornificadas)

# Prueba de normalidad para proporción de células cornificadas en Control
shapiro.test(datos$`Proporción de células cornificadas (%)`[datos$CONDICIÓN == "CONTROL"])

# Prueba de normalidad para proporción de células cornificadas en Experimental
shapiro.test(datos$`Proporción de células cornificadas (%)`[datos$CONDICIÓN == "EXPERIMENTAL"])

# RESULTADOS:
# PARA CONTROL: W = 0.86226, p-value = 0.2365
# PARA EXPERIMENTAL: W = 0.85959, p-value = 0.2268

# Dado que los datos parecen ser normales
t.test(`Proporción de células cornificadas (%)` ~ CONDICIÓN, data = datos)

# Boxplot para la proporción de células cornificadas
ggplot(datos, aes(x = CONDICIÓN, y = `Proporción de células cornificadas (%)`, fill = CONDICIÓN)) +
  geom_boxplot() +
  labs(title = "Comparación de Proporción de Células Cornificadas",
       x = "Condición",
       y = "Proporción de Células Cornificadas (%)") +
  theme_minimal()
##########################################################################################################

##########################################################################################################
# ESTADÍSTICA PARA LEUCOCITOS

# Estadísticas descriptivas para la proporción de leucocitos por condición
descriptivas_leucocitos <- datos %>%
  group_by(CONDICIÓN) %>%
  summarise(
    media = mean(`Proporción de leucocitos (%)`, na.rm = TRUE),
    mediana = median(`Proporción de leucocitos (%)`, na.rm = TRUE),
    sd = sd(`Proporción de leucocitos (%)`, na.rm = TRUE),
    minimo = min(`Proporción de leucocitos (%)`, na.rm = TRUE),
    maximo = max(`Proporción de leucocitos (%)`, na.rm = TRUE)
  )

# Mostrar resultados
print(descriptivas_leucocitos)

# Prueba de normalidad para proporción de leucocitos en Control
shapiro.test(datos$`Proporción de leucocitos (%)`[datos$CONDICIÓN == "CONTROL"])

# Prueba de normalidad para proporción de leucocitos en Experimental
shapiro.test(datos$`Proporción de leucocitos (%)`[datos$CONDICIÓN == "EXPERIMENTAL"])
####################

# RESULTADOS:
# PARA CONTROL: W = 0.80613, p-value = 0.09083
# PARA EXPERIMENTAL: W = 0.64497, p-value = 0.00232

# Dado que los datos parecen NO ser normales
wilcox.test(`Proporción de leucocitos (%)` ~ CONDICIÓN, data = datos)

# Boxplot para la proporción de leucocitos
ggplot(datos, aes(x = CONDICIÓN, y = `Proporción de leucocitos (%)`, fill = CONDICIÓN)) +
  geom_boxplot() +
  labs(title = "Comparación de Proporción de Leucocitos",
       x = "Condición",
       y = "Proporción de Leucocitos (%)") +
  theme_minimal()
##########################################################################################################

##########################################################################################################
#GRÁFRICA PARA TODAS
install.packages("patchwork")  # Solo si no lo tienes instalado
library(patchwork)

# Ejemplo de cómo podrías haber creado tus gráficas
grafica_nucleadas <- ggplot(datos, aes(x = CONDICIÓN, y = `Celulas nucleadas`)) + 
  geom_boxplot() + 
  labs(title = "Células Nucleadas por Condición")

grafica_cornificadas <- ggplot(datos, aes(x = CONDICIÓN, y = `Células cornificadas`)) + 
  geom_boxplot() + 
  labs(title = "Células Cornificadas por Condición")

grafica_leucocitos <- ggplot(datos, aes(x = CONDICIÓN, y = Leucocitos)) + 
  geom_boxplot() + 
  labs(title = "Leucocitos por Condición")
# Combinar las tres gráficas
combinacion_graficas <- grafica_nucleadas + grafica_cornificadas + grafica_leucocitos + 
  plot_layout(ncol = 1)  # Cambia ncol a 2 o más para un diseño diferente

# Mostrar la combinación de gráficas
print(combinacion_graficas)
##########################################################################################################

# Gráfico de violín para Células Nucleadas
grafico_violin_nucleadas <- ggplot(datos, aes(x = CONDICIÓN, y = `Proporción de células nucleadas (%)`, fill = CONDICIÓN)) +
  geom_violin(trim = FALSE) +
  labs(title = "Distribución de Proporción de Células Nucleadas por Condición", 
       x = "Condición", y = "Proporción de Células Nucleadas (%)") +
  theme_minimal()

# Mostrar gráfico
print(grafico_violin_nucleadas)

# Gráfico de violín para Células Cornificadas
grafico_violin_cornificadas <- ggplot(datos, aes(x = CONDICIÓN, y = `Proporción de células cornificadas (%)`, fill = CONDICIÓN)) +
  geom_violin(trim = FALSE) +
  labs(title = "Distribución de Proporción de Células Cornificadas por Condición", 
       x = "Condición", y = "Proporción de Células Cornificadas (%)") +
  theme_minimal()

# Mostrar gráfico
print(grafico_violin_cornificadas)

# Gráfico de violín para Leucocitos
grafico_violin_leucocitos <- ggplot(datos, aes(x = CONDICIÓN, y = `Proporción de leucocitos (%)`, fill = CONDICIÓN)) +
  geom_violin(trim = FALSE) +
  labs(title = "Distribución de Proporción de Leucocitos por Condición", 
       x = "Condición", y = "Proporción de Leucocitos (%)") +
  theme_minimal()

# Mostrar gráfico
print(grafico_violin_leucocitos)

# Gráfico de barras apiladas para todas las proporciones de células
library(tidyr)  # Necesario para gather

grafico_barras_apiladas <- datos %>%
  gather(key = "TipoCelula", value = "Proporcion", 
         `Proporción de células nucleadas (%)`, 
         `Proporción de células cornificadas (%)`, 
         `Proporción de leucocitos (%)`) %>%
  ggplot(aes(x = CONDICIÓN, y = Proporcion, fill = TipoCelula)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Composición de Tipos de Células por Condición",
       x = "Condición", y = "Proporción (%)") +
  theme_minimal()

# Mostrar gráfico
print(grafico_barras_apiladas)
