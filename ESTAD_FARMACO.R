# ESTADÍSTICA PARA PRÁCTICA VI TALLER
# Caballero Rosas Santiago

# Cargar paquetes necesarios
library(ggplot2)
library(dplyr)
library(car) # Para ANOVA

# Crear los datos
data <- data.frame(
  ID = 1:10,
  Peso = c(33, 28, 36, 29, 26, 29, 27, 21, 28, 23),
  Tratamiento = c("TBE_500", "TBE_250", "Ket_100", "Ket_150", "Salina", 
                  "TBE_500", "TBE_250", "Ket_100", "Ket_150", "Ket_150"),
  Dosis_ul = c(200, 140, 200, 200, NA, 200, 130, 210, 110, 200),
  Via = c("Intraperitoneal", "Intraperitoneal", "Intraperitoneal", 
          "Intraperitoneal", "Intraperitoneal", "Subdermica", 
          "Subdermica", "Subdermica", "Subdermica", "Subdermica"),
  Latencia_Inicial = c(9.04, 1.16, 3.17, 1.47, NA, 6, 7, 3.04, 3.30, 3.58),
  Latencia_Profunda = c(15, 11.50, NA, 9.47, NA, 14, 12.4, NA, 10.4, 8.47),
  Duracion_Anestesia = c(6, 10.34, NA, 8, NA, 8, 5.4, NA, 7.1, 4.49)
)

# ANOVA para latencia inicial
anova_latencia <- aov(Latencia_Inicial ~ Tratamiento + Via + Peso, data = data)
summary(anova_latencia)

# ANOVA para duración de la anestesia
anova_duracion <- aov(Duracion_Anestesia ~ Tratamiento + Via + Peso, data = data)
summary(anova_duracion)

# Relación Peso vs Latencia Inicial
linear_model <- lm(Latencia_Inicial ~ Peso, data = data)
summary(linear_model)

# Ecuación de la recta
coef <- coef(linear_model)
cat("Ecuación de la recta: Latencia_Inicial =", round(coef[2], 2), "* Peso +", round(coef[1], 2), "\n")

# Gráfico de caja: Latencia Inicial por Tratamiento y Vía
ggplot(data, aes(x = Tratamiento, y = Latencia_Inicial, fill = Via)) +
  geom_boxplot() +
  labs(title = "Latencia Inicial por Tratamiento y Vía de Administración",
       x = "Tratamiento", y = "Latencia Inicial (min)") +
  theme_minimal()

# Gráfico del modelo lineal: Peso vs Latencia Inicial
ggplot(data, aes(x = Peso, y = Latencia_Inicial)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", formula = y ~ x) +
  labs(title = "Relación entre Peso y Latencia Inicial",
       x = "Peso (g)", y = "Latencia Inicial (min)") +
  theme_minimal()

# Relación Dosis vs Tiempo
ggplot(data, aes(x = Dosis_ul, y = Latencia_Inicial, color = Tratamiento)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relación entre Dosis Administrada y Latencia Inicial",
       x = "Dosis (μL)", y = "Latencia Inicial (min)") +
  theme_minimal()

# Relación Tiempo vs Vía de Administración
ggplot(data, aes(x = Via, y = Latencia_Inicial, fill = Via)) +
  geom_boxplot() +
  labs(title = "Latencia Inicial por Vía de Administración",
       x = "Vía de Administración", y = "Latencia Inicial (min)") +
  theme_minimal()

# Comparación: TBE vs Ketamina
ggplot(data %>% filter(Tratamiento %in% c("TBE_500", "TBE_250", "Ket_100", "Ket_150")), 
       aes(x = Tratamiento, y = Latencia_Inicial, fill = Tratamiento)) +
  geom_boxplot() +
  labs(title = "Comparación de TBE y Ketamina en Latencia Inicial",
       x = "Tratamiento", y = "Latencia Inicial (min)") +
  theme_minimal()

# Comparación: TBE vs Ketamina en Duración
ggplot(data %>% filter(Tratamiento %in% c("TBE_500", "TBE_250", "Ket_100", "Ket_150")), 
       aes(x = Tratamiento, y = Duracion_Anestesia, fill = Tratamiento)) +
  geom_boxplot() +
  labs(title = "Comparación de TBE y Ketamina en Duración de Anestesia",
       x = "Tratamiento", y = "Duración de Anestesia (min)") +
  theme_minimal()

# Estadística básica para los datos
estadisticas <- data %>%
  summarise(
    Peso_media = mean(Peso, na.rm = TRUE),
    Peso_mediana = median(Peso, na.rm = TRUE),
    Peso_sd = sd(Peso, na.rm = TRUE),
    Peso_min = min(Peso, na.rm = TRUE),
    Peso_max = max(Peso, na.rm = TRUE),
    
    Latencia_Inicial_media = mean(Latencia_Inicial, na.rm = TRUE),
    Latencia_Inicial_mediana = median(Latencia_Inicial, na.rm = TRUE),
    Latencia_Inicial_sd = sd(Latencia_Inicial, na.rm = TRUE),
    Latencia_Inicial_min = min(Latencia_Inicial, na.rm = TRUE),
    Latencia_Inicial_max = max(Latencia_Inicial, na.rm = TRUE),
    
    Duracion_Anestesia_media = mean(Duracion_Anestesia, na.rm = TRUE),
    Duracion_Anestesia_mediana = median(Duracion_Anestesia, na.rm = TRUE),
    Duracion_Anestesia_sd = sd(Duracion_Anestesia, na.rm = TRUE),
    Duracion_Anestesia_min = min(Duracion_Anestesia, na.rm = TRUE),
    Duracion_Anestesia_max = max(Duracion_Anestesia, na.rm = TRUE),
    
    Dosis_media = mean(Dosis_ul, na.rm = TRUE),
    Dosis_mediana = median(Dosis_ul, na.rm = TRUE),
    Dosis_sd = sd(Dosis_ul, na.rm = TRUE),
    Dosis_min = min(Dosis_ul, na.rm = TRUE),
    Dosis_max = max(Dosis_ul, na.rm = TRUE)
  )

print(estadisticas)

# Estadística por Tratamiento
estadisticas_tratamiento <- data %>%
  group_by(Tratamiento) %>%
  summarise(
    Peso_media = mean(Peso, na.rm = TRUE),
    Latencia_Inicial_media = mean(Latencia_Inicial, na.rm = TRUE),
    Latencia_Inicial_sd = sd(Latencia_Inicial, na.rm = TRUE),
    Duracion_Anestesia_media = mean(Duracion_Anestesia, na.rm = TRUE),
    Duracion_Anestesia_sd = sd(Duracion_Anestesia, na.rm = TRUE),
    Dosis_media = mean(Dosis_ul, na.rm = TRUE)
  )

print(estadisticas_tratamiento)

# Estadística por Vía de Administración
estadisticas_via <- data %>%
  group_by(Via) %>%
  summarise(
    Peso_media = mean(Peso, na.rm = TRUE),
    Latencia_Inicial_media = mean(Latencia_Inicial, na.rm = TRUE),
    Latencia_Inicial_sd = sd(Latencia_Inicial, na.rm = TRUE),
    Duracion_Anestesia_media = mean(Duracion_Anestesia, na.rm = TRUE),
    Duracion_Anestesia_sd = sd(Duracion_Anestesia, na.rm = TRUE),
    Dosis_media = mean(Dosis_ul, na.rm = TRUE)
  )

print(estadisticas_via)