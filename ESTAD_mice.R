# Cargar paquetes
library(ggplot2)
library(dplyr)

# Definir los datos
datos <- data.frame(
  RATON = 1:10,
  CEPA = c("FVB/N", "FVB/N", "FVB/N", "FVB/N", "FVB/N", "Balb/C", "Balb/C", "Balb/C", "Balb/C", "Balb/C"),
  PESO = c(22, 24, 23, 25, 23, 26, 24, 26, 22, 24),
  TALLA = c(9, 8, 8, 8.5, 7.7, 8, 8, 9, 9, 7.5),
  EDAD = c(9, 6, 5, 6.5, 5, 15, 14, 15, 11, 14),
  DISTANCIA_ANOGENITAL = c(1.5, 1.7, 1.9, 1.7, 1.5, 0.7, 1.0, 0.5, 1.2, 1.3),
  SEXO = c("MACHO", "MACHO", "MACHO", "MACHO", "MACHO", "HEMBRA", "HEMBRA", "HEMBRA", "HEMBRA", "HEMBRA")
)

# Pregunta 1: ¿Existen diferencias entre las características de los individuos independientemente del sexo?

# ANOVA por cepa
anova_peso <- aov(PESO ~ CEPA, data = datos)
anova_talla <- aov(TALLA ~ CEPA, data = datos)
anova_edad <- aov(EDAD ~ CEPA, data = datos)
anova_distancia <- aov(DISTANCIA_ANOGENITAL ~ CEPA, data = datos)

summary(anova_peso)
summary(anova_talla)
summary(anova_edad)
summary(anova_distancia)

# Gráficos para cada característica por cepa
ggplot(datos, aes(x = CEPA, y = PESO)) +
  geom_boxplot(aes(fill = CEPA)) +
  theme_minimal() +
  labs(title = "Peso por cepa", y = "Peso (g)") +
  theme(legend.position = "none")

ggplot(datos, aes(x = CEPA, y = TALLA)) +
  geom_boxplot(aes(fill = CEPA)) +
  theme_minimal() +
  labs(title = "Talla por cepa", y = "Talla (cm)") +
  theme(legend.position = "none")

ggplot(datos, aes(x = CEPA, y = EDAD)) +
  geom_boxplot(aes(fill = CEPA)) +
  theme_minimal() +
  labs(title = "Edad por cepa", y = "Edad (semanas)") +
  theme(legend.position = "none")

ggplot(datos, aes(x = CEPA, y = DISTANCIA_ANOGENITAL)) +
  geom_boxplot(aes(fill = CEPA)) +
  theme_minimal() +
  labs(title = "Distancia anogenital por cepa", y = "Distancia Anogenital (cm)") +
  theme(legend.position = "none")

# Pregunta 2: ¿Existen diferencias entre las características de los ratones dependientes del sexo?

# Pruebas t-student por sexo
t_peso <- t.test(PESO ~ SEXO, data = datos)
t_talla <- t.test(TALLA ~ SEXO, data = datos)
t_edad <- t.test(EDAD ~ SEXO, data = datos)
t_distancia <- t.test(DISTANCIA_ANOGENITAL ~ SEXO, data = datos)

t_peso
t_talla
t_edad
t_distancia

# Gráficos de comparación entre sexos
ggplot(datos, aes(x = SEXO, y = PESO, fill = SEXO)) +
  geom_boxplot() +
  labs(title = "Peso según el Sexo", y = "Peso (g)") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = SEXO, y = TALLA, fill = SEXO)) +
  geom_boxplot() +
  labs(title = "Talla según el Sexo", y = "Talla (cm)") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(datos, aes(x = SEXO, y = DISTANCIA_ANOGENITAL, fill = SEXO)) +
  geom_boxplot() +
  labs(title = "Distancia Anogenital según el Sexo", y = "Distancia Anogenital (cm)") +
  theme_minimal() +
  theme(legend.position = "none")


# Pregunta 4: ¿La edad de los ratones correlaciona con su talla y peso?

# Correlaciones
cor_edad_talla <- cor(datos$EDAD, datos$TALLA)
cor_edad_peso <- cor(datos$EDAD, datos$PESO)

cat("Correlación entre Edad y Talla:", cor_edad_talla, "\n")
cat("Correlación entre Edad y Peso:", cor_edad_peso, "\n")

# Gráfico de dispersión con líneas de regresión para edad y talla/peso
ggplot(datos, aes(x = EDAD, y = TALLA)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
  labs(title = "Relación entre Edad y Talla", x = "Edad (semanas)", y = "Talla (cm)") +
  theme_minimal()

ggplot(datos, aes(x = EDAD, y = PESO)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "Relación entre Edad y Peso", x = "Edad (semanas)", y = "Peso (g)") +
  theme_minimal()

