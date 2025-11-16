# Josep Matias_Trabajo2.R
# Trabajo final Bioinformática - Curso 25/26
# Análisis de parámetros biomédicos por tratamiento

# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)

library(tidyverse)

tryCatch({
  datos <- read_csv("datos_biomed.csv")
  
  # Mostrar la estructura y las primeras filas para verificar la carga
  cat(" Datos cargados correctamente en el dataframe 'datos'.\n\n")
  print(glimpse(datos))
  
}, error = function(e) {
  cat(paste(" Error al cargar el archivo:", conditionMessage(e), "\n"))
  cat("Asegúrate de que el archivo 'datos_biomed.csv' esté en el directorio de trabajo.\n")
})

# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)

if (!exists("datos")) {
 
  library(readr)
 
  datos <- read_csv("datos_biomed.csv")
}

cat("--- head(datos) (Primeras filas) ---\n")
print(head(datos))

cat("\n--- summary(datos) (Resumen estadístico) ---\n")
print(summary(datos))

cat("\n--- dim(datos) (Dimensiones) ---\n")
print(dim(datos))

cat("\n--- str(datos) (Estructura) ---\n")
print(str(datos))

# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)

datos$Tratamiento <- as.factor(datos$Tratamiento)

datos_long <- datos %>%
  pivot_longer(
    cols = c(Glucosa, Presion, Colesterol),  pivotar
    names_to = "Variable",                 de nombres
    values_to = "Valor"                   de valores
  )

grafico_boxplots <- ggplot(datos_long, aes(x = Tratamiento, y = Valor, fill = Tratamiento)) +
 
  geom_boxplot(alpha = 0.7) +
 
  facet_wrap(~ Variable, scales = "free_y") + 
  
  labs(
    title = "Distribución de Variables Biomédicas por Grupo de Tratamiento",
    x = "Grupo de Tratamiento",
    y = "Valor Medido"
  ) +
  
  theme_minimal() +
 
  theme(plot.title = element_text(hjust = 0.5))

print(grafico_boxplots)

# 4. Realiza un violin plot (investiga qué es). (1 pt)

grafico_violin <- ggplot(datos_long, aes(x = Tratamiento, y = Valor, fill = Tratamiento)) +
  

  geom_violin(trim = FALSE) +  
 
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.6) +
  
  facet_wrap(~ Variable, scales = "free_y") + 
 
  labs(
    title = "Distribución de Densidad de Variables Biomédicas por Grupo de Tratamiento (Violin Plot)",
    caption = "El ancho del violín representa la densidad de pacientes en ese rango de valores.",
    x = "Grupo de Tratamiento",
    y = "Valor Medido"
  ) +
  
  theme_minimal() +
 
  theme(plot.title = element_text(hjust = 0.5))

print(grafico_violin)

# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)

grafico_dispersion <- ggplot(datos, aes(x = Glucosa, y = Presion, color = Tratamiento)) +
  
  geom_point(size = 3, alpha = 0.7) +
  
  labs(
    title = "Relación entre Glucosa y Presión por Tratamiento",
    x = "Nivel de Glucosa",
    y = "Nivel de Presión",
    color = "Tratamiento"
  ) +
   
  theme_minimal() +
  
  theme(
   
    legend.position = "bottom",
   legend.justification = "right" 
  )
  
print(grafico_dispersion)

# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)
grafico_facet_grid <- ggplot(datos, aes(x = Presion, y = Colesterol, color = Tratamiento)) +
  
  geom_point(size = 3, alpha = 0.7) +
  
 geom_smooth(method = "lm", se = FALSE, color = "darkgrey", linetype = "dashed") +
  
  facet_grid(. ~ Tratamiento) + 
   
  labs(
    title = "Colesterol vs. Presión por Grupo de Tratamiento (Facet Grid)",
    x = "Nivel de Presión",
    y = "Nivel de Colesterol"
  ) +
   
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

print(grafico_facet_grid)

# 7. Realiza un histogramas para cada variable. (0.5 pts)
grafico_histogramas <- ggplot(datos_long, aes(x = Valor)) +
  

  geom_histogram(bins = 15, aes(fill = Tratamiento), 
                 color = "white", position = "identity", alpha = 0.7) +
 
  facet_wrap(~ Variable, scales = "free_x", ncol = 1) + 
  
  labs(
    title = "Distribución de Frecuencia de las Variables Biomédicas",
    x = "Valor",
    y = "Frecuencia (Número de Pacientes)"
  ) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

print(grafico_histogramas)

# 8. Crea un factor a partir del tratamiento. Investifa factor(). (1 pt)
datos$Tratamiento <- factor(datos$Tratamiento)

cat(" Columna 'Tratamiento' convertida a factor.\n\n")

cat("--- Estructura de la columna Tratamiento (factor) ---\n")
print(str(datos$Tratamiento))

cat("\n--- Niveles del factor Tratamiento ---\n")
print(levels(datos$Tratamiento))

# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)

resumen_glucosa <- aggregate(Glucosa ~ Tratamiento, 
                             data = datos,
                             FUN = function(x) c(
                               media = mean(x), 
                               desv_est = sd(x)
                             ))

resumen_glucosa_limpio <- do.call(data.frame, resumen_glucosa)
colnames(resumen_glucosa_limpio) <- c("Tratamiento", "Glucosa_Media", "Glucosa_Desv_Est")

cat(" Resumen de Glucosa por Tratamiento:\n")
print(resumen_glucosa_limpio)

# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)

farmacoA <- datos %>%
  filter(Tratamiento == "FarmacoA")

# 2. Extraer los datos del Farmaco B
farmacoB <- datos %>%
  filter(Tratamiento == "FarmacoB")

# 3. Extraer los datos del Placebo
placebo <- datos %>%
  filter(Tratamiento == "Placebo")


cat(" Datos extraídos correctamente en tres variables: farmacoA, farmacoB, y placebo.\n\n")

cat("Dimensiones de farmacoA:\n")
print(dim(farmacoA)) # Debería ser 33 filas (33 pacientes)

cat("\nDimensiones de farmacoB:\n")
print(dim(farmacoB)) # Debería ser 33 filas (33 pacientes)

cat("\nDimensiones de placebo:\n")
print(dim(placebo)) # Debería ser 34 filas (34 pacientes)


# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)

cat("--- Test de Shapiro-Wilk para Glucosa ---\n")

# FarmacoA
shapiro_A <- shapiro.test(farmacoA$Glucosa)
cat("\nFarmacoA:\n")
print(shapiro_A)

# FarmacoB
shapiro_B <- shapiro.test(farmacoB$Glucosa)
cat("\nFarmacoB:\n")
print(shapiro_B)

# Placebo
shapiro_P <- shapiro.test(placebo$Glucosa)
cat("\nPlacebo:\n")
print(shapiro_P)

# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)

anova_glucosa <- aov(Glucosa ~ Tratamiento, data = datos)

cat("--- Resultados del Análisis de Varianza (ANOVA) para Glucosa ---\n")
print(summary(anova_glucosa))

cat("\n--- Medias de Glucosa por Tratamiento ---\n")
aggregate(Glucosa ~ Tratamiento, data = datos, FUN = mean)


