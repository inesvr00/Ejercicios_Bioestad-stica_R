library(lmtest)

# Cargar los datos en un data frame
manchas <- read.table("manchas.txt", header = TRUE)

# Análisis exploratorio de datos
plot(manchas$Manchas, manchas$Nivel, main = "Nivel del lago vs Manchas solares. 1902-1921.", xlab = "Manchas solares", ylab = "Nivel del lago")
correlacion <- cor(manchas$Manchas, manchas$Nivel) # Correlación entre las variables
cat("La correlación entre las variables Manchas y Nivel es:", correlacion, "\n")

# Calcular la recta de regresión de Y sobre X
regresion <- lm(Nivel ~ Manchas, data = manchas)
summary(regresion) # Resumen del modelo

# Mostrar la ecuación de la recta de regresión
cat("La ecuación de la recta de regresión es: y =", round(pendiente, 3), "x +", round(coef(regresion)[1], 3), "\n")

# Analizar la significancia de la recta de regresión
pendiente <- coef(regresion)[2] # Pendiente de la recta de regresión

# Añadir la función de la recta de regresión al plot
abline(regresion, col = "red")

# Obtener el p-valor para la hipótesis nula de que la pendiente es cero
p_valor <- summary(regresion)$coefficients[2, 4]

# Mostrar el resultado del p-valor
if (p_valor > 0.05) {
  cat("El p-valor es alto (p =", p_valor, "), lo que indica que la recta de regresión no es significativa.\n")
} else {
  cat("El p-valor es bajo (p =", p_valor, "), lo que indica que la recta de regresión es significativa.\n")
}
