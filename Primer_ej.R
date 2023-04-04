# Cargar los datos en un data frame
ldl <- read.table("LDL_2.txt", header = TRUE)$LDL

# Especificar el diseño de la ventana gráfica
par(mfrow = c(2, 2))

# Histograma
hist(ldl, main = "Histograma de niveles de colesterol LDL", xlab = "Niveles de LDL (mg/dL)", col = "blue")

# Gráfico de densidad
plot(density(ldl), main = "Gráfico de densidad de niveles de colesterol LDL", xlab = "Niveles de LDL (mg/dL)", ylab = "Densidad", col = "red")

# Test de Shapiro-Wilk
swtest <- shapiro.test(ldl)
cat("Test de Shapiro-Wilk:\n")
cat("Estadístico de prueba: ", swtest$statistic, "\n")
cat("Valor p: ", swtest$p.value, "\n")

# Prueba de Lilliefors
library(nortest)
lillie_test <- lillie.test(ldl)
cat("Prueba de Lilliefors: \n")
cat("Valor p: ", lillie_test$p.value, "\n")

# Prueba de Anderson-Darling
ad_test <- ad.test(ldl)
cat("Prueba de Anderson-Darling: \n")
cat("Valor p: ", ad_test$p.value, "\n")

# Media
media <- mean(ldl)
cat("Media: ", media, "\n")

#Mediana
mediana <- median(ldl)
cat("Mediana: ", mediana, "\n")

# Moda
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
moda_result <- moda(ldl)
cat("Moda: ", moda_result, "\n")

# Percentiles 5 y 95
p5 <- quantile(ldl, 0.05)
cat("Percentil 5: ", p5, "\n")

p95 <- quantile(ldl, 0.95)
cat("Percentil 95: ", p95, "\n")

# Recorrido
recorrido <- range(ldl)
cat("Recorrido: ", recorrido, "\n")

# Cuasidesviación típica
sd <- sqrt(mean((ldl - mean(ldl))^2))
cat("Cuasidesviación típica: ", sd, "\n")

# Coeficiente de asimetría
library(e1071)
asimetria_3 <- skewness(ldl)
asimetria_2 <- skewness(ldl, type=2)
cat("Coeficiente de asimetría tipo 3 (Fisher): ", asimetria_3, "\n")
cat("Coeficiente de asimetría tipo 2: ", asimetria_2, "\n")

# Representar histograma con distribución normal
# Crear el histograma con 30 bins
hist(ldl, breaks = 30, main = "Histograma de niveles de colesterol LDL", xlab = "Niveles de LDL (mg/dL)", col = "blue", freq = FALSE)

# Calcular los límites del histograma
hist_limits <- par("usr")

# Crear la curva de densidad normal
x <- seq(hist_limits[1], hist_limits[2], length.out = 100)
y <- dnorm(x, mean = media, sd = sd)
lines(x, y, col = "red")