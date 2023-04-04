# Cargar los datos en un data frame
ldl <- read.table("LDL_2.txt", header = TRUE)$LDL

# Calcular la media y la desviación estándar
media <- mean(ldl)
sd <- sd(ldl)

# Tamaño de la muestra
n <- length(ldl)

# Coeficiente de confianza
conf_level <- 0.9

# Valor crítico t
t_value <- qt((1 - conf_level) / 2, n - 1, lower.tail = FALSE)

# Calcular el límite inferior y superior del intervalo de confianza
lower_limit <- media - t_value * (sd / sqrt(n))
upper_limit <- media + t_value * (sd / sqrt(n))

# Mostrar el intervalo de confianza t Student
cat("Intervalo de confianza del 90% para el nivel medio de colesterol LDL: (", lower_limit, ", ", upper_limit, ")\n")
