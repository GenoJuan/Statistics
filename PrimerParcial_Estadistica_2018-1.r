### Se cargan los datos registrados. 
 datos = read.csv("Calificaciones.csv", sep = ",")

# Se grafican los boxplots de ambas calificaciones en función del género.
par(mfrow = c(1,2))
boxplot(Literatura ~ Sexo, datos, main = "Calificaciones en Literatura", las = 1, col = c("red", "blue"), ylab = "Calificaciones")
boxplot(Matematicas ~ Sexo, datos, main = "Calificaciones en Matematicas", las = 1, col = c("red", "blue"))

# Se separan en cuatro variables diferentes las calificaciones obtenidas por hombres y mujeres en cada una de las asignaturas.
hombres.l = subset(datos[,5], datos[,6] == "H")
mujeres.l = subset(datos[,5], datos[,6] == "M")

hombres.m = subset(datos[,4], datos[,6] == "H")
mujeres.m = subset(datos[,4], datos[,6] == "M")

# Se realiza la prueba de Shapiro-Wilk para cada una de los grupos de datos anteriores.
shapiro.test(hombres.l)
shapiro.test(mujeres.l)
shapiro.test(hombres.m)
shapiro.test(mujeres.m)

# Se grafican los graficos Q-Q para cada grupo de datos y se anotan los resultados obtenidos en la prueba de Shapiro-Wilk.
# Además de una linea y = a + bx con a = 0 y b = 1.
par(mfrow = c(2,2))
qqnorm(scale(hombres.l), pch = 16, xlim = c(-4, 4), ylim = c(-4, 4), main = paste("Normal Q-Q Plot\nHombres literatura"))
abline(a = 0, b = 1, lty = 2, lwd = 2, col = "red")
grid(lwd = 1.5, col = "gray")
text(x = -2, y = 4, labels = paste("W =", 0.906))
text(x = -2, y = 3.5, labels = paste("p-value = 4.92e-09"))

qqnorm(scale(mujeres.l), pch = 16, xlim = c(-4, 4), ylim = c(-4, 4), main = paste("Normal Q-Q Plot\nMujeres literatura"))
abline(a = 0, b = 1, lty = 2, lwd = 2, col = "red")
grid(lwd = 1.5, col = "gray")
text(x = -2, y = 4, labels = paste("W =", 0.918))
text(x = -2, y = 3.5, labels = paste("p-value = 4.76e-08"))

qqnorm(scale(hombres.m), pch = 16, xlim = c(-4, 4), ylim = c(-4, 4), main = paste("Normal Q-Q Plot\nHombres matematicas"))
abline(a = 0, b = 1, lty = 2, lwd = 2, col = "red")
grid(lwd = 1.5, col = "gray")
text(x = -2, y = 4, labels = paste("W =", 0.967))
text(x = -2, y = 3.5, labels = paste("p-value = 4.39e-04"))

qqnorm(scale(mujeres.m), pch = 16, xlim = c(-4, 4), ylim = c(-4, 4), main = paste("Normal Q-Q Plot\nMujeres matematicas"))
abline(a = 0, b = 1, lty = 2, lwd = 2, col = "red")
grid(lwd = 1.5, col = "gray")
text(x = -2, y = 4, labels = paste("W =", 0.984))
text(x = -2, y = 3.5, labels = paste("p-value = 0.0479"))


# Se realiza una prueba de Wilcoxon para comparar las medianas de las calificaciones entre hombres y mujeres en cada asignatura. 
wilcox.test(hombres.l, mujeres.l)
wilcox.test(hombres.m, mujeres.m)


# Se obtienen las diferencias entre las calificaciones de literatura y matemática para hombres y mujeres.
mujeres = mujeres.l - mujeres.m
hombres = hombres.l - hombres.m

# Se grafican la distribución de las diferencias anteriores para hombres y mujeres.
x = list(H = hombres, M = mujeres)
boxplot(x, col = c("blue", "red"), las = 1, main = paste("Diferencias entre\nliteratura y matematicas"))

# Se realiza una prueba de Shapiro-Wilk
shapiro.test(mujeres)
shapiro.test(hombres)

# Se grafican los graficos Q-Q para cada grupo de datos y se anotan los resultados obtenidos en la prueba de Shapiro-Wilk.
# Además de una linea y = a + bx con a = 0 y b = 1.

par(mfrow = c(1,2))
qqnorm(scale(hombres), pch = 16, xlim = c(-4, 4), ylim = c(-4, 4))
abline(a = 0, b = 1, lty = 2, lwd = 2, col = "red")
grid()
qqnorm(scale(mujeres), pch = 16, xlim = c(-4, 4), ylim = c(-4, 4))
abline(a = 0, b = 1, lty = 2, lwd = 2, col = "red")
grid()

# Se realizan las pruebas adecuadas para cada caso.
wilcox.test(hombres, alternative = "less")
t.test(mujeres, alternative = "greater")




