#
# Análisis de Discriminación Lineal (ADL)
# Variables: iris
#

library(MASS)
attach(iris)

datos <- iris

# Scatterplot con grupos y correlaciones
panel.pearson <- function(x, y, ...) {
    horizontal <- (par("usr")[1] + par("usr")[2]) / 2
    vertical <- (par("usr")[3] + par("usr")[4]) / 2
    text(horizontal,
         vertical,
         format(abs(cor(x, y)), digits = 2),
         cex = 1.7)
}
pairs(datos[1:4],
      pch = 21,
      bg = c("red","green3","blue")[unclass(datos$Species)],
      upper.panel = panel.pearson)

# ADL
datos.adl <- lda(Species ~ .,
    data = datos,
    na.action = "na.omit")

# Resultados
datos.adl

# Resultados de clasificación
ct <- table(datos$Species, datos.adl$class)
ct
prop.table(ct, 1)

# Porcentage de clasificaciones correctas
sum(diag(prop.table(ct)))

# Gráfica fácil
plot(datos.adl)

# Scatterplot con las primeras dos
# fuciones discriminantes
proyecciones <- cbind(scale(as.matrix(iris[, -5]), scale = FALSE) %*% datos.adl$scaling, iris[, 5, drop = FALSE])
ggplot(data = proyecciones,
    aes(x = LD1, y = LD2, col = Species)) + geom_point()

# Densidades en la primer función discriminante
plot(datos.adl, dimen = 1, type = "both")
