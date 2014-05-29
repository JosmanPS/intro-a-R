#
# Análisis de Componentes Principales (ACP)
# Variables: marginación
#

require(ggplot2)
require(GGally)
require(rgl)

#
# Datos
#
bds <- read.csv("BDs.csv", header = TRUE)
r <- 2                  # Radio del círculo
vars <- c(9:17)         # Variables de marginación
comps <- c(1:9)         # Número de componentes
datos <- bds[, vars]    # Datos que usaremos
abrv <- as.character(bds[, 3])                  # Abreviaciones
meds <- as.character(colnames(bds[, c(vars)]))  # Mediciones

#
# PCA
#
pca <- prcomp(datos, center = TRUE, scale. = TRUE)
componentes <- pca$x
correlaciones <- cor(datos, componentes) * r
dsvstd <- data.frame(cbind(comps, pca$sdev))

#
# Gráficas
#

# Matriz de correlaciones
ggpairs(datos, alpha = 0.5)

# Gráfica fácil
# plot(pca)

# Varianzas de componentes principales
ggplot() +
    geom_hline(
        yintercept = 1,
        color = "gray") +
    geom_path(
        data = dsvstd,
        aes(x = comps, y = V2),
        color = "dodgerblue3") +
    geom_point(
        data = dsvstd,
        aes(x = comps, y = V2),
        size = 4,
        color = "dodgerblue3") +
    labs(
        x = "",
        y = "Desviacion Estandar") +
    scale_x_continuous(
        breaks = comps,
        labels = colnames(componentes))

# Gráfica fácil
# biplot(pca)

# Coordenadas de correlaciones
k <- 1
corrs <- replicate(3, matrix(0, 9, 4), simplify = FALSE)
for (i in 1:2) {
    for (j in (i + 1):3) {
        corrs[[k]] <- data.frame(x = rep(0, length(comps)),
                                 y = rep(0, length(comps)),
                                 xend = correlaciones[, i],
                                 yend = correlaciones[, j])
        k <- k + 1
    }
}

# Círculo de correlaciones
circulo <- function(center = c(0, 0), npoints = 100) {
    t <- seq(0, 2 * pi, length = npoints)
    x <- center[1] + r * cos(t)
    y <- center[1] + r * sin(t)
    return(data.frame(x = x, y = y))
}
corcir <- circulo(c(0, 0), npoints = 100)

# Gráfica: CP1 vs CP2
eje_x <- paste("PC1 (",
    round(pca$sdev[1] / sum(pca$sdev) * 100, digits = 2), "%)",
    sep = "", collapse = "")
eje_y <- paste("PC2 (",
    round(pca$sdev[2] / sum(pca$sdev) * 100, digits = 2), "%)",
    sep = "", collapse = "")
ggplot() +
    geom_hline(
        yintercept = 0,
        color = "gray") +
    geom_vline(
        xintercept = 0,
        color = "gray") +
    geom_path(
        data = corcir,
        aes(x = x, y = y),
        color = "gray") +
    geom_segment(aes(
        x = corrs[[1]][1],
        y = corrs[[1]][2],
        xend = corrs[[1]][3],
        yend = corrs[[1]][4]), color = "gray") +
    geom_point(aes(
        x = corrs[[1]][[3]],
        y = corrs[[1]][[4]]),
        color = "gray") +
    geom_text(size = 3, aes(
        x = correlaciones[, 1] + 0.2,
        y = correlaciones[, 2],
        label = meds),
        color = "dodgerblue3") +
    geom_text(size = 3, aes(
        x = componentes[, 1],
        y = componentes[, 2],
        label = abrv),
        color = "orangered3") +
    labs(
        x = eje_x,
        y = eje_y) +
    theme(
        axis.title = element_text(size = 10)) +
    coord_equal()

# Gráfica: CP1 vs CP3
eje_x <- paste("PC1 (",
    round(pca$sdev[1] / sum(pca$sdev) * 100, digits = 2), "%)",
    sep = "", collapse = "")
eje_y <- paste("PC3 (",
    round(pca$sdev[3] / sum(pca$sdev) * 100, digits = 2), "%)",
    sep = "", collapse = "")
ggplot() +
    geom_hline(
        yintercept = 0,
        color = "gray") +
    geom_vline(
        xintercept = 0,
        color = "gray") +
    geom_path(
        data = corcir,
        aes(x = x, y = y),
        color = "gray") +
    geom_segment(aes(
        x = corrs[[2]][1],
        y = corrs[[2]][2],
        xend = corrs[[2]][3],
        yend = corrs[[2]][4]), color = "gray") +
    geom_point(aes(
        x = corrs[[2]][[3]],
        y = corrs[[2]][[4]]),
        color = "gray") +
    geom_text(size = 3, aes(
        x = correlaciones[, 1] + 0.2,
        y = correlaciones[, 3],
        label = meds),
        color = "dodgerblue3") +
    geom_text(size = 3, aes(
        x = componentes[, 1],
        y = componentes[, 3],
        label = abrv),
        color = "orangered3") +
    labs(
        x = eje_x,
        y = eje_y) +
    theme(
        axis.title = element_text(size = 10)) +
    coord_equal()

# Gráfica: CP2 vs CP3
eje_x <- paste("PC2 (",
    round(pca$sdev[2] / sum(pca$sdev) * 100, digits = 2), "%)",
    sep = "", collapse = "")
eje_y <- paste("PC3 (",
    round(pca$sdev[3] / sum(pca$sdev) * 100, digits = 2), "%)",
    sep = "", collapse = "")
ggplot() +
    geom_hline(
        yintercept = 0,
        color = "gray") +
    geom_vline(
        xintercept = 0,
        color = "gray") +
    geom_path(
        data = corcir,
        aes(x = x, y = y),
        color = "gray") +
    geom_segment(aes(
        x = corrs[[3]][1],
        y = corrs[[3]][2],
        xend = corrs[[3]][3],
        yend = corrs[[3]][4]), color = "gray") +
    geom_point(aes(
        x = corrs[[3]][[3]],
        y = corrs[[3]][[4]]),
        color = "gray") +
    geom_text(size = 3, aes(
        x = correlaciones[, 2] + 0.2,
        y = correlaciones[, 3],
        label = meds),
        color = "dodgerblue3") +
    geom_text(size = 3, aes(
        x = componentes[, 2],
        y = componentes[, 3],
        label = abrv),
        color = "orangered3") +
    labs(
        x = eje_x,
        y = eje_y) +
    theme(
        axis.title = element_text(size = 10)) +
    coord_equal()

# Gráfica 3D
corrs3D <- data.frame(x1 = rep(0, length(vars)),
                      y1 = rep(0, length(vars)),
                      z1 = rep(0, length(vars)),
                      x2 = correlaciones[, 1],
                      y2 = correlaciones[, 2],
                      z2 = correlaciones[, 3])

# Reformatear "corrs3D" para usar en "segments3d()"
corrs3D <- as.matrix(corrs3D)
c3D <- matrix(data = NA, nrow = 2 * length(vars), ncol = 3)
for (i in 1:length(vars)) {
    c3D[2 * i - 1, 1:3] <- corrs3D[i, 1:3]
    c3D[2 * i, 1:3] <- corrs3D[i, 4:6]
}

eje_x <- paste("PC1 (",
    round(pca$sdev[1] / sum(pca$sdev) * 100, digits = 2), "%)",
    sep = "",
    collapse = "")
eje_y <- paste("PC2 (",
    round(pca$sdev[2] / sum(pca$sdev) * 100, digits = 2), "%)",
    sep = "",
    collapse = "")
eje_z <- paste("PC3 (",
    round(pca$sdev[3] / sum(pca$sdev) * 100, digits = 2), "%)",
    sep = "",
    collapse = "")

plot3d(
    componentes[, 1],
    componentes[, 2],
    componentes[, 3],
    xlab = eje_x,
    ylab = eje_y,
    zlab = eje_z,
    box = TRUE,
    type = 'n') +
# Estados
text3d(
    componentes[, 1],
    componentes[, 2],
    componentes[, 3],
    abrv,
    size = 3,
    color = "orangered3") +
# Variables
segments3d(
    c3D,
    color = "gray") +
text3d(
    correlaciones[, 1:3],
    size = 3,
    color = "dodgerblue3",
    texts = rownames(correlaciones))

#
# Clustering
#

# Jerárquico
dist <- dist(datos)
hclust <- hclust(dist, method = "complete")
plot(hclust, labels = abrv, hang = -1,
    xlab = "Entidades Federativas",
    ylab = "Distancias",
    main = "Dendrograma")
rect.hclust(hclust, 3)

colores <- groups.3 <- cutree(hclust, 3)
colores[colores == 1] <- "forestgreen"
colores[colores == 2] <- "orangered3"
colores[colores == 3] <- "dodgerblue3"

# Gráfica 3D
eje_x <- paste("PC1 (",
    round(pca$sdev[1] / sum(pca$sdev) * 100, digits = 2), "%)",
    sep = "",
    collapse = "")
eje_y <- paste("PC2 (",
    round(pca$sdev[2] / sum(pca$sdev) * 100, digits = 2), "%)",
    sep = "",
    collapse = "")
eje_z <- paste("PC3 (",
    round(pca$sdev[3] / sum(pca$sdev) * 100, digits = 2), "%)",
    sep = "",
    collapse = "")
plot3d(
    componentes[, 1],
    componentes[, 2],
    componentes[, 3],
    xlab = eje_x,
    ylab = eje_y,
    zlab = eje_z,
    box = TRUE,
    type = 'n') +
# Estados
text3d(
    componentes[, 1],
    componentes[, 2],
    componentes[, 3],
    abrv,
    size = 3,
    color = colores) +
# Variables
segments3d(
    c3D,
    color = "gray") +
text3d(
    correlaciones[, 1:3],
    size = 3,
    color = "mediumslateblue",
    texts = rownames(correlaciones))
