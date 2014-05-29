#
# Análisis de Correlación Canónica (ACC)
# Variables: elección y marginación
#

require(ggplot2)
require(GGally)
require(CCA)

#
# Datos
#
bds <- read.csv("BDs.csv", header = TRUE)
vars.elec <- c(4:8)     # Variables de elección
vars.marg <- c(9:17)    # Variables de marginación
abrv <- as.character(bds[, 3])  # Abreviaciones
r <- 2                  # Radio de círculo
alpha <- 0.7            # Nivel de transparencia

# Intra-correlaciones
ggpairs(bds[, vars.elec], alpha = 0.5)
ggpairs(bds[, vars.marg], alpha = 0.5)

# Intra e inter-correlaciones
matcor(bds[, vars.elec], bds[, vars.marg])
img.matcor(matcor(bds[, vars.elec], bds[, vars.marg]), 1)

#
# CCA
#
cca <- cc(bds[, vars.elec], bds[, vars.marg])
cca$cor             # Correlacioens
cca$scores          # Factores

# Gráfica fácil
# plt.cc(cca,
#        int = 1,
#        ind.names = abrv,
#        var.label = TRUE)

# Círculo
circulo <- function(center = c(0, 0), npoints = 100) {
    t <- seq(0, 2 * pi, length = npoints)
    x <- center[1] + r * cos(t)
    y <- center[1] + r * sin(t)
    return(data.frame(x = x, y = y))
}
corcir <- circulo(c(0, 0), npoints = 100)

# Gráfica de variables (dims 1 y 2)
vars.uno <- cbind(
    matrix(0, nrow = length(cca$scores$corr.X.xscores[,1]), ncol = 2),
    cca$scores$corr.X.xscores[, 1:2])
vars.dos <- cbind(
    matrix(0, nrow = length(cca$scores$corr.Y.xscores[,1]), ncol = 2),
    cca$scores$corr.Y.xscores[, 1:2])
vars.uno <- vars.uno * r
vars.dos <- vars.dos * r
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
    # Variables de elecciones
    geom_segment(aes(
        x = vars.uno[, 1],
        y = vars.uno[, 2],
        xend = vars.uno[, 3],
        yend = vars.uno[, 4]),
        color = "dodgerblue3",
        alpha = alpha) +
    geom_point(aes(
        x = vars.uno[, 3],
        y = vars.uno[, 4]),
        color = "dodgerblue3",
        alpha = alpha) +
    geom_text(size = 4, aes(
        x = cca$scores$corr.X.xscores[, 1] * r + 0.05,
        y = cca$scores$corr.X.xscores[, 2] * r + 0.05,
        label = rownames(cca$scores$corr.X.xscores)),
        color = "dodgerblue3",
        alpha = alpha) +
    # Variables de marginación
    geom_segment(aes(
        x = vars.dos[, 1],
        y = vars.dos[, 2],
        xend = vars.dos[, 3],
        yend = vars.dos[, 4]),
        color = "orangered3",
        alpha = alpha) +
    geom_point(aes(
        x = vars.dos[, 3],
        y = vars.dos[, 4]),
        color = "orangered3",
        alpha = alpha) +
    geom_text(size = 4, aes(
        x = cca$scores$corr.Y.xscores[, 1] * r + 0.05,
        y = cca$scores$corr.Y.xscores[, 2] * r + 0.05,
        label = rownames(cca$scores$corr.Y.xscores)),
        color = "orangered3",
        alpha = alpha) +
    # Estados
    geom_text(size = 4, aes(
        x = cca$scores$xscores[, 1],
        y = cca$scores$xscores[, 2],
        label = abrv),
        color = "forestgreen") +
    labs(
        x = "Dimension 1",
        y = "Dimension 2") +
    theme(
        axis.title = element_text(size = 10)) +
    coord_equal()
