#
# Análisis de Correspondencias Múltiple
# Variables: tea
#

require(FactoMineR)
require(ggplot2)

data(tea)
data <- tea[, c("Tea", "how", "where", "age_Q")]

categories <- apply(data, 2, function(x) nlevels(as.factor(x)))
categories

# MCA
data.mca <- MCA(data, graph = FALSE)
data.mca

# Valores propios
data.mca$eig

# Coordenadas de variables
data.mca.vars <- data.frame(data.mca$var$coord,
                       Variable = rep(names(categories), categories))

# Coordenadas de observaciones
data.mca.obs <- data.frame(data.mca$ind$coord)

# Gráfica fácil
# plot(data.mca)

# Gráfica de variables
ggplot(data = data.mca.vars,
       aes(x = Dim.1, y = Dim.2, label = rownames(data.mca.vars))) +
    geom_hline(yintercept = 0, colour = "gray70") +
    geom_vline(xintercept = 0, colour = "gray70") +
    geom_text(aes(colour = Variable)) +
    xlab("") +
    ylab("")

# Gráfica de variables y observaciones
ggplot(data = data.mca.obs, aes(x = Dim.1, y = Dim.2)) +
    geom_hline(yintercept = 0, colour = "gray70") +
    geom_vline(xintercept = 0, colour = "gray70") +
    geom_point(colour = "gray50", alpha = 0.5) +
    geom_density2d(colour = "gray80") +
    geom_text(data = data.mca.vars,
              aes(x = Dim.1, y = Dim.2,
                  label = rownames(data.mca.vars), colour = Variable)) +
    scale_colour_discrete(name = "Variable") +
    xlab("") +
    ylab("")

# Elipses de confianza
plotellipses(data.mca, level = 0.95)
