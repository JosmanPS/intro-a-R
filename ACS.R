#
# Análisis de Correspondencias Simple (ACS)
# Variables: tea
#

require(ca)
require(MASS)
require(FactoMineR)

data(tea)
# Variables: tipo y edad
datos <- tea[, c("Tea", "age_Q")]

# Tabla de contingencia
tab_cont <- table(datos)

# Prueba Xi-cuadrada de independencia
chisq.test(tab_cont)

# Distribuciones con márgenes
addmargins(as.matrix(tab_cont))

# Distribucinoes condicionales
round(prop.table(as.matrix(tab_cont), 1), digits = 2)

# Distribución condicional con márgenes
round(addmargins(prop.table(as.matrix(tab_cont), 1)), digits = 2)

# Barplot absoluto
barplot(t(tab_cont), beside = TRUE, legend = TRUE)

# Barplot condicional
barplot(t(prop.table(as.matrix(tab_cont), 1)),
        beside = TRUE,
        legend = TRUE)

tab_cont.sca.ca <- ca(tab_cont)
tab_cont.sca.CA <- CA(tab_cont,
                  ncp = 3,
                  row.sup = NULL,
                  col.sup = NULL,
                  axes = c(1, 2))
