#
# === Preparación de BDs ===
#
# - IFE, elección federal 2011:
#   - Observaciones a nivel casilla
#   - Números absolutos
# - CONAPO, indice de marginación 2010:
#   - Observaciones a nivel municipio
#   - Números porcentuales
#

#
# Datos
#
ife <- read.table("IFE_2011.txt", header = TRUE, sep = "|")
conapo <- read.csv("CONAPO_2010.csv", header = TRUE)

# Quitamos datos de más
conapo <- conapo[1:2456, 1:17]

#
# CONAPO
#
# Los números son porcentuales lo que
# facilita mucho la limpieza.
conapo_limpia <- matrix(0, 32, 10)
conapo_limpia[, 1] <- c(1:32)
colnames(conapo_limpia) <- colnames(conapo[, c(1,5:13)])
for (i in 1:32) {
    for (j in 1:9) {
        conapo_limpia[i, j + 1] <-
            mean(as.matrix(conapo[conapo$clv_ent_fed == i, ][j + 4]))
    }
}

#
# IFE
#
# Los números están en cantidades absolutas.
# Debemos pasar todo a números porcentuales
# tomando en cuenta las alianzas.
colnames(ife) <- tolower(colnames(ife))
ife_limpia <- matrix(0, 32, 8)
ife_limpia[, 1] <- c(1:32)
entidades <- unique(ife[, 1])
ife_limpia[, 2] <- as.character(entidades)

# Abreviaciones oficiales de tres letras
ife_limpia[, 3] <- c("AGU", "BCN", "BCS", "CAM", "COA", "COL",
    "CHP", "CHH", "DIF", "DUR", "GUA", "GRO", "HID",
    "JAL", "MEX", "MIC", "MOR", "NAY", "NLE", "OAX",
    "PUE", "QUE", "ROO", "SLP", "SIN", "SON", "TAB",
    "TAM", "TLA", "VER", "YUC", "ZAC")
colnames(ife_limpia) <- c("clv_ent_fed",
    "ent_fed",
    "abr_ent_fed",
    "pan",
    "pri",
    "prd",
    "nulos",
    "abst")
for (i in 1:32) {
    total <- as.matrix(ife[ife$nombre_estado == entidades[i], ][21])

    # %PAN
    pan <- as.matrix(ife[ife$nombre_estado == entidades[i], ][6])
    p_pan <- pan/total
    # Remover NaN's por falta de votos (0/0)
    p_pan[is.nan(p_pan) | is.na(p_pan)] <- 0
    ife_limpia[i, 4] <- mean(p_pan)

    # %PRI con alianza (PVEM)
    pri <- as.matrix(ife[ife$nombre_estado == entidades[i], ][7])
    pri_pvem <- as.matrix(ife[ife$nombre_estado == entidades[i], ][13])
    p_pri <- (pri + pri_pvem) / total
    # Remover NaN's por falta de votos (0/0)
    p_pri[is.nan(p_pri) | is.na(p_pri)] <- 0
    ife_limpia[i, 5] <- mean(p_pri)

    # %PRD con alianzas (PT, MC, PT-MC)
    prd <- as.matrix(ife[ife$nombre_estado == entidades[i], ][8])
    prd_pt_mc <- as.matrix(ife[ife$nombre_estado == entidades[i], ][14])
    prd_pt <- as.matrix(ife[ife$nombre_estado == entidades[i], ][15])
    prd_mc <- as.matrix(ife[ife$nombre_estado == entidades[i], ][16])
    p_prd <- (prd + prd_pt_mc + prd_pt + prd_mc) / total
    # Remover NaN's por falta de votos (0/0)
    p_prd[is.nan(p_prd) | is.na(p_prd)] <- 0
    ife_limpia[i, 6] <- mean(p_prd)

    # %Nulos = mean([nul_j/tot_j])
    nulos <- as.matrix(ife[ife$nombre_estado == entidades[i], ][19])
    p_nulos <- nulos / total
    p_nulos[is.nan(p_nulos) | is.na(p_nulos)] <- 0
    ife_limpia[i, 7] <- mean(p_nulos)

    # %Abstencionismo =
    #       mean([(lista_nominal_j - tot_j) / lista_nominal_j])
    # Nota: cuidado con los 0's en lista_nominal
    total_slnz <- as.matrix(ife[
        ife$nombre_estado == entidades[i] &
        ife$lista_nominal != 0, ][21])
    lista_nominal <- as.matrix(ife[
        ife$nombre_estado == entidades[i] &
        ife$lista_nominal != 0, ][22])
    abst <- lista_nominal - total_slnz
    p_abst <- abst/lista_nominal
    p_abst[is.nan(p_abst) | is.na(p_abst)] <- 0
    ife_limpia[i, 8] <- mean(p_abst)
}

#
# Fusionamos las BDs
#
datos <- merge(ife_limpia, conapo_limpia,
    by = "clv_ent_fed",
    sort = FALSE)

#
# Guardamos
#
write.table(as.matrix(datos), "BDs.csv",
    row.names = FALSE,
    quote = FALSE,
    sep = ",")
