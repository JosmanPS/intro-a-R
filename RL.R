#
# Regresión Logística
# Variables: terrorismo
#

require(reshape2)
require(GGally)

data <- read.csv("GTD.csv", header = TRUE)
data$target <- relevel(data$target, ref = "Airports & Aircraft")
data$weapon <- relevel(data$weapon, ref = "Bombs")
data$death <- relevel(data$death, ref = "yes")

#
# Muestra
#
set.seed(123454321)
trn_sample <- sample(dim(data)[1], dim(data)[1] * .8)
data.train <- data[ trn_sample, ]
data.test  <- data[-trn_sample, ]

#
# Estadística descriptiva
#
ggplot(data = data.train, aes(death, fill = attack)) +
    geom_histogram(position = "dodge")

ggplot(data = data.train, aes(death, fill = target)) +
    geom_histogram(position = "dodge")

ggplot(data = data.train, aes(death, fill = weapon)) +
    geom_histogram(position = "dodge")

#
# Rregresión logística
#
log_fit <- glm(death ~ weapon + target,
    data = data.train,
    family = "binomial")

summary(log_fit)
exp(coef(log_fit))      # Momios
exp(confint(log_fit))   # 95% CI de momios

#
# Predicción
#
pred <- predict(log_fit, newdata = data.test, type = "response")
pred <- pred >= .5
table(pred, data.test$bnm)
prop.table(table(pred, data.test$bnm))
sum(diag(table(pred, data.test$bnm)))/dim(data.test)[1]

#
# Ajuste de cutoff
#
est <- data.frame(matrix(0, ncol = 3, nrow = 300))
colnames(est) <- c("Cutoff", "Porcentage", "Mediciones")
for (i in 1:100) {
    pred <- predict(log_fit, newdata = data.test, type = "response")
    pred <- pred >= i/100
    t <- table(pred, data.test$bnm)
    est[i, 1] <- i/100
    est[i + 100, 1] <- i/100
    est[i + 200, 1] <- i/100
    est[i + 300, 1] <- i/100
    est[i, 2] <- sum(diag(t))/dim(data.test)[1]
    est[i + 100, 2] <- t[2]/dim(data.test)[1]
    est[i + 200, 2] <- t[3]/dim(data.test)[1]
    est[i, 3] <- "Correctos"
    est[i + 100, 3] <- "Falso positivo"
    est[i + 200, 3] <- "Falso negativo"
}
est <- na.omit(est)
est <- subset(est, Cutoff != .99 & Cutoff != 1.00)
cutoff <- est[which.max(est[est$Mediciones == "Correctos", 2]), 1]
ggplot(data = est,
    aes(x = Cutoff,
        y = Porcentage,
        colour = Mediciones)) +
    geom_line() +
    geom_vline(xintercept = 0.5,
        color = "darkgreen",
        alpha = 0.3) +
    geom_vline(xintercept = cutoff,
        color = "red",
        alpha = 0.2)

pred <- predict(log_fit, newdata = data.test, type = "response")
pred <- pred >= cutoff
table(pred, data.test$bnm)
prop.table(table(pred, data.test$bnm))
sum(diag(table(pred, data.test$bnm)))/dim(data.test)[1]

