## ---- eval=FALSE, include=TRUE-------------------------------------------
## "Protocolo:
## 
##  1. Daniel Felipe Villa Rengifo
## 
##  2. Lenguaje: R
## 
##  3. Tema: Regresión logistica binaria: Modelos de impacto combinado
## 
##  4. Fuentes:
##     https://dlegorreta.wordpress.com/tag/e1071/"


## ------------------------------------------------------------------------
# Ejemplo:

#Uno de los mejores predictores de donaciones futuras es el historial de donaciones anteriores y cuanto mas recientes, frecuentes y grandes mejor. En términos de comercialización, esto se conoce como R/F/M (Recency Frequency Money).

#Es muy probable que el impacto combinado de reciente y frecuencia puede ser mayor que la suma de los efectos por separado, si uno ha dado dinero a una ONG hace muy poco será poco probable que de otra vez enseguida.

#Debido a que estos predictores juntos tienen un mayor impacto en la variable dependiente, su efecto conjunto __debe modelarse como una interacción__.

# Esto en la formulación del modelo se identifica por un `*` en lugar de un `+`.


# Leemos la tabla de datos
donors<-read.csv("donors.csv",header = TRUE)

#observamos los datos:
head(donors)

# obsevamos la clase las 13 variables:
str(donors)


# Construimos un modelo complejo
rfm_model <- glm(donated ~ money + recency* frequency ,
                 data = donors,family = "binomial")


# Resumen del modelo RFM 
summary(rfm_model)

#observemos la variable de interes un resumen:
write.table(summary(rfm_model)$coefficients,
            file = "rfm_model_coefficients.txt", row.names = T, sep = " , ")

# Calculamos las predicciones del modelo RFM
rfm_prob <- predict(rfm_model, type = "response")

head(rfm_prob)

# Graficamos la curva ROC para ver el efecto del modelo y calculamos el area AUC
library(pROC)
ROC <- roc(donors$donated, rfm_prob)

# exportamos el modelo
png(filename = "ROC_donors.png")

plot(ROC, col = "red", main = "Curva ROC, de la base donors")

dev.off()


## ------------------------------------------------------------------------
"Cuando a priori no sabemos qué variables tienen más dependencia para crear el modelo una forma de hacerlo es usando la regresión gradual. Esto consiste en aplicar una función que va incrementando las variables y detecta el mejor modelo de regresión.

Para construirlo hacemos lo siguiente:

1.  creamos un modelo glm() sin predictores. se hace estableciendo la variable explicativa igual a uno.

2. Se crea otro modelo con todos las variables usando `~ .`.

3. Se aplica la función `step()` entre ambos modelos para realizar una regresión progresiva hacia adelante.

+ Debe indicarse la dirección con `direction = \"forward\"`

4. Usamos la función `predict()` sobre la lista de modelos creados con `step`"


# 1. Modelo sin predictores

null_model <- glm(donated ~1, data = donors, family = "binomial")

# 2. modelo completo
full_model <- glm(donated ~ ., data = donors, family = "binomial")
    
# 3. funcion step()
step_model <- step(null_model,
                   scope = list(lower = null_model,upper = full_model),
                   direction = "forward")

# un resumen del modelo step
summary(step_model)

# estimamos la probabilidad
step_prob <- predict(step_model, type = "response")

# Pintamos  ROC of the stepwise model
ROC <- roc(donors$donated, step_prob)

png(filename = "Optimizado.png")

plot(ROC, col = "red", main = "Modelo Optimizado sobre Curvas ROC")

dev.off()
