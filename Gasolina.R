precios <- read_excel("Descargas/Series de tiempos/preciosdegasolina.xlsx",  sheet = "promedios", col_types = c("text","date", "numeric"))
View(preciosdegasolina)   
# Lista de paquetes a verificar e instalar si es necesario
paquetes <- c("readxl", "dplyr", "tidyverse", "zoo", "ggplot2", "tseries", "urca", "forecast", 
              "ggsignif", "RcmdrMisc", "knitr", "kableExtra", "astsa")

# Verificar si los paquetes están instalados
paquetes_faltantes <- paquetes[!(paquetes %in% installed.packages())]

# Instalar los paquetes faltantes
if (length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes, dependencies = TRUE)
}

# Cargar los paquetes
invisible(sapply(paquetes, require, character.only = TRUE))

# Confirmar que los paquetes estén cargados
print(search())



library(readxl)
library(dplyr)
library(tidyverse)
library(tidyverse)
library(zoo)
library(ggplot2)
library(tseries)
library(urca)
library(tseries)
library(forecast)
library(readxl)
library(ggplot2)
library(ggsignif)
library(RcmdrMisc)
library(knitr)
library(kableExtra)
library(astsa)

gasolina <- read_excel("gasolina.xlsx", col_types = c("date","numeric"))
gasolina
base<-gasolina

# Calcular el promedio mensual para los meses con múltiples observaciones
base_promedio <- base %>%
  mutate(mes = as.Date(format(fecha, "%Y-%m-01"))) %>%
  group_by(mes) %>%
  summarise(precio = mean(precio))

# Crear un dataframe con todos los meses desde el primero hasta el último
todos_los_meses <- data.frame(mes = seq(min(base_promedio$mes), max(base_promedio$mes), by = "1 month"))

# Unir con los datos reales para detectar los meses faltantes y asignar NA a los precios
base_completa <- merge(todos_los_meses, base_promedio, by = "mes", all.x = TRUE)

# Imprimir el resultado
print(base_completa)
# Cambiar el formato de la columna "mes" a solo año y mes
base_completa$mes <- format(base_completa$mes, "%Y-%m")

# Imprimir el resultado
print(base_completa)
datos<-base_completa

max(datos$mes)
tail(datos)
datos
min(mes)
attach(datos)
summary(fit <- lm(datos$precio~time(datos$precio)))
head(datos)
ts.precio<-ts(base_completa$precio,start = c(2013,10),end = c(2023,2),frequency = 12)
na.contiguous(ts.precio)
plot(ts.precio,main = "Serie de Tiempo del Precio promedio mensual del galón de gasolina", xlab = "Años", ylab = "Precio x Galón")
#Interpolación linear
ts.precio<-tsclean(ts.precio,replace.missing = TRUE)
plot(ts.precio,main = "Serie de Tiempo del Precio promedio mensual del galón de gasolina", xlab = "Años", ylab = "Precio x Galón")

#estacionatiedad
adf.test(ts.precio)
ndiffs(ts.precio,test = "adf",alpha = 0.05)
ts.precio.diff<-diff(ts.precio,differences = 2)
#volvemos a probar
adf.test(ts.precio.diff)

acf(ts.precio.diff, type = "correlation",main ="Función de autocorrelación simple")
acf(ts.precio.diff,type= "partial",main ="Función de autocorrelación parcial")
adf.test(ts.precio, k=12)
auto.arima(ts.precio.diff)

# Ajuste de modelos AR(p)
ar_models <- list()

for (p in 6:9) {
  ar_model <- arima(ts.precio.diff, order = c(p, 0, 0))
  ar_models[[paste0("AR(", p, ")")]] <- ar_model
}

# Obtener métricas para modelos AR(p)
ar_metrics <- data.frame(Model = character(),
                         AIC = numeric(),
                         SCE = numeric(),
                         Num_Parameters = integer(),
                         R_squared_adj = numeric(),
                         stringsAsFactors = FALSE)

for (p in 6:9) {
  ar_model <- ar_models[[paste0("AR(", p, ")")]]
  
  # Obtener los residuos del modelo
  residuals <- residuals(ar_model)
  
  # Calcular el número de parámetros
  num_parameters <- length(ar_model$coef)-1
  
  # Calcular el R cuadrado ajustado
  r_squared_adj <- 1 - (ar_model$loglik - num_parameters) / length(AirPassengers)
  
  # Calcular el AIC y SCE
  aic <- AIC(ar_model)
  sce <- sum(residuals(ar_model)^2)
  
  # Agregar los resultados al dataframe de métricas
  ar_metrics <- rbind(ar_metrics, data.frame(Model = paste0("AR(", p, ")"),
                                             AIC = aic,
                                             SCE = sce,
                                             Num_Parameters = num_parameters,
                                             R_squared_adj = r_squared_adj))
}

# Imprimir las métricas para los modelos AR(p)
kable(ar_metrics)


# Ajuste de modelos MA(q)
ma_models <- list()

for (q in 6:9) {
  ma_model <- arima(ts.precio.diff, order = c(0, 0, q))
  ma_models[[paste0("MA(", q, ")")]] <- ma_model
}

ma_metrics <- data.frame(Model = character(),
                         AIC = numeric(),
                         SCE = numeric(),
                         Num_Parameters = integer(),
                         R_squared_adj = numeric(),
                         stringsAsFactors = FALSE)

for (q in 6:9) {
  ma_model <- ma_models[[paste0("MA(", q, ")")]]
  
  # Obtener los residuos del modelo
  residuals <- residuals(ma_model)
  
  # Calcular el número de parámetros
  num_parameters <- length(ma_model$coef)-1
  
  # Calcular el R cuadrado ajustado
  r_squared_adj <- 1 - (ma_model$loglik - num_parameters) / length(AirPassengers)
  
  # Calcular el AIC y SCE
  aic <- AIC(ma_model)
  sce <- sce <- sum(residuals(ma_model)^2)
  
  # Agregar los resultados al dataframe de métricas
  ma_metrics <- rbind(ma_metrics, data.frame(Model = paste0("MA(", q, ")"),
                                             AIC = aic,
                                             SCE = sce,
                                             Num_Parameters = num_parameters,
                                             R_squared_adj = r_squared_adj))
}

# Imprimir las métricas para los modelos MA(q)
kable(ma_metrics)



# Ajuste de modelos ARMA(p,q)
arma_models <- list()


arma_model <- arima(ts.precio.diff, order = c(1, 0, 1))
arma_models[[paste0("ARMA(",1,",",1,")")]] <- arma_model

arma_model <- arima(ts.precio.diff, order = c(2, 0, 2))
arma_models[[paste0("ARMA(",2,",",2,")")]] <- arma_model

arma_model <- arima(ts.precio.diff, order = c(3, 0, 4))
arma_models[[paste0("ARMA(",3,",",4,")")]] <- arma_model

arma_model <- arima(ts.precio.diff, order = c(2, 0, 1))
arma_models[[paste0("ARMA(",2,",",1,")")]] <- arma_model

# Obtener métricas para modelos MA(q)
arma_metrics <- data.frame(Model = character(),
                           AIC = numeric(),
                           SCE = numeric(),
                           Num_Parameters = integer(),
                           R_squared_adj = numeric(),
                           stringsAsFactors = FALSE)

#valores_p <- c(1, 2, 3,2)  # Valores para la p
#valores_q <- c(1, 2, 4,1)  # Valores para la q

# Ciclo for para iterar sobre los valores de las variables
combinaciones <- list(
  list(p = 1, q = 1),
  list(p = 2, q = 2),
  list(p = 3, q = 4),
  list(p = 2, q = 1))

for (i in 1:length(combinaciones)) {
  p <- combinaciones[[i]]$p
  q <- combinaciones[[i]]$q
  
  arma_model <- arma_models[[paste0("ARMA(",p,",",q,")")]]
  
  # Obtener los residuos del modelo
  residuals <- residuals(arma_model)
  
  # Calcular el número de parámetros
  num_parameters <- length(arma_model$coef)-1
  
  # Calcular el R cuadrado ajustado
  r_squared_adj <- 1 - (arma_model$loglik - num_parameters) / length(ts.precio.diff)
  
  # Calcular el AIC y SCE
  aic <- AIC(arma_model)
  sce <- sce <- sum(residuals(arma_model)^2)
  
  # Agregar los resultados al dataframe de métricas
  arma_metrics <- rbind(arma_metrics, data.frame(Model = paste0("ARMA(",p,",",q,")"),
                                                 AIC = aic,
                                                 SCE = sce,
                                                 Num_Parameters = num_parameters,
                                                 R_squared_adj = r_squared_adj))
}


kable(arma_metrics)


# Unir los dataframes ar_metrics y ma_metrics
combined_metrics <- rbind(ar_metrics, ma_metrics, arma_metrics)

# Ordenar por AIC de menor a mayor
combined_metrics <- combined_metrics[order(combined_metrics$AIC), ]

# Mostrar la tabla resultante
kable(combined_metrics)



#ajustar el modelo arma(1,1)
modelo <- arima(ts.precio.diff, order = c(1, 0, 1))
checkresiduals(modelo)
# Visualizar los diagnósticos y las pruebas
plot(modelo)


#seguir
