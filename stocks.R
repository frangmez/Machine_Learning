library(caret)
library(datasets)
library(tidyverse)
library(reshape2)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                             LECTURA DE DATOS
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Lectura del dataset de stocks de mercado entre 1991 y 1998:
data("EuStockMarkets")

# Mostrar la estructura del dataset:
str(EuStockMarkets)

# Impresión de la cabecera:
head(EuStockMarkets)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                             PREPROCESAMIENTO
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Representación gráfica del dataset:
par(bty = "n",lwd = 2)
plot(EuStockMarkets)
grid(lty = 2)

# ------------------------------------------------------------------------------
#                   BÚSQUEDA DE TENDENCIAS Y ESTACIONALIDAD
# ------------------------------------------------------------------------------

# Convertir el conjunto de datos a una serie de tiempo:
stocks_ts <- ts(EuStockMarkets,frequency = 365)

# Descomposición de la serie temporal:
f <- decompose(stocks_ts)
index_names <- c("DAX","SMI","CAC","FTSE")
colnames(f$trend) <- index_names
colnames(f$random) <- index_names

# Crear una lista para almacenar las componentes estacionales
decomposition_list <- list()

# Realizar la descomposición personalizada para cada índice
for (index in colnames(EuStockMarkets)) {
  index_series <- EuStockMarkets[,index]
  decomposition <- decompose(index_series)
  decomposition_list[[index]] <- decomposition
}

# Crear una matriz para almacenar las componentes estacionales de los índices
num_rows <- length(decomposition_list[[1]]$seasonal)
num_indices <- length(decomposition_list)
seasonal_matrix <- matrix(NA,
                          nrow = num_rows,
                          ncol = num_indices)

# Llenar la matriz con las componentes estacionales
for (i in 1:num_indices) {
  seasonal_matrix[,i] <- decomposition_list[[i]]$seasonal
}

# Convertir la matriz en un objeto de series de tiempo
seasonal_series <- ts(seasonal_matrix,
                      start = start(decomposition_list[[1]]$seasonal),
                      frequency = frequency(decomposition_list[[1]]$seasonal))

# Cambiar los nombres de las columnas de seasonal_series
colnames(seasonal_series) <- index_names
head(seasonal_series)

# ------------------------------------------------------------------------------
#                   CREACIÓN DE DATAFRAMES ÚTILES
# ------------------------------------------------------------------------------

index_dataframes <- list()

# Recorrer los nombres de los índices bursátiles
for (index_name in colnames(f$x)) {
  index_data <- f$x[,index_name]
  index_trend <- f$trend[,index_name]
  index_random <- f$random[,index_name]
  index_seasonal <- seasonal_series[,index_name]
  
  # Crear un dataframe para el índice actual
  index_dataframe <- data.frame(
    Date = time(index_data),
    Price = as.vector(index_data),
    Trend = as.vector(index_trend),
    Randomness = as.vector(index_random),
    Seasonal = as.vector(index_seasonal)
  )
  
  # Agregar el dataframe a la lista
  index_dataframes[[index_name]] <- index_dataframe
}

# Dataframes separados para cada índice bursátil:
dax_df <- index_dataframes$DAX
smi_df <- index_dataframes$SMI
cac_df <- index_dataframes$CAC
ftse_df <- index_dataframes$FTSE

# ------------------------------------------------------------------------------
#                      COMPARATIVA DE LOS ÍNDICES
# ------------------------------------------------------------------------------

# Gráfica para los precios de los índices:
price_plot <- ggplot() + 
  geom_line(data = dax_df,aes(x = Date,y = Price,color = "DAX")) +
  geom_line(data = smi_df,aes(x = Date,y = Price,color = "SMI")) +
  geom_line(data = cac_df,aes(x = Date,y = Price,color = "CAC")) +
  geom_line(data = ftse_df,aes(x = Date,y = Price,color = "FTSE")) +
  labs(title = "Prices variations",
       x = "Year",
       y = "Price (points)") +
  scale_color_manual(values = c("DAX" = "blue",
                                "SMI" = "green",
                                "CAC" = "red",
                                "FTSE" = "purple")) +
  guides(color = guide_legend(title = NULL)) +
  theme_minimal()
print(price_plot)

# Gráfica para la tendencia de los precios:
trend_plot <- ggplot() + 
  geom_line(data = dax_df,aes(x = Date,y = Trend,color = "DAX")) +
  geom_line(data = smi_df,aes(x = Date,y = Trend,color = "SMI")) +
  geom_line(data = cac_df,aes(x = Date,y = Trend,color = "CAC")) +
  geom_line(data = ftse_df,aes(x = Date,y = Trend,color = "FTSE")) +
  labs(title = "General trends",
       x = "Year",
       y = "Trend (points)") +
  scale_color_manual(values = c("DAX" = "blue",
                                "SMI" = "green",
                                "CAC" = "red",
                                "FTSE" = "purple")) +
  guides(color = guide_legend(title = NULL)) +
  theme_minimal()
print(trend_plot)

# Gráfica para la estacionalidad de los precios:
season_plot <- ggplot() + 
  geom_line(data = dax_df,aes(x = Date,y = Seasonal,color = "DAX")) +
  geom_line(data = smi_df,aes(x = Date,y = Seasonal,color = "SMI")) +
  geom_line(data = cac_df,aes(x = Date,y = Seasonal,color = "CAC")) +
  geom_line(data = ftse_df,aes(x = Date,y = Seasonal,color = "FTSE")) +
  labs(title = "Seasonality in prices",
       x = "Year",
       y = "Seasonality (points)") +
  scale_color_manual(values = c("DAX" = "blue",
                                "SMI" = "green",
                                "CAC" = "red",
                                "FTSE" = "purple")) +
  guides(color = guide_legend(title = NULL)) +
  theme_minimal()
print(season_plot)

# Gráfica para la aleatoriedad de los precios:
random_plot <- ggplot() + 
  geom_line(data = dax_df,aes(x = Date,y = Randomness,color = "DAX")) +
  geom_line(data = smi_df,aes(x = Date,y = Randomness,color = "SMI")) +
  geom_line(data = cac_df,aes(x = Date,y = Randomness,color = "CAC")) +
  geom_line(data = ftse_df,aes(x = Date,y = Randomness,color = "FTSE")) +
  labs(title = "Randomness in prices",
       x = "Year",
       y = "Randomness (points)") +
  scale_color_manual(values = c("DAX" = "blue",
                                "SMI" = "green",
                                "CAC" = "red",
                                "FTSE" = "purple")) +
  guides(color = guide_legend(title = NULL)) +
  theme_minimal()
print(random_plot)

# Comparación de los índices:
library(gridExtra)
grid.arrange(price_plot,trend_plot,season_plot,random_plot,
             ncol = 2)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                              MODELO ARIMA
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

library(forecast)
library(lubridate)
library(foreign)
library(quantmod)
library(tseries)

# Conversión de dataframes a series temporales:
dax_ts <- ts(dax_df$Price,start <- c(1991.496,1),frequency = 365)
cac_ts <- ts(cac_df$Price,start <- c(1991.496,1),frequency = 365)
smi_ts <- ts(smi_df$Price,start <- c(1991.496,1),frequency = 365)
ftse_ts <- ts(ftse_df$Price,start <- c(1991.496,1),frequency = 365)

# ------------------------------------------------------------------------------
#                      DESCOMPOSICIÓN DE DATAFRAMES
# ------------------------------------------------------------------------------

dax_dec <- decompose(dax_ts)
dax_dec_multi <- decompose(dax_ts,type = "mult")
cac_dec <- decompose(cac_ts)
cac_dec_multi <- decompose(cac_ts,type = "mult")
smi_dec <- decompose(smi_ts)
smi_dec_multi <- decompose(smi_ts,type = "mult")
ftse_dec <- decompose(ftse_ts)
ftse_dec_multi <- decompose(ftse_ts,type = "mult")

plot(dax_dec)
plot(dax_dec_multi)

# ------------------------------------------------------------------------------
#                       BÚSQUEDA DE ESTACIONALIDAD
# ------------------------------------------------------------------------------

adf.test(dax_ts)
adf.test(cac_ts)
adf.test(smi_ts)
adf.test(ftse_ts)

# Estacionalidad aplicando logaritmos:      (NO ESTACIONAL)
dax_log <- log(dax_ts)
plot(dax_log)
adf.test(dax_log)

cac_log <- log(cac_ts)
plot(cac_log)
adf.test(cac_log)

smi_log <- log(smi_ts)
plot(smi_log)
adf.test(smi_log)

ftse_log <- log(ftse_ts)
plot(ftse_log)
adf.test(ftse_log)

# Estacionalidad aplicando diferencia:      (ESTACIONAL)
dax_diff <- diff(dax_ts)
dax_diff2 <- diff(dax_diff)
plot(dax_diff)
adf.test(dax_diff)

cac_diff <- diff(cac_ts)
plot(cac_diff)
adf.test(cac_diff)

smi_diff <- diff(smi_ts)
plot(smi_diff)
adf.test(smi_diff)

ftse_diff <- diff(ftse_ts)
plot(ftse_diff)
adf.test(ftse_diff)

# ------------------------------------------------------------------------------
#                         MODELO PARA EL DAX
# ------------------------------------------------------------------------------

# Función de autocorrelación y de autocorrelación parcial:
par(mfrow = c(2,1))
acf(dax_diff)
pacf(dax_diff)

# Modelo ARIMA:
dax_arima <- arima(dax_ts,order = c(1,1,1))
dax_arima

dax_model <- auto.arima(dax_ts,stepwise = FALSE,approximation = FALSE)
summary(dax_model)

# Pruebas de validación:
tsdiag(dax_arima)
Box.test(residuals(dax_arima),type = "Ljung-Box")
error_dax <- residuals(dax_arima)
plot(error_dax)

# Pronóstico a 1 año vista:
par(mfrow = c(2,1))
pronostico_dax <- forecast::forecast(dax_arima,h = 365)
pronostico_dax_auto <- forecast::forecast(dax_model,h = 365)
plot(pronostico_dax)
plot(pronostico_dax_auto)

# ------------------------------------------------------------------------------
#                         MODELO PARA EL CAC
# ------------------------------------------------------------------------------

# Función de autocorrelación y de autocorrelación parcial:
par(mfrow = c(2,1))
acf(cac_diff)
pacf(cac_diff)

# Modelo ARIMA:
cac_arima <- arima(dax_ts,order = c(0,1,1))
cac_arima

cac_model <- auto.arima(cac_ts,stepwise = FALSE,approximation = FALSE)
summary(cac_model)

# Pruebas de validación:
tsdiag(cac_arima)
Box.test(residuals(cac_arima),type = "Ljung-Box")
error_cac <- residuals(cac_arima)
plot(error_cac)

# Pronóstico a 1 año vista:
par(mfrow = c(2,1))
pronostico_cac <- forecast::forecast(cac_arima,h = 365)
pronostico_cac_auto <- forecast::forecast(cac_model,h = 365)
plot(pronostico_cac)
plot(pronostico_cac_auto)

# ------------------------------------------------------------------------------
#                         MODELO PARA EL SMI
# ------------------------------------------------------------------------------

# Función de autocorrelación y de autocorrelación parcial:
par(mfrow = c(2,1))
acf(smi_diff)
pacf(smi_diff)

# Modelo ARIMA:
smi_arima <- arima(smi_ts,order = c(3,1,2))
smi_arima

smi_model <- auto.arima(smi_ts,stepwise = FALSE,approximation = FALSE)
summary(smi_model)

# Pruebas de validación:
tsdiag(smi_arima)
Box.test(residuals(smi_arima),type = "Ljung-Box")
error_smi <- residuals(smi_arima)
plot(error_dax)

# Pronóstico a 1 año vista:
par(mfrow = c(2,1))
pronostico_smi <- forecast::forecast(smi_arima,h = 365)
pronostico_smi_auto <- forecast::forecast(smi_model,h = 365)
plot(pronostico_smi)
plot(pronostico_smi_auto)

# ------------------------------------------------------------------------------
#                         MODELO PARA EL FTSE
# ------------------------------------------------------------------------------

# Función de autocorrelación y de autocorrelación parcial:
par(mfrow = c(2,1))
acf(ftse_diff)
pacf(ftse_diff)

# Modelo ARIMA:
ftse_arima <- arima(ftse_ts,order = c(3,1,2))
ftse_arima

ftse_model <- auto.arima(ftse_ts,stepwise = FALSE,approximation = FALSE)
summary(ftse_model)

# Pruebas de validación:
tsdiag(ftse_arima)
Box.test(residuals(ftse_arima),type = "Ljung-Box")
error_ftse <- residuals(ftse_arima)
plot(error_ftse)

# Pronóstico a 1 año vista:
par(mfrow = c(2,1))
pronostico_ftse <- forecast::forecast(ftse_arima,h = 365)
pronostico_ftse_auto <- forecast::forecast(ftse_model,h = 365)
plot(pronostico_ftse)
plot(pronostico_ftse_auto)

# ------------------------------------------------------------------------------
#                       COMPARATIVA DE PREDICCIONES
# ------------------------------------------------------------------------------

par(mfrow = c(4,2),lwd = 2)
plot(pronostico_dax,
     main = "DAX",
     xlab = "Year",
     ylab = "Price (points)")
grid(lty = 2)
plot(pronostico_dax_auto,
     main = "DAX with drift",
     xlab = "Year",
     ylab = "Price (points)")
grid(lty = 2)
plot(pronostico_cac,
     main = "CAC",
     xlab = "Year",
     ylab = "Price (points)")
grid(lty = 2)
plot(pronostico_cac_auto,
     main = "CAC with drift",
     xlab = "Year",
     ylab = "Price (points)")
grid(lty = 2)
plot(pronostico_smi,
     main = "SMI",
     xlab = "Year",
     ylab = "Price (points)")
grid(lty = 2)
plot(pronostico_smi_auto,
     main = "SMI with drift",
     xlab = "Year",
     ylab = "Price (points)")
grid(lty = 2)
plot(pronostico_ftse,
     main = "FTSE",
     xlab = "Year",
     ylab = "Price (points)")
grid(lty = 2)
plot(pronostico_ftse_auto,
     main = "FTSE with drift",
     xlab = "Year",
     ylab = "Price (points)")
grid(lty = 2)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                              MODELO GARCH
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

library(fGarch)
library(rugarch)
library(rmgarch)
library(dynlm)
library(tseries)
library(lmtest)
library(MASS)
library(vars)

# ------------------------------------------------------------------------------
#                                   DAX
# ------------------------------------------------------------------------------

# Residuos al cuadrado del modelo ARIMA usado para este índice:
res_dax <- resid(dax_arima)^2
res_dax

# Visionado y funciones de correlación y de correlación parcial de los residuos:
par(mfrow = c(3,1),lwd = 2)
plot(res_dax,
     main = "Residuals for ARIMA model (DAX)",
     xlab = "Year",
     ylab = "Residuals")
grid(lty = 2)
acf(res_dax,
    main = "ACF for residuals (DAX)")
grid(lty = 2)
pacf(res_dax,
     main = "PACF for residuals (DAX)")
grid(lty = 2)

# Regresión con residuos al cuadrado rezagados:
dax_arch <- dynlm(res_dax ~ L(res_dax),
                  data = dax_df$Price)
summary(dax_arch)

# Seleccionar las columnas "Date" y "Price" del dataframe dax_df:
selected_cols <- dax_df[,c("Date","Price")]
selected_dax_df <- data.frame(Date = selected_cols$Date,
                              Price = selected_cols$Price)

# Arch test:
var_model_dax <- VAR(selected_dax_df,p = 1)  
dax_arch_test_result <- arch.test(var_model_dax)
print(dax_arch_test_result)

# Modelo GARCH:
ugspec_dax <- ugarchspec(mean.model = list(armaOrder = c(2,1)))
ugfit_dax <- ugarchfit(spec = ugspec_dax,
                        data = dax_ts)
ugfit_dax
ugfit_dax@fit$coef

ugfit_dax_var <- ugfit_dax@fit$var
ugfit_dax_res <- (ugfit_dax@fit$residuals)^2
plot(ugfit_dax_res,type = "l")
lines(ugfit_dax_var,col = "green")

# Pronóstico a 120 días vista:
pronostico_dax120 <- forecast::forecast(dax_model,h = 120)
ugfore_dax <- ugarchforecast(ugfit_dax,n.ahead = 120)

# Predicción y volatilidad de la predicción:
par(mfrow = c(2,1),lwd = 2)
plot(pronostico_dax120,
     main = "DAX (ARIMA model)",
     xlab = "Year",
     ylab = "Price (points)",
     xlim = c(1996,1996.95),
     ylim = c(4000,7000))
grid(lty = 2)
plot(ugfore_dax)

# ------------------------------------------------------------------------------
#                                   CAC
# ------------------------------------------------------------------------------

# Residuos al cuadrado del modelo ARIMA usado para este índice:
res_cac <- resid(cac_arima)^2
res_cac

# Visionado y funciones de correlación y de correlación parcial de los residuos:
par(mfrow = c(3,1),lwd = 2)
plot(res_cac,
     main = "Residuals for ARIMA model (CAC)",
     xlab = "Year",
     ylab = "Residuals")
grid(lty = 2)
acf(res_cac,
    main = "ACF for residuals (CAC)")
grid(lty = 2)
pacf(res_cac,
     main = "PACF for residuals (CAC)")
grid(lty = 2)

# Regresión con residuos al cuadrado rezagados:
cac_arch <- dynlm(res_cac ~ L(res_cac),
                  data = cac_df$Price)
summary(cac_arch)

# Seleccionar las columnas "Date" y "Price" del dataframe dax_df:
selected_cols_cac <- cac_df[,c("Date","Price")]
selected_cac_df <- data.frame(Date = selected_cols_cac$Date,
                              Price = selected_cols_cac$Price)

# Arch test:
var_model_cac <- VAR(selected_cac_df,p = 1)  
cac_arch_test_result <- arch.test(var_model_cac)
print(cac_arch_test_result)

# Modelo GARCH:
ugspec_cac <- ugarchspec(mean.model = list(armaOrder = c(2,0)))
ugfit_cac <- ugarchfit(spec = ugspec_cac,
                        data = cac_ts)
ugfit_cac
ugfit_cac@fit$coef

ugfit_cac_var <- ugfit_cac@fit$var
ugfit_cac_res <- (ugfit_cac@fit$residuals)^2
plot(ugfit_cac_res,type = "l")
lines(ugfit_cac_var,col = "green")

# Pronóstico a 120 días vista:
pronostico_cac120 <- forecast::forecast(cac_model,h = 120)
ugfore_cac <- ugarchforecast(ugfit_cac,n.ahead = 120)

# Predicción y volatilidad de la predicción:
par(mfrow = c(2,1),lwd = 2)
plot(pronostico_cac120,
     main = "CAC (ARIMA model)",
     xlab = "Year",
     ylab = "Price (points)",
     xlim = c(1996,1996.84),
     ylim = c(4000,6500))
grid(lty = 2)
plot(ugfore_cac)

# ------------------------------------------------------------------------------
#                                   SMI
# ------------------------------------------------------------------------------

# Residuos al cuadrado del modelo ARIMA usado para este índice:
res_smi <- resid(smi_arima)^2
res_smi

# Visionado y funciones de correlación y de correlación parcial de los residuos:
par(mfrow = c(3,1),lwd = 2)
plot(res_smi,
     main = "Residuals for ARIMA model (SMI)",
     xlab = "Year",
     ylab = "Residuals")
grid(lty = 2)
acf(res_smi,
    main = "ACF for residuals (SMI)")
grid(lty = 2)
pacf(res_smi,
     main = "PACF for residuals (SMI)")
grid(lty = 2)

# Regresión con residuos al cuadrado rezagados:
smi_arch <- dynlm(res_dax ~ L(res_smi),
                  data = smi_df$Price)
summary(smi_arch)

# Seleccionar las columnas "Date" y "Price" del dataframe dax_df:
selected_cols_smi <- smi_df[,c("Date","Price")]
selected_smi_df <- data.frame(Date = selected_cols_smi$Date,
                              Price = selected_cols_smi$Price)

# Arch test:
var_model_smi <- VAR(selected_smi_df,p = 1)  
smi_arch_test_result <- arch.test(var_model_smi)
print(smi_arch_test_result)

# Modelo GARCH:
ugspec_smi <- ugarchspec(mean.model = list(armaOrder = c(2,0)))
ugfit_smi <- ugarchfit(spec = ugspec_smi,
                        data = smi_ts)
ugfit_smi
ugfit_smi@fit$coef

ugfit_smi_var <- ugfit_smi@fit$var
ugfit_smi_res <- (ugfit_smi@fit$residuals)^2
plot(ugfit_smi_res,type = "l")
lines(ugfit_smi_var,col = "green")

# Pronóstico a 120 días vista:
pronostico_smi120 <- forecast::forecast(smi_model,h = 120)
ugfore_smi <- ugarchforecast(ugfit_smi,n.ahead = 120)

# Predicción y volatilidad de la predicción:
par(mfrow = c(2,1),lwd = 2)
plot(pronostico_smi120,
     main = "SMI (ARIMA model)",
     xlab = "Year",
     ylab = "Price (points)",
     xlim = c(1996,1996.95),
     ylim = c(5000,8500))
grid(lty = 2)
plot(ugfore_smi,
     main = "FTSE (GARCH model)")

# ------------------------------------------------------------------------------
#                                   FTSE
# ------------------------------------------------------------------------------

# Residuos al cuadrado del modelo ARIMA usado para este índice:
res_ftse <- resid(ftse_model)^2

# Visionado y funciones de correlación y de correlación parcial de los residuos:
par(mfrow = c(3,1),lwd = 2)
plot(res_ftse,
     main = "Residuals for ARIMA model (FTSE)",
     xlab = "Year",
     ylab = "Residuals")
grid(lty = 2)
acf(res_ftse,
    main = "ACF for residuals (FTSE)")
grid(lty = 2)
pacf(res_ftse,
     main = "PACF for residuals (FTSE)")
grid(lty = 2)
par(mfrow = c(1,1),lwd = 2)

# Regresión con residuos al cuadrado rezagados:
ftse_arch <- dynlm(res_ftse ~ L(res_ftse),
                  data = ftse_df$Price)
summary(ftse_arch)

# Seleccionar las columnas "Date" y "Price" del dataframe dax_df:
selected_cols_ftse <- ftse_df[,c("Date","Price")]
selected_ftse_df <- data.frame(Date = selected_cols_ftse$Date,
                               Price = selected_cols_ftse$Price)

# Arch test:
var_model_ftse <- VAR(selected_ftse_df,p = 1)  
ftse_arch_test_result <- arch.test(var_model_ftse)
print(ftse_arch_test_result)

# Modelo GARCH:
ugspec_ftse <- ugarchspec(mean.model = list(armaOrder = c(5,0)))
ugfit_ftse <- ugarchfit(spec = ugspec_ftse,
                          data = ftse_ts)
ugfit_ftse@fit$coef

ugfit_ftse_var <- ugfit_ftse@fit$var
ugfit_ftse_res <- (ugfit_ftse@fit$residuals)^2
plot(ugfit_ftse_res,type = "l")
lines(ugfit_ftse_var,col = "green")

# Pronóstico a 120 días vista:
pronostico_ftse_auto120 <- forecast::forecast(ftse_model,h = 120)
ugfore_ftse <- ugarchforecast(ugfit_ftse,n.ahead = 120)

# Predicción y volatilidad de la predicción:
par(mfrow = c(2,1),lwd = 2)
plot(pronostico_ftse_auto120,
     main = "FTSE (ARIMA model)",
     xlab = "Year",
     ylab = "Price (points)",
     xlim = c(1996,1996.84),
     ylim = c(4000,6500))
grid(lty = 2)
plot(ugfore_ftse50,
     main = "FTSE (GARCH model)")

# ------------------------------------------------------------------------------
#                        COMPARACIÓN DE PRONÓSTICOS
# ------------------------------------------------------------------------------


par(mfrow = c(4,2),lwd = 2)

plot(pronostico_dax120,
     main = "DAX (ARIMA model)",
     xlab = "Year",
     ylab = "Price (points)",
     xlim = c(1996,1996.95),
     ylim = c(4000,7000))
grid(lty = 2)
plot(ugfore_dax)

plot(pronostico_cac120,
     main = "CAC (ARIMA model)",
     xlab = "Year",
     ylab = "Price (points)",
     xlim = c(1996,1996.95),
     ylim = c(2500,6000))
grid(lty = 2)
plot(ugfore_cac)

plot(pronostico_smi120,
     main = "SMI (ARIMA model)",
     xlab = "Year",
     ylab = "Price (points)",
     xlim = c(1996,1996.95),
     ylim = c(5000,8500))
grid(lty = 2)
plot(ugfore_smi,
     main = "FTSE (GARCH model)")

plot(pronostico_ftse_auto120,
     main = "FTSE (ARIMA model)",
     xlab = "Year",
     ylab = "Price (points)",
     xlim = c(1996,1996.95),
     ylim = c(4000,6500))
grid(lty = 2)
plot(ugfore_ftse50,
     main = "FTSE (GARCH model)")



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                              MODELO SARIMA
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

best_sarima <- auto.arima(dax_diff,seasonal = TRUE)

# Mostrar los resultados del mejor modelo
summary(best_sarima)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                             MODELO XGBOOST
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

library(xgboost)
library(dplyr)
library(Metrics)

# ------------------------------------------------------------------------------
#                           DATAFRAMES REQUERIDOS
# ------------------------------------------------------------------------------

# Lista para almacenar los datos de entrenamiento y prueba para cada índice:
index_names <- c("dax","cac","smi","ftse")

train_data_list <- list()
test_data_list <- list()
train_matrix_list <- list()
train_labels_list <- list()
test_matrix_list <- list()
test_labels_list <- list()
train_df_list <- list()

# Definir el número de lags:
num_lags <- 3

# Creación de un bucle para procesar los datos de cada índice bursátil:
for (index_name in index_names) {
  
  # Seleccionar el dataframe original correspondiente al índice:
  index_df <- get(paste0(index_name,"_df"))
  
  # Crear un dataframe con las columnas Date y Price:
  index_df_subset <- index_df[c("Date","Price")]
  
  # Crear las variables de retraso (lags) en el dataframe:
  for (lag in 1:num_lags) {
    index_df_subset[paste0("Price_lag_",lag)] <- lag(index_df_subset$Price,lag)
  }
  index_df_subset <- index_df_subset[complete.cases(index_df_subset),]
  
  # Conjuntos de train y test:
  train_data <- index_df_subset[1:1500,]
  test_data <- index_df_subset[1501:1860,]
  
  # Matrices y etiquetas de train y test:
  train_matrix <- as.matrix(train_data[,-(1:3)])
  train_labels <- train_data$Price
  test_matrix <- as.matrix(test_data[,-(1:3)])
  test_labels <- test_data$Price
  
  # Dataframe necesario para el modelo:
  train_df <- index_df_subset[1:(nrow(index_df_subset)-30),]
  
  # Almacenamiento de los datos:
  train_data_list[[index_name]] <- train_data
  test_data_list[[index_name]] <- test_data
  train_matrix_list[[index_name]] <- train_matrix
  train_labels_list[[index_name]] <- train_labels
  test_matrix_list[[index_name]] <- test_matrix
  test_labels_list[[index_name]] <- test_labels
  train_df_list[[index_name]] <- train_df
}

# Creación de un bucle para procesar los datos de cada índice bursátil:
for (index_name in index_names) {
  
  # Seleccionar el dataframe original correspondiente al índice:
  index_df <- get(paste0(index_name,"_df"))
  
  # Crear un dataframe con las columnas Date y Price:
  assign(paste0(index_name,"_df_subset"),
         index_df[c("Date","Price")])
  
  # Crear las variables de retraso (lags) en el dataframe:
  for (lag in 1:num_lags) {
    assign(paste0(index_name,"_df_subset$Price_lag_",lag),lag(index_df$Price, lag))
  }
  
  # Eliminar filas con NA
  assign(paste0(index_name,"_df_subset"),
         index_df_subset[complete.cases(index_df_subset),])
  
  # Conjuntos de train y test:
  assign(paste0("train_data_",index_name),index_df_subset[1:1500,])
  assign(paste0("test_data_",index_name),index_df_subset[1501:1860,])
  
  # Matrices y etiquetas de train y test:
  assign(paste0("train_matrix_",index_name),
         as.matrix(get(paste0("train_data_",index_name))[,-(1:3)]))
  assign(paste0("train_labels_",index_name),
         get(paste0("train_data_",index_name))$Price)
  assign(paste0("test_matrix_",index_name),
         as.matrix(get(paste0("test_data_",index_name))[,-(1:3)]))
  assign(paste0("test_labels_",index_name),
         get(paste0("test_data_",index_name))$Price)
  
  # Dataframe necesario para el modelo:
  assign(paste0(index_name,"_train_df"),
         get(paste0("train_data_",index_name))[1:(nrow(get(paste0("train_data_",index_name)))-30),])
}

# ------------------------------------------------------------------------------
#                        MODELO PARA EL DAX
# ------------------------------------------------------------------------------

# Definir la matriz de características y el vector de etiquetas
X_dax <- as.matrix(dax_df_subset[,-(1:3)])  
y_dax <- dax_df_subset$Price

# Define la lista de hiperparámetros para probar
param_grid <- list(
  max_depth = c(7,8,9,10),
  eta = c(0.80,0.82,0.85,0.88,0.90),
  nrounds = c(500,600,700)
)

best_metrics_dax <- NULL

# Bucle a través de las combinaciones de hiperparámetros
for (max_depth in param_grid$max_depth) {
 for (eta in param_grid$eta) {
    for (nrounds in param_grid$nrounds) {
      # Define los parámetros del modelo:
      params <- list(
        max_depth = max_depth,
        eta = eta,
        objective = "reg:squarederror"
      )
      
      # Entrena el modelo XGBoost:
      set.seed(123)
      xgb_dax <- xgboost(data = X_dax,
                           label = y_dax,
                           nrounds = nrounds,
                           max_depth = max_depth,
                           eta = eta,
                           objective = "reg:squarederror",
                           eval_metric = "rmse")
      
      # Realiza predicciones en el conjunto de entrenamiento:
      train_predictions <- predict(xgb_dax,X_dax)
      
      # Calcula las métricas
      mae_dax <- mean(abs(train_predictions-y_dax))
      mse_dax <- mean((train_predictions-y_dax)^2)
      rmse_dax <- sqrt(mse_dax)
      
      # Guarda los resultados
      result_dax <- data.frame(max_depth,
                               eta,
                               nrounds,
                               mae_dax,
                               mse_dax,
                               rmse_dax)
      best_metrics_dax <- rbind(best_metrics_dax,result_dax)
    }
  }
}

# Imprimir los resultados de métricas para diferentes combinaciones de hiperparámetros
#print(best_metrics)

# Encuentra la fila con el menor RMSE y el menor número de iteraciones:
#best_row_dax <- best_metrics[which.min(best_metrics$rmse_dax) && 
#                             which.min(best_metrics$nrounds),]

# Imprime la fila con el menor RMSE
#print(best_row)

# Entrenar el modelo XGBoost con los mejores parámetros
#xgb_dax <- xgboost(data = X_dax,
#                   label = y_dax,
#                   nrounds = best_row_dax$nrounds,
#                   eta = best_row_dax$eta,
#                   max_depth = best_row_dax$max_depth,
#                   objective = "reg:squarederror",
#                   eval_metric = "rmse")

set.seed(123)
xgb_dax <- xgboost(data = X_dax,
                   label = y_dax,
                   nrounds = 500,
                   eta = 0.85,
                   max_depth = 8,
                   objective = "reg:squarederror",
                   eval_metric = "rmse")

# Definir la matriz de características para pronóstico en el conjunto de prueba
X_test_dax <- as.matrix(test_data_dax[,-(1:3)])  

# Realizar pronósticos en el conjunto de prueba con el modelo XGBoost
predictions_dax <- predict(xgb_dax,newdata = X_test_dax)
predictions_dax
plot(predictions_dax)

# Visualizar los resultados
plot(test_data_dax$Date,
     test_data_dax$Price,
     type = "l",
     col = "blue",
     lwd = 3,
     main = "DAX",
     xlab = "Año",
     ylab = "Precio (puntos)")
grid(lty = 2)
lines(test_data_dax$Date,
      predictions_dax,
      col = "cyan",
      lty = 3)
legend("topleft",
       legend = c("Real","Pronóstico"),
       col = c("blue","cyan"),
       lty = 1)

# ------------------------------------------------------------------------------
#                        PREDICCIÓN A UN MES VISTA


# Crear una matriz para almacenar las predicciones futuras:
num_periods <- 30
pred_days <- 30
future_predictions_dax <- matrix(NA,
                                 nrow = pred_days,
                                 ncol = num_periods)


# Obtener las características necesarias para la predicción inicial
initial_data <- dax_df_subset[(nrow(dax_df_subset)-num_lags+1):nrow(dax_df_subset),-(1:3)]

# Realizar predicciones iterativas a futuro
for (i in 1:num_periods) {
  # Convertir los datos de entrada en una matriz numérica
  x_input <- as.matrix(initial_data)
  
  # Realizar la predicción con el modelo xgb_ftse
  prediction <- predict(xgb_dax,newdata = x_input)
  
  # Agregar la predicción al registro de predicciones futuras
  future_predictions_dax[i,] <- prediction
  
  # Actualizar las características para el siguiente período
  initial_data <- rbind(initial_data[-1,],
                        c(prediction,rep(NA,num_lags-1)))
}

# Crear un vector con las fechas futuras
future_dates_dax <- seq(from = max(dax_df_subset$Date)+(1/365),
                        to = max(dax_df_subset$Date)+(30/365),
                        length.out = num_periods)

# Crear un dataframe con las predicciones futuras y las fechas correspondientes
future_predictions_df_dax <- data.frame(Date = future_dates_dax)
future_predictions_df_dax$Prediction <- future_predictions_dax

# Promediado de los valores obtenidos para los distintos días:
average_by_day_dax <- rowMeans(future_predictions_dax)

# Crear un dataframe con las fechas futuras y las predicciones promedio por día
average_dax_df <- data.frame(Date = future_dates_dax,
                              Prediction = average_by_day_dax)

# Imprimir el dataframe de predicciones promedio por día
print(average_dax_df)

# Plot de las predicciones promedio por día
plot(train_data_dax$Date,
     train_data_dax$Price,
     xlim = c(1,6.2),
     ylim = c(1000,6200),
     type = "l",
     col = "blue",
     xlab = "Año",
     ylab = "Precio (puntos)",
     main = "Evolución del precio de DAX")
grid(lty = 2)
lines(test_data_dax$Date,
      test_data_dax$Price,
      col = "red")
lines(average_dax_df$Date,
      average_dax_df$Prediction,
      col = "purple",
      lwd = 2)
legend("topleft",
       legend = c("Real - conjunto de entrenamiento",
                  "Real - conjunto de test",
                  "Predicción a 30 días"),
       col = c("blue","red","purple"),
       lty = 1)


# ------------------------------------------------------------------------------
#                        MODELO PARA EL CAC
# ------------------------------------------------------------------------------

# Definir la matriz de características y el vector de etiquetas
X_cac <- as.matrix(cac_df_subset[,-(1:3)])  
y_cac <- cac_df_subset$Price

# Define la lista de hiperparámetros para probar
param_grid <- list(
  max_depth = c(9,10,11),
  eta = c(0.78,0.80,0.82,0.84),
  nrounds = c(345,350,355)
)

best_metrics_cac <- NULL

# Bucle a través de las combinaciones de hiperparámetros
#for (max_depth in param_grid$max_depth) {
#  for (eta in param_grid$eta) {
    for (nrounds in param_grid$nrounds) {
      # Define los parámetros del modelo:
      params <- list(
        max_depth = max_depth,
        eta = eta,
        objective = "reg:squarederror"
      )
      
      # Entrena el modelo XGBoost:
      set.seed(123)
      xgb_model <- xgboost(data = X_cac,
                           label = y_cac,
                           nrounds = nrounds,
                           max_depth = max_depth,
                           eta = eta,
                           objective = "reg:squarederror",
                           eval_metric = "rmse")
      
      # Realiza predicciones en el conjunto de entrenamiento:
      train_predictions_cac <- predict(xgb_model,X_cac)
      
      # Calcula las métricas
      mae_cac <- mean(abs(train_predictions_cac-y_cac))
      mse_cac <- mean((train_predictions_cac-y_cac)^2)
      rmse_cac <- sqrt(mse_cac)
      
      # Guarda los resultados
      result_cac <- data.frame(max_depth,
                                eta,
                                nrounds,
                                mae_cac,
                                mse_cac,
                                rmse_cac)
      best_metrics_cac <- rbind(best_metrics_cac,result_cac)
    }
  #}
#}

# Imprimir los resultados de métricas para diferentes combinaciones de hiperparámetros
#print(best_metrics_cac)

# Encuentra la fila con el menor RMSE
#best_row_cac <- best_metrics_cac[which.min(best_metrics_cac$rmse_cac),]

# Imprime la fila con el menor RMSE
#print(best_row_cac)

# Entrenar el modelo XGBoost con los mejores parámetros
#xgb_cac <- xgboost(data = X_cac,
#                   label = y_cac,
#                   nrounds = best_row_cac$nrounds,
#                   eta = best_row_cac$eta,
#                   max_depth = best_row_cac$max_depth,
#                   objective = "reg:squarederror",
#                   eval_metric = "rmse")

set.seed(123)
xgb_cac <- xgboost(data = X_cac,
                   label = y_cac,
                   nrounds = 345,
                   eta = 0.8,
                   max_depth = 10,
                   objective = "reg:squarederror",
                   eval_metric = "rmse")

# Definir la matriz de características para pronóstico en el conjunto de prueba
X_test_cac <- as.matrix(test_data_cac[,-(1:3)])  

# Realizar pronósticos en el conjunto de prueba con el modelo XGBoost
predictions_cac <- predict(xgb_cac,newdata = X_test_cac)

# Visualizar los resultados
plot(test_data_cac$Date,
     test_data_cac$Price,
     type = "l",
     col = "blue",
     lwd = 3,
     main = "Predicciones para CAC",
     xlab = "Año",
     ylab = "Precio (puntos)",
     ylim = c(min(test_data_cac$Price,predictions_cac),
              max(test_data_cac$Price,predictions_cac)))
grid(lty = 2)
lines(test_data_cac$Date,
      predictions_cac,
      col = "cyan",
      lty = 3)
legend("topleft",
       legend = c("Real","Pronóstico"),
       col = c("blue","cyan"),
       lty = 1)

# ------------------------------------------------------------------------------
#                        PREDICCIÓN A UN MES VISTA


# Crear una matriz para almacenar las predicciones futuras
future_predictions_cac <- matrix(NA,
                                 nrow = pred_days,
                                 ncol = num_periods)


# Obtener las características necesarias para la predicción inicial
initial_data <- cac_df_subset[(nrow(cac_df_subset)-num_lags+1):nrow(cac_df_subset),-(1:3)]

# Realizar predicciones iterativas a futuro
for (i in 1:num_periods) {
  # Convertir los datos de entrada en una matriz numérica
  x_input <- as.matrix(initial_data)
  
  # Realizar la predicción con el modelo xgb_ftse
  prediction <- predict(xgb_cac,newdata = x_input)
  
  # Agregar la predicción al registro de predicciones futuras
  future_predictions_cac[i,] <- prediction
  
  # Actualizar las características para el siguiente período
  initial_data <- rbind(initial_data[-1,],
                        c(prediction,rep(NA,num_lags-1)))
}

# Crear un vector con las fechas futuras
future_dates_cac <- seq(from = max(cac_df_subset$Date)+(1/365),
                        to = max(cac_df_subset$Date)+(30/365),
                        length.out = num_periods)

# Crear un dataframe con las predicciones futuras y las fechas correspondientes
future_predictions_df_cac <- data.frame(Date = future_dates_cac)
future_predictions_df_cac$Prediction <- future_predictions_cac

# Promediado de los valores obtenidos para los distintos días:
average_by_day_cac <- rowMeans(future_predictions_cac)

# Crear un dataframe con las fechas futuras y las predicciones promedio por día
average_cac_df <- data.frame(Date = future_dates_cac,
                             Prediction = average_by_day_cac)

# Imprimir el dataframe de predicciones promedio por día
print(average_cac_df)

# Plot de las predicciones promedio por día
plot(train_data_cac$Date,
     train_data_cac$Price,
     xlim = c(1,6.2),
     ylim = c(1000,5000),
     type = "l",
     col = "blue",
     xlab = "Año",
     ylab = "Precio (puntos)",
     main = "Evolución del precio de CAC")
grid(lty = 2)
lines(test_data_cac$Date,
      test_data_cac$Price,
      col = "red")
lines(average_cac_df$Date,
      average_cac_df$Prediction,
      col = "purple",
      lwd = 2)
legend("topleft",
       legend = c("Real - conjunto de entrenamiento",
                  "Real - conjunto de test",
                  "Predicción a 30 días"),
       col = c("blue","red","purple"),
       lty = 1)


# ------------------------------------------------------------------------------
#                        MODELO PARA EL SMI
# ------------------------------------------------------------------------------

# Definir la matriz de características y el vector de etiquetas
X_smi <- as.matrix(smi_df_subset[,-(1:3)])  
y_smi <- smi_df_subset$Price

# Define la lista de hiperparámetros para probar
param_grid <- list(
  max_depth = c(7,8,9),
  eta = c(0.75,0.80,0.85),
  nrounds = c(550,555,560)
)

best_metrics_smi <- NULL

# Bucle a través de las combinaciones de hiperparámetros
#for (max_depth in param_grid$max_depth) {
#  for (eta in param_grid$eta) {
    for (nrounds in param_grid$nrounds) {
      # Define los parámetros del modelo:
      params <- list(
        max_depth = max_depth,
        eta = eta,
        objective = "reg:squarederror"
      )
      
      # Entrena el modelo XGBoost:
      set.seed(123)
      xgb_model <- xgboost(data = X_smi,
                           label = y_smi,
                           nrounds = nrounds,
                           max_depth = max_depth,
                           eta = eta,
                           objective = "reg:squarederror",
                           eval_metric = "rmse")
      
      # Realiza predicciones en el conjunto de entrenamiento:
      train_predictions <- predict(xgb_model,X_smi)
      
      # Calcula las métricas
      mae_smi <- mean(abs(train_predictions-y_smi))
      mse_smi <- mean((train_predictions-y_smi)^2)
      rmse_smi <- sqrt(mse_smi)
      
      # Guarda los resultados
      result_smi <- data.frame(max_depth,
                               eta,
                               nrounds,
                               mae_smi,
                               mse_smi,
                               rmse_smi)
      best_metrics_smi <- rbind(best_metrics_smi,result_smi)
    }
  #}
#}

# Imprimir los resultados de métricas para diferentes combinaciones de hiperparámetros
#print(best_metrics_smi)

# Encuentra la fila con el menor RMSE
#best_row_smi <- best_metrics_smi[which.min(best_metrics_smi$rmse_smi),]

# Imprime la fila con el menor RMSE
#print(best_row_smi)

# Entrenar el modelo XGBoost con los mejores parámetros
#xgb_smi <- xgboost(data = X_smi,
#                   label = y_smi,
#                   nrounds = best_row_smi$nrounds,
#                   eta = best_row_smi$eta,
#                   max_depth = best_row_smi$max_depth,
#                   objective = "reg:squarederror",
#                   eval_metric = "rmse")

set.seed(123)
xgb_smi <- xgboost(data = X_smi,
                   label = y_smi,
                   nrounds = 555,
                   eta = 0.8,
                   max_depth = 8,
                   objective = "reg:squarederror",
                   eval_metric = "rmse")

# Definir la matriz de características para pronóstico en el conjunto de prueba
X_test_smi <- as.matrix(test_data_smi[,-(1:3)])  

# Realizar pronósticos en el conjunto de prueba con el modelo XGBoost
predictions_smi <- predict(xgb_smi,newdata = X_test_smi)

# Visualizar los resultados
plot(test_data_smi$Date,
     test_data_smi$Price,
     type = "l",
     col = "blue",
     lwd = 3,
     main = "Predicción para SMI",
     xlab = "Año",
     ylab = "Precio (puntos)",
     ylim = c(min(test_data_smi$Price,predictions_smi),
              max(test_data_smi$Price,predictions_smi)))
grid(lty = 2)
lines(test_data_smi$Date,
      predictions_smi,
      col = "cyan",
      lty = 3)
legend("topleft",
       legend = c("Real","Pronóstico"),
       col = c("blue","cyan"),
       lty = 1)

# ------------------------------------------------------------------------------
#                        PREDICCIÓN A UN MES VISTA


# Crear una matriz para almacenar las predicciones futuras
future_predictions_smi <- matrix(NA,
                                 nrow = pred_days,
                                 ncol = num_periods)


# Obtener las características necesarias para la predicción inicial
initial_data <- smi_df_subset[(nrow(smi_df_subset)-num_lags+1):nrow(smi_df_subset),-(1:3)]

# Realizar predicciones iterativas a futuro
for (i in 1:num_periods) {
  # Convertir los datos de entrada en una matriz numérica
  x_input <- as.matrix(initial_data)
  
  # Realizar la predicción con el modelo xgb_ftse
  prediction <- predict(xgb_smi,newdata = x_input)
  
  # Agregar la predicción al registro de predicciones futuras
  future_predictions_smi[i,] <- prediction
  
  # Actualizar las características para el siguiente período
  initial_data <- rbind(initial_data[-1,],
                        c(prediction,rep(NA,num_lags-1)))
}

# Crear un vector con las fechas futuras
future_dates_smi <- seq(from = max(smi_df_subset$Date)+(1/365),
                        to = max(smi_df_subset$Date)+(30/365),
                        length.out = num_periods)

# Crear un dataframe con las predicciones futuras y las fechas correspondientes
future_predictions_df_smi <- data.frame(Date = future_dates_smi)
future_predictions_df_smi$Prediction <- future_predictions_smi

# Promediado de los valores obtenidos para los distintos días:
average_by_day_smi <- rowMeans(future_predictions_smi)

# Crear un dataframe con las fechas futuras y las predicciones promedio por día
average_smi_df <- data.frame(Date = future_dates_smi,
                             Prediction = average_by_day_smi)

# Imprimir el dataframe de predicciones promedio por día
print(average_smi_df)

# Plot de las predicciones promedio por día
plot(train_data_smi$Date,
     train_data_smi$Price,
     xlim = c(1,6.2),
     ylim = c(1000,8200),
     type = "l",
     col = "blue",
     xlab = "Año",
     ylab = "Precio (puntos)",
     main = "Evolución del precio de SMI")
grid(lty = 2)
lines(test_data_smi$Date,
      test_data_smi$Price,
      col = "red")
lines(average_smi_df$Date,
      average_smi_df$Prediction,
      col = "purple",
      lwd = 2)
legend("topleft",
       legend = c("Real - conjunto de entrenamiento",
                  "Real - conjunto de test",
                  "Predicción a 30 días"),
       col = c("blue","red","purple"),
       lty = 1)


# ------------------------------------------------------------------------------
#                        MODELO PARA EL FTSE
# ------------------------------------------------------------------------------

# Definir la matriz de características y el vector de etiquetas
X_ftse <- as.matrix(ftse_df_subset[,-(1:3)])  
y_ftse <- ftse_df_subset$Price

# Define la lista de hiperparámetros para probar
param_grid <- list(
  max_depth = c(10,11,12),
  eta = c(0.65,0.68,0.7),
  nrounds = c(375,400,425)
)

best_metrics_ftse <- NULL

# Bucle a través de las combinaciones de hiperparámetros
#for (max_depth in param_grid$max_depth) {
  for (eta in param_grid$eta) {
    for (nrounds in param_grid$nrounds) {
      # Define los parámetros del modelo:
      params <- list(
        max_depth = max_depth,
        eta = eta,
        objective = "reg:squarederror"
      )
      
      # Entrena el modelo XGBoost:
      set.seed(123)
      xgb_model <- xgboost(data = X_ftse,
                           label = y_ftse,
                           nrounds = nrounds,
                           max_depth = max_depth,
                           eta = eta,
                           objective = "reg:squarederror",
                           eval_metric = "rmse")
      
      # Realiza predicciones en el conjunto de entrenamiento:
      train_predictions <- predict(xgb_model,X_ftse)
      
      # Calcula las métricas
      mae_ftse <- mean(abs(train_predictions-y_ftse))
      mse_ftse <- mean((train_predictions-y_ftse)^2)
      rmse_ftse <- sqrt(mse_ftse)
      
      # Guarda los resultados
      result_ftse <- data.frame(max_depth,
                                eta,
                                nrounds,
                                mae_ftse,
                                mse_ftse,
                                rmse_ftse)
      best_metrics_ftse <- rbind(best_metrics_ftse,result_ftse)
    }
  }
#}

# Imprimir los resultados de métricas para diferentes combinaciones de hiperparámetros
#print(best_metrics_ftse)

# Encuentra la fila con el menor RMSE
#best_row_ftse <- best_metrics_ftse[which.min(best_metrics_ftse$rmse_ftse),]

# Imprime la fila con el menor RMSE
#print(best_row_ftse)

# Entrenar el modelo XGBoost con los mejores parámetros
#set.seed(123)
#xgb_ftse <- xgboost(data = X_ftse,
 #                   label = y_ftse,
#                    nrounds = best_row_ftse$nrounds,
#                    eta = best_row_ftse$eta,
 #                   max_depth = best_row_ftse$max_depth,
  #                  objective = "reg:squarederror",
   #                 eval_metric = "rmse")

set.seed(123)
xgb_ftse <- xgboost(data = X_ftse,
                    label = y_ftse,
                    nrounds = 400,
                    eta = 0.68,
                    max_depth = 11,
                    objective = "reg:squarederror",
                    eval_metric = "rmse")

# Definir la matriz de características para pronóstico en el conjunto de prueba
X_test_ftse <- as.matrix(test_data_ftse[,-(1:3)])  

# Realizar pronósticos en el conjunto de prueba con el modelo XGBoost
predictions_ftse <- predict(xgb_ftse,newdata = X_test_ftse)

# Visualizar los resultados
plot(test_data_ftse$Date,
     test_data_ftse$Price,
     type = "l",
     col = "blue",
     lwd = 3,
     main = "FTSE",
     xlab = "Año",
     ylab = "Precio (puntos)",
     ylim = c(min(test_data_ftse$Price,predictions_ftse),
              max(test_data_ftse$Price,predictions_ftse)))
grid(lty = 2)
lines(test_data_ftse$Date,
      predictions_ftse,
      col = "cyan",
      lty = 3)
legend("topleft",
       legend = c("Real","Pronóstico"),
       col = c("blue","cyan"),
       lty = 1)

# ------------------------------------------------------------------------------
#                        PREDICCIÓN A UN MES VISTA


# Crear una matriz para almacenar las predicciones futuras
future_predictions_ftse <- matrix(NA,
                                  nrow = pred_days,
                                  ncol = num_periods)


# Obtener las características necesarias para la predicción inicial
initial_data <- ftse_df_subset[(nrow(ftse_df_subset)-num_lags+1):nrow(ftse_df_subset),-(1:3)]

# Realizar predicciones iterativas a futuro
for (i in 1:num_periods) {
  # Convertir los datos de entrada en una matriz numérica
  x_input <- as.matrix(initial_data)
  
  # Realizar la predicción con el modelo xgb_ftse
  prediction <- predict(xgb_ftse, newdata = x_input)
  
  # Agregar la predicción al registro de predicciones futuras
  future_predictions_ftse[i, ] <- prediction
  
  # Actualizar las características para el siguiente período
  initial_data <- rbind(initial_data[-1,],
                        c(prediction,rep(NA,num_lags-1)))
}

# Crear un vector con las fechas futuras
future_dates_ftse <- seq(from = max(ftse_df_subset$Date)+(1/365),
                         to = max(ftse_df_subset$Date)+(30/365),
                         length.out = num_periods)

# Crear un dataframe con las predicciones futuras y las fechas correspondientes
future_predictions_df_ftse <- data.frame(Date = future_dates_ftse)
future_predictions_df_ftse$Prediction <- future_predictions_ftse

# Promediado de los valores obtenidos para los distintos días:
average_by_day_ftse <- rowMeans(future_predictions_ftse)

# Crear un dataframe con las fechas futuras y las predicciones promedio por día
average_ftse_df <- data.frame(Date = future_dates_ftse,
                              Prediction = average_by_day_ftse)

# Imprimir el dataframe de predicciones promedio por día
print(average_ftse_df)

# Plot de las predicciones promedio por día
plot(train_data_ftse$Date,
     train_data_ftse$Price,
     xlim = c(1,6.2),
     ylim = c(2000,6200),
     type = "l",
     col = "blue",
     xlab = "Año",
     ylab = "Precio (puntos)",
     main = "Evolución del precio de FTSE")
grid(lty = 2)
lines(test_data_ftse$Date,
      test_data_ftse$Price,
      col = "red")
lines(average_ftse_df$Date,
      average_ftse_df$Prediction,
      col = "purple",
      lwd = 2)
legend("topleft",
       legend = c("Real - conjunto de entrenamiento",
                  "Real - conjunto de test",
                  "Predicción a 30 días"),
       col = c("blue","red","purple"),
       lty = 1)

# ------------------------------------------------------------------------------
#                        VISIONADO DE LOS MODELOS
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#                   COMPARACIONES CON CONJUNTOS DE TEST

par(mfrow = c(2,2))

plot(test_data_dax$Date,
     test_data_dax$Price,
     type = "l",
     col = "blue",
     lwd = 3,
     main = "Predicción para DAX",
     xlab = "Año",
     ylab = "Precio (puntos)",
     ylim = c(min(test_data_dax$Price,predictions_dax),
              max(test_data_dax$Price,predictions_dax)))
grid(lty = 2)
lines(test_data_dax$Date,
      predictions_dax,
      col = "cyan",
      lty = 3)
legend("topleft",
       legend = c("Real - Conjunto de test",
                  "Pronóstico"),
       col = c("blue","cyan"),
       lty = 1)

plot(test_data_cac$Date,
     test_data_cac$Price,
     type = "l",
     col = "blue",
     lwd = 3,
     main = "Predicción para CAC",
     xlab = "Año",
     ylab = "Precio (puntos)",
     ylim = c(min(test_data_cac$Price,predictions_cac),
              max(test_data_cac$Price,predictions_cac)))
grid(lty = 2)
lines(test_data_cac$Date,
      predictions_cac,
      col = "cyan",
      lty = 3)
legend("topleft",
       legend = c("Real - Conjunto de test",
                  "Pronóstico"),
       col = c("blue","cyan"),
       lty = 1)

plot(test_data_smi$Date,
     test_data_smi$Price,
     type = "l",
     col = "blue",
     lwd = 3,
     main = "Predicción para SMI",
     xlab = "Año",
     ylab = "Precio (puntos)",
     ylim = c(min(test_data_smi$Price,predictions_smi),
              max(test_data_smi$Price,predictions_smi)))
grid(lty = 2)
lines(test_data_smi$Date,
      predictions_smi,
      col = "cyan",
      lty = 3)
legend("topleft",
       legend = c("Real - Conjunto de test",
                  "Pronóstico"),
       col = c("blue","cyan"),
       lty = 1)

plot(test_data_ftse$Date,
     test_data_ftse$Price,
     type = "l",
     col = "blue",
     lwd = 3,
     main = "Predicción para FTSE",
     xlab = "Año",
     ylab = "Precio (puntos)",
     ylim = c(min(test_data_ftse$Price,predictions_ftse),
              max(test_data_ftse$Price,predictions_ftse)))
grid(lty = 2)
lines(test_data_ftse$Date,
      predictions_ftse,
      col = "cyan",
      lty = 3)
legend("topleft",
       legend = c("Real - Conjunto de test",
                  "Pronóstico"),
       col = c("blue","cyan"),
       lty = 1)

# ------------------------------------------------------------------------------
#                        PRECISIÓN DE CADA MODELO

# Creación de un dataframe con las métricas de cada índice:
results_df_xgb <- data.frame(
  Index = rep(c("DAX","CAC","SMI","FTSE"),each = 3),
  Metric = rep(c("MAE","MSE","RMSE"),times = 4),
  Value = c(mae_dax,mse_dax,rmse_dax,
            mae_cac,mse_cac,rmse_cac,
            mae_smi,mse_smi,rmse_smi,
            mae_ftse,mse_ftse,rmse_ftse)
)

# Crear el gráfico de barras para las métricas:
ggplot(results_df_xgb,
       aes(x = Index,
           y = Value,
           fill = Metric)) +
  geom_bar(stat = "identity",position = "dodge") +
  labs(x = "Índice Económico",
       y = "Valor") +
  ggtitle("Comparación de métricas en modelos XGBoost para índices económicos") +
  theme_minimal() +
  facet_wrap(~ Metric,
             scales = "free_y")

# ------------------------------------------------------------------------------
#                       PREDICCIONES A 30 DÍAS

par(mfrow = c(2,2))

plot(test_data_dax$Date,
     test_data_dax$Price,
     col = "blue",
     xlab = "Año",
     ylab = "Precio (puntos)",
     main = "Evolución del precio de DAX",
     type = "l",
     xlim = c(5,6.2))
grid(lty = 2)
lines(average_dax_df$Date,
      average_dax_df$Prediction,
      type = "l",
      col = "red",
      lwd = 2)
legend("topleft",
       legend = c("Real - Conjunto de test",
                  "Pronóstico a 30 días"),
       col = c("blue","red"),
       lty = 1)

plot(test_data_cac$Date,
     test_data_cac$Price,
     col = "blue",
     xlab = "Año",
     ylab = "Precio (puntos)",
     main = "Evolución del precio de CAC",
     type = "l",
     xlim = c(5,6.2))
grid(lty = 2)
lines(average_cac_df$Date,
      average_cac_df$Prediction,
      type = "l",
      col = "red",
      lwd = 2)
legend("topleft",
       legend = c("Real - Conjunto de test",
                  "Pronóstico a 30 días"),
       col = c("blue","red"),
       lty = 1)

plot(test_data_smi$Date,
     test_data_smi$Price,
     col = "blue",
     xlab = "Año",
     ylab = "Precio (puntos)",
     main = "Evolución del precio de SMI",
     type = "l",
     xlim = c(5,6.2))
grid(lty = 2)
lines(average_smi_df$Date,
      average_smi_df$Prediction,
      type = "l",
      col = "red",
      lwd = 2)
legend("topleft",
       legend = c("Real - Conjunto de test",
                  "Pronóstico a 30 días"),
       col = c("blue","red"),
       lty = 1)

plot(test_data_ftse$Date,
     test_data_ftse$Price,
     col = "blue",
     xlab = "Año",
     ylab = "Precio (puntos)",
     main = "Evolución del precio de FTSE",
     type = "l",
     xlim = c(5,6.2))
grid(lty = 2)
lines(average_ftse_df$Date,
      average_ftse_df$Prediction,
      type = "l",
      col = "red",
      lwd = 2)
legend("topleft",
       legend = c("Real - Conjunto de test",
                  "Pronóstico a 30 días"),
       col = c("blue","red"),
       lty = 1)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#                            MODELO RANDOMFOREST
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

library(randomForest)
library(dplyr)
library(caret)

# ------------------------------------------------------------------------------
#                           DATAFRAMES REQUERIDOS
# ------------------------------------------------------------------------------

# Lista para almacenar los datos de entrenamiento y prueba para cada índice:
index_names <- c("dax","cac","smi","ftse")

train_data_list <- list()
test_data_list <- list()
train_matrix_list <- list()
train_labels_list <- list()
test_matrix_list <- list()
test_labels_list <- list()
train_df_list <- list()

# Definir el número de lags:
num_lags <- 3

# Creación de un bucle para procesar los datos de cada índice bursátil:
for (index_name in index_names) {
  
  # Seleccionar el dataframe original correspondiente al índice:
  index_df <- get(paste0(index_name,"_df"))
  
  # Crear un dataframe con las columnas Date y Price:
  index_df_subset <- index_df[c("Date","Price")]
  
  # Crear las variables de retraso (lags) en el dataframe:
  for (lag in 1:num_lags) {
    index_df_subset[paste0("Price_lag_",lag)] <- lag(index_df_subset$Price,lag)
  }
  index_df_subset <- index_df_subset[complete.cases(index_df_subset),]
  
  # Conjuntos de train y test:
  train_data <- index_df_subset[1:1500,]
  test_data <- index_df_subset[1501:1860,]
  
  # Matrices y etiquetas de train y test:
  train_matrix <- as.matrix(train_data[,-(1:3)])
  train_labels <- train_data$Price
  test_matrix <- as.matrix(test_data[,-(1:3)])
  test_labels <- test_data$Price
  
  # Dataframe necesario para el modelo:
  train_df <- index_df_subset[1:(nrow(index_df_subset)-30),]
  
  # Almacenamiento de los datos:
  train_data_list[[index_name]] <- train_data
  test_data_list[[index_name]] <- test_data
  train_matrix_list[[index_name]] <- train_matrix
  train_labels_list[[index_name]] <- train_labels
  test_matrix_list[[index_name]] <- test_matrix
  test_labels_list[[index_name]] <- test_labels
  train_df_list[[index_name]] <- train_df
}

# Creación de un bucle para procesar los datos de cada índice bursátil:
for (index_name in index_names) {
  
  # Seleccionar el dataframe original correspondiente al índice:
  index_df <- get(paste0(index_name,"_df"))
  
  # Crear un dataframe con las columnas Date y Price:
  assign(paste0(index_name,"_df_subset"),
         index_df[c("Date","Price")])
  
  # Crear las variables de retraso (lags) en el dataframe:
  for (lag in 1:num_lags) {
    assign(paste0(index_name,"_df_subset$Price_lag_",lag),lag(index_df$Price, lag))
  }
  
  # Eliminar filas con NA
  assign(paste0(index_name,"_df_subset"),index_df_subset[complete.cases(index_df_subset),])
  
  # Conjuntos de train y test:
  assign(paste0("train_data_",index_name),index_df_subset[1:1500,])
  assign(paste0("test_data_",index_name),index_df_subset[1501:1860,])
  
  # Matrices y etiquetas de train y test:
  assign(paste0("train_matrix_",index_name),as.matrix(get(paste0("train_data_",index_name))[,-(1:3)]))
  assign(paste0("train_labels_",index_name),get(paste0("train_data_",index_name))$Price)
  assign(paste0("test_matrix_",index_name),as.matrix(get(paste0("test_data_",index_name))[,-(1:3)]))
  assign(paste0("test_labels_",index_name),get(paste0("test_data_",index_name))$Price)
  
  # Dataframe necesario para el modelo:
  assign(paste0(index_name,"_train_df"),get(paste0("train_data_",index_name))[1:(nrow(get(paste0("train_data_",index_name)))-30),])
}

# ------------------------------------------------------------------------------
#                         CREACIÓN DE LOS MODELOS
# ------------------------------------------------------------------------------

# Listas para almacenar los datos y modelos de cada índice:
data_list <- list(dax = dax_train_df,
                  cac = cac_train_df,
                  smi = smi_train_df,
                  ftse = ftse_train_df)
model_list <- list(dax = NULL,
                   cac = NULL,
                   smi = NULL,
                   ftse = NULL)

# Listas para almacenar los resultados de RMSE y las mejores combinaciones:
rmse_results_list <- list(dax = numeric(0),
                          cac = numeric(0),
                          smi = numeric(0),
                          ftse = numeric(0))
result_list <- list(dax = data.frame(ntrees = numeric(0),nodesize = numeric(0),mtry = numeric(0),rmse = numeric(0)),
                    cac = data.frame(ntrees = numeric(0),nodesize = numeric(0),mtry = numeric(0),rmse = numeric(0)),
                    smi = data.frame(ntrees = numeric(0),nodesize = numeric(0),mtry = numeric(0),rmse = numeric(0)),
                    ftse = data.frame(ntrees = numeric(0),nodesize = numeric(0),mtry = numeric(0),rmse = numeric(0)))

# Hiperparámetros a probar:
ntrees <- c(800,825,850,875,900,925,950,975,1000,1025,1050,1075,1100,
            1125,1150,1175,1200,1225,1250,1275,1300,1325,1350,1375,1400,
            1425,1450,1475,1500,1525,1550,1575,1600,1625,1650,1675,1700)
nodesize <- c(3,4,5,6,7,8,9,10,11,12,13,14,15)
mtry <- c(2,3)

# Bucle para encontrar el mejor modelo para cada índice:
for (index_name in index_names) {
  data <- data_list[[index_name]]
  
  rmse_results <- numeric(length = length(ntrees)*length(nodesize)*length(mtry))
  result <- data.frame(ntrees = numeric(0),
                       nodesize = numeric(0),
                       mtry = numeric(0),
                       rmse = numeric(0))
  rmse <- 0  
  
  for (i in 1:length(ntrees)) {
    for (j in 1:length(nodesize)) {
      for (k in 1:length(mtry)) {
        cat("Index:",index_name,"| Iteración:",i,"de",length(ntrees), 
            "| Nodesize:",nodesize[j],"| Mtry:",mtry[k],"\n")
        
        # Depurador:
        #browser()
        
        # Entrenamiento de los modelos:
        set.seed(123)
        model_rf <- randomForest(Price ~ .,
                                 data = data,
                                 ntree = ntrees[i],
                                 mtry = mtry[k],
                                 nodesize = nodesize[j],
                                 na.action = na.omit)
        
        # Predicciones en conjunto de test:
        predictions_rf <- predict(model_rf,newdata = test_data_list[[index_name]])
        rmse <- sqrt(mean((predictions_rf-test_data_list[[index_name]]$Price)^2))
        
        # Asignar el valor de rmse a rmse_results:
        rmse_results[i+(j-1)*length(ntrees)+(k-1)*length(ntrees)*length(nodesize)] <- rmse
        
        # Agregado de datos al dataframe:
        result <- rbind(result,data.frame(ntrees = ntrees[i],
                                          nodesize = nodesize[j],
                                          mtry = mtry[k],
                                          rmse = rmse))
      }
    }
  }
  
  # Guardar los resultados en las listas correspondientes
  rmse_results_list[[index_name]] <- rmse_results
  result_list[[index_name]] <- result
  
  # Encontrar la combinación de hiperparámetros que produce el RMSE mínimo
  best_combination <- result[which.min(result$rmse), ]
  
  # Imprimir los mejores hiperparámetros y el valor de RMSE mínimo para este índice
  cat("Mejores hiperparámetros para",index_name,":\n")
  cat("Número de árboles:",best_combination$ntrees,"\n")
  cat("Tamaño mínimo de nodo:",best_combination$nodesize,"\n")
  cat("Número de características consideradas:",best_combination$mtry,"\n")
  cat("RMSE mínimo:",best_combination$rmse,"\n")
  
  # Entrenar el modelo RandomForest con los mejores hiperparámetros para este índice
  best_model_rf <- randomForest(Price ~ .,
                                data = data,
                                ntree = best_combination$ntrees,
                                mtry = best_combination$mtry,
                                nodesize = best_combination$nodesize)
  
  # Guardar el mejor modelo en la lista de modelos
  model_list[[index_name]] <- best_model_rf
}


