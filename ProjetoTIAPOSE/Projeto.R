library(rminer)
library(forecast)


d <- read.table("walmart.csv", sep = ",", header = TRUE)

indice_divisao <- floor(0.7 * nrow(d)) # 80% para treino
conjunto_treino <- d[1:indice_divisao, ]
conjunto_teste <- d[(indice_divisao + 1):nrow(d), ]

calcular_metricas <- function(observed, predicted) {
  rmse <- sqrt(mean((observed - predicted)^2))
  mse <- mean((observed - predicted)^2)
  return(list(RMSE = rmse, MSE = mse))
}

modelo_reg_linear <- lm(WSdep1 ~ Fuel_Price + IsHoliday, data = conjunto_treino)

# Prever com o modelo de Regressão Linear
previsao_reg_linear <- predict(modelo_reg_linear, newdata = conjunto_teste)

# Avaliar o desempenho do modelo de Regressão Linear
metricas_reg_linear <- calcular_metricas(conjunto_teste$WSdep1, previsao_reg_linear)

calcular_metricas <- function(observed, predicted) {
  rmse <- sqrt(mean((observed - predicted)^2))
  mse <- mean((observed - predicted)^2)
  return(list(RMSE = rmse, MSE = mse))
}

print("Métricas do modelo de Regressão Linear:")
print(metricas_reg_linear)

# Converter a data para o formato apropriado
conjunto_treino$Date <- as.Date(conjunto_treino$Date)
conjunto_teste$Date <- as.Date(conjunto_teste$Date)

# Criar uma série temporal para WSdep1
serie_wsdep1_treino <- ts(conjunto_treino$WSdep1, frequency = 7)
serie_wsdep1_teste <- ts(conjunto_teste$WSdep1, frequency = 7)

# Ajustar modelo ARIMA para WSdep1
modelo_arima <- auto.arima(serie_wsdep1_treino)

# Prever com o modelo ARIMA
previsao_arima <- forecast(modelo_arima, h = length(serie_wsdep1_teste))

# Avaliar o desempenho do modelo ARIMA
metricas_arima <- calcular_metricas(serie_wsdep1_teste, previsao_arima$mean)

# Exibir as métricas do modelo ARIMA
print("Métricas do modelo ARIMA:")
print(metricas_arima)

modelos <- list()
previsoes <- list()

# Função para calcular métricas de avaliação
calcular_metricas <- function(observed, predicted) {
  rmse <- sqrt(mean((observed - predicted)^2))
  mse <- mean((observed - predicted)^2)
  return(list(RMSE = rmse, MSE = mse))
}

# Treinar e prever com diferentes modelos
modelos <- list(
  "Linear Regression" = lm(WSdep1 + WSdep2 + WSdep3 + WSdep4 ~ Fuel_Price + IsHoliday, data = conjunto_treinamento),
  "Random Forest" = randomForest(cbind(WSdep1, WSdep2, WSdep3, WSdep4) ~ Fuel_Price + IsHoliday, data = conjunto_treinamento)
  # Adicione mais modelos aqui, se desejar
)

# Fazer previsões com os modelos
for (model_name in names(modelos)) {
  modelo <- modelos[[model_name]]
  previsoes[[model_name]] <- predict(modelo, newdata = conjunto_teste)
}

# Avaliar a qualidade das previsões
metricas <- list()
for (model_name in names(previsoes)) {
  previsao <- previsoes[[model_name]]
  metricas[[model_name]] <- calcular_metricas(conjunto_teste[, c("WSdep1", "WSdep2", "WSdep3", "WSdep4")], previsao)
}

# Imprimir as métricas de avaliação
for (model_name in names(metricas)) {
  cat("Modelo:", model_name, "\n")
  cat("RMSE:", metricas[[model_name]]$RMSE, "\n")
  cat("MSE:", metricas[[model_name]]$MSE, "\n\n")
}