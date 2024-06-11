# Carregar as bibliotecas necessárias
library(forecast)
library(rminer)

# Ler os dados do Walmart Sales
cat("A ler as vendas do Walmart\n")
TS <- read.csv("walmart.csv", header = TRUE, sep = ",")

# Loop das colunas 4 a 7, que correspondem aos departamentos
for (i in 4:7) {
  cat("Departamento:", colnames(TS)[i], "\n")
  
  # Seleciona a coluna
  dep_data <- TS[, i]
  
  # Define os parâmetros
  K <- 4
  H <- 4
  L <- length(dep_data)
  Test <- K # number of multi-ahead steps
  S <- 4 # step jump
  Runs <- 10 # número de iteraçoes
  
  # forecast:
  W <- (L - Test) - (Runs - 1) * S # initial training window size for the ts space (forecast methods)
  
  # rminer:
  timelags <- c(1:4)
  D <- CasesSeries(dep_data, timelags) # note: nrow(D) is smaller by max timelags than length(dep_data)
  W2 <- W - max(timelags) # initial training window size for the D space (CasesSeries, rminer methods)
  
  YR <- diff(range(dep_data)) # global Y range, use the same range for the NMAE calculation in all iterations
  
  # growing window: Começa a treinar com 103 semanas, faz previsoes para as 4 seguintes. 
  # Na 2ª iteração, treina com 103+4 semanas e continua a fazer as iteraçoes seguintes
  
  par(mfrow=c(1, 1)) # Define o layout do gráfico
  
  for (b in 1:Runs) { # cycle of the incremental window training (growing window)
    
    # code for rminer package methods:
    H2 <- holdout(D$y, ratio = Test, mode = "incremental", iter = b, window = W2, increment = S)   
    
    # Fit Random Forest
    M_RF <- fit(y ~ ., D[H2$tr, ], model = "randomForest") # modelo randomForest
    Pred_RF <- lforecast(M_RF, D, start = (length(H2$tr) + 1), Test) # multi-step ahead forecasts
    
    # Fit Linear Regression
    M_LR <- fit(y ~ ., D[H2$tr, ], model = "lm") # modelo de regressão linear
    Pred_LR <- lforecast(M_LR, D, start = (length(H2$tr) + 1), Test) # multi-step ahead forecasts
    
    # Fit MLPE
    M_MLPE <- fit(y ~ ., D[H2$tr, ], model = "mlpe") # modelo de regressão linear
    Pred_MLPE <- lforecast(M_LR, D, start = (length(H2$tr) + 1), Test) # multi-step ahead forecasts
    
    # Fit ARIMA model
    dtr <- ts(dep_data[H2$tr], frequency = K)
    M_ARIMA <- suppressWarnings(auto.arima(dtr))
    Pred_ARIMA <- forecast(M_ARIMA, h = length(H2$ts))$mean[1:Test]
    
    # Fit ETS model
    M_ETS <- suppressWarnings(ets(dep_data[H2$tr]))
    Pred_ETS <- forecast(M_ETS, h = length(H2$ts))$mean[1:Test]
    
    # Fit Holt-Winters model
    dtr_hw <- ts(dep_data[H2$tr], frequency = K)
    M_HW <- suppressWarnings(HoltWinters(dtr_hw))
    Pred_HW <- forecast(M_HW, h = length(H2$ts))$mean[1:Test]
    
    # Calcula e imprime métricas para Random Forest
    ev_RF_NMAE <- mmetric(y = dep_data[H2$ts], x = Pred_RF, metric = "NMAE", val = YR)
    ev_RF_MAE <- mmetric(y = dep_data[H2$ts], x = Pred_RF, metric = "MAE", val = YR)
    ev_RF_RMSE <- mmetric(y = dep_data[H2$ts], x = Pred_RF, metric = "RMSE", val = YR)
    ev_RF_RRSE <- mmetric(y = dep_data[H2$ts], x = Pred_RF, metric = "RRSE", val = YR)
    ev_RF_R2 <- mmetric(y = dep_data[H2$ts], x = Pred_RF, metric = "R2", val = YR)
    
    cat("Random Forest - Iteração:", b, "\n",
        "NMAE:", ev_RF_NMAE, "\n",
        "MAE:", ev_RF_MAE, "\n",
        "RMSE:", ev_RF_RMSE, "\n",
        "RRSE:", ev_RF_RRSE, "\n",
        "R2:", ev_RF_R2, "\n")
    
    # Calcula e imprime métricas para MLPE
    ev_MLPE_NMAE <- mmetric(y = dep_data[H2$ts], x = Pred_RF, metric = "NMAE", val = YR)
    ev_MLPE_MAE <- mmetric(y = dep_data[H2$ts], x = Pred_RF, metric = "MAE", val = YR)
    ev_MLPE_RMSE <- mmetric(y = dep_data[H2$ts], x = Pred_RF, metric = "RMSE", val = YR)
    ev_MLPE_RRSE <- mmetric(y = dep_data[H2$ts], x = Pred_RF, metric = "RRSE", val = YR)
    ev_MLPE_R2 <- mmetric(y = dep_data[H2$ts], x = Pred_RF, metric = "R2", val = YR)
    
    cat("MLPE - Iteração:", b, "\n",
        "NMAE:", ev_RF_NMAE, "\n",
        "MAE:", ev_RF_MAE, "\n",
        "RMSE:", ev_RF_RMSE, "\n",
        "RRSE:", ev_RF_RRSE, "\n",
        "R2:", ev_RF_R2, "\n")
    
    # Calcula e imprime métricas para Regressão Linear
    ev_LR_NMAE <- mmetric(y = dep_data[H2$ts], x = Pred_LR, metric = "NMAE", val = YR)
    ev_LR_MAE <- mmetric(y = dep_data[H2$ts], x = Pred_LR, metric = "MAE", val = YR)
    ev_LR_RMSE <- mmetric(y = dep_data[H2$ts], x = Pred_LR, metric = "RMSE", val = YR)
    ev_LR_RRSE <- mmetric(y = dep_data[H2$ts], x = Pred_LR, metric = "RRSE", val = YR)
    ev_LR_R2 <- mmetric(y = dep_data[H2$ts], x = Pred_LR, metric = "R2", val = YR)
    
    cat("Linear Regression - Iteração:", b, "\n",
        "NMAE:", ev_LR_NMAE, "\n",
        "MAE:", ev_LR_MAE, "\n",
        "RMSE:", ev_LR_RMSE, "\n",
        "RRSE:", ev_LR_RRSE, "\n",
        "R2:", ev_LR_R2, "\n")
    
    
    
    # Calcula e imprime métricas para ARIMA
    ev_ARIMA_NMAE <- mmetric(y = dep_data[H2$ts], x = Pred_ARIMA, metric = "NMAE", val = YR)
    ev_ARIMA_MAE <- mmetric(y = dep_data[H2$ts], x = Pred_ARIMA, metric = "MAE", val = YR)
    ev_ARIMA_RMSE <- mmetric(y = dep_data[H2$ts], x = Pred_ARIMA, metric = "RMSE", val = YR)
    ev_ARIMA_RRSE <- mmetric(y = dep_data[H2$ts], x = Pred_ARIMA, metric = "RRSE", val = YR)
    ev_ARIMA_R2 <- mmetric(y = dep_data[H2$ts], x = Pred_ARIMA, metric = "R2", val = YR)
    
    cat("ARIMA - Iteração:", b, "\n",
        "NMAE:", ev_ARIMA_NMAE, "\n",
        "MAE:", ev_ARIMA_MAE, "\n",
        "RMSE:", ev_ARIMA_RMSE, "\n",
        "RRSE:", ev_ARIMA_RRSE, "\n",
        "R2:", ev_ARIMA_R2, "\n")
    
    # Calcula e imprime métricas para ETS
    ev_ETS_NMAE <- mmetric(y = dep_data[H2$ts], x = Pred_ETS, metric = "NMAE", val = YR)
    ev_ETS_MAE <- mmetric(y = dep_data[H2$ts], x = Pred_ETS, metric = "MAE", val = YR)
    ev_ETS_RMSE <- mmetric(y = dep_data[H2$ts], x = Pred_ETS, metric = "RMSE", val = YR)
    ev_ETS_RRSE <- mmetric(y = dep_data[H2$ts], x = Pred_ETS, metric = "RRSE", val = YR)
    ev_ETS_R2 <- mmetric(y = dep_data[H2$ts], x = Pred_ETS, metric = "R2", val = YR)
    
    cat("ETS - Iteração:", b, "\n",
        "NMAE:", ev_ETS_NMAE, "\n",
        "MAE:", ev_ETS_MAE, "\n",
        "RMSE:", ev_ETS_RMSE, "\n",
        "RRSE:", ev_ETS_RRSE, "\n",
        "R2:", ev_ETS_R2, "\n")
    
    # Calcula e imprime métricas para Holt-Winters
    ev_HW_NMAE <- mmetric(y = dep_data[H2$ts], x = Pred_HW, metric = "NMAE", val = YR)
    ev_HW_MAE <- mmetric(y = dep_data[H2$ts], x = Pred_HW, metric = "MAE", val = YR)
    ev_HW_RMSE <- mmetric(y = dep_data[H2$ts], x = Pred_HW, metric = "RMSE", val = YR)
    ev_HW_RRSE <- mmetric(y = dep_data[H2$ts], x = Pred_HW, metric = "RRSE", val = YR)
    ev_HW_R2 <- mmetric(y = dep_data[H2$ts], x = Pred_HW, metric = "R2", val = YR)
    
    cat("Holt-Winters - Iteração:", b, "\n",
        "NMAE:", ev_HW_NMAE, "\n",
        "MAE:", ev_HW_MAE, "\n",
        "RMSE:", ev_HW_RMSE, "\n",
        "RRSE:", ev_HW_RRSE, "\n",
        "R2:", ev_HW_R2, "\n")
    
    # Plotar valores reais e previstos
    plot(dep_data[H2$ts], type = "l", col = "black", ylim = c(min(dep_data), max(dep_data, Pred_RF, Pred_LR, Pred_ARIMA, Pred_ETS)), xlab = "Time", ylab = "Value", main = paste("Real vs Todos Modelos - Departamento:", colnames(TS)[i], "Iteração:", b))
    lines(Pred_RF, col = "blue")
    lines(Pred_LR, col = "red")
    lines(Pred_MLPE, col ="yellow")
    lines(Pred_ARIMA, col = "green")
    lines(Pred_ETS, col = "orange")
    lines(Pred_HW, col = "purple")
    legend("topright", legend = c("Real", "Random Forest", "Linear Regression", "MLPE", "ARIMA", "ETS", "HW"), col = c("black", "blue", "red", "green", "orange", "purple"), lty = 1)
    
    # Aguardar para ver o gráfico antes de passar para a próxima iteração
    mpause()
    
  }
  
}
