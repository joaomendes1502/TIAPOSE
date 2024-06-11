library(rminer)
library(forecast)

# Definir função reshow
reshow <- function(label = "", Y, Pred, metric = "MAE", PLOT = TRUE) {
  main <- paste(label, metric, ":", round(mmetric(Y, Pred, metric = metric), digits = 1))
  LEG <- c("target", paste(label, "pred.")) # Legenda do gráfico
  
  if (PLOT) {
    plot(Y, type = "l", col = "black", lwd = 2, xlab = "Time", ylab = "Value", main = main)
    lines(Pred, col = "blue", lwd = 2)
    legend("topleft", legend = LEG, col = c("black", "blue"), lwd = 2)
  } else {
    cat(main, "\n")
  }
}

# Carregar e pré-processar os dados
cat("Lendo as vendas do Walmart\n")
TS <- read.table("walmart.csv", sep = ",", header = TRUE)

# Loop pelos departamentos desejados (ajustar índices das colunas conforme necessário)
for (i in 4:7) {
  cat("Departamento:", colnames(TS)[i], "\n")
  
  dep_data <- TS[, i]
  
  # Definir a frequência (por exemplo, semanas)
  K <- 4
  
  # Janela de avaliação
  Test <- K
  S <- round(K / 3)
  
  Runs <- 8
  
  L <- length(dep_data)
  W <- (L - Test) - (Runs - 1) * S
  
  # Criar objeto CasesSeries com as defasagens
  timelags <- c(1:4)
  D <- CasesSeries(dep_data, timelags)
  W2 <- W - max(timelags)
  
  # Vetores para guardar as métricas
  ev2 <- vector(length = Runs)
  evNMAE <- vector(length = Runs)
  evRMSE <- vector(length = Runs)
  evRRSE <- vector(length = Runs)
  evR2 <- vector(length = Runs)
  
  # Growing window
  for (b in 1:Runs) {
    # Dividir em treinamento e teste
    H2 <- holdout(D$y, ratio = Test, mode = "incremental", iter = b, window = W2, increment = S) 
    trinit <- H2$tr[1]
    dtr <- ts(dep_data[H2$tr], frequency = K)
    M2 <- fit(y ~ ., D[H2$tr, ], model = "mlpe") # Modelo de previsão
    Pred2 <- lforecast(M2, D, start = (length(H2$tr) + 1), Test) # Previsões multi-adiante
    ev2[b] <- mmetric(dep_data[H2$ts], Pred2, metric = "MAE", val = diff(range(dep_data)))
    evNMAE[b] <- mmetric(dep_data[H2$ts], Pred2, metric = "NMAE", val = diff(range(dep_data)))
    evRMSE[b] <- mmetric(dep_data[H2$ts], Pred2, metric = "RMSE", val = diff(range(dep_data)))
    evRRSE[b] <- mmetric(dep_data[H2$ts], Pred2, metric = "RRSE", val = diff(range(dep_data)))
    evR2[b] <- mmetric(dep_data[H2$ts], Pred2, metric = "R2")
    
    reshow(colnames(TS)[i], dep_data[H2$ts], Pred2, "MAE")
    mpause()
  }
  
  cat("Median MAE for RF:", median(ev2), "\n")
  cat("Median NMAE:", median(evNMAE), "\n")
  cat("Median RMSE:", median(evRMSE), "\n")
  cat("Median RRSE:", median(evRRSE), "\n")
  cat("Median R2:", median(evR2), "\n \n")
  
  # Implementar Seasonal Naive (snaive)
  dep_ts <- ts(tail(dep_data, 4))
  snaive_forecast <- snaive(dep_ts, h = 4)
  
  # Exibir as previsões geradas pelo Seasonal Naive
  cat("Previsões do Seasonal Naive:\n")
  print(snaive_forecast)
  
  # Comparar as previsões do Seasonal Naive com as previsões da Growing Window
  cat("Comparação das previsões:\n")
  print(data.frame(Growing_Window = Pred2, Seasonal_Naive = snaive_forecast$mean))
  
  mpause()
}
