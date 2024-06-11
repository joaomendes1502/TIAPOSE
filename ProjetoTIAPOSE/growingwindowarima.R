library(forecast)

reshow <- function(label = "", Y, Pred, metric = "MAE", PLOT = TRUE) {
  main <- paste(label, metric, ":", round(mmetric(Y, Pred, metric = metric), digits = 1))
  LEG <- c("target", paste(label, "pred.")) 
  
  if (PLOT) {
    plot(Y, type = "l", col = "black", lwd = 2, xlab = "Time", ylab = "Value", main = main)
    lines(Pred, col = "blue", lwd = 2)
    legend("topleft", legend = LEG, col = c("black", "blue"), lwd = 2)
  } else {
    cat(main, "\n")
  }
}

cat("Lendo as Vendas do Walmart\n")
TS <- read.table("walmart.csv", sep = ",", header = TRUE)
# Convertendo a coluna de Data para o formato de Data:
# TS$Date <- as.Date(TS$Date)

# Loop através das colunas 4 a 7
for (i in 4:7) {
  cat("Departamento:", colnames(TS)[i], "\n")
  
  dep_data <- TS[, i]
  
  K <- 4
  L <- length(dep_data)
  NTS <- K
  H <- NTS
  
  YR=diff(range(dep_data))
  
  # Avaliação com janela crescente
  Test <- K
  S <- round(K/3)
  
  Runs <- 8
  YR <- diff(range(dep_data))
  
  W <- (L - Test) - (Runs - 1) * S
  ev <- vector(length = Runs)
  evNMAE <- vector(length = Runs) # Vetor para guardar o valor do erro em cada iteração
  evRMSE <- vector(length = Runs)
  evRRSE <- vector(length = Runs)
  evR2 <- vector(length = Runs)
  
  for (b in 1:Runs) {
    H <- holdout(dep_data, ratio = Test, mode = "incremental", iter = b, window = W, increment = S)
    LTR <- length(H$tr)
    dtr <- ts(dep_data[H$tr], frequency = K)
    # Ajustando o modelo ARIMA
    M <- suppressWarnings(auto.arima(dtr))
    Pred <- forecast(M, h = length(H$ts))$mean[1:Test]
    ev[b] <- mmetric(dep_data[H$ts], Pred, metric = "MAE")
    evNMAE[b] <- mmetric(dep_data[H$ts], Pred, metric = "NMAE", val= YR)
    evRMSE[b] <- mmetric(dep_data[H$ts], Pred, metric = "RMSE")
    evRRSE[b] <- mmetric(dep_data[H$ts], Pred, metric = "RRSE")
    evR2[b] <- mmetric(dep_data[H$ts], Pred, metric = "R2")
    
    cat("iter:", b, "TR size:", LTR, "TS size:", length(H$ts),
        "MAE:", ev[b], "\n",
        "NMAE:", evNMAE[b], "\n", 
        "RMSE:", evRMSE[b], "\n", 
        "RRSE:", evRRSE[b], "\n",
        "R2:", evR2[b], "\n")
    reshow(colnames(TS)[i], dep_data[H$ts], Pred, "MAE")
    mpause() 
  }
  
  cat("Median MAE:", median(ev), "\n")
  cat("Median NMAE:", median(evNMAE), "\n")
  cat("Median RMSE:", median(evRMSE), "\n")
  cat("Median RRSE:", median(evRRSE), "\n")
  cat("Median R2:", median(evR2), "\n \n")
  
  mpause() 
}

