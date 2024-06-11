# Import Libraries

library(rminer)

# Define reshow function
reshow <- function(label = "", Y, Pred, metric = "MAE", PLOT = TRUE) {
  main <- paste(label, metric, ":", round(mmetric(Y, Pred2, metric = metric), digits = 1))
  LEG <- c("target", paste(label, "pred.")) #legenda do gráfico
  
  if (PLOT) {
    plot(Y, type = "l", col = "black", lwd = 2, xlab = "Time", ylab = "Value", main = main)
    lines(Pred, col = "blue", lwd = 2)
    legend("topleft", legend = LEG, col = c("black", "blue"), lwd = 2)
  } else {
    cat(main, "\n")
  }
}

# Load and Preprocess Data
cat("read Walmart Sales\n")
TS <- read.table("walmart.csv", sep=",", header=TRUE)
# Convert Date column to Date format (if applicable)
# TS$Date <- as.Date(TS$Date)

# Loop through desired columns (adapt column indices)
for (i in 4:7) {
  cat("Departamento:", colnames(TS)[i], "\n")
  
  dep_data <- TS[, i]
  
  K <- 4  # Frequency (e.g., weeks)
  L <- length(dep_data)
  NTS <- K  # Number of predictions
  H <- NTS  # Horizon
  
  # Growing window evaluation
  Test <- K  # H, the number of multi-ahead steps
  S <- round(K/3)  # Step jump
  
  Runs <- 8 # Number of growing window iterations
  
  W=(L-Test)-(Runs-1)*S # initial training window size for the ts space (forecast methods)
  
  # Define lags based on your knowledge (e.g., 1, 12, 13)
  timelags=c(1:4)
  D <- CasesSeries(dep_data, timelags)  # Create CasesSeries object with lags
  W2=W-max(timelags) # initial training window size for the D space (CasesSeries, rminer methods)
  YR=diff(range(dep_data))
  
  ev2 <- vector(length = Runs)  # Error vector for "mlpe"
  evNMAE <- vector(length = Runs) # Vetor para guardar o valor do erro em cada iteração
  evRMSE <- vector(length = Runs)
  evRRSE <- vector(length = Runs)
  evR2 <- vector(length = Runs)
  
  
  # Growing window
  for (b in 1:Runs) {
    # Train-Test Split
    H2=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S) 
    trinit=H2$tr[1]
    dtr=ts(dep_data[H2$tr],frequency=K)
    M2=fit(y~.,D[H2$tr,],model="randomForest") # create forecasting model
    Pred2=lforecast(M2,D,start=(length(H2$tr)+1),Test) # multi-step ahead forecasts
    ev2[b]=mmetric(dep_data[H2$ts], Pred2,metric="MAE")
    evNMAE[b] <- mmetric(dep_data[H2$ts], Pred2, metric = "NMAE", val= YR)
    evRMSE[b] <- mmetric(dep_data[H2$ts], Pred2, metric = "RMSE")
    evRRSE[b] <- mmetric(dep_data[H2$ts], Pred2, metric = "RRSE")
    evR2[b] <- mmetric(dep_data[H2$ts], Pred2, metric = "R2")
    
    
    cat("iter:", b, "TR from:",trinit,"to:",(trinit+length(H2$tr)-1),"size:",length(H2$tr),
        "TS from:",H2$ts[1],"to:",H2$ts[length(H2$ts)],"size:",length(H2$ts),
        "MAE:", ev2[b], "\n",
        "NMAE:", evNMAE[b], "\n", 
        "RMSE:", evRMSE[b], "\n", 
        "RRSE:", evRRSE[b], "\n",
        "R2:", evR2[b], "\n")
    reshow(colnames(TS)[i], dep_data[H2$ts], Pred2, "MAE")
    mpause()
  }
  
  cat("Median MAE for RF:", median(ev2), "\n")
  cat("Median NMAE:", median(evNMAE), "\n")
  cat("Median RMSE:", median(evRMSE), "\n")
  cat("Median RRSE:", median(evRRSE), "\n")
  cat("Median R2:", median(evR2), "\n \n")
  mpause()
}

