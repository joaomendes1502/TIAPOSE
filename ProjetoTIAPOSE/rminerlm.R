library(rminer)

# Define reshow function
reshow <- function(label = "", Y, Pred, metric = "MAE", PLOT = TRUE) {
  main <- paste(label, metric, ":", round(mmetric(Y, Pred, metric = metric), digits = 1))
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
  
  # Define lags based on your knowledge (e.g., 1, 12, 13)
  d <- CasesSeries(dep_data, c(1:4))  # Create CasesSeries object with lags
  
  # Train-Test Split
  NTS <- 4  # Number of predictions
  H <- NTS  # Horizon
  LTR <- L - H
  Y <- dep_data[(LTR + 1):L]
  
  YR=diff(range(dep_data))
  
  # Train Random Forest Model
  hd <- holdout(d$y, ratio=NTS, mode="order")
  LM <- fit(y~., d[hd$tr,], model="lm")
  
  # Multi-step Forecasts
  init <- hd$ts[1]
  F5 <- lforecast(LM, d, start=init, horizon=H)
  Pred5 <- F5
  
  # Evaluate Forecasts
  reshow(colnames(TS)[i], Y, Pred5, "MAE")
  cat("MAE:",mmetric(Y,Pred5,metric="MAE"),"\n")
  cat("NMAE:",mmetric(Y,Pred5,metric="NMAE",val=YR),"\n")
  cat("RMSE:",mmetric(Y,Pred5,metric="RMSE"),"\n")
  cat("RRSE:",mmetric(Y,Pred5,metric="RRSE"),"\n")
  cat("R2:",mmetric(Y,Pred5,metric="R22"),"\n") # press R measure
  mpause()
}
