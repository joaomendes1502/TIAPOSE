library(forecast)

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

cat("read Walmart Sales\n")
TS <- read.table("walmart.csv", sep = ",", header = TRUE)
# Convert Date column to Date format:
TS$Date <- as.Date(TS$Date)

# Loop through columns 4 to 7
for (i in 4:7) { #vai fazer um loop desde a coluna 4 (dep1) até à coluna 7 (dep4)
  cat("Departamento:", colnames(TS)[i], "\n")
  
  dep_data <- TS[, i]
  
  K <- 4
  # K é a frequencia (numero de observaçoes antes que o padrao sazonal se repita) de 4 semanas
  # H é a previsão temporal (Horizonte temporal)- tempo à frente, multi-steap ahead
  
  L <- length(dep_data)  #determina o numero total de observaçoes da serie temporal
  NTS <- K # number of predictions
  H <- NTS # from 1 to H ahead predictions
  
  YR=diff(range(dep_data))
  
  # this time series object only includes TRAIN (older) data:
  LTR <- L - H #subtraindo é possivel obter o indice onde a serie temporal de treinamento termina (com a data mais antiga)
  
  TR <- ts(dep_data[1:LTR], frequency = K) #cria um objeto de serie temporal "TR, usando os primeiros LTR como dados de treino
  plot(TR) #mostra data de treino
  mpause() #pressEnter
  
  # target predictions:
  Y <- dep_data[(LTR + 1):L] #define Y como sendo os vetores do periodo de teste (valores que não foram usados para treinar o modelo)
  # define como sendo os dados de teste, permitindo a comparação das previsoes feitas pelo modelo e os dados reais
  
  # modelar ETS
  print("model> ets")
  ETS_model <- ets(TR)
  
  print(ETS_model)
  print("show ETS forecasts:")
  
  FETS <- forecast(ETS_model, h = H) #o modelo ETS vai fazer previsões para H passos à frente. essas previsoes são guardadas na variavel FETS
  PredETS <- FETS$mean[1:H] #pega nos primeiros valores de H e guarda na variavel PredETS
  
  reshow(colnames(TS)[i], Y, PredETS, "MAE")
  cat("MAE:",mmetric(Y,PredETS,metric="MAE"),"\n")
  cat("NMAE:",mmetric(Y,PredETS,metric="NMAE",val=YR),"\n")
  cat("RMSE:",mmetric(Y,PredETS,metric="RMSE"),"\n")
  cat("RRSE:",mmetric(Y,PredETS,metric="RRSE"),"\n")
  cat("R2:",mmetric(Y,PredETS,metric="R22"),"\n") # press R measure
  mpause()
}
