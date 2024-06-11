library(vars)
library(rminer)
library(forecast)

source("multi-utils.R") # load multi-variate utility forecasting functions

# função para fazer a comparação entre 3 series de dados multivariados
fshow=function(Y,Pred1,Pred2,Pred3,method,name1,name2,name3) {
  par(mfrow = c(1,3)) # three graphs inside a plot
  
  # Plot para cada série de dados prevista
  for (i in 1:3) {
    # métricas de avaliação
    mae=round(mmetric(Y[,i], get(paste0("Pred", i)), metric="MAE"), 1)
    nmae <- round(mmetric(Y[,i], get(paste0("Pred", i)), metric = "NMAE", val= YR), 4)
    cor=round(mmetric(Y[,i], get(paste0("Pred", i)), metric="COR"), digits=2)
    
    # Título para o gráfico
    main=paste(method," ",get(paste0("name", i)), " (MAE =", mae, ", NMAE =", nmae, ", COR =", cor, ")", sep = "")
    
    # Plot das previsões
    mgraph(Y[,i], get(paste0("Pred", i)), main=main, graph="REG", Grid=10, col=c("black","blue"), leg=list(pos="topleft", leg=c("target",method)))
  }
}

cat("read Walmart Sales\n")
data <- read.csv("walmart.csv", header = TRUE, sep = ",")

K=4 
LTS=K 
fuel <- data[["Fuel_Price"]] # preço combustível
holiday <- data[["IsHoliday"]] # feriado


# Departamentos
deps <- c("WSdep1", "WSdep2", "WSdep3", "WSdep4")

# Loop para prever cada departamento individualmente
for (dep in deps) {
  # Dados do departamento atual
  dep_data <- data[[dep]]
  
  YR=diff(range(dep_data))
  
  # Divisão dos dados em treino e teste
  hd = holdout(fuel, ratio=LTS, mode="order")
  
  # Combinação dos dados de combustível e feriado com os dados do departamento
  cdata = cbind(fuel, holiday, dep_data)
  
  # Objeto de série temporal para os dados de treino
  mtr = ts(cdata[hd$tr, ], frequency=K)
  
  # Dados de teste (target)
  Y = cdata[hd$ts, ]
  
  # Criação do modelo VAR
  mvar = autoVAR(mtr, LAGMAX=16)
  
  # Previsões multi-step ahead
  FV = forecastVAR(mvar, h=LTS)
  
  # Previsões para o departamento atual
  Pred1 = FV[[1]] 
  Pred2 = FV[[2]]
  Pred3 = FV[[3]]
  
  # Avaliação e plot das previsões
  fshow(Y, Pred1, Pred2, Pred3, "VAR", "fuel", "holiday", dep)
  
  print(mvar)
  mpause()
}

