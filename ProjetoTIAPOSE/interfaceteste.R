library(rminer)
library(forecast)
library(lubridate)  # Para trabalhar com datas
source("montecarlo.R")  # Carregar função Monte Carlo

# Perguntas ao usuário
ano <- as.integer(readline("Digite o ano (ex: 2012) para otimizar: "))
mes <- as.integer(readline("Digite o mês (1 a 12) para otimizar: "))

modelo_previsao <- readline("Escolha um modelo de previsão (Random Forest por padrão): ")
if (modelo_previsao == "") {
  modelo_previsao <- "Random Forest"
}

modelo_otimizacao <- readline("Escolha um modelo de otimização (Monte Carlo por padrão): ")
if (modelo_otimizacao == "") {
  modelo_otimizacao <- "Monte Carlo"
}

# Carregar dados e filtrar para o mês/ano especificados
d <- read.table("walmart.csv", header = TRUE, sep = ",")
d$Date <- as.Date(d$Date)  # Converter para formato Date

# Selecionar linhas do mês e ano especificados
dados_do_mes <- subset(d, year(d$Date) == ano & month(d$Date) == mes)

# Verificar se existem dados suficientes para o mês especificado
if (nrow(dados_do_mes) < 4) {
  stop("Menos de 4 semanas para o mês selecionado. Escolha outro mês ou ano.")
}

# Obter vendas reais para as 4 semanas do mês selecionado
colunas_interesse <- 4:7  # As colunas de vendas de interesse (WSDep1 a WSDep4)
vendasReais <- dados_do_mes[1:4, colunas_interesse]  # Obter as primeiras 4 semanas do mês especificado

# Imprimir as vendas reais para o mês solicitado
cat("Vendas reais para o mês selecionado:\n")
print(vendasReais)

# Configurações para Growing Window
K <- 4  # Frequência de 4 semanas
S <- 1  # Incremento para Growing Window
Runs <- 8  # Número de iterações

vendas_previstas <- matrix(0, nrow = 4, ncol = 4)  # Matriz para armazenar previsões para 4 semanas

# Previsões para cada departamento com Growing Window
if (modelo_previsao == "Random Forest") {
  for (i in 1:4) {
    dep_data <- vendasReais[, i]
    L <- length(dep_data)
    Test <- K  # Horizonte para previsão
    W <- (L - Test) - (Runs - 1) * S  # Tamanho da janela de treinamento
    timelags <- c(1:4)  # Lags
    D <- CasesSeries(dep_data, timelags)  # Criar objeto de séries temporais
    W2 <- W - max(timelags)  # Ajuste para lags
    
    for (b in 1:Runs) {
      H2 <- holdout(D$y, ratio = Test, mode = "incremental", iter = b, window = W2, increment = S)
      modelo <- fit(y ~ ., D[H2$tr], model = "randomForest")
      previsoes <- lforecast(modelo, D, start = length(H2$tr) + 1, Test)
      vendas_previstas[b, i] <- round(previsoes)
    }
  }
} else {
  stop("Modelo de previsão inválido. Use 'Random Forest'.")
}

# Imprimir valores previstos após a previsão com Growing Window
cat("Valores previstos após Growing Window:\n")
print(vendas_previstas)

# Monte Carlo para otimização
if (modelo_otimizacao == "Monte Carlo") {
  lower <- rep(0, 28)  # Limites inferiores
  upper <- c(
    rep(10, 12),  # Funcionários (máximo 10 por posição)
    rep(100000, 16)  # Encomendas (máximo permitido por semana/departamento)
  )
  
  N <- 1000  # Número de amostras para Monte Carlo
  resultado <- mcsearch(fn = function(s) eval(s, vendas_previstas), lower = lower, upper = upper, N = N, type = "max")
  
  # Resultado da otimização com Monte Carlo
  cat("Resultado da otimização com Monte Carlo:\n")
  print(round(resultado$sol))
  print(paste("Melhor lucro obtido:", resultado$value))
} else {
  stop("Modelo de otimização inválido. Use 'Monte Carlo'.")
}