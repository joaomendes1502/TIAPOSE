library(rminer)
library(adana)

walmart <- read.csv("walmart.csv", header = TRUE)

previsoes_rf <- vector("list", 4)
for (i in 4:7) {
  dep_data <- walmart[, i]
  K <- 4
  L <- length(dep_data)
  NTS <- K
  H <- NTS
  YR <- diff(range(dep_data))
  d <- CasesSeries(dep_data, c(1:4))
  LTR <- L - H
  Y <- dep_data[(LTR + 1):L]
  hd <- holdout(d$y, ratio = NTS, mode = "order")
  RF <- fit(y ~ ., d[hd$tr,], model = "randomForest")
  init <- hd$ts[1]
  F5 <- lforecast(RF, d, start = init, horizon = H)
  PredRF <- F5
  previsoes_rf[[i - 3]] <- PredRF
}

funcHon <- function(s) {
  # Parâmetros
  custo_funcionario <- c(6000, 8000, 9750) # por tipo
  custo_produto <- c(6, 8, 9, 11) # por departamento
  custo_stock <- c(3, 5, 6, 8) # por departamento
  preco_venda <- c(8, 10, 12, 16) # por departamento
  capacidade_funcionario <- c(4000, 7000, 9500) # por tipo
  
  # Decisões
  funcionarios <- rep(0, 12) # 12 decisões (4 departamentos x 3 tipos)
  encomendas <- rep(0, 16) # 16 decisões (4 departamentos x 4 semanas)
  
  # Lucro
  lucro <- 0
  for (d in 1:4) {
    for (s in 1:4) {
      # Vendas efetivas
      vendas_efetivas <- min(previsoes_rf[[d]][s], funcionarios[d + 3 * (s - 1)] * capacidade_funcionario[s], encomendas[4 * (d - 1) + s])
      
      # Receitas
      receitas <- vendas_efetivas * preco_venda[d]
      
      # Despesas
      despesas_funcionarios <- funcionarios[d + 3 * (s - 1)] * custo_funcionario[s]
      despesas_produtos <- encomendas[4 * (d - 1) + s] * custo_produto[d]
      despesas_stock <- (previsoes_rf[[d]][s] - vendas_efetivas) * custo_stock[d]
      
      # Lucro
      lucro <- lucro + receitas - despesas_funcionarios - despesas_produtos - despesas_stock
    }
  }
  
  # Esforço
  esforco <- sum(encomendas) + sum(funcionarios)
  
  # Retornar lucro e esforço
  return(c(lucro, esforco))
}