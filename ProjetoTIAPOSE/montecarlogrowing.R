library(rminer)
source("montecarlo.R")  

# Função para gerar previsões com Growing Window
generate_forecasts <- function(dep_data, Test, S, Runs, W2, D) {
  # Matriz para armazenar previsões das últimas 4 semanas
  forecasts <- matrix(0, nrow = Runs, ncol = Test)
  
  for (b in 1:Runs) {
    # Configuração do holdout
    H2 <- holdout(D$y, ratio = Test, mode = "incremental", iter = b, window = W2, increment = S)
    
    # Criar o modelo Random Forest
    modelo <- fit(y ~ ., D[H2$tr,], model = "randomForest")
    
    # Prever as últimas 4 semanas
    previsoes <- lforecast(modelo, D, start = length(H2$tr) + 1, Test)
    forecasts[b, ] <- round(previsoes)
  }
  
  return(forecasts)
}

# Função de avaliação para Monte Carlo usando valores previstos
eval <- function(s, vendas_previstas) {
  s <- round(s)  # Garantir que os valores são inteiros
  
  # Criar a matriz de funcionários
  hired_workers <- matrix(s[1:12], nrow = 3, ncol = 4, byrow = TRUE)
  
  # Custos por tipo de funcionário
  custo_junior <- 6000
  custo_normal <- 8000
  custo_senior <- 9750
  
  # Custo total dos trabalhadores
  total_cost_workers <- sum(
    hired_workers[1, ] * custo_junior,
    hired_workers[2, ] * custo_normal,
    hired_workers[3, ] * custo_senior
  )
  
  # Criar a matriz de encomendas
  product_orders <- matrix(s[13:28], nrow = 4, ncol = 4, byrow = TRUE)
  
  # Custos das encomendas por departamento
  custo_encomenda <- c(6, 8, 9, 11)
  total_cost_orders <- sum(
    product_orders[, 1] * custo_encomenda[1],
    product_orders[, 2] * custo_encomenda[2],
    product_orders[, 3] * custo_encomenda[3],
    product_orders[, 4] * custo_encomenda[4]
  )
  
  capacidade_junior <- 4000
  capacidade_normal <- 7000
  capacidade_senior <- 9500
  
  # Calcular a capacidade máxima por departamento
  worker_max_support_per_dept <- apply(hired_workers, 2, function(col) {
    col[1] * capacidade_junior + col[2] * capacidade_normal + col[3] * capacidade_senior
  })
  
  # Receita total com vendas efetivas
  vendas_efetivas <- matrix(0, nrow = nrow(vendas_previstas), ncol = ncol(vendas_previstas))
  
  for (i in 1:nrow(vendas_previstas)) {
    for (j in 1:ncol(vendas_previstas)) {
      if (vendas_previstas[i, j] <= product_orders[i, j] & vendas_previstas[i, j] <= worker_max_support_per_dept[j]) {
        vendas_efetivas[i, j] <- vendas_previstas[i, j]
      } else {
        vendas_efetivas[i, j] <- min(product_orders[i, j], worker_max_support_per_dept[j])
      }
    }
  }
  
  # Valores por departamento em dólares
  valor_venda <- c(8, 10, 12, 16)
  
  # Receita total em dólares
  total_revenue_sales <- sum(vendas_efetivas * outer(rep(1, 4), valor_venda))
  
  # Criar a matriz de estoque
  stock <- matrix(0, nrow = 4, ncol = 4)
  for (s in 1:4) {
    for (d in 1:4) {
      if (s == 1) {
        stock[s, d] <- product_orders[s, d] - vendas_efetivas[s, d]
      } else {
        stock[s, d] <- stock[s - 1, d] + product_orders[s, d] - vendas_efetivas[s, d]
      }
    }
  }
  
  # Custos do estoque por produto
  stock_cost_per_product <- c(3, 5, 6, 8)
  stock_costs <- matrix(0, nrow = 4, ncol = 4)
  for (i in 1:4) {
    stock_costs[, i] <- stock[, i] * stock_cost_per_product[i]
  }
  
  # Custos totais
  total_costs <- total_cost_workers + total_cost_orders + sum(stock_costs)
  
  # Lucro total
  lucro <- total_revenue_sales - total_costs
  
  return(lucro)
}

# Definir limites para a otimização
lower <- rep(0, 28)  # Limites inferiores

calcular_upper <- function(dados_departamento) {
  tipo <- c(4000, 7000, 9500)
  actual_sales_by_dep <- colSums(dados_departamento)
  upper <- numeric(28) # Vetor para armazenar os limites superiores
  
  # Calcular limites superiores para os funcionários
  for (i in 1:ncol(dados_departamento)) {
    max_sales <- max(dados_departamento[[i]])
    if (max_sales != 0) {
      for (j in 1:length(tipo)) {
        upper[(i - 1) * 3 + j] <- ceiling(max_sales / tipo[j])
      }
    }
  }
  
  # Calcular limites superiores para as encomendas
  for (i in 1:ncol(dados_departamento)) {
    for (j in 1:4) {
      if (j == 1) {
        upper[12 + (i - 1) * 4 + j] <- sum(dados_departamento[, i])
      } else if (j == 2) {
        upper[12 + (i - 1) * 4 + j] <- sum(dados_departamento[, i][-1])
      } else if (j == 3) {
        upper[12 + (i - 1) * 4 + j] <- sum(dados_departamento[, i][-c(1:2)])
      } else {
        upper[12 + (i - 1) * 4 + j] <- dados_departamento[nrow(dados_departamento), i]
      }
    }
  }
  
  return(upper)
}

# Número de amostras para Monte Carlo
N <- 1000

# Calcular previsões para cada departamento com Growing Window
TS <- read.table("walmart.csv", sep = ",", header = TRUE)
vendas_previstas <- matrix(0, nrow = 4, ncol = 4)  # Matriz para armazenar previsões

# Configuração para Growing Window
K <- 4  # Frequência de 4 semanas
S <- 1  # Incremento
Runs <- 8  # Número de iterações
timelags <- c(1:4)  # Lags

# Previsões para cada departamento
for (i in 4:7) {
  dep_data <- TS[, i]
  L <- length(dep_data)
  Test <- K  # Horizonte para previsão
  W <- (L - Test) - (Runs - 1) * S  # Tamanho da janela de treinamento
  D <- CasesSeries(dep_data, timelags)  # Criar objeto de séries temporais
  W2 <- W - max(timelags)  # Ajuste para lags
  
  # Gerar previsões para as últimas 4 semanas
  forecasts <- generate_forecasts(dep_data, Test, S, Runs, W2, D)
  
  # Preencher a matriz `vendas_previstas` com as últimas 4 previsões
  vendas_previstas[, i - 3] <- forecasts[nrow(forecasts), ]
}

# Otimização Monte Carlo para cada execução do "Growing Window"
lucros <- vector("numeric", Runs)  # Armazenar lucros por iteração
solucoes <- list()

for (b in 1:Runs) {
  resultado <- mcsearch(fn = function(s) eval(s, vendas_previstas), lower = lower, upper = upper, N = N, type = "max")
  lucros[b] <- resultado$eval
  solucoes[[b]] <- resultado$sol
}

indice_melhor_solucao <- which.max(lucros)  # Índice do melhor lucro
melhor_solucao <- solucoes[[indice_melhor_solucao]] 

# Calcular estatísticas de lucro
melhor_lucro <- max(lucros)
lucro_medio <- mean(lucros)
lucro_mediano <- median(lucros)

# Imprimir resultados
print("Lucros obtidos em cada execução:")
print(lucros)

print(paste("Melhor lucro:", melhor_lucro))
print(paste("Média do lucro:", lucro_medio))
print(paste("Mediana do lucro:", lucro_mediano))

print("Melhor solução com melhor lucro:")
print(round(melhor_solucao))
