library(rminer)

# Carregar funções auxiliares
source("montecarlo.R")  
source("blind.R")       
source("data.R")

prever_vendas <- function(TS, colunas) {
  previsoes <- matrix(0, nrow = 4, ncol = length(colunas))
  
  for (i in 1:length(colunas)) {
    coluna <- colunas[i]
    dep_data <- TS[, coluna]
    
    K <- 4  # Frequência (ex: semanas)
    L <- length(dep_data)
    NTS <- K  # Número de previsões
    H <- NTS  # Horizonte
    
    # Growing window evaluation
    Test <- K  # H, número de passos multi-ahead
    S <- round(K/3)  # Step jump
    
    Runs <- 8 # Número de iterações de growing window
    
    W = (L - Test) - (Runs - 1) * S # tamanho da janela de treino inicial
    
    # Definir lags com base no conhecimento (ex: 1, 2, 3, 4)
    timelags <- c(1:4)
    D <- CasesSeries(dep_data, timelags)  # Criar objeto CasesSeries com lags
    W2 <- W - max(timelags) # tamanho da janela de treino inicial para o espaço D
    
    ev2 <- vector(length = Runs)  # Vetor de erro para "mlpe"
    previsao_final <- numeric()
    
    # Growing window
    for (b in 1:Runs) {
      # Train-Test Split
      H2 <- holdout(D$y, ratio = Test, mode = "incremental", iter = b, window = W2, increment = S) 
      M2 <- fit(y ~ ., D[H2$tr, ], model = "randomForest") # criar modelo de previsão
      Pred2 <- lforecast(M2, D, start = (length(H2$tr) + 1), Test) # previsões multi-step ahead
      
      if (b == Runs) {
        previsao_final <- Pred2
      }
    }
    
    previsoes[, i] <- previsao_final
  }
  
  return(previsoes)
}

dados <- read.csv("walmart.csv", header = TRUE, sep = ",")
colunas <- 4:7

ano <- as.integer(readline(prompt = "Por favor, insira o ano (por exemplo, 2010): "))
mes <- as.integer(readline(prompt = "Por favor, insira o mês (1-12): "))
dados_filtrados <- load_and_filter_data("walmart.csv", ano, mes)

vendas_previstas <- dados_filtrados[, colunas]
print("Matriz com os valores reais para o mês e ano selecionados:")
print(vendas_previstas)

previsoes_vendas <- prever_vendas(dados, colunas)
print("Matriz com os valores previstos baseados nos dados filtrados:")
print(previsoes_vendas)

# Função de avaliação adaptada para usar os valores previstos
eval <- function(s, previsoes_vendas) {
  s <- round(s)  # Arredondar para valores inteiros
  
  dados <- read.table("walmart.csv", header = TRUE, sep = ",")
  colunas <- 4:7  # Colunas 4 a 7
  vendas_previstas <- previsoes_vendas  # Usar valores previstos
  
  colnames(vendas_previstas) <- c("WSDep1", "WSDep2", "WSDep3", "WSDep4")
  
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
  
  effortworkers <- sum(hired_workers)
  
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
  
  effortorders <- sum(product_orders != 0)
  
  capacidade_junior <- 4000
  capacidade_normal <- 7000
  capacidade_senior <- 9500
  
  worker_max_support_per_dept <- apply(hired_workers, 2, function(col) {
    col[1] * capacidade_junior + col[2] * capacidade_normal + col[3] * capacidade_senior
  })
  
  # Receita total
  calcularVendasEfetivas <- function(vendas_previstas, product_orders, worker_max_support_per_dept) {
    # Inicializar uma matriz para armazenar as vendas efetivas
    vendasEfetivas <- matrix(0, nrow = nrow(vendas_previstas), ncol = ncol(vendas_previstas))
    
    # Calcular as vendas efetivas com base nas condições especificadas
    for (i in 1:nrow(vendas_previstas)) {
      for (j in 1:ncol(vendas_previstas)) {
        if (vendas_previstas[i, j] <= product_orders[i, j] & vendas_previstas[i, j] <= worker_max_support_per_dept[j]) {
          vendasEfetivas[i, j] <- vendas_previstas[i, j]
        } else if (i > 1 && vendas_previstas[i - 1, j] <= product_orders[i - 1, j]) {
          vendasEfetivas[i, j] <- product_orders[i-1, j] - vendas_previstas[i - 1, j]
        } else {
          vendasEfetivas[i, j] <- min(product_orders[i, j], worker_max_support_per_dept[j])
        }
      }
    }
    
    # Retornar a matriz de vendas efetivas
    return(vendasEfetivas)
  }
  
  # Calcular a matriz de vendas efetivas
  sales <- calcularVendasEfetivas(vendas_previstas, product_orders, worker_max_support_per_dept)
  
  # Valores em dólares por venda por departamento
  valor_venda <- c(8, 10, 12, 16)
  
  # Criar a matriz `sales in USD`
  sales_in_usd <- matrix(0, nrow = 4, ncol = 4)
  for (i in 1:4) {
    sales_in_usd[, i] <- sales[, i] * valor_venda[i]
  }
  
  # Receita total
  total_revenue_sales <- sum(sales_in_usd)
  
  # Criar a matriz `stock`
  stock <- matrix(0, nrow = 4, ncol = 4)
  for (s in 1:4) {
    for (d in 1:4) {
      if (s == 1) {
        stock[s, d] <- product_orders[s, d] - sales[s, d]
      } else {
        stock[s, d] <- stock[s - 1, d] + product_orders[s, d] - sales[s, d]
      }
    }
  }
  
  # Custos do estoque por produto
  stock_cost_per_product <- c(3, 5, 6, 8)
  stock_costs <- matrix(0, nrow = 4, ncol = 4)
  for (i in 1:4) {
    stock_costs[, i] <- stock[, i] * stock_cost_per_product[i]
  }
  
  total_stock_cost <- sum(stock_costs)
  
  # Custo total
  total_costs <- total_cost_workers + total_cost_orders + total_stock_cost
  
  # Lucro (F1)
  month_profit <- total_revenue_sales - total_costs
  
  month_effort <- effortworkers + effortorders
  
  # Retornar o lucro (F1)
  return(month_profit)
}

lower <- rep(0, 28)  # Limites inferiores (todos zero)

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

upper <- calcular_upper(previsoes_vendas)
print(upper)

N <- 25000

resultado <- mcsearch(fn = function(s) eval(s, previsoes_vendas), lower = lower, upper = upper, N = N, type = "max")

print("Melhor solução:")
melhor_solucao <- round(resultado$sol)  # Arredondar para garantir valores inteiros
print(melhor_solucao)  # Vetor de 28 valores (funcionários e encomendas)

print(paste("Melhor lucro obtido:", resultado$eval))  # Avaliação da melhor solução
