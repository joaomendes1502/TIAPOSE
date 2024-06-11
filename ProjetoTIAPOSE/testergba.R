library(GA)
library(genalg)

eval_rgba <- function(s, previsoes_vendas) {
  s <- round(s)  # Arredondar para valores inteiros
  
  vendas_previstas <- previsoes_vendas
  
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
  
  # Esforço (F2)
  month_effort <- effortworkers + effortorders
  
  # Ponderações e constante para combinação dos objetivos
  w1 <- 0.5
  w2 <- 0.5
  K <- 500000 # Pode ajustar esses parâmetros conforme necessário
  
  # Valor objetivo combinado
  objective_value <- -w1 * month_profit + w2 * month_effort * K  # Maximizar lucro e minimizar esforço
  
  return(list(profit = -month_profit))
}

calcular_upper <- function(previsoes_vendas) {
  tipo <- c(4000, 7000, 9500)
  actual_sales_by_dep <- colSums(previsoes_vendas)
  upper <- numeric(28) # Vetor para armazenar os limites superiores
  
  # Calcular limites superiores para os funcionários
  for (i in 1:ncol(previsoes_vendas)) {
    max_sales <- max(previsoes_vendas[[i]])
    if (max_sales != 0) {
      for (j in 1:length(tipo)) {
        upper[(i - 1) * 3 + j] <- ceiling(max_sales / tipo[j])
      }
    }
  }
  
  # Calcular limites superiores para as encomendas
  for (i in 1:ncol(previsoes_vendas)) {
    for (j in 1:4) {
      if (j == 1) {
        upper[12 + (i - 1) * 4 + j] <- sum(previsoes_vendas[, i])
      } else if (j == 2) {
        upper[12 + (i - 1) * 4 + j] <- sum(previsoes_vendas[, i][-1])
      } else if (j == 3) {
        upper[12 + (i - 1) * 4 + j] <- sum(previsoes_vendas[, i][-c(1:2)])
      } else {
        upper[12 + (i - 1) * 4 + j] <- previsoes_vendas[nrow(previsoes_vendas), i]
      }
    }
  }
  
  return(upper)
}

# Função para otimização com RGBA (Real-Valued Genetic Algorithm)
rgba_optimizar <- function(previsoes_vendas) {
  lower <- rep(0, 28)
  upper <- calcular_upper(previsoes_vendas)
  
  # Definir a função de avaliação para o algoritmo genético
  eval_fn_ga <- function(s) {
    res <- eval_rgba(s, previsoes_vendas)
    return(res$profit)
  }
  
  # Configurar e executar o algoritmo genético
  ga_result <- ga(
    type = "real-valued", 
    fitness = function(s) -eval_fn_ga(s),  # GA maximiza a função de fitness, então usamos o negativo
    lower = lower, 
    upper = upper, 
    popSize = 50,          # Tamanho da população
    maxiter = 100,         # Número máximo de iterações
    run = 50,              # Número de iterações sem melhora para parar
    pmutation = 0.2        # Taxa de mutação
  )
  
  melhor_solucao <- round(ga_result@solution)
  melhor_resultado <- eval_rgba(melhor_solucao, previsoes_vendas)
  
  return(list(
    solucao = melhor_solucao, 
    lucro = -melhor_resultado$profit
  ))
}
