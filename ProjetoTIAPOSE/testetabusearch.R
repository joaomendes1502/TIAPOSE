
# Instalar e carregar o pacote tabuSearch
if (!requireNamespace("tabuSearch", quietly = TRUE)) {
  install.packages("tabuSearch")
}
library(tabuSearch)

# Função bin2real para converter binário para decimal
bin2real <- function(x, lower, upper, bits) {
  x_str <- paste(x, collapse = "")
  n <- strtoi(x_str, base = 2)
  real_val <- lower + (upper - lower) * n / (2^bits - 1)
  return(real_val)
}

# Função de avaliação adaptada
eval_function <- function(x, bits, lower, upper, eval_fn, previsoes_vendas) {
  D <- length(lower)
  real_vals <- numeric(D)
  
  for (i in 1:D) {
    real_vals[i] <- bin2real(x[((i - 1) * bits + 1):(i * bits)], lower[i], upper[i], bits)
  }
  
  return(eval_fn(real_vals, previsoes_vendas))
}

# Função de avaliação original
eval_tb <- function(s, previsoes_vendas) {
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
  
  worker_max_support_per_dept <- apply(hired_workers, 2, function(col) {
    col[1] * capacidade_junior + col[2] * capacidade_normal + col[3] * capacidade_senior
  })
  
  calcularVendasEfetivas <- function(vendas_previstas, product_orders, worker_max_support_per_dept) {
    vendasEfetivas <- matrix(0, nrow = nrow(vendas_previstas), ncol = ncol(vendas_previstas))
    
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
    
    return(vendasEfetivas)
  }
  
  sales <- calcularVendasEfetivas(vendas_previstas, product_orders, worker_max_support_per_dept)
  
  valor_venda <- c(8, 10, 12, 16)
  
  sales_in_usd <- matrix(0, nrow = 4, ncol = 4)
  for (i in 1:4) {
    sales_in_usd[, i] <- sales[, i] * valor_venda[i]
  }
  
  total_revenue_sales <- sum(sales_in_usd)
  
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
  
  stock_cost_per_product <- c(3, 5, 6, 8)
  stock_costs <- matrix(0, nrow = 4, ncol = 4)
  for (i in 1:4) {
    stock_costs[, i] <- stock[, i] * stock_cost_per_product[i]
  }
  
  total_stock_cost <- sum(stock_costs)
  
  total_costs <- total_cost_workers + total_cost_orders + total_stock_cost
  
  month_profit <- total_revenue_sales - total_costs
  
  return(month_profit)
}

calcular_upper <- function(previsoes_vendas) {
  tipo <- c(4000, 7000, 9500)
  actual_sales_by_dep <- colSums(previsoes_vendas)
  upper <- numeric(28)
  
  for (i in 1:ncol(previsoes_vendas)) {
    max_sales <- max(previsoes_vendas[[i]])
    if (max_sales != 0) {
      for (j in 1:length(tipo)) {
        upper[(i - 1) * 3 + j] <- ceiling(max_sales / tipo[j])
      }
    }
  }
  
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

# Função para otimização com Tabu Search
tabu_optimizar <- function(previsoes_vendas) {
  lower <- rep(0, 28)
  upper <- calcular_upper(previsoes_vendas)
  
  D <- 28
  BITS <- 8
  size <- D * BITS
  N <- 100
  
  tabu_eval <- function(x) {
    eval_function(x, BITS, lower, upper, function(s, previsoes_vendas) eval_tb(s, previsoes_vendas), previsoes_vendas)
  }
  
  set.seed(123)
  s <- tabuSearch(size, iters = N, objFunc = tabu_eval, verbose = TRUE)
  
  best_index <- which.max(s$eUtilityKeep)
  best_solution_binary <- s$configKeep[best_index, ]
  
  best_solution_decimal <- numeric(D)
  for (i in 1:D) {
    best_solution_decimal[i] <- bin2real(best_solution_binary[((i - 1) * BITS + 1):(i * BITS)], lower[i], upper[i], BITS)
  }
  
  melhor_lucro <- max(s$eUtilityKeep)
  
  return(list(solucao = round(best_solution_decimal), lucro = melhor_lucro))
}

