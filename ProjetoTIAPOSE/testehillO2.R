source("hill.R")

rchange1 <- function(par, lower, upper) {
  hchange(par, lower = lower, upper = upper, rnorm, mean = 1, sd = 0.2, round = TRUE)  # Small changes
}

# Updated evaluation function considering profit and effort
eval_fn <- function(s, previsoes_vendas) {
  s <- round(s)  # Ensure values are integers
  
  # Using provided forecasts
  vendas_previstas <- previsoes_vendas
  
  # Create the workers matrix
  hired_workers <- matrix(s[1:12], nrow = 3, ncol = 4, byrow = TRUE)
  
  # Costs per worker type
  custo_junior <- 6000
  custo_normal <- 8000
  custo_senior <- 9750
  
  # Total cost of workers
  total_cost_workers <- sum(
    hired_workers[1, ] * custo_junior,
    hired_workers[2, ] * custo_normal,
    hired_workers[3, ] * custo_senior
  )
  
  # Create the orders matrix
  product_orders <- matrix(s[13:28], nrow = 4, ncol = 4, byrow = TRUE)
  
  # Order costs per department
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
  
  # Calculate effort as the total number of workers hired
  total_workers <- sum(hired_workers)
  
  # Composite score (profit - weight * effort)
  weight <- 0.01  # Adjust the weight as needed
  composite_score <- month_profit - (weight * total_workers)
  
  return(composite_score)
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

hillo2_optimizar <- function(previsoes_vendas) {
  lower <- rep(0, 28)
  upper <- calcular_upper(previsoes_vendas)
  
  control <- list(maxit = 5000, REPORT = 100)
  
  resultado <- hclimbing(
    par = runif(28, lower, upper),
    fn = function(s) eval_fn(s, previsoes_vendas),
    change = rchange1,
    lower = lower,
    upper = upper,
    control = control,
    type = "max"
  )
  
  melhor_solucao <- round(resultado$sol)
  melhor_lucro <- resultado$eval
  
  return(list(solucao = melhor_solucao, lucro = melhor_lucro))
}
