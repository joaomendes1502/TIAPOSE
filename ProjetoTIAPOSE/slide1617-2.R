eval <- function(s) {
  # Arredondar para garantir valores inteiros
  s <- round(s)
  
  dados <- read.table("walmart.csv", header = TRUE, sep = ",")
  colunas <- 4:7  # Colunas 4 a 7
  vendas_previstas <- dados[(nrow(dados) - 3):nrow(dados), colunas]  # Correção no índice
  colnames(vendas_previstas) <- c("WSDep1", "WSDep2", "WSDep3", "WSDep4")
  print("Vendas previstas para as últimas quatro semanas:")
  print(vendas_previstas)
  
  # Criar a matriz `hired_workers`
  hired_workers <- matrix(s[1:12], nrow = 3, ncol = 4, byrow = TRUE)
  
  # Custos por tipo de funcionário
  custo_junior <- 6000
  custo_normal <- 8000
  custo_senior <- 9750
  
  # Calcular o custo total dos trabalhadores
  total_cost_workers <- sum(
    hired_workers[1, ] * custo_junior,
    hired_workers[2, ] * custo_normal,
    hired_workers[3, ] * custo_senior
  )
  
  # Criar a matriz `product_orders`
  product_orders <- matrix(s[13:28], nrow = 4, ncol = 4, byrow = TRUE)
  
  # Custos das encomendas por departamento
  custo_encomenda <- c(6, 8, 9, 11)
  total_cost_orders <- sum(
    product_orders[, 1] * custo_encomenda[1],
    product_orders[, 2] * custo_encomenda[2],
    product_orders[, 3] * custo_encomenda[3],
    product_orders[, 4] * custo_encomenda[4]
  )
  
  # Capacidade máxima por departamento
  capacidade_junior <- 4000
  capacidade_normal <- 7000
  capacidade_senior <- 9500
  
  worker_max_support_per_dept <- apply(hired_workers, 2, function(col) {
    col[1] * capacidade_junior + col[2] * capacidade_normal + col[3] * capacidade_senior
  })
  
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
  
  # Valores em dólares por departamento
  valor_venda <- c(8, 10, 12, 16)
  
  # Criar a matriz `sales in USD`
  sales_in_usd <- matrix(0, nrow = 4, ncol = 4)
  for (i in 1:4) {
    sales_in_usd[, i] <- sales[, i] * valor_venda[i]
  }
  
  # Calcular a receita total
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
  
  # Calcular o custo total do estoque
  total_stock_cost <- sum(stock_costs)
  
  # Calcular o custo total
  total_costs <- total_cost_workers + total_cost_orders + total_stock_cost
  
  # Calcular o lucro (F1)
  month_profit <- total_revenue_sales - total_costs
  
  # Calcular o esforço total (F2)
  total_workers <- sum(hired_workers)
  total_orders <- sum(product_orders != 0)
  month_effort <- total_workers + total_orders
  
  # Retornar o lucro (F1)
  return(month_profit)
}

# Solução de teste
s1 <- c(5, 4, 3, 2, 6, 5, 4, 3, 7, 6, 5, 4, 61662, 78292, 56434, 24182, 0, 0, 0, 0, 12985, 55403, 69133, 37167, 39924, 75160, 62131, 99708)

# Testar a função `eval`
profit <- eval(s1)

print(paste("Lucro para a solução s1:", profit))

