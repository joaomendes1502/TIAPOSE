dados <- read.table("walmart.csv", header = TRUE, sep = ",")
colunas <- 4:7  # Colunas 4 a 7
vendas_previstas <- dados[(nrow(dados) - 3):nrow(dados), colunas]  # Correção no índice
colnames(vendas_previstas) <- c("WSDep1", "WSDep2", "WSDep3", "WSDep4")
print("Vendas previstas para as últimas quatro semanas:")
print(vendas_previstas)

# Solução fornecida
s1 <- c(5, 4, 3, 2, 6, 5, 4, 3, 7, 6, 5, 4)

# Criar a matriz `hired_workers`
hired_workers <- matrix(s1, nrow = 3, ncol = 4, byrow = TRUE)

# Definir nomes das colunas e linhas
colnames(hired_workers) <- c("WSDep1", "WSDep2", "WSDep3", "WSDep4")
rownames(hired_workers) <- c("junior", "normal", "senior")

# Imprimir a matriz `hired_workers`
print("Matriz hired_workers:")
print(hired_workers)

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

print(paste("Total Cost of Workers:", total_cost_workers)) 

# Calcular o esforço total (total de trabalhadores)
effortworkers <- sum(hired_workers)

print(paste("Total Number of Workers (Effort):", effortworkers))  # Deve ser 54

# Capacidade máxima por departamento
capacidade_junior <- 4000
capacidade_normal <- 7000
capacidade_senior <- 9500

worker_max_support_per_dept <- apply(hired_workers, 2, function(col) {
  col[1] * capacidade_junior + col[2] * capacidade_normal + col[3] * capacidade_senior
})

print("Worker Max Support per Department:")
print(worker_max_support_per_dept)  # Deve dar os valores máximos de suporte por departamento


# Dados para a matriz `product_orders`
values <- c(61662, 78292, 56434, 24182, 0, 0, 0, 0, 12985, 55403, 69133, 37167, 39924, 75160, 62131, 99708)

# Criar a matriz `product_orders` (4 semanas x 4 departamentos)
product_orders <- matrix(values, nrow = 4, ncol = 4, byrow = TRUE)

# Definir os nomes das colunas (departamentos) e linhas (semanas)
colnames(product_orders) <- c("WSDep1", "WSDep2", "WSDep3", "WSDep4")
rownames(product_orders) <- paste("Semana", 1:4)

# Imprimir a matriz `product_orders`
print("Matriz product_orders:")
print(product_orders)

# Custos das encomendas por departamento (USD por produto)
custo_encomenda <- c(6, 8, 9, 11)  # Para cada departamento

# Calcular o custo total das encomendas
total_cost_orders <- sum(
  product_orders[, 1] * custo_encomenda[1],  # WSDep1
  product_orders[, 2] * custo_encomenda[2],  # WSDep2
  product_orders[, 3] * custo_encomenda[3],  # WSDep3
  product_orders[, 4] * custo_encomenda[4]   # WSDep4
)

print(paste("Total Cost of Orders:", total_cost_orders))  # Deve ser 5819175

effortorders <- sum(product_orders != 0)  # Contar valores não-zero

print(paste("Total Number of Orders (Effort):", effortorders))  # Deve ser 12

# Função para calcular vendas efetivas
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

# Definir nomes das colunas e linhas para a matriz `sales`
colnames(sales) <- c("WSDep1", "WSDep2", "WSDep3", "WSDep4")
rownames(sales) <- paste("Semana", 1:4)

print("Matriz sales:")
print(sales)

# Valores em dólares por venda por departamento
valor_venda <- c(8, 10, 12, 16)

# Criar a matriz `sales in USD`
sales_in_usd <- matrix(0, nrow = 4, ncol = 4)
for (i in 1:4) {
  sales_in_usd[, i] <- sales[, i] * valor_venda[i]
}

colnames(sales_in_usd) <- colnames(sales)
rownames(sales_in_usd) <- rownames(sales)

print("Matriz sales in USD:")
print(sales_in_usd)

# Calcular a receita total (total revenue sales)
total_revenue_sales <- sum(sales_in_usd)  # Soma de todos os valores na matriz `sales in USD`

print(paste("Total Revenue Sales:", total_revenue_sales))

stock <- matrix(0, nrow = 4, ncol = 4)  # Estoque inicialmente zero

# Calcular o estoque para cada semana e departamento
for (s in 1:4) {
  for (d in 1:4) {
    if (s == 1) {
      # Para a primeira semana, só se consideram as encomendas e vendas
      stock[s, d] <- product_orders[s, d] - sales[s, d]
    } else {
      # Para as demais semanas, o estoque é o estoque anterior + encomendas - vendas
      stock[s, d] = stock[s - 1, d] + product_orders[s, d] - sales[s, d]
    }
  }
}

# Definir nomes das colunas e linhas
colnames(stock) <- c("WSDep1", "WSDep2", "WSDep3", "WSDep4")
rownames(stock) <- paste("Semana", 1:4)

# Imprimir a matriz `stock`
print("Matriz stock:")
print(stock)

# Custos do estoque por departamento (USD por produto)
stock_cost_per_product <- c(3, 5, 6, 8)  # Para WSDep1, WSDep2, WSDep3, WSDep4

# Criar a matriz `stock costs` com base no estoque e nos custos por departamento
stock_costs <- matrix(0, nrow = 4, ncol = 4)
for (i in 1:4) {
  stock_costs[, i] <- stock[, i] * stock_cost_per_product[i]
}

# Definir nomes das colunas e linhas
colnames(stock_costs) <- c("WSDep1", "WSDep2", "WSDep3", "WSDep4")
rownames(stock_costs) <- paste("Semana", 1:4)

# Imprimir a matriz `stock costs`
print("Matriz stock costs:")
print(stock_costs)

# Calcular o custo total do estoque
total_stock_cost <- sum(stock_costs)

print(paste("Total Stock Cost:", total_stock_cost))

# Calcular o custo total (funcionários + encomendas + estoque)
total_costs <- total_cost_workers + total_cost_orders + total_stock_cost

# Imprimir o custo total
print(paste("Total Costs:", total_costs))

month_profit <- total_revenue_sales - total_costs
month_effort <- effortworkers + effortorders

print(paste("F1 (Month Profit):", month_profit))
print(paste("F2 (Month Effort):", month_effort))


