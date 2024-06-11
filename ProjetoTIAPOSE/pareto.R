# Bibliotecas necessárias
library(mco)  # Para NSGA-II
library(ggplot2)  # Para gráficos
library(rminer)  # Para séries temporais e geração de dados

# Carregar dados e criar a matriz de vendas previstas
dados <- read.table("walmart.csv", header = TRUE, sep = ",")
vendas_previstas <- dados[(nrow(dados) - 3):nrow(dados), 4:7]  # Últimas 4 semanas
colnames(vendas_previstas) <- c("WSDep1", "WSDep2", "WSDep3", "WSDep4")


eval2 <- function(s) {
  s <- round(s)  # Garantir valores inteiros
  
  
  # Criar a matriz de funcionários e calcular custos
  hired_workers <- matrix(s[1:12], nrow = 3, ncol = 4, byrow = TRUE)
  custo_junior <- 6000
  custo_normal <- 8000
  custo_senior <- 9750
  total_cost_workers <- sum(
    hired_workers[1, ] * custo_junior,
    hired_workers[2, ] * custo_normal,
    hired_workers[3, ] * custo_senior
  )
  
  # Criar a matriz de encomendas e calcular custos
  product_orders <- matrix(s[13:28], nrow = 4, ncol = 4, byrow = TRUE)
  custo_encomenda <- c(6, 8, 9, 11)
  total_cost_orders <- sum(
    product_orders[, 1] * custo_encomenda[1],
    product_orders[, 2] * custo_encomenda[2],
    product_orders[, 3] * custo_encomenda[3],
    product_orders[, 4] * custo_encomenda[4]
  )
  
  # Calcular receita total com vendas efetivas
  capacidade_junior <- 4000
  capacidade_normal <- 7000
  capacidade_senior <- 9500
  worker_max_support_per_dept <- apply(hired_workers, 2, function(col) {
    col[1] * capacidade_junior + col[2] * capacidade_normal + col[3] * capacidade_senior
  })
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
  
  valor_venda <- c(8, 10, 12, 16)
  sales_in_usd <- matrix(0, nrow = 4, ncol = 4)
  for (i in 1:4) {
    sales_in_usd[, i] <- vendas_efetivas[, i] * valor_venda[i]
  }
  
  total_revenue_sales <- sum(sales_in_usd)
  
  # Criar a matriz `stock` para calcular custos de estoque
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
  
  stock_cost_per_product <- c(3, 5, 6, 8)
  stock_costs <- matrix(0, nrow = 4, ncol = 4)
  for (i in 1:4) {
    stock_costs[, i] <- stock[, i] * stock_cost_per_product[i]
  }
  
  total_stock_cost <- sum(stock_costs)
  
  # Calcular lucro total e esforço
  total_costs <- total_cost_workers + total_cost_orders + total_stock_cost
  month_profit <- total_revenue_sales - total_costs
  effort_workers <- sum(hired_workers)
  effort_orders <- sum(product_orders != 0)
  total_effort <- effort_workers + effort_orders
  
  # Retornar vetor com lucro negativo e esforço
  return(c(-month_profit, total_effort))
}

# Configurações para NSGA-II
D <- 28  # Dimensão do problema
popsize <- 100  # Tamanho da população
generations <- 100  # Número de gerações

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

upper <- calcular_upper(vendas_previstas)

# Executar NSGA-II para otimização multi-objetivo
result <- nsga2(
  fn = eval2,
  idim = D,
  odim = 2,  # Dois objetivos: lucro e esforço
  lower.bounds = lower,
  upper.bounds = upper,
  popsize = popsize,
  generations = generations
)

# Obter as soluções da frente de Pareto
pareto_solutions <- result$pareto.optimal

# Criar um dataframe para o gráfico de Pareto
pareto_data <- data.frame(
  lucro = result$value[pareto_solutions, 1],  # Lucro positivo
  esforço = result$value[pareto_solutions, 2]  # Esforço
)

# Criar um gráfico de Pareto usando ggplot2
ggplot(pareto_data, aes(x = lucro, y = esforço)) +
  geom_point() +  # Pontos para soluções na frente de Pareto
  geom_line() +  # Linha conectando as soluções da frente de Pareto
  labs(title = "Curva de Pareto para Lucro e Esforço",
       x = "Lucro",
       y = "Esforço") +
  theme_minimal()  # Tema minimalista para o gráfico


