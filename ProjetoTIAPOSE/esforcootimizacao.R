# Função de avaliação para Monte Carlo com maximização do lucro e minimização do esforço
eval <- function(s) {
  s <- round(s)  # Arredondar para garantir inteiros
  
  # Dados para avaliação
  dados <- read.table("walmart.csv", header = TRUE, sep = ",")
  colunas <- 4:7  # Colunas 4 a 7
  vendas_previstas <- dados[(nrow(dados) - 3):nrow(dados), colunas]
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
  
  # Calcular suporte máximo por departamento
  worker_max_support_per_dept <- apply(hired_workers, 2, function(col) {
    col[1] * capacidade_junior + col[2] * capacidade_normal + col[3] * capacidade_senior
  })
  
  # Calcular vendas efetivas
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
  
  # Receita total em dólares por venda por departamento
  valor_venda <- c(8, 10, 12, 16)
  total_revenue_sales <- sum(vendas_efetivas * outer(rep(1, 4), valor_venda))
  
  # Criar a matriz de estoque
  stock <- matrix(0, nrow = 4, ncol = 4)
  stock_costs <- matrix(0, nrow = 4, ncol = 4)
  stock_cost_per_product <- c(3, 5, 6, 8)
  
  for (s in 1:4) {
    for (d in 1:4) {
      if (s == 1) {
        stock[s, d] <- product_orders[s, d] - vendas_efetivas[s, d]
      } else {
        stock[s, d] <- stock[s - 1, d] + product_orders[s, d] - vendas_efetivas[s, d]
      }
      stock_costs[s, d] <- stock[s, d] * stock_cost_per_product[d]
    }
  }
  
  total_stock_cost <- sum(stock_costs)
  
  # Custos totais (funcionários + encomendas + estoque)
  total_costs <- total_cost_workers + total_cost_orders + total_stock_cost
  
  # Lucro (F1)
  month_profit <- total_revenue_sales - total_costs
  
  # Esforço total (F2)
  effort_workers <- sum(hired_workers)
  effort_orders <- sum(product_orders != 0)
  total_effort <- effort_workers + effort_orders  # Soma do esforço
  
  # Pesos para o lucro e o esforço
  w1 <- 0.5  # Peso para lucro
  w2 <- 0.5  # Peso para esforço
  K <- 500000  # Fator de escala para equilibrar os pesos
  
  # Função composta para otimizar lucro e minimizar esforço
  combined_objective <- -w1 * month_profit + w2 * total_effort * K
  
  # Retornar função composta, lucro e esforço
  return(list(combined_objective = combined_objective, lucro = month_profit, esforço = total_effort))
}

# Limites para Monte Carlo
lower <- rep(0, 28)  # Limites inferiores
upper <- c(
  rep(10, 12),  # Funcionários (máximo 10 por posição)
  rep(100000, 16)  # Encomendas (máximo permitido por semana/departamento)
)

# Número de amostras para Monte Carlo
N <- 1000

# Método Monte Carlo para minimizar a função composta
resultado <- mcsearch(fn = function(s) eval(s)$combined_objective, lower = lower, upper = upper, N = N, type = "min")

# Melhor solução encontrada
print("Melhor solução:")
melhor_solucao <- round(resultado$sol)  # Arredondar para garantir valores inteiros
print(melhor_solucao)

# Melhor valor da função composta
print(paste("Melhor valor da função combinada:", resultado$eval))

# Avaliar o lucro e o esforço associados à melhor solução
avaliacao <- eval(melhor_solucao)

print(paste("Lucro:", avaliacao$lucro))
print(paste("Esforço:", avaliacao$esforço))

