library(shiny)
library(rminer)
library(forecast)
library(randomForest)
library(nnet)

# Função genérica para previsões com diferentes modelos
testModel <- function(data, modelo, Runs) {
  cat("Modelo: ", deparse(substitute(modelo)), "\n\n")
  
  pred_list <- list()
  nmae_list <- list()
  
  for (i in 4:7) { # Departamento
    d1 <- data[, i]
    
    L <- length(d1)
    K <- 4
    Test <- K 
    S <- 4 
    
    W <- (L - Test) - (Runs - 1) * S
    timelags <- c(1:4)
    YR <- diff(range(d1))
    
    pred_matrix <- matrix(nrow = Runs, ncol = Test)
    nmae_vector <- vector(length = Runs)
    
    if (modelo %in% c("auto.arima", "ets", "HoltWinters")) {
      for (b in 1:Runs) {
        H <- holdout(d1, ratio = Test, mode = "incremental", iter = b, window = W, increment = S)
        LTR <- length(H$tr)
        dtr <- ts(d1[H$tr], frequency = K)
        
        if (modelo == "auto.arima") {
          M <- suppressWarnings(auto.arima(dtr))
        } else if (modelo == "ets") {
          M <- suppressWarnings(ets(dtr))
        } else if (modelo == "HoltWinters") {
          M <- suppressWarnings(HoltWinters(dtr))
        }
        
        Pred <- forecast(M, h = length(H$ts))$mean[1:Test]
        nmae_vector[b] <- mmetric(d1[H$ts], Pred, metric = "NMAE", val = YR)
        
        pred_matrix[b, ] <- Pred
      }
    } else {
      D <- CasesSeries(d1, timelags)
      W2 <- W - max(timelags)
      
      for (b in 1:Runs) {
        H <- holdout(D$y, ratio = Test, mode = "incremental", iter = b, window = W2, increment = S)
        trinit <- H$tr[1]
        M <- fit(y ~ ., D[H$tr,], model = modelo)
        Pred2 <- lforecast(M, D, start = (length(H$tr) + 1), Test)
        nmae_vector[b] <- mmetric(d1[H$ts], Pred2, metric = "NMAE", val = YR)
        
        pred_matrix[b, ] <- Pred2
      }
    }
    
    pred_list[[paste("Departamento", i - 3)]] <- pred_matrix
    nmae_list[[paste("Departamento", i - 3)]] <- nmae_vector
  }
  
  return(list(predictions = pred_list, nmae = nmae_list))
}

ui <- fluidPage(
  titlePanel("Previsões de Vendas do Walmart"),
  tabsetPanel(
    tabPanel("Previsão",
             sidebarLayout(
               sidebarPanel(
                 selectInput("modelo", "Escolha o modelo:", 
                             choices = c("Random Forest" = "randomForest",
                                         "ARIMA" = "auto.arima",
                                         "ETS" = "ets",
                                         "Holt-Winters" = "HoltWinters",
                                         "LM (Recomendado)" = "lm",
                                         "MLP" = "mlpe")),
                 numericInput("runs", "Número de meses a prever:", value = 1, min = 1, max = 10),
                 actionButton("executar", "Executar"),
                 uiOutput("mes_selector")
               ),
               mainPanel(
                 tableOutput("valores_previstos"),
                 tableOutput("valores_erros")
               )
             )
    ),
    tabPanel("Otimização",
             sidebarLayout(
               sidebarPanel(
                 selectInput("metodo_otimizacao", "Método de Otimização:", 
                             choices = c("Monte Carlo" = "montecarlo", 
                                         "Hill Climbing" = "hill", 
                                         "Tabu Search (Recomendado)" = "tabu",
                                         "Differential Evolution" = "de",
                                         "RGBA" = "rgba"
                                         )),
                 selectInput("tipo_otimizacao", "Tipo de Otimização:", 
                             choices = c("Maximizar o Lucro" = "O1", 
                                         "Maximizar o Lucro e Minimizar o Esforço" = "O2")),
                 uiOutput("mes_selector_otim"),
                 actionButton("executar_otimizacao", "Executar Otimização")
               ),
               mainPanel(
                 tableOutput("matriz_otimizacao"),
                 tableOutput("resultados_otimizacao"),
                 tableOutput("mediana_lucros"),
                 tableOutput("melhor_solucao_empregados"),
                 tableOutput("melhor_solucao_encomendas"),
                 tableOutput("custo_empregados"),
                 tableOutput("custo_encomendas"),
                 tableOutput("vendas_efetivas"),
                 tableOutput("sales_in_usd"),
                 textOutput("total_revenue_sales"),
                 tableOutput("stock"),                
                 tableOutput("stock_costs"),           
                 textOutput("total_stock_cost") 
               )
             )
    )
  )
)

# Lógica do servidor
server <- function(input, output, session) {
  valores <- reactiveValues(predictions = NULL, nmae = NULL, matriz_selecionada = NULL, lucros = NULL, solucoes = NULL, vendas_efetivas = NULL, sales_in_usd = NULL, total_revenue_sales = NULL, stock = NULL, 
                            stock_costs = NULL, 
                            total_stock_cost = NULL)
  
  observeEvent(input$executar, {
    # Carregar dados
    file_path <- "walmart.csv"
    if (!file.exists(file_path)) {
      output$valores_previstos <- renderTable({
        data.frame(Erro = "Arquivo não encontrado")
      })
      return()
    }
    data <- read.csv(file_path)
    
    modelo <- switch(input$modelo,
                     "randomForest" = "randomForest",
                     "auto.arima" = "auto.arima",
                     "ets" = "ets",
                     "HoltWinters" = "HoltWinters",
                     "lm" = "lm",
                     "mlpe" = "mlpe")
    
    results <- testModel(data, modelo, input$runs)
    
    valores$predictions <- results$predictions
    valores$nmae <- results$nmae
    
    output$mes_selector <- renderUI({
      numericInput("mes", "Selecionar Mês:", value = 1, min = 1, max = input$runs)
    })
    
    output$mes_selector_otim <- renderUI({
      numericInput("mes_otim", "Selecionar Mês:", value = 1, min = 1, max = input$runs)
    })
  })
  
  observeEvent(input$mes, {
    if (!is.null(valores$predictions) && !is.null(valores$nmae)) {
      mes <- input$mes
      if (mes > input$runs) {
        output$valores_previstos <- renderTable({
          data.frame(Erro = "Mês selecionado excede o número de iterações")
        })
        output$valores_erros <- renderTable({
          data.frame(Erro = "Mês selecionado excede o número de iterações")
        })
        return()
      }
      
      predictions <- lapply(valores$predictions, function(mat) mat[mes, , drop = FALSE])
      pred_df <- do.call(cbind, lapply(predictions, function(mat) as.vector(t(mat))))
      pred_df <- as.data.frame(pred_df)
      pred_df <- cbind(Semana = paste("Semana", 1:4), pred_df)
      colnames(pred_df) <- c("Semana", paste("Departamento", 1:4))
      
      output$valores_previstos <- renderUI({
        tagList(
          h3("Vendas Previstas"),
          renderTable({
            pred_df
          })
        )
      })
      
      nmae_df <- as.data.frame(t(sapply(valores$nmae, function(vec) vec[mes])))
      colnames(nmae_df) <- paste("Departamento", 1:4)
      nmae_df <- cbind(Métrica = "NMAE", nmae_df)  # Adiciona a coluna "Métrica" com o valor "NMAE"
      
      output$valores_erros <- renderTable({
        nmae_df
      }, rownames = FALSE)
      
      valores$matriz_selecionada <- pred_df
    }
  })
  
  observeEvent(input$mes_otim, {
    if (!is.null(valores$predictions)) {
      mes_otim <- input$mes_otim
      if (mes_otim > input$runs) {
        output$matriz_otimizacao <- renderTable({
          data.frame(Erro = "Mês selecionado excede o número de iterações")
        })
        return()
      }
      
      predictions <- lapply(valores$predictions, function(mat) mat[mes_otim, , drop = FALSE])
      pred_df <- do.call(cbind, lapply(predictions, function(mat) as.vector(t(mat))))
      pred_df <- as.data.frame(pred_df)
      pred_df <- cbind(Semana = paste("Semana", 1:4), pred_df)
      colnames(pred_df) <- c("Semana", paste("Departamento", 1:4))
      
      output$matriz_otimizacao <- renderUI({
        tagList(
          h3("Vendas Previstas"),
          renderTable({
            pred_df
          })
        )
      })
    }
  })
  
  observeEvent(input$executar_otimizacao, {
    if (!is.null(valores$predictions)) {
      source("testemontecarlo.R")
      source("testehill.R")
      source("testesann.R")
      source("testedifferentialevo.R")
      source("testetabusearch.R")
      source("testergba.R")
      source("testemontecarloO2.R")
      source("testedifferentialevoO2.R")
      source("testehillO2.R")
      source("testetabusearchO2.R")
      source("testergbaO2.R")
      metodo <- input$metodo_otimizacao
      Runs <- input$runs
      
      lucros <- numeric(Runs)
      solucoes <- vector("list", Runs)
      esforços <- numeric(Runs)
      vendas_efetivas <- vector("list", Runs)
      sales_in_usd <- vector("list", Runs)
      total_revenue_sales <- numeric(Runs)
      stock <- vector("list", Runs)
      stock_costs <- vector("list", Runs)
      total_stock_cost <- numeric(Runs)
      
      
      for (b in 1:Runs) {
        predictions <- lapply(valores$predictions, function(mat) mat[b, , drop = FALSE])
        pred_df <- do.call(cbind, lapply(predictions, function(mat) as.vector(t(mat))))
        previsoes_vendas <- as.matrix(pred_df)
        
        cat("Iniciando a iteração", b, "com os dados da matriz:\n")
        print(previsoes_vendas)
        
        # Atualize a função de otimização para aceitar previsoes_vendas como argumento
        if (metodo == "montecarlo" && input$tipo_otimizacao == "O1") {
          resultado <- montecarlo_optimizar(previsoes_vendas)
        } else if (metodo == "montecarlo" && input$tipo_otimizacao == "O2") {
          resultado <- montecarloo2_optimizar(previsoes_vendas)
        } else if (metodo == "hill" && input$tipo_otimizacao == "O1") {
          resultado <- hill_optimizar(previsoes_vendas)
        } else if (metodo == "hill" && input$tipo_otimizacao == "O2") {
          resultado <- hillo2_optimizar(previsoes_vendas)
        } else if (metodo == "tabu" && input$tipo_otimizacao == "O1") {
          resultado <- tabu_optimizar(previsoes_vendas)
        } else if (metodo == "tabu" && input$tipo_otimizacao == "O2") {
          resultado <- tabuo2_optimizar(previsoes_vendas)
        } else if (metodo == "de" && input$tipo_otimizacao == "O1") {
          resultado <- de_optimizar(previsoes_vendas)
        } else if (metodo == "de" && input$tipo_otimizacao == "O2") {
          resultado <- deo2_optimizar(previsoes_vendas)
        } else if (metodo == "rgba" && input$tipo_otimizacao == "O1") {
          resultado <- rgba_optimizar(previsoes_vendas)
        } else if (metodo == "rgba" && input$tipo_otimizacao == "O2") {
          resultado <- rgbao2_optimizar(previsoes_vendas)
        } else {
          stop("Método desconhecido.")
        }
        
        lucros[b] <- resultado$lucro
        solucoes[[b]] <- resultado$solucao
        
        # Calculate monthly effort
        hired_workers <- matrix(resultado$solucao[1:12], nrow = 3, ncol = 4, byrow = TRUE)
        product_orders <- matrix(resultado$solucao[13:28], nrow = 4, ncol = 4, byrow = TRUE)
        effortorders <- sum(product_orders != 0)
        effortworkers <- sum(hired_workers)
        esforços[b] <- effortworkers + effortorders
        
        vendas_previstas <- previsoes_vendas
        
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
        vendas_efetivas[[b]] <- sales
        
        valor_venda <- c(8, 10, 12, 16)
        
        sales_in_usd_b <- matrix(0, nrow = 4, ncol = 4)
        for (i in 1:4) {
          sales_in_usd_b[, i] <- sales[, i] * valor_venda[i]
        }
        
        total_revenue_sales_b <- sum(sales_in_usd_b)
        
        sales_in_usd[[b]] <- sales_in_usd_b
        total_revenue_sales[b] <- total_revenue_sales_b
        
        stock_b <- matrix(0, nrow = 4, ncol = 4)
        for (s in 1:4) {
          for (d in 1:4) {
            if (s == 1) {
              stock_b[s, d] <- product_orders[s, d] - sales[s, d]
            } else {
              stock_b[s, d] <- stock_b[s - 1, d] + product_orders[s, d] - sales[s, d]
            }
          }
        }
        
        stock_cost_per_product <- c(3, 5, 6, 8)
        stock_costs_b <- matrix(0, nrow = 4, ncol = 4)
        for (i in 1:4) {
          stock_costs_b[, i] <- stock_b[, i] * stock_cost_per_product[i]
        }
        
        total_stock_cost_b <- sum(stock_costs_b)
        
        stock[[b]] <- stock_b
        stock_costs[[b]] <- stock_costs_b
        total_stock_cost[b] <- total_stock_cost_b
        
      }
      
      valores$lucros <- lucros
      valores$solucoes <- solucoes
      valores$esforços <- esforços
      valores$vendas_efetivas <- vendas_efetivas
      valores$sales_in_usd <- sales_in_usd
      valores$total_revenue_sales <- total_revenue_sales
      valores$stock <- stock
      valores$stock_costs <- stock_costs
      valores$total_stock_cost <- total_stock_cost
      
      output$resultados_otimizacao <- renderTable({
        if (length(lucros) > 0 && length(solucoes) > 0) {
          data.frame(Iteracao = 1:Runs, Lucro = lucros, Esforço = esforços, Solucao = sapply(solucoes, function(x) ifelse(length(x) > 0, paste(x, collapse = ", "), "Nenhum resultado disponível")))
        } else {
          data.frame(Erro = "Nenhum resultado disponível")
        }
      })
      
      output$mediana_lucros <- renderTable({
        if (length(lucros) > 0) {
          data.frame(Mediana_Lucros = median(lucros), Mediana_Esforço = median(esforços))
        } else {
          data.frame(Erro = "Nenhum lucro disponível para calcular a mediana")
        }
      })
      
      output$custo_empregados <- renderUI({
        tagList(
          h3("Custo de contratação de funcionários por departamento"),
          renderTable({
            if (!is.null(valores$solucoes)) {
              mes_otim <- input$mes_otim
              if (mes_otim <= length(valores$solucoes)) {
                custo_junior <- 6000
                custo_normal <- 8000
                custo_senior <- 9750
                
                hired_workers <- matrix(valores$solucoes[[mes_otim]][1:12], nrow = 3, ncol = 4, byrow = TRUE)
                total_cost_per_dept <- colSums(hired_workers * c(custo_junior, custo_normal, custo_senior))
                total_cost <- sum(total_cost_per_dept)
                
                df_costs <- data.frame(
                  Departamento = paste("Departamento", 1:4),
                  Custo_Junior = hired_workers[1, ] * custo_junior,
                  Custo_Normal = hired_workers[2, ] * custo_normal,
                  Custo_Senior = hired_workers[3, ] * custo_senior,
                  Total = total_cost_per_dept
                )
                
                df_costs <- rbind(df_costs, c("Total", NA, NA, NA, total_cost))
                return(df_costs)
              } else {
                data.frame(Erro = "Mês selecionado excede o número de soluções disponíveis")
              }
            } else {
              data.frame(Erro = "Nenhuma solução disponível")
            }
          })
        )
      })
      
      output$custo_encomendas <- renderUI({
        tagList(
          h3("Custo de encomendas por departamento"),
          renderTable({
            if (!is.null(valores$solucoes)) {
              mes_otim <- input$mes_otim
              if (mes_otim <= length(valores$solucoes)) {
                custo_encomenda <- c(6, 8, 9, 11)
                
                product_orders <- matrix(valores$solucoes[[mes_otim]][13:28], nrow = 4, ncol = 4, byrow = TRUE)
                
                # Calcular o custo por departamento e semana
                custo_por_semana <- t(apply(product_orders, 1, function(row) row * custo_encomenda))
                total_cost_per_dept <- colSums(custo_por_semana)
                total_cost <- sum(total_cost_per_dept)
                
                df_costs <- data.frame(
                  Departamento = paste("Departamento", 1:4),
                  Custo_Semana_1 = custo_por_semana[1, ],
                  Custo_Semana_2 = custo_por_semana[2, ],
                  Custo_Semana_3 = custo_por_semana[3, ],
                  Custo_Semana_4 = custo_por_semana[4, ],
                  Total = total_cost_per_dept
                )
                df_costs <- rbind(df_costs, c("Total", NA, NA, NA, NA, total_cost))
                return(df_costs)
              } else {
                data.frame(Erro = "Mês selecionado excede o número de soluções disponíveis")
              }
            } else {
              data.frame(Erro = "Nenhuma solução disponível")
            }
          })
        )
      })
      
      output$vendas_efetivas <- renderUI({
        tagList(
          h3("Vendas efetivas por semana"),
          renderTable({
            if (!is.null(valores$vendas_efetivas)) {
              mes_otim <- input$mes_otim
              if (mes_otim <= length(valores$vendas_efetivas)) {
                sales_df <- as.data.frame(valores$vendas_efetivas[[mes_otim]])
                colnames(sales_df) <- paste("Departamento", 1:4)
                sales_df <- cbind(Semana = paste("Semana", 1:4), sales_df)
                return(sales_df)
              } else {
                data.frame(Erro = "Mês selecionado excede o número de soluções disponíveis")
              }
            } else {
              data.frame(Erro = "Nenhuma solução disponível")
            }
          }, rownames = FALSE) # Especifica para não mostrar nomes das linhas, pois a coluna "Semana" já os cobre
        )
      })
      
      output$sales_in_usd <- renderUI({
        tagList(
          h3("Receitas das vendas efetivas por semana"),
          renderTable({
            if (!is.null(valores$sales_in_usd)) {
              mes_otim <- input$mes_otim
              if (mes_otim <= length(valores$sales_in_usd)) {
                sales_df <- as.data.frame(valores$sales_in_usd[[mes_otim]])
                colnames(sales_df) <- paste("Departamento", 1:4)
                sales_df <- cbind(Semana = paste("Semana", 1:4), sales_df)
                return(sales_df)
              } else {
                data.frame(Erro = "Mês selecionado excede o número de soluções disponíveis")
              }
            } else {
              data.frame(Erro = "Nenhuma venda em USD disponível")
            }
          }, rownames = FALSE) # Especifica para não mostrar nomes das linhas, pois a coluna "Semana" já os cobre
        )
      })
      
      output$total_revenue_sales <- renderText({
        if (!is.null(valores$total_revenue_sales)) {
          return(paste("Total de receita obtida no mês: $", valores$total_revenue_sales[input$mes_otim]))
        } else {
          "Nenhuma receita total disponível"
        }
      })
      
      output$stock <- renderUI({
        tagList(
          h3("Total de stock retido por semana"),
          renderTable({
            if (!is.null(valores$stock)) {
              mes_otim <- input$mes_otim
              if (mes_otim <= length(valores$stock)) {
                stock_df <- as.data.frame(valores$stock[[mes_otim]])
                colnames(stock_df) <- paste("Departamento", 1:4)
                stock_df <- cbind(Semana = paste("Semana", 1:4), stock_df)
                return(stock_df)
              } else {
                data.frame(Erro = "Mês selecionado excede o número de soluções disponíveis")
              }
            } else {
              data.frame(Erro = "Nenhum stock disponível")
            }
          }, rownames = FALSE) # Especifica para não mostrar nomes das linhas, pois a coluna "Semana" já os cobre
        )
      })
      
      output$stock_costs <- renderUI({
        tagList(
          h3("Total de custos relativos ao stock"),
          renderTable({
            if (!is.null(valores$stock_costs)) {
              mes_otim <- input$mes_otim
              if (mes_otim <= length(valores$stock_costs)) {
                stock_costs_df <- as.data.frame(valores$stock_costs[[mes_otim]])
                colnames(stock_costs_df) <- paste("Departamento", 1:4)
                stock_costs_df <- cbind(Semana = paste("Semana", 1:nrow(stock_costs_df)), stock_costs_df)
                return(stock_costs_df)
              } else {
                data.frame(Erro = "Mês selecionado excede o número de soluções disponíveis")
              }
            } else {
              data.frame(Erro = "Nenhum custo de stock disponível")
            }
          }, rownames = FALSE) # Especifica para não mostrar nomes das linhas, pois a coluna "Semana" já os cobre
        )
      })
      
      output$total_stock_cost <- renderText({
        if (!is.null(valores$total_stock_cost)) {
          return(paste("Custo Total de Stock: $", valores$total_stock_cost[input$mes_otim]))
        } else {
          "Nenhum custo total de stock disponível"
        }
      })
      
    }
  })
  
  output$numero_empregados <- renderTable({
    if (!is.null(valores$solucoes)) {
      mes_otim <- input$mes_otim
      if (mes_otim <= length(valores$solucoes)) {
        hired_workers <- matrix(valores$solucoes[[mes_otim]][1:12], nrow = 3, ncol = 4, byrow = TRUE)
        df_workers <- data.frame(
          Tipo = c("Junior", "Normal", "Senior"),
          `Departamento 1` = hired_workers[, 1],
          `Departamento 2` = hired_workers[, 2],
          `Departamento 3` = hired_workers[, 3],
          `Departamento 4` = hired_workers[, 4]
        )
        return(df_workers)
      } else {
        data.frame(Erro = "Mês selecionado excede o número de soluções disponíveis")
      }
    } else {
      data.frame(Erro = "Nenhuma solução disponível")
    }
  })
  
  observeEvent(input$mes_otim, {
    if (!is.null(valores$predictions) && !is.null(valores$lucros) && !is.null(valores$solucoes)) {
      mes_otim <- input$mes_otim
      if (mes_otim > input$runs) {
        output$matriz_otimizacao <- renderTable({
          data.frame(Erro = "Mês selecionado excede o número de iterações")
        })
        return()
      }
      
      predictions <- lapply(valores$predictions, function(mat) mat[mes_otim, , drop = FALSE])
      pred_df <- do.call(cbind, lapply(predictions, function(mat) as.vector(t(mat))))
      pred_df <- as.data.frame(pred_df)
      pred_df <- cbind(Semana = paste("Semana", 1:4), pred_df)
      colnames(pred_df) <- c("Semana", paste("Departamento", 1:4))
      
      output$matriz_otimizacao <- renderUI({
        tagList(
          h3("Vendas Previstas"),
          renderTable({
            pred_df
          })
        )
      })
      
      output$resultados_otimizacao <- renderTable({
        if (length(valores$lucros) >= mes_otim && length(valores$solucoes) >= mes_otim) {
          data.frame(Iteracao = mes_otim, Lucro = valores$lucros[mes_otim], Esforço = valores$esforços[mes_otim], Solucao = paste(valores$solucoes[[mes_otim]], collapse = ", "))
        } else {
          data.frame(Erro = "Nenhum resultado disponível para a iteração selecionada")
        }
      })
      
      output$melhor_solucao_empregados <- renderUI({
        tagList(
          h3("Número de funcionários necessários para cada departamento"),
          renderTable({
            if (length(valores$solucoes) >= mes_otim) {
              solucao <- matrix(unlist(valores$solucoes[[mes_otim]]), ncol = 4, byrow = TRUE)
              empregados <- solucao[1:3, ]
              empregados <- cbind(Tipo = c("Junior", "Normal", "Senior"), empregados)
              colnames(empregados)[-1] <- paste("Departamento", 1:4) # Atualiza os nomes das colunas, exceto a primeira
              return(empregados)
            } else {
              data.frame(Erro = "Nenhuma solução disponível")
            }
          }, rownames = FALSE) # Especifica para não mostrar nomes das linhas, pois a coluna "Tipo" já os cobre
        )
      })
      
      output$melhor_solucao_encomendas <- renderUI({
        tagList(
          h3("Número de encomendas necessárias para cada departamento"),
          renderTable({
            if (length(valores$solucoes) >= mes_otim) {
              solucao <- matrix(unlist(valores$solucoes[[mes_otim]]), ncol = 4, byrow = TRUE)
              encomendas <- solucao[4:7, ]
              encomendas <- cbind(Semana = paste("Semana", 1:4), encomendas)
              colnames(encomendas)[-1] <- paste("Departamento", 1:4) # Atualiza os nomes das colunas, exceto a primeira
              return(encomendas)
            } else {
              data.frame(Erro = "Nenhuma solução disponível")
            }
          }, rownames = FALSE) # Especifica para não mostrar nomes das linhas, pois a coluna "Semana" já os cobre
        )
      })
    }
  })
}

# Rodar a aplicação
shinyApp(ui = ui, server = server)

    