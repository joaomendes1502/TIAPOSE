library(rminer)
library(forecast)

# Define the testModel function
testModel <- function(data, modelo, Runs){
  cat("Modelo: ", deparse(substitute(modelo)), "\n\n")
  
  # Lista para armazenar as matrizes de previsões para cada departamento
  pred_list <- list()
  
  for(i in 4:7) { # Departamento
    d1 <- data[, i]
    
    L <- length(d1)
    K <- 4
    Test <- K 
    S <- 4 
    
    W <- (L - Test) - (Runs-1) * S
    
    timelags <- c(1:4)
    D <- CasesSeries(d1, timelags)
    W2 <- W - max(timelags)
    YR <- diff(range(d1))
    
    # Matriz para armazenar as previsões do departamento atual
    pred_matrix <- matrix(nrow=Runs, ncol=Test)
    
    for (b in 1:Runs) { # Growing window
      H <- holdout(D$y, ratio = Test, mode = "incremental", iter = b, window = W2, increment = S)
      trinit <- H$tr[1]
      M <- fit(y ~ ., D[H$tr,], model = modelo)
      Pred2 <- lforecast(M, D, start = (length(H$tr) + 1), Test)
      
      # Armazenar as previsões na matriz
      pred_matrix[b, ] <- Pred2
    }
    
    # Adicionar a matriz de previsões na lista de previsões
    pred_list[[paste("Departamento", i-3)]] <- pred_matrix
    
  }
  
  #print(pred_list)
  # Transformar a lista de matrizes em um único dataframe
  pred_df <- do.call(cbind, lapply(pred_list, function(mat) as.vector(t(mat))))
  pred_df <- as.data.frame(pred_df)
  colnames(pred_df) <- paste("Departamento", 1:4)
  
  return(pred_df)
}

