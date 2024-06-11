load_and_filter_data <- function(filepath, ano, mes) {
  library(lubridate)
  
  # Carregar os dados
  dados <- read.csv(filepath, header = TRUE, stringsAsFactors = FALSE)
  
  # Converter a coluna de datas para o formato Date se ainda não estiver
  if (!inherits(dados$Date, "Date")) {
    dados$Date <- as.Date(dados$Date, format="%Y-%m-%d")
  }
  
  # Filtrar os dados para o mês e ano especificados
  vendas_filtradas <- dados[year(dados$Date) == ano & month(dados$Date) == mes, ]
  
  # Assegurar que estamos pegando apenas 4 semanas
  if (nrow(vendas_filtradas) > 4) {
    vendas_filtradas <- vendas_filtradas[1:4, ]
  }
  
  return(vendas_filtradas)
}
