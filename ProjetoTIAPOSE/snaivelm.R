library(rminer)
library(forecast)

cat("read Walmart Sales\n")
data <- read.csv("C:/Users/jpska/OneDrive/Ambiente de Trabalho/TIAPOSE/projeto/walmart.csv")

# Convert Date column to Date format:
data$Date <- as.Date(data$Date)

# Initialize empty lists to store overall predictions and actual values
all_predictions <- c()
all_actuals <- c()
all_improvements <- c()

# Loop through columns 4 to 7
for (i in 4:7) {
  cat("Departamento:", colnames(data)[i], "\n")
  
  # read the time series into object S
  S <- data[, i] 
  
  NPRED <- 4 # number of predictions
  srange <- diff(range(S)) # calculates the difference between the maximum and minimum values in S
  
  # creates a dataframe
  # each row contains past observations
  # each column represents the value of the time series
  # uses rolling window 
  D <- CasesSeries(S, c(1:4)) 
  
  N <- nrow(D) # number of D examples
  
  NTR <- N - NPRED # calculates the number of examples in the training set by subtracting the predictions
  
  # training data, excluding NPRED rows
  TR <- 1:NTR 
  
  # test data
  TS <- (NTR + 1):N 
  
  # fit a random forest
  RF <- fit(y ~ ., D[TR,], model = "lm", search = "heuristic")
  
  # 1-ahead predictions:
  LTS <- length(TS) # length of the test set
  
  START <- nrow(D) - LTS + 1 # START is the row from D of the first test example
  PRF <- lforecast(RF, D, start = START, horizon = LTS)
  
  # store the output target into object Y
  Y <- D[TS,]$y # real observed values
  
  # Store predictions and actual values
  all_predictions <- c(all_predictions, PRF)
  all_actuals <- c(all_actuals, Y)
  
  # Métricas do modelo snaive
  Pred_snaive <- forecast::snaive(data[, i], h = NPRED)$mean
  NMAE_snaive <- mmetric(Y, Pred_snaive, metric = "NMAE", val = srange)
  
  # Improvement calculation
  NMAE_RF <- mmetric(Y, PRF, metric = "NMAE", val = srange)
  improvement <- ((NMAE_snaive - NMAE_RF) / NMAE_snaive) * 100
  all_improvements <- c(all_improvements, improvement)
}

# Calculate overall improvement
overall_improvement <- mean(all_improvements)

# Create a general plot
cat("General Plot for All Departments\n")
main <- paste("Linear Regression Predictions vs Actual Values\nOverall Improvement:", round(overall_improvement, 2), "%")
mgraph(all_actuals, all_predictions, main = main, graph = "REG", Grid = 10, lty = 1, col = c("black", "blue"), leg = list(pos = "topright", leg = c("target", "predictions")))
