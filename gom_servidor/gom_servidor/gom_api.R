# gom_api.R
library(plumber)
library(Rcpp)
library(inline)

# Carregar a função do modelo GoM
source("GoMRcpp.R")

# Middleware CORS (para comunicação com React)
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "*")
  plumber::forward()
}

#* @post /upload
#* @parser multi
function(file, k = 3){
  # file: o arquivo CSV enviado via POST
  # k: número de perfis (default = 3)
  
  csv_path <- file$datapath
  dados <- read.csv(csv_path, stringsAsFactors = TRUE)
  
  modelo <- GoMRcpp(
    data.object = dados,
    initial.K = k, final.K = k,
    gamma.algorithm = "gradient.1992",
    initial.gamma = "equal.values",
    gamma.fit = TRUE,
    lambda.algorithm = "gradient.1992",
    initial.lambda = "random",
    lambda.fit = TRUE,
    case.id = "SubjID",
    internal.var = c("Var1", "Var2", "Var3"),
    order.K = TRUE,
    dec.char = "."
  )
  
  # Retorna o resultado simplificado para exibição
  return(list(
    k = k,
    loglik = modelo$log.likelihood,
    gamma = modelo$gamma,
    lambda = modelo$lambda
  ))
}

# Iniciar API com CORS
pr() %>%
  pr_hook("preroute", cors) %>%
  pr_run(port = 8000)
