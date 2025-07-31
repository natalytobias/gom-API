# gom_api.R

library(plumber)
library(Rcpp)
library(inline)

teste <- read.csv("teste.csv", stringsAsFactors = TRUE)


# Carregar função do modelo GoM
source("GoMRcpp.R")

# Rodar modelos
gom.models <- list()
for (k in 2:4) {
  gom.models[[paste0("K", k)]] <- GoMRcpp(
    data.object = teste,
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
}

# Endpoints

#* @get /model
#* @param k Número de perfis
function(k = 2) {
  nome <- paste0("K", k)
  if (!is.null(gom.models[[nome]])) return(gom.models[[nome]])
  else return(list(erro = "Modelo não encontrado"))
}

#* @get /model/gamma
#* @param k Número de perfis
function(k = 2) {
  nome <- paste0("K", k)
  if (!is.null(gom.models[[nome]])) return(gom.models[[nome]]$gamma)
  else return(list(erro = "Modelo não encontrado"))
}

#* @get /model/lambda
#* @param k Número de perfis
function(k = 2) {
  nome <- paste0("K", k)
  if (!is.null(gom.models[[nome]])) return(gom.models[[nome]]$lambda)
  else return(list(erro = "Modelo não encontrado"))
}

#* @get /model/loglik
#* @param k Número de perfis
function(k = 2) {
  nome <- paste0("K", k)
  if (!is.null(gom.models[[nome]])) return(list(loglik = gom.models[[nome]]$log.likelihood))
  else return(list(erro = "Modelo não encontrado"))
}