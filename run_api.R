library(plumber)

# Carrega o plumber a partir do arquivo gom_api.R
r <- plumb("gom_api.R")
r$run(port = 8000)
