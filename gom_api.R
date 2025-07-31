# gom_api.R

library(plumber)
library(Rcpp)
library(inline)

# ---
### Global Variables
# ---
# This will store the loaded data. Initialize as NULL.
data_object <- NULL

# This will store the trained GoM models. Initialize as an empty list.
gom.models <- list()

# ---
### Helper Functions
# ---

# Function to load GoMRcpp model function
source("GoMRcpp.R") # Make sure GoMRcpp.R is in the same directory or adjust path

# Function to train GoM models
train_gom_models <- function(data) {
  if (is.null(data)) {
    stop("No data provided for model training.")
  }
  
  models <- list()
  for (k in 2:4) {
    message(paste0("Training model for K = ", k, "..."))
    models[[paste0("K", k)]] <- GoMRcpp(
      data.object = data,
      initial.K = k, final.K = k,
      gamma.algorithm = "gradient.1992",
      initial.gamma = "equal.values",
      gamma.fit = TRUE,
      lambda.algorithm = "gradient.1992",
      initial.lambda = "random",
      lambda.fit = TRUE,
      case.id = "SubjID", # Adjust if your CSV has a different ID column
      internal.var = c("Var1", "Var2", "Var3"), # Adjust these to your actual variable names
      order.K = TRUE,
      dec.char = "."
    )
  }
  return(models)
}

# ---
### Endpoints
# ---

#* Endpoint to upload a CSV file
#* @post /upload
#* @parser multi
#* @param file:file The CSV file to upload
function(file) {
  if (is.null(file)) {
    return(list(status = "error", message = "No file uploaded."))
  }
  
  # Get the temporary file path and content type
  file_path <- file$datapath
  file_type <- file$type
  
  # Check if the file is a CSV
  if (file_type != "text/csv") {
    return(list(status = "error", message = "Invalid file type. Please upload a CSV file."))
  }
  
  tryCatch({
    # Read the uploaded CSV file
    new_data <- read.csv(file_path, stringsAsFactors = TRUE)
    
    # Update the global data object
    data_object <<- new_data
    
    # Retrain models with the new data
    gom.models <<- train_gom_models(data_object)
    
    return(list(status = "success", message = "CSV uploaded and models retrained successfully!"))
  }, error = function(e) {
    return(list(status = "error", message = paste("Error processing file:", e$message)))
  })
}

#* @get /model
#* @param k Número de perfis
function(k = 2) {
  nome <- paste0("K", k)
  if (!is.null(gom.models[[nome]])) return(gom.models[[nome]])
  else return(list(error = "Model not found. Please upload a CSV and ensure models are trained."))
}

#* @get /model/gamma
#* @param k Número de perfis
function(k = 2) {
  nome <- paste0("K", k)
  if (!is.null(gom.models[[nome]])) return(gom.models[[nome]]$gamma)
  else return(list(error = "Model not found. Please upload a CSV and ensure models are trained."))
}

#* @get /model/lambda
#* @param k Número de perfis
function(k = 2) {
  nome <- paste0("K", k)
  if (!is.null(gom.models[[nome]])) return(gom.models[[nome]]$lambda)
  else return(list(error = "Model not found. Please upload a CSV and ensure models are trained."))
}

#* @get /model/loglik
#* @param k Número de perfis
function(k = 2) {
  nome <- paste0("K", k)
  if (!is.null(gom.models[[nome]])) return(list(loglik = gom.models[[nome]]$log.likelihood))
  else return(list(error = "Model not found. Please upload a CSV and ensure models are trained."))
}

# ---
### Initialization (Optional - for initial run without upload)
# ---
# If you want to initially load a default 'teste.csv' and train models
# when the API starts, uncomment the following block.
# Otherwise, models will only be trained after a successful upload.
#
# tryCatch({
#   if (file.exists("teste.csv")) {
#     data_object <<- read.csv("teste.csv", stringsAsFactors = TRUE)
#     gom.models <<- train_gom_models(data_object)
#     message("Initial 'teste.csv' loaded and models trained.")
#   } else {
#     message("No 'teste.csv' found. Models will be trained after a file upload.")
#   }
# }, error = function(e) {
#   message(paste("Error during initial data loading/training:", e$message))
# })