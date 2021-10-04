library(hash)
library(reticulate)
library(berryFunctions)
np <- import("numpy")


load_network_outputs <- function(outputs_path)
{
  types <- c("train", "val", "test")
  networks <- list.dirs(outputs_path, recursive = FALSE, full.names = FALSE)
  output <- hash()
  output[["networks"]] <- networks
  
  
  for (type in types)
  {
    labels_name <- paste(type, "labels", sep = "_")
    labels_path <- file.path(outputs_path, networks[[1]], paste(labels_name, "npy", sep="."))
    labels <- np$load(labels_path)
    outputs_list <- list()
    li <- 1
    
    for (net in networks)
    {
      net_out_path <- file.path(outputs_path, networks[[li]], paste(type, "outputs.npy", sep="_"))
      out <- np$load(net_out_path)
      dim(out) <- c(1, dim(out))
      outputs_list[[li]] <- out
      li <- li + 1
    }
    net_outputs <- do.call(abind::abind, list(outputs_list, rev.along=3, use.dnns=TRUE))
    output[[paste(type, "labels", sep = "_")]] <- labels
    output[[paste(type, "outputs", sep = "_")]] <- net_outputs
  }
  
  return(output)
}

load_ensemble_outputs <- function(outputs_path, methods, precision)
{
  output <- hash()
  for (method in methods)
  {
    method_name <- paste(method, precision, sep = "_")
    method_path <- file.path(outputs_path, paste("ens_test_outputs_", method_name, ".npy", sep=""))
    ens_out <- np$load(method_path)
    output[[method_name]] <- ens_out
  }
  
  return(output)
}