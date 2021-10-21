library(hash)
library(reticulate)
library(berryFunctions)
library(dplyr)
np <- import("numpy")


load_network_outputs <- function(outputs_path, replications)
{
  types <- c("train", "val", "test")
  outputs_folder <- "outputs"
  networks <- list.dirs(file.path(outputs_path, replications[1], outputs_folder), recursive = FALSE, full.names = FALSE)
  output <- hash()
  output[["networks"]] <- networks
  for (type in types)
  {
    labels_list_rep <- list()
    outputs_list_rep <- list()
    li_rep <- 1
    
    for (repli in replications)
    {
      labels_name <- paste(type, "labels", sep = "_")
      labels_path <- file.path(outputs_path, repli, outputs_folder, networks[[1]], paste(labels_name, "npy", sep="."))
      labels <- np$load(labels_path)
      
      outputs_list_net <- list()
      li_net <- 1
      for (net in networks)
      {
        net_out_path <- file.path(outputs_path, repli, outputs_folder, networks[[li_net]], paste(type, "outputs.npy", sep="_"))
        out <- np$load(net_out_path)
        dim(out) <- c(1, dim(out))
        outputs_list_net[[li_net]] <- out
        li_net <- li_net + 1
      }
      net_outputs <- do.call(abind::abind, list(outputs_list_net, along=1, use.dnns=TRUE))
      dim(labels) <- c(1, dim(labels))
      dim(net_outputs) <- c(1, dim(net_outputs))
      labels_list_rep[[li_rep]] <- labels
      outputs_list_rep[[li_rep]] <- net_outputs
      li_rep <- li_rep + 1
    }
    
    net_out_type <- do.call(abind::abind, list(outputs_list_rep, along=1, use.dnns=TRUE))
    net_lab_type <- do.call(abind::abind, list(labels_list_rep, along=1, use.dnns=TRUE))
    
    output[[paste(type, "labels", sep = "_")]] <- net_lab_type
    output[[paste(type, "outputs", sep = "_")]] <- net_out_type
  }
  
  return(output)
}

load_ensemble_outputs <- function(outputs_path, replications, folds=NULL)
{
  train_types <- c("train_training", "val_training")
  outputs_folder <- "comb_outputs"
  outputs_match <- "ens_test_outputs_"
  if (is.null(folds))
  {
    pattern <- paste("^", outputs_match, "(.*).npy$", sep="")
  }
  else
  {
    pattern <- paste("^fold_\\d+_", outputs_match, "(.*).npy$", sep="")
  }
  files <- list.files(
    file.path(outputs_path, replications[1], outputs_folder, train_types[1]), 
    recursive=FALSE, full.names=FALSE)
  methods <- str_match(files, pattern)[, 2]
  methods <- methods[!is.na(methods)]
  methods <- unique(methods)
  
  output <- hash()
  output[["methods"]] <- methods
  for (t_type in train_types)
  {
    outputs_list_rep <- list()
    li_rep <- 1
    for (repli in replications)
    {
      outputs_list_met <- list()
      li_met <- 1
      for (method in methods)
      {
        if (is.null(folds))
        {
          file_name <- paste(outputs_match, method, ".npy", sep = "")
          output_path <- file.path(outputs_path, repli, outputs_folder, t_type, file_name)
          out_met <- np$load(output_path)
        }
        else
        {
          outputs_list_fold <- list()
          li_fold <- 1
          for (foldi in folds)
          {
            file_name <- paste("fold_", foldi, "_", outputs_match, method, ".npy", sep="")
            output_path <- file.path(outputs_path, repli, outputs_folder, t_type, file_name)
            out_fold <- np$load(output_path)
            dim(out_fold) <- c(1, dim(out_fold))
            outputs_list_fold[[li_fold]] <- out_fold
            li_fold <- li_fold + 1
          }
          out_met <- do.call(abind::abind, list(outputs_list_fold, along=1, use.dnns=TRUE))
        }
        dim(out_met) <- c(1, dim(out_met))
        outputs_list_met[[li_met]] <- out_met
        li_met <- li_met + 1
      }
      out_rep <- do.call(abind::abind, list(outputs_list_met, along=1, use.dnns=TRUE))
      dim(out_rep) <- c(1, dim(out_rep))
      outputs_list_rep[[li_rep]] <- out_rep
      li_rep <- li_rep + 1
    }
    out_type <- do.call(abind::abind, list(outputs_list_rep, along=1, use.dnns=TRUE))
    output[[t_type]] <- out_type
  }
  
  return(output)
}