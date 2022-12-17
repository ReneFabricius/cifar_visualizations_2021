library(hash)
library(reticulate)
library(berryFunctions)
library(namedCapture)
library(dplyr)
library(purrr)
library(reshape2)
np <- import("numpy")


geom_hopline <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHpline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

GeomHpline <- ggproto("GeomHpline", GeomSegment,
  required_aes = c("x", "y"),
  non_missing_aes = c("linewidth", "colour", "linetype", "width"),
  default_aes = aes(
    width = 0.5, colour = "black", linewidth = 2, linetype = 1,
    alpha = NA
  ),

  draw_panel = function(self, data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                        lineend = "butt", linejoin = "round", na.rm = FALSE) {
    data <- mutate(data, x = x - width/2, xend = x + width, yend = y)
    ggproto_parent(GeomSegment, self)$draw_panel(
      data, panel_params, coord, arrow = arrow, arrow.fill = arrow.fill,
      lineend = lineend, linejoin = linejoin, na.rm = na.rm
    )
  }
)


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

load_ensemble_outputs <- function(outputs_path, replications, folds=NULL, gather=NULL)
{
  train_types <- c("train_training", "val_training")
  outputs_folder <- "comb_outputs"
  outputs_match <- "ens_test_outputs_"
  if (is.null(folds))
  {
    pattern <- paste("^", outputs_match, "co_(.*?)_cp_(.*?)_prec_(.*?).npy$", sep="")
  }
  else
  {
    pattern <- paste("^fold_\\d+_", outputs_match, "co_(.*?)_cp_(.*?)_prec_(.*?).npy$", sep="")
  }
  files <- list.files(
    file.path(outputs_path, replications[1], outputs_folder, train_types[1]), 
    recursive=FALSE, full.names=FALSE)
  files_match <- str_match(files, pattern)
  combining_methods <- files_match[, 2]
  combining_methods <- unique(combining_methods[!is.na(combining_methods)])
  coupling_methods <- files_match[, 3]
  coupling_methods <- unique(coupling_methods[!is.na(coupling_methods)])
  precision <- files_match[, 4]
  precision <- unique(precision[!is.na(precision)])
  
  if (length(precision) > 1)
  {
    print("Load ensemble outputs in multiple precisions not supported.")
    return(NA)
  }
  
  output <- hash()
  output[["combining_methods"]] <- combining_methods
  output[["coupling_methods"]] <- coupling_methods
  output[["precision"]] <- precision
  for (t_type in train_types)
  {
    outputs_list_rep <- list()
    li_rep <- 1
    for (repli in replications)
    {
      outputs_list_co_m <- list()
      li_co_m <- 1
      for (co_m in combining_methods)
      {
        outputs_list_cp_m <- list()
        li_cp_m <- 1
        for (cp_m in coupling_methods)
        {
          if (is.null(folds))
          {
            file_name <- paste(outputs_match, "co_", co_m, "_cp_", cp_m, "_prec_", precision[1], ".npy", sep = "")
            output_path <- file.path(outputs_path, repli, outputs_folder, t_type, file_name)
            out_cp_m <- np$load(output_path)
            if (!is.null(gather))
            {
              out_cp_m <- gather(data=out_cp_m, index=gather, index_dim=1, along=2)
            }
          }
          else
          {
            outputs_list_fold <- list()
            li_fold <- 1
            for (foldi in folds)
            {
              file_name <- paste("fold_", foldi, "_", outputs_match, "co_", co_m, "_cp_", cp_m, "_prec_", precision[1], ".npy", sep="")
              output_path <- file.path(outputs_path, repli, outputs_folder, t_type, file_name)
              out_fold <- np$load(output_path)
              if (!is.null(gather))
              {
                out_fold <- gather(data=out_fold, index=gather, index_dim=1, along=2)
              }
              dim(out_fold) <- c(1, dim(out_fold))
              outputs_list_fold[[li_fold]] <- out_fold
              li_fold <- li_fold + 1
            }
            out_cp_m <- do.call(abind::abind, list(outputs_list_fold, along=1, use.dnns=TRUE))
          }
          dim(out_cp_m) <- c(1, dim(out_cp_m))
          outputs_list_cp_m[[li_cp_m]] <- out_cp_m
          li_cp_m <- li_cp_m + 1
        }
        out_co_m <- do.call(abind::abind, list(outputs_list_cp_m, along=1, use.dnns=TRUE))
        dim(out_co_m) <- c(1, dim(out_co_m))
        outputs_list_co_m[[li_co_m]] <- out_co_m
        li_co_m <- li_co_m + 1
      }
      out_rep <- do.call(abind::abind, list(outputs_list_co_m, along=1, use.dnns=TRUE))
      dim(out_rep) <- c(1, dim(out_rep))
      outputs_list_rep[[li_rep]] <- out_rep
      li_rep <- li_rep + 1
    }
    out_type <- do.call(abind::abind, list(outputs_list_rep, along=1, use.dnns=TRUE))
    output[[t_type]] <- out_type
  }
  
  return(output)
}

load_R_matrices <- function(outputs_path, replications, folds=NULL)
{
  train_types <- c("train_training", "val_training")
  outputs_folder <- "comb_outputs"
  outputs_match <- "ens_test_R_"
  if (is.null(folds))
  {
    pattern <- paste("^", outputs_match, "co_(.*?)_prec_(.*?).npy$", sep="")
  }
  else
  {
    pattern <- paste("^fold_\\d+_", outputs_match, "co_(.*?)_prec_(.*?).npy$", sep="")
  }
  files <- list.files(
    file.path(outputs_path, replications[1], outputs_folder, train_types[1]), 
    recursive=FALSE, full.names=FALSE)
  files_match <- str_match(files, pattern)
  combining_methods <- files_match[, 2]
  combining_methods <- unique(combining_methods[!is.na(combining_methods)])
  precisions <- files_match[, 3]
  precisions <- unique(precisions[!is.na(precisions)])
  
  output <- hash()
  output[["precisions"]] <- precisions
  output[["combining_methods"]] <- combining_methods
  
  for (t_type in train_types)
  {
    outputs_list_rep <- list()
    li_rep <- 1
    for (repli in replications)
    {
      outputs_list_co_m <- list()
      li_co_m <- 1
      for (co_m in combining_methods)
      {
        outputs_list_prec <- list()
        li_prec <- 1
        for (prec in precisions)
        {
          if (is.null(folds))
          {
            file_name <- paste(outputs_match, "co_", co_m, "_prec_", prec, ".npy", sep = "")
            output_path <- file.path(outputs_path, repli, outputs_folder, t_type, file_name)
            out_prec <- np$load(output_path)
          }
          else
          {
            outputs_list_fold <- list()
            li_fold <- 1
            for (foldi in folds)
            {
              file_name <- paste("fold_", foldi, "_", outputs_match, "co_", co_m, "_prec_", prec, ".npy", sep="")
              output_path <- file.path(outputs_path, repli, outputs_folder, t_type, file_name)
              out_fold <- np$load(output_path)
              dim(out_fold) <- c(1, dim(out_fold))
              outputs_list_fold[[li_fold]] <- out_fold
              li_fold <- li_fold + 1
            }
            out_prec <- do.call(abind::abind, list(outputs_list_fold, along=1, use.dnns=TRUE))
          }
          dim(out_prec) <- c(1, dim(out_prec))
          outputs_list_prec[[li_prec]] <- out_prec
          li_prec <- li_prec + 1
        }
        out_co_m <- do.call(abind::abind, list(outputs_list_prec, along=1, use.dnns=TRUE))
        dim(out_co_m) <- c(1, dim(out_co_m))
        outputs_list_co_m[[li_co_m]] <- out_co_m
        li_co_m <- li_co_m + 1
      }
      out_rep <- do.call(abind::abind, list(outputs_list_co_m, along=1, use.dnns=TRUE))
      dim(out_rep) <- c(1, dim(out_rep))
      outputs_list_rep[[li_rep]] <- out_rep
      li_rep <- li_rep + 1
    }
    out_type <- do.call(abind::abind, list(outputs_list_rep, along=1, use.dnns=TRUE))
    output[[t_type]] <- out_type
  }
  
  return(output)
  
}

load_class_averaged_R_matrices <- function(outputs_path, labels, replications, folds=NULL)
{
  train_types <- c("train_training", "val_training")
  outputs_folder <- "comb_outputs"
  outputs_match <- "ens_test_R_"
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
  precisions <- str_match(files, pattern)[, 2]
  precisions <- precisions[!is.na(precisions)]
  precisions <- unique(precisions)
  
  class_aggreg_Rs <- data.frame()
  
  for (t_type in train_types)
  {
    for (repli in replications)
    {
      for (prec in precisions)
      {
        if (is.null(folds))
        {
          file_name <- paste(outputs_match, prec, ".npy", sep = "")
          output_path <- file.path(outputs_path, repli, outputs_folder, t_type, file_name)
          out_prec <- np$load(output_path)
          prec_melted <- melt(out_prec)
          names(prec_melted) <- c("sample", "class1", "class2", "prob")
          prec_melted[, c("class1", "class2")] <- lapply(prec_melted[, c("class1", "class2")], factor)
          prec_melted$class <- as.factor(labels[prec_melted$sample] + 1)
          
        }
        else
        {
          prec_melted <- data.frame()
          for (foldi in folds)
          {
            file_name <- paste("fold_", foldi, "_", outputs_match, prec, ".npy", sep="")
            output_path <- file.path(outputs_path, repli, outputs_folder, t_type, file_name)
            out_fold <- np$load(output_path)
            fold_melted <- melt(out_fold)
            names(fold_melted) <- c("sample", "class1", "class2", "prob")
            fold_melted[, c("class1", "class2")] <- lapply(fold_melted[, c("class1", "class2")], as.factor)
            fold_melted$class <- as.factor(labels[fold_melted$sample] + 1)
            fold_aggreg <- fold_melted %>% group_by(class1, class2, class) %>% summarise(prob=mean(prob)) %>% ungroup()
            fold_aggreg$fold <- foldi
            prec_melted <- rbind(prec_melted, fold_aggreg)
          }
        }
        
        prec_aggreg <- prec_melted %>% group_by(class1, class2, class) %>% summarise(prob=mean(prob)) %>% ungroup()
        prec_aggreg$precision <- prec
        prec_aggreg$replication <- repli
        prec_aggreg$train_type <- t_type
        
        class_aggreg_Rs <- rbind(class_aggreg_Rs, prec_aggreg)
      }
    }
  }
  class_aggreg_Rs <- class_aggreg_Rs %>% group_by(precision, train_type, class1, class2, class) %>% summarise(prob=mean(prob)) %>% ungroup()
  
  return(class_aggreg_Rs)
}

load_combiner_coefs <- function(outputs_path, replications, folds=NULL)
{
  train_types <- c("train_training", "val_training")
  outputs_folder <- "comb_outputs"
  net_ord = "networks_order.txt"
  outputs_match <- "lda_coefs_"
  if (is.null(folds))
  {
    pattern <- paste("^(.*?)_coefs_(.*).csv$", sep="")
  }
  else
  {
    pattern <- paste("^fold_\\d+_(.*?)_coefs_(.*).csv$", sep="")
  }
  files <- list.files(
    file.path(outputs_path, replications[1], outputs_folder, train_types[1]), 
    recursive=FALSE, full.names=FALSE)
  file_match <- str_match(files, pattern)
  combining_methods <- file_match[, 2]
  combining_methods <- unique(combining_methods[!is.na(combining_methods)])
  precisions <- file_match[, 3]
  precisions <- unique(precisions[!is.na(precisions)])
  
  output <- data.frame()
  for (t_type in train_types)
  {
    type_df <- data.frame()
    for (repli in replications)
    {
      repli_df <- data.frame()
      for (co_m in combining_methods)
      {
        co_m_df <- data.frame()
        for (prec in precisions)
        {
          prec_df <- data.frame()
          if (is.null(folds))
          {
            file_name <- paste(co_m, "_coefs_", prec, ".csv", sep = "")
            file_path <- file.path(outputs_path, repli, outputs_folder, t_type, file_name)
            prec_df <- read.csv(file_path)
          }
          else
          {
            for (foldi in folds)
            {
              file_name <- paste("fold_", foldi, "_", co_m, "_coefs_", prec, ".csv", sep="")
              file_path <- file.path(outputs_path, repli, outputs_folder, t_type, file_name)
              fold_df <- read.csv(file_path)
              fold_df$fold <- foldi
              prec_df <- rbind(prec_df, fold_df)
            }
          }
          prec_df$precision <- prec
          co_m_df <- rbind(co_m_df, prec_df)
        }
        co_m_df$combining_method <- co_m
        repli_df <- rbind(repli_df, co_m_df)
      }
      repli_df$replication <- repli
      type_df <- rbind(type_df, repli_df)
    }
    type_df$train_type <- t_type
    output <- rbind(output, type_df)
  }
  colnames(output)[which(names(output) == "i")] <- "class1"
  colnames(output)[which(names(output) == "j")] <- "class2"
  output$class1 <- output$class1 + 1
  output$class2 <- output$class2 + 1
  
  nets <- read.csv(file.path(outputs_path, replications[1], outputs_folder, net_ord), header=FALSE)
  for (ni in seq_along(nets[, 1]))
  {
    colnames(output)[which(names(output) == paste("coef", (ni - 1), sep=""))] <- nets[ni, 1]
  }
  
  output <- pivot_longer(output, cols=c(nets[, 1], "interc"), names_to="coefficient", values_to="value")
  output$coefficient <- factor(output$coefficient, levels=c(nets[, 1], "interc"))
  
  
  return(output)
}

#' Gathers elements from array data according to vector index.
#' Vector index spans along one dimension and picks data along another.
#' Dimension along which index spans is given by index_dim,
#' dimension along which it picks data is given by along.
#' Output dimensions are same as data dimensions expect in 
#' along dimension, where the output has size 1.
gather <- function(data, index, index_dim, along)
{
  data_dim <- dim(data)
  aux_dims <- data_dim
  aux_dims[c(index_dim, along)] <- 1
  aux_dims_seq <- lapply(aux_dims, seq)
  aux_dims_seq[index_dim] <- list(1:length(index))
  aux_dims_seq <- aux_dims_seq[-along]
  inds <- expand.grid(aux_dims_seq)
  along_inds <- index[inds[, ifelse(index_dim < along, index_dim, index_dim - 1)]]
  ind_M <- cbind(inds[, seq(1, along - 1)],
                 along_inds,
                 inds[, seq(along, length(data_dim), length.out=length(data_dim) - along)])
  
  vec_select <- data[as.matrix(ind_M)]
  
  out_dim <- data_dim
  out_dim[along] <- 1
  arr_sel <- array(vec_select, dim=out_dim)
  
  return(arr_sel)
}

add_combination_metrics <- function(net_df, ens_df_cal, ens_df_pwc)
{
  networks <- net_df$network

  acc_cols <- c("acc_min", "acc_max", "acc_avg", "acc_var")
  acck_cols <- c(
    "acc1_min", "acc1_max", "acc1_avg", "acc1_var",
    "acc5_min", "acc5_max", "acc5_avg", "acc5_var"
  )

  has_acck <- "accuracy5" %in% colnames(net_df)

  comb_stats_df <- data.frame(matrix(
    ncol = ifelse(has_acck, 18, 14), nrow = 0,
    dimnames = list(NULL, c(
      "combination_size", "combination_id",
      if (has_acck) acck_cols else acc_cols,
      "nll_min", "nll_max", "nll_avg", "nll_var",
      "ece_min", "ece_max", "ece_avg", "ece_var"
    ))
  ))

  for (sss in unique(ens_df_cal$combination_size))
  {
      for (ssi in unique(ens_df_cal %>%
          filter(combination_size == sss) %>%
          pull(combination_id)))
      {
          cur_nets_vec <- to_vec(
              for (net in networks) {
                  if (str_replace_all(net, "-", ".") %in% colnames(ens_df_cal) &&
                  (ens_df_cal %>%
                      filter(combination_size == sss & combination_id == ssi) %>%
                      pull(str_replace_all(net, "-", ".")))[1] == "True") {
                  net
                  }
              }
          )
          cur_nets <- net_df %>% filter(network %in% cur_nets_vec)
          if (has_acck)
            {
              acc_stats <- c(
                min(cur_nets$accuracy1), max(cur_nets$accuracy1), mean(cur_nets$accuracy1), var(cur_nets$accuracy1),
                min(cur_nets$accuracy5), max(cur_nets$accuracy5), mean(cur_nets$accuracy5), var(cur_nets$accuracy5)
              )
            }
            else 
            {
              acc_stats <- c(
                min(cur_nets$accuracy), max(cur_nets$accuracy), mean(cur_nets$accuracy), var(cur_nets$accuracy)
              )
            }

          comb_stats_df[nrow(comb_stats_df) + 1, ] <- c(
          sss, ssi,
          acc_stats,
          min(cur_nets$nll), max(cur_nets$nll), mean(cur_nets$nll), var(cur_nets$nll),
          min(cur_nets$ece), max(cur_nets$ece), mean(cur_nets$ece), var(cur_nets$ece)
          )
      }
  }

  ens_df_cal <- merge(ens_df_cal, comb_stats_df)
  if (has_acck)
  {
    ens_df_cal$acc1_imp_avg <- ens_df_cal$accuracy1 - ens_df_cal$acc1_avg
    ens_df_cal$acc5_imp_avg <- ens_df_cal$accuracy5 - ens_df_cal$acc5_avg
    ens_df_cal$acc1_imp_best <- ens_df_cal$accuracy1 - ens_df_cal$acc1_max
    ens_df_cal$acc5_imp_best <- ens_df_cal$accuracy5 - ens_df_cal$acc5_max
  }
  else 
  {
    ens_df_cal$acc_imp_avg <- ens_df_cal$accuracy - ens_df_cal$acc_avg
    ens_df_cal$acc_imp_best <- ens_df_cal$accuracy - ens_df_cal$acc_max
  }
  ens_df_cal$nll_imp_avg <- -(ens_df_cal$nll - ens_df_cal$nll_avg)
  ens_df_cal$nll_imp_best <- -(ens_df_cal$nll - ens_df_cal$nll_min)
  ens_df_cal$ece_imp_avg <- -(ens_df_cal$ece - ens_df_cal$ece_avg)
  ens_df_cal$ece_imp_best <- -(ens_df_cal$ece - ens_df_cal$ece_min)

  ens_df_pwc <- merge(ens_df_pwc, comb_stats_df)
  if (has_acck)
  {
    ens_df_pwc$acc1_imp_avg <- ens_df_pwc$accuracy1 - ens_df_pwc$acc1_avg
    ens_df_pwc$acc5_imp_avg <- ens_df_pwc$accuracy5 - ens_df_pwc$acc5_avg
    ens_df_pwc$acc1_imp_best <- ens_df_pwc$accuracy1 - ens_df_pwc$acc1_max
    ens_df_pwc$acc5_imp_best <- ens_df_pwc$accuracy5 - ens_df_pwc$acc5_max
  }
  else 
  {
    ens_df_pwc$acc_imp_avg <- ens_df_pwc$accuracy - ens_df_pwc$acc_avg
    ens_df_pwc$acc_imp_best <- ens_df_pwc$accuracy - ens_df_pwc$acc_max
  }
  ens_df_pwc$nll_imp_avg <- -(ens_df_pwc$nll - ens_df_pwc$nll_avg)
  ens_df_pwc$nll_imp_best <- -(ens_df_pwc$nll - ens_df_pwc$nll_min)
  ens_df_pwc$ece_imp_avg <- -(ens_df_pwc$ece - ens_df_pwc$ece_avg)
  ens_df_pwc$ece_imp_best <- -(ens_df_pwc$ece - ens_df_pwc$ece_min)

  return(list(ens_df_cal, ens_df_pwc))
}

#' Creates a data frame containing pwc ensemble fields and filenames with specified pattern
get_id_ood_files <- function(id_ptrn, ood_ptrn, dir)
{
  files <- list.files(path = dir, pattern = "*.npy")
  ood_match <- as.data.frame(str_match_named(files, ood_ptrn))
  ood_files <- files[!is.na(ood_match$nets)]
  ood_groups <- ood_match[!is.na(ood_match$nets),]
  ood_groups$file_ood <- ood_files

  id_match <- as.data.frame(str_match_named(files, id_ptrn))
  id_files <- files[!is.na(id_match$nets)]
  id_groups <- id_match[!is.na(id_match$nets),]
  id_groups$file_id <- id_files

  uncert_files <- merge(ood_groups, id_groups)
  return(uncert_files)

}

get_unc_files_id_ood <- function(dir)
{
  ood_ptrn <- "^(?<nets>.*?)_ens_ood_uncerts_co_(?<combining_method>.*?)_cp_(?<coupling_method>.*?)_prec_(?<precision>.*?)_topl_(?<topl>[+-]?\\d+).npy$"
  id_ptrn <- "^(?<nets>.*?)_ens_test_uncerts_co_(?<combining_method>.*?)_cp_(?<coupling_method>.*?)_prec_(?<precision>.*?)_topl_(?<topl>[+-]?\\d+).npy$"
  return(get_id_ood_files(id_ptrn = id_ptrn, ood_ptrn = ood_ptrn, dir = dir))
}

get_output_files_id_ood <- function(dir)
{
  ood_ptrn <- "^(?<nets>.*?)_ens_ood_outputs_co_(?<combining_method>.*?)_cp_(?<coupling_method>.*?)_prec_(?<precision>.*?)_topl_(?<topl>[+-]?\\d+).npy$"
  id_ptrn <- "^(?<nets>.*?)_ens_test_outputs_co_(?<combining_method>.*?)_cp_(?<coupling_method>.*?)_prec_(?<precision>.*?)_topl_(?<topl>[+-]?\\d+).npy$"
  return(get_id_ood_files(id_ptrn = id_ptrn, ood_ptrn = ood_ptrn, dir = dir))
}

CAL_ENS_PTRN <- function(subject, suffix)
{
  list(
    nets = ".*?",
    "_",
    subject,
    "_cal_",
    calibrating_method = ".*?",
    "_prec_",
    computational_precision = ".*?",
    suffix
  )
}

PWC_ENS_PTRN <- function(subject, suffix)
{
  list(
    nets = ".*?",
    "_",
    subject,
    "_co_",
    combining_method = ".*?",
    "_cp_",
    coupling_method = ".*?",
    "_prec_",
    computational_precision = ".*?",
    "_topl_",
    topl = list("[+-]?\\d+", as.integer),
    suffix
  ) 
}

PATTERNS <- list(
  cal_ens_test_output = CAL_ENS_PTRN(subject = "ens_test_outputs", suffix = ".npy"),
  cal_ens_ood_output = CAL_ENS_PTRN(subject = "ens_ood_outputs", suffix = ".npy"),
  cal_ens_train_output = CAL_ENS_PTRN(subject = "ens_train_outputs", suffix = ".npy"),
  pwc_ens_test_output = PWC_ENS_PTRN(subject = "ens_test_outputs", suffix = ".npy"),
  pwc_ens_ood_output = PWC_ENS_PTRN(subject = "ens_ood_outputs", suffix = ".npy"),
  pwc_ens_train_output = PWC_ENS_PTRN(subject = "ens_train_outputs", suffix = ".npy"),
  pwc_ens_test_unc = PWC_ENS_PTRN(subject = "ens_test_uncerts", suffix = ".npy"),
  pwc_ens_ood_unc = PWC_ENS_PTRN(subject = "ens_ood_uncerts", suffix = ".npy"),
  cal_ens_msp_roc = CAL_ENS_PTRN(subject = "ens_msp_roc", suffix = ".csv"),
  cal_ens_msp_prc = CAL_ENS_PTRN(subject = "ens_msp_prc", suffix = ".csv"),
  pwc_ens_msp_roc = PWC_ENS_PTRN(subject = "ens_msp_roc", suffix = ".csv"),
  pwc_ens_msp_prc = PWC_ENS_PTRN(subject = "ens_msp_prc", suffix = ".csv"),
  pwc_ens_unc_roc = PWC_ENS_PTRN(subject = "ens_unc_roc", suffix = ".csv"),
  pwc_ens_unc_prc = PWC_ENS_PTRN(subject = "ens_unc_prc", suffix = ".csv"),
  net_msp_roc = list(net = ".*?", "_roc.csv"),
  net_msp_prc = list(net = ".*?", "_prc.csv")
)

find_files_by_ptrn <- function(dir, ptrns)
{
  files <- list.files(path = dir, pattern = "*")
  filter_files <- function(ptrn_i)
  {
    name <- names(ptrns)[[ptrn_i]]
    pt <- ptrns[[ptrn_i]]
    ptrn <- PATTERNS[[pt]]
    match <- as.data.frame(namedCapture::str_match_variable(files, ptrn))
    matched_files <- files[rowSums(is.na(match)) == 0]
    match <- na.omit(match)
    match[name] <- matched_files
    return(match)
  }
  res_dfs <- lapply(seq_along(ptrns), filter_files)
  merged <- res_dfs %>% reduce(full_join)
}