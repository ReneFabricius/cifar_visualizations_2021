library(ggplot2)
library(dplyr)
library(tidyr)
library("ggpubr")
library(LDATS)
library(stringr)
library(reshape2)
library(ggVennDiagram)
library(reticulate)
library(abind)
library(scales)
np <- import("numpy")

source("utils.R")

base_dir_c10 <- "../data/data_train_val_half_c10"
base_dir_c100 <- "../data/data_train_val_half_c100"
repls <- 0:0
classes <- 10

nets_outputs_c10 <- load_network_outputs(base_dir_c10, repls)
nets_outputs_c100 <- load_network_outputs(base_dir_c100, repls)

sort_ind <- function(lst) {
    return(sort(lst, index.return = TRUE, decreasing = TRUE)$ix)
}

plot_venn_correctness <- function(nets_outputs, plot_name)
{
    nets_test_top_indices <- apply(X = nets_outputs$test_outputs, MARGIN = c(1, 2, 3), FUN = sort_ind)[1, , , ]
    r_n <- length(repls)
    samples_n <- dim(nets_outputs$test_labels)[2]
    nets_n <- length(nets_outputs$networks)
    test_labs <- nets_outputs$test_labels + 1
    dim(test_labs) <- c(r_n, 1, samples_n)
    test_labs <- aperm(abind(array(rep(aperm(test_labs, perm = c(2, 1, 3)), nets_n), c(r_n, samples_n, nets_n)), along = 3), perm = c(1, 3, 2))
    if (r_n == 1) {
        dim(test_labs) <- dim(test_labs)[-1]
    }
    nets_test_cor_preds <- test_labs == nets_test_top_indices

    nets_cor_list <- list()
    incor <- 1:samples_n
    for (ni in 1:nets_n)
    {
        cor_list <- which(nets_test_cor_preds[ni, ])
        nets_cor_list[[nets_outputs$networks[ni]]] <- cor_list
        incor <- setdiff(incor, cor_list)
    }
    venn <- Venn(nets_cor_list)
    min_count <- min(process_region_data(venn)$count)
    min_count <- max(1, min_count)
    max_count <- max(process_region_data(venn)$count)
    incor_n <- length(incor)
    venn_diag <- ggVennDiagram(nets_cor_list) +
        scale_fill_gradient2(
            trans = "log2",
            midpoint = (log2(min_count) + log2(max_count)) / 2,
            name = "počet",
            high = "#03fc7f",
            limits = c(min_count, 10000)) +
        annotate(geom = "text", x = 0.5, y = 0.9, label = paste("Nesprávne ", incor_n, "\n", round(incor_n / samples_n * 100), "%")) +
        scale_x_continuous(limits = c(-0.1, 1.1)) +
        theme(legend.position = c(0.9, 0.5))

    

    ggsave(plot = venn_diag, filename = plot_name, device = cairo_pdf)
}

plot_venn_correctness(nets_outputs = nets_outputs_c10, plot_name = "network_outputs_sk/CIF10_venn_correctness.pdf")
plot_venn_correctness(nets_outputs = nets_outputs_c100, plot_name = "network_outputs_sk/CIF100_venn_correctness.pdf")
