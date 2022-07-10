library(ggplot2)
library(dplyr)
library(tidyr)
library(comprehenr)
library(stringr)
library(ungeviz)
library(relayer)
library(patchwork)
library(xtable)
library(namedCapture)
library(reticulate)
source("utils.R")

np <- import("numpy")

base_dir_C10 <- "D:/skola/1/weighted_ensembles/tests/test_cifar_ood_2022/C10vsC100_metrics"
base_dir_C100 <- "D:/skola/1/weighted_ensembles/tests/test_cifar_ood_2022/C100vsC10_metrics"

plot_uncert_dists <- function(dir, dts)
{
    uncert_files <- get_unc_files_id_ood(dir = dir)
    tps <- unique(uncert_files[, c("combining_method", "coupling_method")])

    plot_distribution <- function(x, outputs_folder = "cifar_ood")
    {
        id_unc <- np$load(file.path(dir, x["file_id"]))
        ood_unc <- np$load(file.path(dir, x["file_ood"]))
        uncs <- data.frame(id = id_unc, ood = ood_unc)
        uncs <- pivot_longer(data = uncs, cols = c("id", "ood"), names_to = "dist")
        min_nz <- min(uncs[uncs$value != 0, "value"])
        uncs[uncs$value == 0, "value"] <- min_nz / 10
        print(paste0("Min unc: ", min(uncs$value), " max unc: ", max(uncs$value)))
        plot <- ggplot(data = uncs, mapping = aes(x = value, color = dist)) + 
            geom_histogram(position = "identity", fill = "white", alpha = 0.5, bins = 50) +
            scale_x_log10() +
            ggtitle(label = x["nets"]) 
        print(plot)
    }

    select_ens_type <- function(x, outputs_folder = "cifar_ood")
    {
        print(paste0("Processing co: ", x["combining_method"], " cp: ", x["coupling_method"]))
        pdf(file.path(outputs_folder, paste0(dts, "_unc_", x["combining_method"], "+", x["coupling_method"], ".pdf")))
        filt_unc_files <- uncert_files %>% filter(combining_method == x["combining_method"] & coupling_method == x["coupling_method"])

        apply(filt_unc_files, 1, plot_distribution)

        dev.off()
    }
   
    apply(tps, 1, select_ens_type)
}

plot_uncert_dists(base_dir_C10, dts = "C10vC100")
plot_uncert_dists(base_dir_C100, dts = "C100vC10")