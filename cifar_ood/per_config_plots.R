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
library(Rfast)
source("utils.R")

np <- import("numpy")

C10_labels <- "/mnt/d/skola/1/weighted_ensembles/tests/test_cifar_ood_2022/C10vsC100/B16/test_labels.npy"
C100_labels <- "/mnt/d/skola/1/weighted_ensembles/tests/test_cifar_ood_2022/C100vsC10/B16/test_labels.npy"
cifar_data <- "/mnt/d/skola/1/weighted_ensembles/tests/test_cifar_2021/data/cifar_data"

plot_per_config <- function(dir, dts, outputs_folder = "cifar_ood_new")
{
    files <- find_files_by_ptrn(dir = dir, ptrn = list(
        unc_ind = "pwc_ens_test_unc",
        unc_ood = "pwc_ens_ood_unc",
        out_ind = "pwc_ens_test_output",
        out_ood = "pwc_ens_ood_output"))
    configs <- unique(files[, c("combining_method", "coupling_method")])
    test_labels_f <- ifelse(dts == "C10vC100", C10_labels, C100_labels)
    test_labels <- data.frame(idx = np$load(test_labels_f))
    class_names <- read.csv(file.path(cifar_data, paste0(str_split(dts, pattern = "v")[[1]][1], "_names.csv")))
    lab_name <- merge(x = test_labels, y = class_names)

    plot_distribution <- function(x, outputs_folder)
    {
        id_unc <- np$load(file.path(dir, x["unc_ind"]))
        ood_unc <- np$load(file.path(dir, x["unc_ood"]))
        uncs <- data.frame(id = id_unc, ood = ood_unc)
        uncs <- pivot_longer(data = uncs, cols = c("id", "ood"), names_to = "dist")
        uncs$value <- uncs$value + ifelse(any(uncs$value == 0), 1e-45, 0)   # can't get to work pseudo_log breaks
        print(paste0("Min unc: ", min(uncs$value), " max unc: ", max(uncs$value)))
        plot <- ggplot(data = uncs, mapping = aes(x = value, color = dist)) + 
            geom_histogram(position = "identity", fill = "white", alpha = 0.5, bins = 50) +
            scale_x_log10() +
            ggtitle(label = x["nets"])
        print(plot)
    }

    plot_per_class_distr <- function(x, outputs_folder)
    {
        id_unc <- np$load(file.path(dir, x["unc_ind"]))
        unc_class <- cbind(lab_name, data.frame(uncert = id_unc))
        unc_class$name <- as.factor(unc_class$name)
        unc_class$uncert <- unc_class$uncert + ifelse(any(unc_class$uncert == 0), 1e-45, 0)
        plot <- ggplot(data = unc_class, mapping = aes(x = uncert, color = name)) +
            geom_density() +
            scale_x_log10() +
            ggtitle(label = x["nets"])
        print(plot)
    }

    plot_msp_unc_distribution <- function(x, outputs_folder = "cifar_ood_new")
    {
        id_unc <- np$load(file.path(dir, x["unc_ind"]))
        ood_unc <- np$load(file.path(dir, x["unc_ood"]))
        id_msp <- rowMaxs(np$load(file.path(dir, x["out_ind"])), TRUE)
        ood_msp <- rowMaxs(np$load(file.path(dir, x["out_ood"])), TRUE)
        id_res <- data.frame(unc = id_unc, msp = id_msp, class = "id")
        ood_res <- data.frame(unc = ood_unc, msp = ood_msp, class = "ood")
        res_df <- rbind(id_res, ood_res)

        min_nz <- min(res_df[res_df$unc != 0, "unc"])
        res_df[res_df$unc == 0, "unc"] <- min_nz / 10
        print(paste0("Min unc: ", min(res_df$unc), " max unc: ", max(res_df$unc)))
        plot <- ggplot(data = res_df, mapping = aes(x = unc, y = msp)) +
            geom_hex(bins = 70) +
            scale_x_log10() +
            scale_fill_continuous(type = "viridis", trans = "log") +
            facet_grid(rows = vars(class)) +
            ggtitle(label = x["nets"])
        print(plot)
    }

    process_configuration <- function(x, outputs_folder)
    {
        print(paste0("Processing co: ", x["combining_method"], " cp: ", x["coupling_method"]))
        pdf(file.path(outputs_folder, paste0(dts, "_unc_", x["combining_method"], "+", x["coupling_method"], ".pdf")))
        plot.new()
        text(0.5, 0.5, "Histograms of pairwise coupling uncertainties for ood and ind samples.")
        config_files <- files %>% filter(combining_method == x["combining_method"] & coupling_method == x["coupling_method"])

        apply(config_files, 1, FUN = plot_distribution, outputs_folder = outputs_folder)

        dev.off()

        pdf(file.path(outputs_folder, paste0(dts, "_per_class_unc_", x["combining_method"], "+", x["coupling_method"], ".pdf")))
        plot.new()
        text(0.5, 0.5, "Class specific densities of IND uncertainties.")

        apply(config_files, 1, FUN = plot_per_class_distr, outputs_folder = outputs_folder)

        dev.off()

        pdf(file.path(outputs_folder, paste0(dts, "_unc_msp_", x["combining_method"], "+", x["coupling_method"], ".pdf")))
        plot.new()
        text(
            0.5, 0.5,
            "Density plots of relation between\nunc - uncertainty produced by coupling method and\nmsp - maximum softmax probability of the corresponding output.")

        apply(config_files, 1, plot_msp_unc_distribution)

        dev.off()
    }

    apply(configs, 1, process_configuration, outputs_folder = outputs_folder)
}

base_dir_C10 <- "/home/mordechaj/school/disertation/data/cifar_ood/C10vsC100_ens" 
base_dir_C100 <- "/home/mordechaj/school/disertation/data/cifar_ood/C100vsC10_ens" 


plot_per_config(base_dir_C10, dts = "C10vC100")
plot_per_config(base_dir_C100, dts = "C100vC10")