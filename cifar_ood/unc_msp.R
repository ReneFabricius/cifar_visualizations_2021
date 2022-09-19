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

base_dir_C10 <- "D:/skola/1/weighted_ensembles/tests/test_cifar_ood_2022/C10vsC100_metrics"
base_dir_C100 <- "D:/skola/1/weighted_ensembles/tests/test_cifar_ood_2022/C100vsC10_metrics"

plot_unc_msp <- function(dir, dts)
{
    uncert_files <- get_unc_files_id_ood(dir = dir)
    uncert_files <- uncert_files %>% rename(unc_f_id = file_id) %>% rename(unc_f_ood = file_ood)
    output_files <- get_output_files_id_ood(dir = dir)
    output_files <- output_files %>% rename(out_f_id = file_id) %>% rename(out_f_ood = file_ood)
    res_files <- merge(uncert_files, output_files)
    tps <- unique(res_files[, c("combining_method", "coupling_method")])

    plot_distribution <- function(x, outputs_folder = "cifar_ood")
    {
        id_unc <- np$load(file.path(dir, x["unc_f_id"]))
        ood_unc <- np$load(file.path(dir, x["unc_f_ood"]))
        id_msp <- rowMaxs(np$load(file.path(dir, x["out_f_id"])), TRUE)
        ood_msp <- rowMaxs(np$load(file.path(dir, x["out_f_ood"])), TRUE)
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

    select_ens_type <- function(x, outputs_folder = "cifar_ood")
    {
        print(paste0("Processing co: ", x["combining_method"], " cp: ", x["coupling_method"]))
        pdf(file.path(outputs_folder, paste0(dts, "_unc_msp_", x["combining_method"], "+", x["coupling_method"], ".pdf")))
        plot.new()
        text(0.5, 0.5, "Density plots of relation between\nunc - uncertainty produced by coupling method and\nmsp - maximum softmax probability of the corresponding output.")
        filt_res_files <- res_files %>% filter(combining_method == x["combining_method"] & coupling_method == x["coupling_method"])

        apply(filt_res_files, 1, plot_distribution)

        dev.off()
    }

    apply(tps, 1, select_ens_type)
}


plot_unc_msp(base_dir_C10, dts = "C10vC100")
plot_unc_msp(base_dir_C100, dts = "C100vC10")