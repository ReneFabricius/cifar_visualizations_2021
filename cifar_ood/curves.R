library(reticulate)
library(dplyr)
library(relayer)
library(stringr)
library(ggplot2)

source("cifar_ood/curves_utils.R")

plot_per_cp <- function(dir, dts, outputs_folder = "cifar_ood")
{
    net_curves <- get_net_curves(dir = dir)
    cal_curves <- get_cal_curves(dir = dir)
    pwc_files <- get_pwc_files(dir = dir)

    process_cp <- function(cp, outputs_folder)
    {
        print(paste0("Processing coupling method: ", cp["coupling_method"]))
        pdf(file.path(outputs_folder, paste0(dts, "_curves_", cp["coupling_method"], ".pdf")))
        plot.new()
        text(0.5, 0.5, "ROC and PRC curves for different OOD detection approaches.")
        cp_pwc_files <- pwc_files %>% filter(coupling_method == cp["coupling_method"])

        plot_curve <- function(comb, plotted_curve)
        {
            print(paste0("Processing combination ", comb["nets"]))
            comb_nets <- net_curves[[plotted_curve]] %>% filter(net %in% stringr::str_split(comb["nets"], fixed("+"), simplify = TRUE))
            comb_cal <- cal_curves[[plotted_curve]] %>% filter(nets == comb["nets"])
            comb_pwc <- load_curves(
                files_info = cp_pwc_files %>% filter(curve == plotted_curve, nets == comb["nets"]),
                file_col = "file",
                nms = list(combining_method = "combining_method", coupling_method = "coupling_method", det_met = "det_met"),
                dir = dir)

            plot <-
                ggplot() +
                (geom_path(
                    data = comb_nets,
                    mapping = aes(
                        x = !! sym(curve_axes[paste0(plotted_curve, ".x")]),
                        y = !! sym(curve_axes[paste0(plotted_curve, ".y")]),
                        net_col = net),
                    linetype = "dashed") %>% relayer::rename_geom_aes(new_aes = c("colour" = "net_col"))) +
                geom_path(
                    data = comb_cal,
                    mapping = aes(
                        x = !! sym(curve_axes[paste0(plotted_curve, ".x")]),
                        y = !! sym(curve_axes[paste0(plotted_curve, ".y")]),
                        color = calibrating_method)
                ) +
                scale_colour_brewer(
                    aesthetics = "net_col", palette = 1,
                    name = "Network", type = "qual") +
                (geom_path(
                    data = comb_pwc,
                    mapping = aes(
                        x = !! sym(curve_axes[paste0(plotted_curve, ".x")]),
                        y = !! sym(curve_axes[paste0(plotted_curve, ".y")]),
                        comb_col = combining_method,
                        linetype = det_met)
                ) %>% relayer::rename_geom_aes(new_aes = c("colour" = "comb_col"))) +
                scale_colour_brewer(
                    aesthetics = "comb_col", palette = 2,
                    name = "Combining method", type = "qual") +
                scale_color_manual(values = c("black"), name = "baseline") +
                ggtitle(paste0("Curve ", plotted_curve, " for networks ", comb["nets"])) +
                theme_classic()
            print(plot)
        }

        combinations <- unique(cp_pwc_files[, c("nets")])

        apply(combinations, 1, plot_curve, plotted_curve = "roc")
        apply(combinations, 1, plot_curve, plotted_curve = "prc")

        dev.off()
    }

    cps <- unique(pwc_files[, c("coupling_method")])
    apply(cps, 1, process_cp, outputs_folder = outputs_folder)
}


base_dir_C10 <- "D:/skola/1/weighted_ensembles/tests/test_cifar_ood_2022/C10vsC100_metrics"
base_dir_C100 <- "D:/skola/1/weighted_ensembles/tests/test_cifar_ood_2022/C100vsC10_metrics"

plot_per_cp(dir = base_dir_C10, dts = "C10vC100")
plot_per_cp(dir = base_dir_C100, dts = "C100vC10")