library(namedCapture)
library(reticulate)
source("utils.R")
library(ggplot2)
library(dplyr)
library(tidyr)
library(comprehenr)
library(stringr)
library(ungeviz)
library(relayer)
library(patchwork)
library(xtable)
library(Cairo)

source("cifar_ood/curves_utils.R")

np <- import("numpy")

C10_labels <- "/mnt/d/skola/1/weighted_ensembles/tests/test_cifar_ood_2022/C10vsC100/B16/test_labels.npy"
C100_labels <- "/mnt/d/skola/1/weighted_ensembles/tests/test_cifar_ood_2022/C100vsC10/B16/test_labels.npy"
cifar_data <- "/mnt/d/skola/1/weighted_ensembles/tests/test_cifar_2021/data/cifar_data"

plot_select_configs <- function(dir, dts, sel_configs, outputs_folder = "cifar_ood_new/print")
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

    net_curves <- get_net_curves(dir = dir)
    cal_curves <- get_cal_curves(dir = dir)
    pwc_curves <- get_pwc_curves(dir = dir)

    process_config <- function(x, outputs_folder)
    {
        print(paste0(
            "Processing co: ", x$config["combining_method"],
            " cp: ", x$config["coupling_method"],
            " nets: ", x$config["nets"]))

        cur_files <- files %>% filter(
            combining_method == x$config["combining_method"] &
            coupling_method == x$config["coupling_method"] &
            nets == x$config["nets"])

        unc_ind <- np$load(file.path(dir, cur_files$unc_ind[1]))
        unc_ood <- np$load(file.path(dir, cur_files$unc_ood[1]))

        plot_uncerts <- function(width = 7, height = 7)
        {
            uncs <- data.frame(IND = unc_ind, OOD = unc_ood)
            uncs <- pivot_longer(data = uncs, cols = c("IND", "OOD"), names_to = "dist")
            uncs$value <- uncs$value + ifelse(any(uncs$value == 0), 1e-45, 0)
            print(paste0("Min unc: ", min(uncs$value), " max unc: ", max(uncs$value)))
            plot <- ggplot(data = uncs, mapping = aes(x = value, color = dist)) + 
                geom_histogram(position = "identity", fill = "white", alpha = 0.15, bins = 50) +
                scale_x_log10() +
                xlab("neistota") +
                ylab("počet") +
                scale_color_discrete(name = "Dáta") +
                theme_classic()
            ggsave(
                plot = plot,
                file = file.path(outputs_folder, paste0(
                    dts, "_unc_", x$config["combining_method"], "+", x$config["coupling_method"], "_nets_", x$config["nets"], ".pdf")),
                device = cairo_pdf,
                width = width,
                height = height)
        }

        plot_per_cl_unc <- function(width = 7, height = 7)
        {
            unc_class <- cbind(lab_name, data.frame(uncert = unc_ind))
            unc_class$name <- as.factor(unc_class$name)
            unc_class$uncert <- unc_class$uncert + ifelse(any(unc_class$uncert == 0), 1e-45, 0)
            plot <- ggplot(data = unc_class, mapping = aes(x = uncert, color = name)) +
                geom_density() +
                scale_x_log10() +
                xlab("neistota") +
                ylab("hustota") +
                scale_color_discrete(name = "Trieda") +
                theme_classic()

            ggsave(
                plot = plot,
                file = file.path(
                    outputs_folder,
                    paste0(
                        dts, "_per_class_unc_", x$config["combining_method"], "+", x$config["coupling_method"], "_nets_", x$config["nets"], ".pdf")),
                device = cairo_pdf,
                width = width,
                height = height)
        }

        plot_curve <- function(plotted_curve, width = 7, height = 7)
        {
            comb_net_curves <- net_curves[[plotted_curve]] %>% filter(net %in% stringr::str_split(x$config["nets"], fixed("+"), simplify = TRUE))
            comb_cal_curves <- cal_curves[[plotted_curve]] %>% filter(nets == x$config["nets"])
            comb_pwc_curves <- pwc_curves[[plotted_curve]] %>% filter(
                combining_method == x$config["combining_method"],
                coupling_method == x$config["coupling_method"],
                nets == x$config["nets"]) %>% mutate(
                    combining_method = recode(combining_method, logreg_torch = "logreg"))

            plot <-
                ggplot() +
                (geom_path(
                    data = comb_net_curves,
                    mapping = aes(
                        x = !! sym(curve_axes[paste0(plotted_curve, ".x")]),
                        y = !! sym(curve_axes[paste0(plotted_curve, ".y")]),
                        net_col = net),
                    linetype = "dashed") %>% relayer::rename_geom_aes(new_aes = c("colour" = "net_col"))) +
                geom_path(
                    data = comb_cal_curves,
                    mapping = aes(
                        x = !! sym(curve_axes[paste0(plotted_curve, ".x")]),
                        y = !! sym(curve_axes[paste0(plotted_curve, ".y")]),
                        color = calibrating_method)
                ) +
                scale_colour_brewer(
                    aesthetics = "net_col", palette = 1,
                    name = "Sieť", type = "qual") +
                (geom_path(
                    data = comb_pwc_curves,
                    mapping = aes(
                        x = !! sym(curve_axes[paste0(plotted_curve, ".x")]),
                        y = !! sym(curve_axes[paste0(plotted_curve, ".y")]),
                        comb_col = combining_method,
                        linetype = det_met)
                ) %>% relayer::rename_geom_aes(new_aes = c("colour" = "comb_col"))) +
                scale_colour_brewer(
                    aesthetics = "comb_col", palette = 2,
                    name = "Kombinačná metóda", type = "qual") +
                scale_color_manual(values = c("black"), name = "baseline") +
                scale_linetype_manual(name = "Metóda detekcie", values = c("solid", "dotted")) +
                theme_classic()

            ggsave(
                filename = file.path(outputs_folder, paste0(
                    dts, "_", plotted_curve, "_",
                    x$config["combining_method"], "+", x$config["coupling_method"],
                    "_nets_", x$config["nets"], ".pdf")),
                plot = plot,
                device = cairo_pdf,
                width = width,
                height = height)
        }

        lapply(X = x$plots, FUN = function(f_info) {do.call(what = f_info$what, args = f_info$args)})
    }

    lapply(sel_configs, process_config, outputs_folder = outputs_folder)
}

conf_10v100 <- list(
    list(
        config = list(
            combining_method = "logreg_torch_no_interc", coupling_method = "bc",
            nets = "R50x1+R101x3+R50_B16+B16+M_B16"),
        plots = list(
            list(what = "plot_uncerts", args = list(width = 7, height = 7))
            )
    ),
    list(
        config = list(combining_method = "logreg_torch_no_interc", coupling_method = "m2", nets = "R50x1+R101x3+R50_B16+B16+M_B16"),
        plots = list(
            list(what = "plot_per_cl_unc", args = list(width = 7, height = 7))
            )
    ),
    list(
        config = list(combining_method = "logreg_torch_no_interc", coupling_method = "m2", nets = "R50x1+R101x3+R50_B16+B16+M_B16"),
        plots = list(
            list(what = "plot_uncerts", args = list(width = 7, height = 7))
        )
    )
)

conf_100v10 <- list(
    list(
        config = list(combining_method = "logreg_torch_no_interc", coupling_method = "m2", nets = "R50x1+R101x3+R50_B16+B16+M_B16"),
        plots = list(
            list(what = "plot_uncerts", args = list(width = 4, height = 4)),
            list(what = "plot_curve", args = list(plotted_curve = "prc", width = 4, height = 4.5))
        )
    )
)

base_dir_C10 <- "/home/mordechaj/school/disertation/data/cifar_ood/C10vsC100_ens"
base_dir_C100 <- "/home/mordechaj/school/disertation/data/cifar_ood/C100vsC10_ens"


plot_select_configs(base_dir_C10, dts = "C10vC100", sel_configs = conf_10v100)
plot_select_configs(base_dir_C100, dts = "C100vC10", sel_configs = conf_100v10)