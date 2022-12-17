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


plot_det_metrics <- function(dir, dts)
{
    pwc_metrics <- read.csv(file.path(dir, "ens_pwc_metrics.csv"))
    net_metrics <- read.csv(file.path(dir, "net_metrics.csv"))

    plot_combination <- function(x, metrics)
    {
        cur_mets <- metrics %>% filter(combination_id == x["combination_id"])
        cur_net_mets <- net_metrics[as.logical(cur_mets[1, net_metrics$network]), ]
        
        auroc_plot <- ggplot() +
        (
            geom_hline(
            data = cur_net_mets,
            mapping = aes(yintercept = MSP_AUROC, colour1 = network),
            linetype = "dashed"
            ) %>%
            rename_geom_aes(new_aes = c("colour" = "colour1"))
        ) +
        (
            geom_hopline(
            data = cur_mets,
            mapping = aes(
                x = coupling_method, y = MSP_AUROC,
                colour2 = "MSP"
            ),
            linewidth = 0.8, width = 0.11,
            position = position_dodge(width = 0.65)
            ) %>%
            rename_geom_aes(new_aes = c("colour" = "colour2"))
        ) +
        (
            geom_hopline(
            data = cur_mets,
            mapping = aes(
                x = coupling_method, y = UNC_AUROC,
                colour2 = "UNC"
            ),
            linewidth = 0.8, width = 0.11,
            position = position_dodge(width = 0.65)
            ) %>%
            rename_geom_aes(new_aes = c("colour" = "colour2"))
        ) +
        scale_colour_brewer(
            aesthetics = "colour1", palette = 1,
            name = "network", type = "qual"
        ) +
        scale_colour_brewer(
            aesthetics = "colour2", palette = 2,
            name = "detection method", type = "qual"
        ) +
        theme_classic() +
        theme(
            axis.text.x = element_blank(),
            axis.title.x = element_blank()
        )
        auprc_plot <- ggplot() +
        (
            geom_hline(
            data = cur_net_mets,
            mapping = aes(yintercept = MSP_AUPRC, colour1 = network),
            linetype = "dashed"
            ) %>%
            rename_geom_aes(new_aes = c("colour" = "colour1"))
        ) +
        (
            geom_hopline(
            data = cur_mets,
            mapping = aes(
                x = coupling_method, y = MSP_AUPRC,
                colour2 = "MSP"
            ),
            linewidth = 0.8, width = 0.11,
            position = position_dodge(width = 0.65)
            ) %>%
            rename_geom_aes(new_aes = c("colour" = "colour2"))
        ) +
        (
            geom_hopline(
            data = cur_mets,
            mapping = aes(
                x = coupling_method, y = UNC_AUPRC,
                colour2 = "UNC"
            ),
            linewidth = 0.8, width = 0.11,
            position = position_dodge(width = 0.65)
            ) %>%
            rename_geom_aes(new_aes = c("colour" = "colour2"))
        ) +
        scale_colour_brewer(
            aesthetics = "colour1", palette = 1,
            name = "network", type = "qual"
        ) +
        scale_colour_brewer(
            aesthetics = "colour2", palette = 2,
            name = "detection method", type = "qual"
        ) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90))

        print(auroc_plot / auprc_plot + plot_layout(guides = "collect") +
        plot_annotation(title = paste(
            "OOD detection metrics",
            sep = "\n"
        )))
    }

    process_co <- function(x, outputs_folder = "cifar_ood")
    {
        print(sprintf("Processing combining method: %s", x["combining_method"]))
        pdf(file.path(outputs_folder,paste0(dts, "_metrics_", x["combining_method"], ".pdf")))
        plot.new()
        text(0.5, 0.5, "Area under ROC and PR curves.\nLong dashed lines represent performance of combined networks.\nShort lines are performances of ensembles.")
        filt_metrics <- pwc_metrics %>% filter(combining_method == x["combining_method"])
        comb_ids <- unique(filt_metrics["combination_id"])
        apply(comb_ids, 1, plot_combination, metrics = filt_metrics)
        dev.off()
    }

    comb_methods <- unique(pwc_metrics["combining_method"])
    apply(comb_methods, 1, process_co)
}

base_dir_C10 <- "D:/skola/1/weighted_ensembles/tests/test_cifar_ood_2022/C10vsC100_metrics"
base_dir_C100 <- "D:/skola/1/weighted_ensembles/tests/test_cifar_ood_2022/C100vsC10_metrics"
plot_det_metrics(base_dir_C10, "C10vC100")
plot_det_metrics(base_dir_C100, "C100vC10")