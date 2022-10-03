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

plot_improvements <- function(dir, dts)
{
    pwc_metrics <- read.csv(file.path(dir, "ens_pwc_metrics.csv"))
    cal_metrics <- read.csv(file.path(dir, "ens_cal_metrics.csv"))
    net_metrics <- read.csv(file.path(dir, "net_metrics.csv"))

    best_net_mets <- function(comb_id)
    {

        cur_net_mets <- net_metrics[as.logical(pwc_metrics[pwc_metrics$combination_id == comb_id, ][1, net_metrics$network]), ]
        comb_mets <- data.frame(as.list(comb_id)) %>% mutate(
            best_net_MSP_AUROC = max(cur_net_mets$MSP_AUROC),
            best_net_MLI_AUROC = max(cur_net_mets$MLI_AUROC),
            best_net_MSP_AUPRC = max(cur_net_mets$MSP_AUPRC),
            best_net_MLI_AUPRC = max(cur_net_mets$MLI_AUPRC))
    }
 
    best_net_aus <- dplyr::bind_rows(
        apply(unique(pwc_metrics[c("combination_id")]), 1, best_net_mets))
    pwc_metrics <- merge(pwc_metrics, best_net_aus)
    cal_metrics <- merge(cal_metrics, best_net_aus)

    pwc_summary <- pwc_metrics %>% group_by(combining_method, coupling_method) %>% summarise(
        UNC_AUROC_o_max_MSP_AUROC = mean(UNC_AUROC - best_net_MSP_AUROC),
        UNC_AUPRC_o_max_MSP_AUPRC = mean(UNC_AUPRC - best_net_MSP_AUPRC),
        ENS_AUROC_o_max_MSP_AUROC = mean(MSP_AUROC - best_net_MSP_AUROC),
        ENS_AUPRC_o_max_MSP_AUPRC = mean(MSP_AUPRC - best_net_MSP_AUPRC)
    )

    au_imps_pwc <- pwc_metrics %>%
        mutate(
            UNC_AUROC_o_max_MSP_AUROC = UNC_AUROC - best_net_MSP_AUROC,
            UNC_AUPRC_o_max_MSP_AUPRC = UNC_AUPRC - best_net_MSP_AUPRC,
            ENS_AUROC_o_max_MSP_AUROC = MSP_AUROC - best_net_MSP_AUROC,
            ENS_AUPRC_o_max_MSP_AUPRC = MSP_AUPRC - best_net_MSP_AUPRC) %>%
        select(
            combination_id, combining_method, coupling_method,
            UNC_AUROC_o_max_MSP_AUROC, UNC_AUPRC_o_max_MSP_AUPRC, ENS_AUROC_o_max_MSP_AUROC, ENS_AUPRC_o_max_MSP_AUPRC) %>%
        pivot_longer(
            cols=c(UNC_AUROC_o_max_MSP_AUROC, UNC_AUPRC_o_max_MSP_AUPRC, ENS_AUROC_o_max_MSP_AUROC, ENS_AUPRC_o_max_MSP_AUPRC),
            names_to=c("ens_detection", "metric"),
            names_pattern="(.*?)_(.*?)_o_max_MSP_.*"
        )

    au_imps_cal <- cal_metrics %>%
        mutate(
            CAL_MSP_o_max_MSP_AUROC = MSP_AUROC - best_net_MSP_AUROC,
            CAL_MSP_o_max_MSP_AUPRC = MSP_AUPRC - best_net_MSP_AUPRC) %>%
        select(combination_id, calibrating_method, CAL_MSP_o_max_MSP_AUROC, CAL_MSP_o_max_MSP_AUPRC) %>%
        pivot_longer(
            cols = c(CAL_MSP_o_max_MSP_AUPRC, CAL_MSP_o_max_MSP_AUROC),
            names_to = c("metric"),
            names_pattern = "CAL_MSP_o_max_MSP_(.*?)$"
        )

    big_box_width <- length(unique(au_imps_pwc$coupling_method))
    big_box_x <- (1 + big_box_width) / 2

    plot <- ggplot() +
                geom_boxplot(au_imps_pwc, mapping = aes(x = coupling_method, met_col = metric, y = value), position = "dodge") %>%
                    relayer::rename_geom_aes(new_aes = c("colour" = "met_col")) +
                geom_boxplot(au_imps_cal, mapping = aes(y = value, color = "TemperatureScaling", x = big_box_x), width = big_box_width) +
                geom_hline(yintercept = 0, color = "green", linetype = "dashed") +
                facet_grid(combining_method ~ ens_detection) +
                scale_colour_brewer(aesthetics = "met_col", name = "metric", type = "qual") +
                scale_color_manual(values = c("black"), name = "baseline") +
                theme_classic()
    
    ggsave(
        filename = file.path("cifar_ood", paste0(dts, "_au_improvements.pdf")),
        plot=plot)
    ggsave(
        filename = file.path("cifar_ood", paste0(dts, "_au_improvements_zoom.pdf")),
        plot=plot + coord_cartesian(ylim = c(-0.05, 0.02)))

}

base_dir_C10 <- "D:/skola/1/weighted_ensembles/tests/test_cifar_ood_2022/C10vsC100_metrics"
base_dir_C100 <- "D:/skola/1/weighted_ensembles/tests/test_cifar_ood_2022/C100vsC10_metrics"
plot_improvements(base_dir_C10, "C10vC100")
plot_improvements(base_dir_C100, "C100vC10")