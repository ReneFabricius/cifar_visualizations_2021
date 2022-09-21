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
    net_metrics <- read.csv(file.path(dir, "net_metrics.csv"))

    for (comb_id in unique(pwc_metrics[, "combination_id"]))
    {
        cur_net_mets <- net_metrics[as.logical(pwc_metrics[pwc_metrics$combination_id == comb_id, ][1, net_metrics$network]), ]
        pwc_metrics[pwc_metrics$combination_id == comb_id, "best_net_MSP_AUROC"] <- max(cur_net_mets$MSP_AUROC)
        pwc_metrics[pwc_metrics$combination_id == comb_id, "best_net_MLI_AUROC"] <- max(cur_net_mets$MLI_AUROC)
        pwc_metrics[pwc_metrics$combination_id == comb_id, "best_net_MSP_AUPRC"] <- max(cur_net_mets$MSP_AUPRC)
        pwc_metrics[pwc_metrics$combination_id == comb_id, "best_net_MLI_AUPRC"] <- max(cur_net_mets$MLI_AUPRC)
    }

    pwc_summary <- pwc_metrics %>% group_by(combining_method, coupling_method) %>% summarise(
        UNC_AUROC_o_max_MSP_AUROC = mean(UNC_AUROC - best_net_MSP_AUROC),
        UNC_AUPRC_o_max_MSP_AUPRC = mean(UNC_AUPRC - best_net_MSP_AUPRC),
        ENS_AUROC_o_max_MSP_AUROC = mean(MSP_AUROC - best_net_MSP_AUROC),
        ENS_AUPRC_o_max_MSP_AUPRC = mean(MSP_AUPRC - best_net_MSP_AUPRC)
    )

    au_imps <- pwc_metrics %>% 
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

    plot <- ggplot(au_imps) +
                geom_boxplot(mapping = aes(x = coupling_method, color = metric, y = value), position = "dodge") +
                geom_hline(yintercept = 0, color = "green", linetype = "dashed") +
                facet_grid(combining_method ~ ens_detection) +
                theme_classic()
    
    ggsave(
        filename = file.path("cifar_ood", paste0(dts, "_au_improvements.pdf")),
        plot=plot)
    ggsave(
        filename = file.path("cifar_ood", paste0(dts, "_au_improvements_zoom.pdf")),
        plot=plot + coord_cartesian(ylim = c(-0.1, 0.05)))

}

base_dir_C10 <- "D:/skola/1/weighted_ensembles/tests/test_cifar_ood_2022/C10vsC100_metrics"
base_dir_C100 <- "D:/skola/1/weighted_ensembles/tests/test_cifar_ood_2022/C100vsC10_metrics"
plot_improvements(base_dir_C10, "C10vC100")
plot_improvements(base_dir_C100, "C100vC10")