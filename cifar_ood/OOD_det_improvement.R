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
library(Cairo)

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

    # au_imps_pwc$coupling_method <- as.factor(toupper(au_imps_pwc$coupling_method))
    au_imps_pwc$combining_method <- as.factor(au_imps_pwc$combining_method)
    au_imps_pwc <- au_imps_pwc %>% dplyr::mutate(
        coupling_method = as.factor(coupling_method),
        combining_method = dplyr::recode(combining_method, logreg_torch = "logreg"))


    plot_metric <- function(ood_method)
    {
        au_imps_pwc_print <- au_imps_pwc %>% filter(ens_detection == ood_method)
        if (ood_method == "UNC")
        {
            au_imps_pwc_print <- au_imps_pwc_print %>% filter(
                combining_method == "logreg",
                coupling_method != "bc")
        }

        big_box_width <- length(unique(au_imps_pwc_print$coupling_method))
        big_box_x <- (1 + big_box_width) / 2

        plot <- ggplot() +
                scale_x_discrete(breaks = levels(au_imps_pwc$coupling_method), name = "párová zväzovacia metóda") +
                geom_boxplot(
                    au_imps_cal,
                    mapping = aes(y = value, color = "TemperatureScaling", x = big_box_x),
                    width = big_box_width) +
                geom_boxplot(
                    au_imps_pwc_print,
                    mapping = aes(
                        x = coupling_method,
                        y = value,
                        color = paste0("WLE ", ifelse(ood_method == "UNC", "neistota", "MSP"))),
                        position = "dodge") +
                geom_hline(yintercept = 0.0, color = "red", linetype = "dashed", alpha = 0.7) +
                facet_grid(combining_method ~ metric) +
                scale_color_brewer(type = "qual", palette = 2, name = "metóda detekcie") +
                ylab(paste0("zlepšenie")) +
                theme_bw() +
                theme(
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())

        plot <- plot + coord_cartesian(ylim = c(ifelse(ood_method == "UNC", -0.2, -0.025), ifelse(ood_method == "UNC", 0.03, 0.04)))
        ggsave(
            filename = file.path("cifar_ood", paste0(dts, "_", ood_method, "_au_improvements.pdf")),
            plot = plot,
            device = cairo_pdf
        )

        return(plot)
    }
    ens_plot <- plot_metric("ENS")
    unc_plot <- plot_metric("UNC")

    return(list(ens_plot, unc_plot))
}

base_dir_C10 <- "/mnt/d/skola/1/weighted_ensembles/tests/test_cifar_ood_2022/C10vsC100_metrics"
base_dir_C100 <- "/mnt/d/skola/1/weighted_ensembles/tests/test_cifar_ood_2022/C100vsC10_metrics"
c10vc100_plots <- plot_improvements(base_dir_C10, "C10vC100")
c100vc10_plots <- plot_improvements(base_dir_C100, "C100vC10")

unc_10v100 <- c10vc100_plots[[2]]
unc_100v10 <- c100vc10_plots[[2]]

unc_plot <- (unc_10v100 | unc_100v10) + plot_layout(guides = "collect")
ggsave(
    filename = file.path("cifar_ood", "both_unc_au_improvements.pdf"),
    plot = unc_plot,
    device = cairo_pdf,
    width = 7,
    height = 3
)