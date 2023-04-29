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
library(coin)

plot_improvements <- function(dir, dts, single_plot = FALSE, configs = NULL)
{
    pwc_metrics <- read.csv(file.path(dir, "ens_pwc_metrics.csv"))
    cal_metrics <- read.csv(file.path(dir, "ens_cal_metrics.csv"))
    net_metrics <- read.csv(file.path(dir, "net_metrics.csv"))

    pwc_metrics <- pwc_metrics %>% 
        mutate(combining_method = recode(
            combining_method, 
            logreg_torch_no_interc = "logreg_no_interc",
            logreg_torch = "logreg"))

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
            combination_id, combining_method, coupling_method, combination_size,
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
        select(combination_id, combination_size, calibrating_method, CAL_MSP_o_max_MSP_AUROC, CAL_MSP_o_max_MSP_AUPRC) %>%
        pivot_longer(
            cols = c(CAL_MSP_o_max_MSP_AUPRC, CAL_MSP_o_max_MSP_AUROC),
            names_to = c("metric"),
            names_pattern = "CAL_MSP_o_max_MSP_(.*?)$"
        )

    # au_imps_pwc$coupling_method <- as.factor(toupper(au_imps_pwc$coupling_method))
    au_imps_pwc$combining_method <- as.factor(au_imps_pwc$combining_method)
    au_imps_pwc <- au_imps_pwc %>% dplyr::mutate(
        coupling_method = as.factor(coupling_method))


    plot_metric <- function(ood_method)
    {
        if (ood_method != "BOTH")
        {
            au_imps_pwc_print <- au_imps_pwc %>% filter(ens_detection == ood_method)
        }
        else 
        {
           au_imps_pwc_print <- au_imps_pwc
        }

        #au_imps_pwc_print <- au_imps_pwc_print %>% filter(
        #    combining_method == "logreg",
        #    coupling_method != "bc")

        big_box_width <- length(unique(au_imps_pwc_print$coupling_method))
        big_box_x <- (1 + big_box_width) / 2

        au_imps_pwc_print <- au_imps_pwc_print %>% mutate(
            ens_detection = dplyr::recode(
                ens_detection,
                UNC = "WLE neistota", ENS = "WLE MSP"))

        plot <- ggplot() +
                scale_x_discrete(breaks = levels(au_imps_pwc$coupling_method), name = "párová zväzovacia metóda") +
                geom_boxplot(
                    au_imps_cal,
                    mapping = aes(y = value, color = "baseline MSP", x = big_box_x),
                    width = big_box_width) +
                geom_boxplot(
                    au_imps_pwc_print,
                    mapping = aes(
                        x = coupling_method,
                        y = value,
                        color = ens_detection),
                    position = position_dodge2(width = 1, padding = 0.2),
                    alpha = 0.7) +
                geom_hline(yintercept = 0.0, color = "red", linetype = "dashed", alpha = 0.7) +
                facet_grid(combining_method ~ metric) +
                scale_color_brewer(type = "qual", palette = 2, name = "metóda detekcie") +
                ylab(paste0("zlepšenie")) +
                theme_bw() +
                theme(
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())

        if (ood_method == "UNC")
        {
            plot <- plot + coord_cartesian(ylim = c(-0.2, 0.03))
        }
        else if (ood_method == "MSP") 
        {
            plot <- plot + coord_cartesian(ylim = c(-0.025, 0.04))
        }
        else if (ood_method == "BOTH")
        {
            if (dts == "C10vC100")
            {
                plot <- plot + coord_cartesian(ylim = c(-0.04, 0.02))
            }
            else if (dts == "C100vC10")
            {
                plot <- plot + coord_cartesian(ylim = c(-0.2, 0.04))
            }
        }

        plot <- plot + ggtitle(dts)

        ggsave(
            filename = file.path("cifar_ood_new", paste0(dts, "_", ood_method, "_au_improvements.pdf")),
            plot = plot,
            device = cairo_pdf
        )

        return(plot)
    }

    plot_per_size <- function(configurations)
    {
        pwc_confs <- au_imps_pwc %>%
            mutate(
                ens_detection = dplyr::recode(
                    ens_detection,
                    UNC = "neistota", ENS = "MSP")) %>%
            mutate(
                method = paste0(
                    combining_method, " + ", coupling_method,
                    " ", ens_detection)) %>%
            filter(method %in% configurations) %>%
            select(-c(combining_method, coupling_method, ens_detection))

        cal_df <- au_imps_cal %>%
                    mutate(method = "baseline MSP") %>%
                    select(-c(calibrating_method))

        plot_df <- rbind(pwc_confs, cal_df)

        plot_df <- plot_df %>%
            mutate(
                method = as.factor(method),
                combination_size = as.factor(combination_size))

        plot <- ggplot() +
                geom_boxplot(
                    data = plot_df,
                    mapping = aes(x = combination_size, y = value, color = method)
                ) +
                geom_hline(yintercept = 0.0, color = "red", linetype = "dashed", alpha = 0.7) +
                facet_grid(. ~ metric) +
                ylab("zlepšenie") +
                xlab("veľkosť ansámblu") +
                theme_bw() +
                theme(
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank()) +
                guides(color = guide_legend(title = "metóda"))

        plot <- plot + ggtitle(dts)

        return(plot)
    }

    perform_stat_tests <- function()
    {
        signif <- 0.05

        pwc_df <- au_imps_pwc %>%
            mutate(
                ens_detection = dplyr::recode(
                    ens_detection,
                    UNC = "neistota", ENS = "MSP"))

        metrics <- c("AUROC", "AUPRC")

        test_df <- data.frame(
            combining_method = c(),
            coupling_method = c(),
            ens_detection = c(),
            metric = c(),
            p_value = c(),
            batter_method = c())

        for (co in unique(pwc_df$combining_method))
        {
            co_pwc_df <- pwc_df %>% filter(combining_method == co)
            for (cp in unique(co_pwc_df$coupling_method))
            {
                cp_pwc_df <- co_pwc_df %>% filter(coupling_method == cp)
                for (ed in unique(cp_pwc_df$ens_detection))
                {
                    ed_pwc_df <- cp_pwc_df %>% filter(ens_detection == ed)
                    for (met in unique(ed_pwc_df$metric))
                    {
                        met_pwc_df <- ed_pwc_df %>% filter(metric == met)
                        
                        pwc_res <- met_pwc_df %>% select(combination_id, value) %>%
                            mutate(method = "WLE")
                        cal_res <- au_imps_cal %>% filter(metric == met) %>%
                            select(combination_id, value) %>% mutate(method = "BSL")

                        test_data <- rbind(pwc_res, cal_res) %>%
                            mutate(
                                method = factor(method, levels = c("WLE", "BSL")),
                                combination_id = factor(combination_id))

                        test <- symmetry_test(
                            formula = value ~ method | combination_id,
                            data = test_data,
                            distribution = "approximate")

                        pval <- pvalue(test)
                        win <- ""
                        if (pval < signif)
                        {
                            test_means <- test_data %>% group_by(method) %>%
                                summarize(mean_met = mean(value), .groups = "drop")

                            means <- test_means$mean_met
                            names(means) <- test_means$method
                            if (means["WLE"] - means["BSL"] > 0)
                            {
                                win <- "WLE"
                            }
                            else {
                               win <- "BSL"
                            }
                        }

                        test_row <- list(
                            "combining_method" = co, "coupling_method" = cp,
                            "ens_detection" = ed, "metric" = met, p.val = pval, better_method = win)

                        test_df <- rbind(test_df, test_row)
                    }
                }
            }
        }

        return(test_df)
    }

    ret <- list()

    if (!single_plot)
    {
        ens_plot <- plot_metric("ENS")
        unc_plot <- plot_metric("UNC")
        ret[["ens_plot"]] <- ens_plot
        ret[["unc_plot"]] <- unc_plot
    }
    else
    {
       ret[["both_plot"]] <- plot_metric("BOTH")
    }

    if (!is.null(configs))
    {
        ret[["per_size_plot"]] <- plot_per_size(configs)
    }

    ret[["tests"]] <- perform_stat_tests()

    return(ret)
}

base_dir_C10 <- "/home/mordechaj/school/disertation/data/cifar_ood/C10vsC100_ens" 
base_dir_C100 <- "/home/mordechaj/school/disertation/data/cifar_ood/C100vsC10_ens" 

c10vc100_plots <- plot_improvements(base_dir_C10, "C10vC100")
c100vc10_plots <- plot_improvements(base_dir_C100, "C100vC10")


unc_10v100 <- c10vc100_plots[["unc_plot"]]
unc_100v10 <- c100vc10_plots[["unc_plot"]]

unc_plot <- (unc_10v100 | unc_100v10) + plot_layout(guides = "collect")
ggsave(
    filename = file.path("cifar_ood_new", "print", "both_unc_au_improvements.pdf"),
    plot = unc_plot,
    device = cairo_pdf,
    width = 7,
    height = 3
)

c10vc100_single <- plot_improvements(
    base_dir_C10, "C10vC100", single_plot = TRUE,
    configs = list("grad_m2 + m1 neistota", "logreg_no_interc + bc MSP"))
c100vc10_single <- plot_improvements(
    base_dir_C100, "C100vC10", single_plot = TRUE,
    configs = list("grad_m2 + m1 neistota", "logreg_no_interc + bc MSP"))

ood_plot <- (c10vc100_single[["both_plot"]] | c100vc10_single[["both_plot"]]) + plot_layout(guides = "collect")
ggsave(
    filename = file.path("cifar_ood_new", "print", "both_both_au_improvements.pdf"),
    plot = ood_plot,
    device = cairo_pdf,
    width = 7,
    height = 4
)

per_size_plot <- (c10vc100_single[["per_size_plot"]] / c100vc10_single[["per_size_plot"]]) + plot_layout(guides = "collect")
ggsave(
    filename = file.path("cifar_ood_new", "print", "both_per_size_improvements.pdf"),
    plot = per_size_plot,
    device = cairo_pdf,
    width = 7,
    height = 7
)

test_results <- rbind(
    c10vc100_single[["tests"]] %>% mutate(dtset = "c10vc100"),
    c100vc10_single[["tests"]] %>% mutate(dtset = "c100vc10"))

write.csv(test_results, file.path("cifar_ood_new", "print", "test_results.csv"), row.names = F)