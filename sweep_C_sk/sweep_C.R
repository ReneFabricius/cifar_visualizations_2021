library(ggplot2)
library(dplyr)
library(tidyr)
library(comprehenr)
library(stringr)
library(ungeviz)
library(relayer)
library(patchwork)
library(xtable)
library(gsubfn)
library(lemon)
library(scales)

source("utils.R")

base_dir_C10 <- "D:/skola/1/weighted_ensembles/tests/test_cifar_2021/data/data_tv_5000_c10/0/glob_sweep_C"
base_dir_C100 <- "D:/skola/1/weighted_ensembles/tests/test_cifar_2021/data/data_tv_5000_c100/0/glob_sweep_C"
eval_dir_C10 <- "D:/skola/1/weighted_ensembles/tests/test_cifar_2021/data/data_tv_5000_c10/0/evaluation_val_train"
eval_dir_C100 <- "D:/skola/1/weighted_ensembles/tests/test_cifar_2021/data/data_tv_5000_c100/0/evaluation_val_train"

plot_plots <- function(base_dir, eval_dir, cifar)
{
    metrics <- c("acc", "nll", "ece")
    metric_names <- c(accuracy = "presnosť", nll = "NLL", ece = "ECE")

    net_df <- read.csv(file.path(base_dir, "net_metrics.csv"))
    ens_df_cal <- read.csv(file.path(base_dir, "ens_cal_metrics.csv"))
    ens_df_pwc <- read.csv(file.path(base_dir, "ens_pwc_metrics.csv"))
    ens_eval_df_pwc <- read.csv(file.path(eval_dir, "ens_pwc_metrics.csv"))

    ens_df_pwc$C <- as.double(str_match(ens_df_pwc$combining_method, "\\{base\\_C\\:(\\d+\\.?\\d*)\\}$")[, 2])
    ens_df_pwc$method <- paste(str_match(ens_df_pwc$combining_method, "^(.+?)\\{.*\\}$")[, 2], ens_df_pwc$coupling_method, sep = " + ")


    list[ens_df_cal, ens_df_pwc] <- add_combination_metrics(net_df = net_df, ens_df_cal = ens_df_cal, ens_df_pwc = ens_df_pwc)

    ens_eval_df_pwc <- ens_eval_df_pwc %>% filter(grepl("\\_sweep\\_C", combining_method)) %>%
                                            mutate(method = paste(str_replace(combining_method, "\\_sweep\\_C", ""), coupling_method, sep = " + "))

    ens_eval_df_pwc <- add_combination_metrics(net_df = net_df, ens_df_cal = ens_df_cal, ens_df_pwc = ens_eval_df_pwc)[[2]]

    ens_eval_df_pwc <- ens_eval_df_pwc %>% group_by(method) %>%
                        summarise(
                            acc_imp_o_avg_median = median(acc_imp_avg),
                            nll_imp_o_avg_median = median(nll_imp_avg),
                            ece_imp_o_avg_median = median(ece_imp_avg))

    limits <- list(
        "acc" = list("10" = c(0.0, 0.03), "100" = c(0.0, 0.1)),
        "nll" = list("10" = c(-0.1, 0.12), "100" = c(-1.8, 0.4)),
        "ece" = list("10" = c(-0.2, 0.07), "100" = c(-0.75, 0.15)))
    limits_single <- list(
        "acc" = list("10" = c(0.0, 0.03), "100" = c(0.0, 0.09)),
        "nll" = list("10" = c(-0.05, 0.12), "100" = c(-0.5, 0.5)),
        "ece" = list("10" = c(-0.1, 0.07), "100" = c(-0.5, 0.15)))

    for (met_i in seq_along(metrics))
    {
        met <- metrics[met_i]
        metric_plot <- ens_df_pwc %>%
                    ggplot() +
                    geom_boxplot(mapping = aes_string(x = "C", y = paste0(met, "_imp_avg"), group = "C")) +
                    geom_hline(data = ens_eval_df_pwc, mapping = aes_string(yintercept = paste0(met, "_imp_o_avg_median"), color = shQuote("red"))) +
                    facet_rep_grid(method ~ ., repeat.tick.labels = TRUE, scales = "free_x") +
                    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x)) +
                    coord_cartesian(ylim = limits[[met]][[as.character(cifar)]]) +
                    theme_bw() +
                    theme(axis.line = element_line())

        file_name <- paste0("c", cifar, "_", met, ".pdf")
        ggsave(filename = file.path("sweep_C_sk", file_name), plot = metric_plot, device = cairo_pdf(), height = 40)
        dev.off()

        metric_plot <- ens_df_pwc %>% filter(str_split(method, pattern = fixed(" + "), simplify = TRUE)[, 1] == "logreg") %>%
                    ggplot() +
                    geom_boxplot(mapping = aes_string(x = "C", y = paste0(met, "_imp_avg"), group = "C")) +
                    geom_hline(
                        data = ens_eval_df_pwc %>% filter(str_split(method, pattern = fixed(" + "), simplify = TRUE)[, 1] == "logreg"),
                        mapping = aes_string(yintercept = paste0(met, "_imp_o_avg_median"), color = shQuote("medián sweep_C"))) +
                    facet_rep_grid(method ~ ., repeat.tick.labels = TRUE, scales = "free_x") +
                    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x)) +
                    coord_cartesian(ylim = limits_single[[met]][[as.character(cifar)]]) +
                    ylab(paste0("Zlepšenie metriky ", metric_names[met_i], " oproti priemeru sietí")) +
                    scale_colour_discrete(name = "") +
                    theme_bw() +
                    theme(axis.line = element_line())

        file_name <- paste0("print_c", cifar, "_", met, ".pdf")
        ggsave(filename = file.path("sweep_C_sk", file_name), plot = metric_plot, device = cairo_pdf(), height = 6)
        dev.off()
    }
}


plot_plots(base_dir = base_dir_C10, eval_dir = eval_dir_C10, cifar = 10)
plot_plots(base_dir = base_dir_C100, eval_dir = eval_dir_C100, cifar = 100)
