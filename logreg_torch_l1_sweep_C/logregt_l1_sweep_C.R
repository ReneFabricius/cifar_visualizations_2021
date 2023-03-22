library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(gsubfn)
library(lemon)
library(scales)
library(Cairo)

source("utils.R")


plot_plots <- function(eval_dir, base_dir, cifar)
{
    metrics <- c("acc1", "nll", "ece")
    metric_names <- c(accuracy = "presnosť", nll = "NLL", ece = "ECE")

    net_df <- read.csv(file.path(eval_dir, "net_metrics.csv"))
    ens_df_pwc <- read.csv(file.path(eval_dir, "ens_pwc_metrics.csv"))
    ens_df_cal <- read.csv(file.path(eval_dir, "ens_cal_metrics.csv"))
    pwc_base <- read.csv(file.path(base_dir, "ens_pwc_metrics.csv")) %>% 
        filter(replication == 0) %>% 
        mutate(combining_method = dplyr::recode(
            combining_method, logreg_torch = "logreg_l1", logreg_torch_no_interc = "logreg_l1_no_interc"))
    cal_base <- read.csv(file.path(base_dir, "ens_cal_metrics.csv")) %>% filter(replication == 0)

    ens_df_pwc$C <- as.double(str_match(ens_df_pwc$combining_method, "\\{base\\_C\\:(\\d+\\.?\\d*)\\}$")[, 2])
    ens_df_pwc$method <- paste(
        str_replace(str_match(ens_df_pwc$combining_method, "^(.+?)\\{.*\\}$")[, 2], fixed("logreg_torch"), "logreg"),
        ens_df_pwc$coupling_method,
        sep = " + ")

    pwc_base <- pwc_base %>% 
        mutate(method = paste(combining_method, coupling_method, sep=" + ")) %>%
        filter(combining_method %in% c("logreg_l1", "logreg_l1_no_interc"))

    list[ens_df_cal, ens_df_pwc] <- add_combination_metrics(net_df = net_df, ens_df_cal = ens_df_cal, ens_df_pwc = ens_df_pwc)
    list[cal_base, pwc_base] <- add_combination_metrics(net_df = net_df, ens_df_cal = cal_base, ens_df_pwc = pwc_base)

    limits <- list(
        "acc1" = list("10" = c(0.0, 0.03), "100" = c(-0.02, 0.05)),
        "nll" = list("10" = c(-0.1, 0.12), "100" = c(-1.8, 0.4)),
        "ece" = list("10" = c(-0.2, 0.07), "100" = c(-0.75, 0.15)))
    limits_single <- list(
        "acc" = list("10" = c(0.0, 0.03), "100" = c(0.0, 0.09)),
        "nll" = list("10" = c(-0.05, 0.12), "100" = c(-0.5, 0.5)),
        "ece" = list("10" = c(-0.1, 0.07), "100" = c(-0.5, 0.15)))

    for (met_i in seq_along(metrics))
    {
        C_range <- log10(max(ens_df_pwc$C)) - log10(min(ens_df_pwc$C)) + 0.2
        print(C_range)
        met <- metrics[met_i]
        metric_plot <- ens_df_pwc %>%
                    ggplot() +
                    geom_boxplot(
                        data = pwc_base,
                        mapping = aes_string(
                            y = paste0(met, "_imp_best"),
                            color = shQuote("logreg_l2")),
                        width = C_range,
                        alpha = 0.5) +
                    geom_boxplot(
                        mapping = aes_string(
                            x = "C",
                            y = paste0(met, "_imp_best"),
                            group = "C"),
                        alpha = 0.5) +
                    facet_rep_grid(method ~ ., repeat.tick.labels = TRUE, scales = "free_x") +
                    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x)) +
                    coord_cartesian(ylim = limits[[met]][[as.character(cifar)]]) +
                    ylab(paste0("Zlepšenie metriky ", metric_names[met_i], " oproti najlepšej sieti")) +
                    scale_colour_discrete(name = "") +
                    ggtitle(paste0("Cifar ", cifar)) +
                    theme_bw() +
                    theme(
                        axis.text.x = element_text(angle = 90),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank())


        file_name <- paste0("logregt_l1_reg_c", cifar, "_", met, ".pdf")
        ggsave(filename = file.path("logreg_torch_l1_sweep_C", file_name), plot = metric_plot, device = cairo_pdf, height = 40)
        #dev.off()
    }
}


eval_dir_hC100 <- "/home/mordechaj/school/disertation/data/cifar/half_c100_logreg_torch_l1_sweep_C"
base_dir_hC100 <- "/home/mordechaj/school/disertation/data/cifar/half_cif100"

plot_plots(eval_dir = eval_dir_hC100, base_dir = base_dir_hC100, cifar = 100)
