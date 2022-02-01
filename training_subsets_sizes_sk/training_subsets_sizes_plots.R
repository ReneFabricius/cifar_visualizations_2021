library(ggplot2)
library(dplyr)
library(scales)
library(Cairo)

metrics <- c("accuracy", "nll", "ece")
metric_names <- c("presnosť", "NLL", "ECE")

ens_metrics_c10 <- read.csv("../data/data_tv_5000_c10/0/exp_subsets_sizes_train_outputs/ens_metrics.csv", stringsAsFactors = TRUE)
nets_metrics_c10 <- read.csv("../data/data_tv_5000_c10/0/exp_subsets_sizes_train_outputs/net_metrics.csv", stringsAsFactors = TRUE)


for (cp_m in unique(ens_metrics_c10$coupling_method))
{
  for (met_i in seq_along(metrics))
  {
    metric <- metrics[met_i]
    box_plt <- ens_metrics_c10 %>%
      filter(coupling_method == cp_m) %>%
      ggplot() +
      geom_boxplot(mapping = aes_string(x = "train_size", y = metric, group = "train_size")) +
      facet_grid(rows = vars(combining_method), scales = "free") +
      scale_x_log10(breaks = log_breaks(n = 10)) +
      xlab("veľkosť trénovacej množiny") +
      ylab(metric_names[met_i]) +
      ggtitle(paste0("Metrika ", metric_names[met_i], " pre rôzne veľkosti trénovacej množiny\npre pwcoup metódu ", toupper(cp_m))) +
      theme_bw()

    plot_name <- paste0("training_subsets_sizes_sk/CIF10_", cp_m, "_", metric, ".pdf")
    ggsave(filename = plot_name, plot = box_plt, device = cairo_pdf)
  }
}


ens_metrics_c100 <- read.csv("../data/data_tv_5000_c100/0/exp_subsets_sizes_train_outputs/ens_metrics.csv", stringsAsFactors = TRUE)
nets_metrics_c100 <- read.csv("../data/data_tv_5000_c100/0/exp_subsets_sizes_train_outputs/net_metrics.csv", stringsAsFactors = TRUE)


for (cp_m in unique(ens_metrics_c100$coupling_method))
{
  for (met_i in seq_along(metrics))
  {
    metric <- metrics[met_i]
    box_plt <- ens_metrics_c100 %>%
      filter(coupling_method == cp_m) %>%
      ggplot() +
      geom_boxplot(mapping = aes_string(x = "train_size", y = metric, group = "train_size")) +
      facet_grid(rows = vars(combining_method), scales = "free") +
      scale_x_log10(breaks = log_breaks(n = 10)) +
      ggtitle(paste0("Metrika ", metric_names[met_i], " pre rôzne veľkosti trénovacej množiny\npre pwcoup metódu ", toupper(cp_m))) +
      theme_bw()

    plot_name <- paste0("training_subsets_sizes_sk/CIF100_", cp_m, "_", metric, ".pdf")
    ggsave(filename = plot_name, plot = box_plt, device = cairo_pdf)
  }
}