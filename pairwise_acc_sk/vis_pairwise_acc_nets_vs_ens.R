library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(LDATS)
library(ggVennDiagram)
library(stringr)
library(abind)
library(patchwork)
library(gridExtra)


#CIFAR-10
base_dir <- "../data/data_tv_5000_c10/0/exp_pairwise_acc_nets_vs_ens"
net_df <- read.csv(file.path(base_dir, "net_pairwise_acc.csv"))
cal_ens_df <- read.csv(file.path(base_dir, "ens_baseline_pairwise_acc.csv"))
ens_df <- read.csv(file.path(base_dir, "ens_pairwise_acc.csv"))

net_df[, c("class1", "class2")] <- lapply(net_df[, c("class1", "class2")], as.factor)
cal_ens_df[, c("class1", "class2")] <- lapply(cal_ens_df[, c("class1", "class2")], as.factor)
ens_df[, c("class1", "class2")] <- lapply(ens_df[, c("class1", "class2")], as.factor)

acc_limits <- c(min(min(net_df$accuracy), min(ens_df$accuracy)), 1.0)

net_plot <- net_df %>%
    ggplot(mapping = aes(x = class2, y = class1, fill = accuracy)) + geom_raster() + facet_wrap(~network, nrow = 1) +
    xlab("Trieda") +
    ylab("Trieda") +
    scale_y_discrete(limits = rev) +
    scale_fill_binned(type = "viridis", limits = acc_limits,  name = "presnosť") +
    coord_fixed() +
    ggtitle("Párové presnosti - neurónové siete") +
    theme(plot.title = element_text(hjust = 0.5),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())
  
ens_plot <- ens_df %>% filter(coupling_method == "bc") %>%
    ggplot(mapping = aes(x = class2, y = class1, fill = accuracy)) + geom_raster() +
    xlab("Trieda") +
    ylab("Trieda") +
    scale_y_discrete(limits = rev) +
    coord_fixed() +
    ggtitle("Párové presnosti - ansámbel") +
    scale_fill_binned(
        type = "viridis",
        limits = acc_limits,
        name = "presnosť",
        guide = guide_legend(
            label.theme = element_text(angle = 90),
            label.position = "bottom")) +
    theme(plot.title = element_text(hjust = 0.5),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom")


    plot <- ggarrange(net_plot, ens_plot, ncol = 1, nrow = 2, heights = c(1.1, 0.8), legend = "none") %>%
        gridExtra::grid.arrange(get_legend(ens_plot), heights = unit(c(8, 1), "in")) %>%
        annotate_figure(top = text_grob("CIFAR-10"))

    plot_name <- "pairwise_acc_sk/CIF-10_pw_acc_plot.pdf"
    ggsave(plot = plot, filename = plot_name, width = 9, height = 9, device = cairo_pdf, units = "in")

#CIFAR-100
base_dir <- "../data/data_tv_5000_c100/0/exp_pairwise_acc_nets_vs_ens"
net_df <- read.csv(file.path(base_dir, "net_pairwise_acc.csv"))
cal_ens_df <- read.csv(file.path(base_dir, "ens_baseline_pairwise_acc.csv"))
ens_df <- read.csv(file.path(base_dir, "ens_pairwise_acc.csv"))

net_df[, c("class1", "class2")] <- lapply(net_df[, c("class1", "class2")], as.factor)
cal_ens_df[, c("class1", "class2")] <- lapply(cal_ens_df[, c("class1", "class2")], as.factor)
ens_df[, c("class1", "class2")] <- lapply(ens_df[, c("class1", "class2")], as.factor)

acc_limits <- c(min(min(net_df$accuracy), min(ens_df$accuracy)), 1.0)

net_plot <- net_df %>%
    ggplot(mapping = aes(x = class2, y = class1, fill = accuracy)) + geom_raster() + facet_wrap(~network, nrow = 1) +
    xlab("Trieda") +
    ylab("Trieda") +
    scale_y_discrete(limits = rev) +
    scale_fill_binned(type = "viridis", limits = acc_limits,  name = "presnosť") +
    coord_fixed() +
    ggtitle("Párové presnosti - neurónové siete") +
    theme(plot.title = element_text(hjust = 0.5),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())
  
ens_plot <- ens_df %>% filter(coupling_method == "bc") %>%
    ggplot(mapping = aes(x = class2, y = class1, fill = accuracy)) + geom_raster() +
    xlab("Trieda") +
    ylab("Trieda") +
    scale_y_discrete(limits = rev) +
    coord_fixed() +
    ggtitle("Párové presnosti - ansámbel") +
    scale_fill_binned(
        type = "viridis",
        limits = acc_limits,
        name = "presnosť",
        guide = guide_legend(
            label.theme = element_text(angle = 90),
            label.position = "bottom")) +
    theme(plot.title = element_text(hjust = 0.5),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom")


    plot <- ggarrange(net_plot, ens_plot, ncol = 1, nrow = 2, heights = c(1.1, 0.8), legend = "none") %>%
        gridExtra::grid.arrange(get_legend(ens_plot), heights = unit(c(8, 1), "in")) %>%
        annotate_figure(top = text_grob("CIFAR-100"))

    plot_name <- "pairwise_acc_sk/CIF-100_pw_acc_plot.pdf"
    ggsave(plot = plot, filename = plot_name, width = 9, height = 9, device = cairo_pdf, units = "in")

