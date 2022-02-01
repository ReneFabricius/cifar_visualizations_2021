library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(Cairo)

metrics <- c("accuracy", "nll", "ece")
metric_names <- c("presnosť", "NLL", "ECE")
metrics_opt <- c("max", "min", "min")

# CIFAR10

net_results_c10 <- read.csv(
    "../data/data_train_val_half_c10/net_metrics.csv",
    stringsAsFactors = TRUE
)
ens_results_c10 <- read.csv(
    "../data/data_train_val_half_c10/ensemble_metrics.csv",
    stringsAsFactors = TRUE
)

ens_results_c10$coupling_method <- toupper(ens_results_c10$coupling_method)
net_cols <- gsub("-", ".", unique(net_results_c10$network))

for (comb_id in unique(ens_results_c10$combination_id))
{
    cur_comb_ens_results <- ens_results_c10 %>%
        filter(combination_id == comb_id)
    comb_nets <- gsub(
        "\\.", "_",
        net_cols[as.logical(cur_comb_ens_results[1, net_cols])]
    )
    for (met_i in seq_along(metrics))
    {
        box_plot <- cur_comb_ens_results %>% ggplot() +
            geom_boxplot(
                mapping = aes_string(
                    x = "train_set",
                    y = metrics[met_i], fill = "train_set"
                )
            ) +
            facet_grid(
                rows = vars(combining_method),
                cols = vars(coupling_method), scales = "free"
            ) +
            ggtitle(paste0(
                "CIFAR-10. Metrika ", metric_names[met_i],
                "ansámblov s kombinačnými metódami\n
                natrénovanými na rôznych trénovacích množinách.\n
                Kombinované siete: ",
                paste(comb_nets, collapse = " ")
            )) +
            xlab("trénovacia množina") +
            ylab(metric_names[met_i]) +
            scale_fill_discrete(
                name = "trénovacia množina",
                labels = c("trénovacia", "validačná")
            ) +
            theme_bw() +
            theme(axis.text.x = element_blank())

        plot_name <- paste0(
            "val_vs_train_training_sk/CIFAR-10_",
            metrics[met_i], "_",
            paste(comb_nets, collapse = "_"), ".pdf"
        )
        ggsave(filename = plot_name, plot = box_plot, device = cairo_pdf)
    }
}

tests_df <- expand.grid(
    combining_method = unique(ens_results_c10$combining_method),
    coupling_method = unique(ens_results_c10$coupling_method),
    metric = metrics,
    val_win = c(0),
    train_win = c(0),
    indecisive = c(0),
    nans = c(0)
)
sig_l <- 0.01

for (co_m in unique(ens_results_c10$combining_method))
{
    cur_co_m <- ens_results_c10 %>% filter(combining_method == co_m)
    for (met_i in seq_along(metrics))
    {
        for (cp_m in unique(cur_co_m$coupling_method))
        {
            test_res <- list(
                val_win = 0,
                train_win = 0,
                indecisive = 0,
                nans = 0
            )
            for (comb_id in unique(cur_co_m$combination_id))
            {
                cur_co_m_cp_m <- cur_co_m %>%
                    filter(coupling_method == cp_m, combination_id == comb_id)
                cur_co_m_cp_m_train <- cur_co_m_cp_m %>%
                    filter(train_set == "tt")
                cur_co_m_cp_m_val <- cur_co_m_cp_m %>%
                    filter(train_set == "vt")
                if (any(is.na(cur_co_m_cp_m_train[[metrics[met_i]]])) |
                    any(is.na(cur_co_m_cp_m_val[[metrics[met_i]]]))) {
                    test_res[["nans"]] <- test_res[["nans"]] + 1
                } else {
                    testr <- t.test(
                        cur_co_m_cp_m_train[[metrics[met_i]]],
                        cur_co_m_cp_m_val[[metrics[met_i]]]
                    )
                    if (testr$p.value >= sig_l) {
                        test_res[["indecisive"]] <- test_res[["indecisive"]] + 1
                    } else {
                        if (
                            (metrics_opt[met_i] == "min" & testr$estimate[[1]] >
                                testr$estimate[[2]]) |
                                (metrics_opt[met_i] == "max" & testr$estimate[[1]] <
                                    testr$estimate[[2]])) {
                            test_res[["val_win"]] <- test_res[["val_win"]] + 1
                        } else {
                            test_res[["train_win"]] <- test_res[["train_win"]] + 1
                        }
                    }
                }
            }
            tests_df[
                which(tests_df$combining_method == co_m & tests_df$coupling_method == cp_m & tests_df$metric == metrics[met_i]),
                c("val_win", "train_win", "indecisive", "nans")
            ] <- test_res
        }
    }
}

tests_df_longer <- pivot_longer(data = tests_df, cols = c("val_win", "train_win", "indecisive", "nans"), names_to = "result", values_to = "count")
for (met_i in seq_along(metrics))
{
    col_plot <- tests_df_longer %>%
        filter(metric == metrics[met_i]) %>%
        ggplot() +
        geom_col(mapping = aes(x = result, y = count, fill = result)) +
        facet_grid(rows = vars(combining_method), cols = vars(coupling_method)) +
        ggtitle(paste0("CIFAR-10. Výsledky štatistického testu pre metriku ", metric_names[met_i])) +
        xlab("výsledok") +
        ylab("počet") +
        scale_fill_brewer(
            type = "qual",
            palette = "Dark2",
            name = "Výsledok testu",
            labels = c("nerozhodne", "neplatné hodnoty", "trénovacia vyhráva", "validačná vyhráva")
        ) +
        theme_bw() +
        theme(
            axis.text.x = element_blank()
        )

    plot_name <- paste0("val_vs_train_training_sk/CIFAR-10_", metrics[met_i], "_test.pdf")
    ggsave(filename = plot_name, plot = col_plot, device = cairo_pdf)
}

met_comp_c10 <- ens_results_c10 %>%
    group_by(combination_id, combining_method, coupling_method, train_set) %>%
    summarise(mean_accuracy = mean(accuracy), mean_nll = mean(nll), mean_ece = mean(ece)) %>%
    pivot_wider(names_from = train_set, values_from = c(mean_accuracy, mean_nll, mean_ece)) %>%
    mutate(
        accuracy_vt_sub_tt = mean_accuracy_vt - mean_accuracy_tt,
        nll_vt_sub_tt = mean_nll_vt - mean_nll_tt,
        ece_vt_sub_tt = mean_ece_vt - mean_ece_tt
    ) %>%
    filter(!(combining_method == "lda" & coupling_method == "SBT"))

for (met_i in seq_along(metrics))
{
    dens_plt <- met_comp_c10 %>% ggplot() +
        geom_density(mapping = aes_string(x = paste0(metrics[met_i], "_vt_sub_tt"), y = "..scaled..")) +
        geom_vline(xintercept = 0.0, color = "red", linetype = "dashed") +
        facet_grid(combining_method ~ coupling_method, scales = "free") +
        ggtitle(paste0(
            "CIFAR-10\nRozdelenie rozdielov v priemere metriky ", metric_names[met_i],
            "\nmedzi ansámblami natrénovanými na validačnej a na trénovacej množine"
        )) +
        xlab(paste0("rozdiel v metrike ", metric_names[met_i])) +
        ylab("škálovaná hustota") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90))

    plot_name <- paste0("val_vs_train_training_sk/CIFAR-10_", metrics[met_i], "_diff.pdf")
    ggsave(filename = plot_name, plot = dens_plt, device = cairo_pdf)
}


# CIFAR100
net_results_c100 <- read.csv("../data/data_train_val_half_c100/net_metrics.csv", stringsAsFactors = TRUE)
ens_results_c100 <- read.csv("../data/data_train_val_half_c100/ensemble_metrics.csv", stringsAsFactors = TRUE)
ens_results_c100$coupling_method <- toupper(ens_results_c100$coupling_method)
net_cols <- gsub("-", ".", unique(net_results_c100$network))

for (comb_id in unique(ens_results_c100$combination_id))
{
    cur_comb_ens_results <- ens_results_c100 %>% filter(combination_id == comb_id)
    comb_nets <- gsub("\\.", "_", net_cols[as.logical(cur_comb_ens_results[1, net_cols])])
    for (met_i in seq_along(metrics))
    {
        box_plot <- cur_comb_ens_results %>% ggplot() +
            geom_boxplot(mapping = aes_string(x = "train_set", y = metrics[met_i], fill = "train_set")) +
            facet_grid(rows = vars(combining_method), cols = vars(coupling_method), scales = "free") +
            ggtitle(paste0(
                "CIFAR-100. Metrika ", metric_names[met_i],
                "ansámblov s kombinačnými metódami\nnatrénovanými na rôznych trénovacích množinách.\nKombinované siete: ",
                paste(comb_nets, collapse = " ")
            )) +
            scale_fill_discrete(name = "trénovacia množina", labels = c("trénovacia", "validačná")) +
            theme_bw() +
            theme(axis.text.x = element_blank())

        plot_name <- paste0("val_vs_train_training_sk/CIFAR-100_", metrics[met_i], "_", paste(comb_nets, collapse = "_"), ".pdf")
        ggsave(filename = plot_name, plot = box_plot, device = cairo_pdf)
    }
}

tests_df <- expand.grid(
    combining_method = unique(ens_results_c100$combining_method),
    coupling_method = unique(ens_results_c100$coupling_method),
    metric = metrics,
    val_win = c(0),
    train_win = c(0),
    indecisive = c(0),
    nans = c(0)
)
sig_l <- 0.01

for (co_m in unique(ens_results_c100$combining_method))
{
    cur_co_m <- ens_results_c100 %>% filter(combining_method == co_m)
    for (met_i in seq_along(metrics))
    {
        for (cp_m in unique(cur_co_m$coupling_method))
        {
            test_res <- list(val_win = 0, train_win = 0, indecisive = 0, nans = 0)
            for (comb_id in unique(cur_co_m$combination_id))
            {
                cur_co_m_cp_m <- cur_co_m %>% filter(coupling_method == cp_m, combination_id == comb_id)
                cur_co_m_cp_m_train <- cur_co_m_cp_m %>% filter(train_set == "tt")
                cur_co_m_cp_m_val <- cur_co_m_cp_m %>% filter(train_set == "vt")
                if (any(is.na(cur_co_m_cp_m_train[[metrics[met_i]]])) |
                    any(is.na(cur_co_m_cp_m_val[[metrics[met_i]]]))) {
                    test_res[["nans"]] <- test_res[["nans"]] + 1
                } else {
                    testr <- t.test(cur_co_m_cp_m_train[[metrics[met_i]]], cur_co_m_cp_m_val[[metrics[met_i]]])
                    if (testr$p.value >= sig_l) {
                        test_res[["indecisive"]] <- test_res[["indecisive"]] + 1
                    } else {
                        if (
                            (metrics_opt[met_i] == "min" & testr$estimate[[1]] > testr$estimate[[2]]) |
                                (metrics_opt[met_i] == "max" & testr$estimate[[1]] < testr$estimate[[2]])) {
                            test_res[["val_win"]] <- test_res[["val_win"]] + 1
                        } else {
                            test_res[["train_win"]] <- test_res[["train_win"]] + 1
                        }
                    }
                }
            }
            tests_df[
                which(tests_df$combining_method == co_m & tests_df$coupling_method == cp_m & tests_df$metric == metrics[met_i]),
                c("val_win", "train_win", "indecisive", "nans")
            ] <- test_res
        }
    }
}

tests_df_longer <- pivot_longer(data = tests_df, cols = c("val_win", "train_win", "indecisive", "nans"), names_to = "result", values_to = "count")
for (met_i in seq_along(metrics))
{
    col_plot <- tests_df_longer %>%
        filter(metric == metrics[met_i]) %>%
        ggplot() +
        geom_col(mapping = aes(x = result, y = count, fill = result)) +
        facet_grid(rows = vars(combining_method), cols = vars(coupling_method)) +
        ggtitle(paste0("CIFAR-100. Výsledky štatistického testu pre metriku ", metric_names[met_i])) +
        xlab("výsledok") +
        ylab("počet") +
        scale_fill_brewer(
            type = "qual",
            palette = "Dark2",
            name = "Výsledok testu",
            labels = c("nerozhodne", "neplatné hodnoty", "trénovacia vyhráva", "validačná vyhráva")
        ) +
        theme_bw() +
        theme(
            axis.text.x = element_blank()
        )

    plot_name <- paste0("val_vs_train_training_sk/CIFAR-100_", metrics[met_i], "_test.pdf")
    ggsave(filename = plot_name, plot = col_plot, device = cairo_pdf)
}

met_comp_c100 <- ens_results_c100 %>%
    group_by(combination_id, combining_method, coupling_method, train_set) %>%
    summarise(mean_accuracy = mean(accuracy), mean_nll = mean(nll), mean_ece = mean(ece)) %>%
    pivot_wider(names_from = train_set, values_from = c(mean_accuracy, mean_nll, mean_ece)) %>%
    mutate(
        accuracy_vt_sub_tt = mean_accuracy_vt - mean_accuracy_tt,
        nll_vt_sub_tt = mean_nll_vt - mean_nll_tt,
        ece_vt_sub_tt = mean_ece_vt - mean_ece_tt
    ) %>%
    filter(!(combining_method == "lda" & coupling_method == "SBT"))

for (met_i in seq_along(metrics))
{
    dens_plt <- met_comp_c100 %>% ggplot() +
        geom_density(mapping = aes_string(x = paste0(metrics[met_i], "_vt_sub_tt"), y = "..scaled..")) +
        geom_vline(xintercept = 0.0, color = "red", linetype = "dashed") +
        facet_grid(combining_method ~ coupling_method, scales = "free") +
        ggtitle(paste0(
            "CIFAR-100\nRozdelenie rozdielov v priemere metriky ", metric_names[met_i],
            "\nmedzi ansámblami natrénovanými na validačnej a na trénovacej množine"
        )) +
        xlab(paste0("rozdiel v metrike ", metric_names[met_i])) +
        ylab("škálovaná hustota") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90))

    plot_name <- paste0("val_vs_train_training_sk/CIFAR-100_", metrics[met_i], "_diff.pdf")
    ggsave(filename = plot_name, plot = dens_plt, device = cairo_pdf)
}