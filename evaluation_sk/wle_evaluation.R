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
source("utils.R")

base_dir_C10 <- "D:/skola/1/weighted_ensembles/tests/test_cifar_2021/data/data_tv_5000_c10/0/evaluation_val_train"
base_dir_C100 <- "D:/skola/1/weighted_ensembles/tests/test_cifar_2021/data/data_tv_5000_c100/0/evaluation_val_train"

plot_plots <- function(base_dir, cifar)
{
    metrics <- c("accuracy", "nll", "ece")
    metric_names <- c(accuracy = "presnosť", nll = "NLL", ece = "ECE")

    net_df <- read.csv(file.path(base_dir, "net_metrics.csv"))
    ens_df_cal <- read.csv(file.path(base_dir, "ens_cal_metrics.csv"))
    ens_df_pwc <- read.csv(file.path(base_dir, "ens_pwc_metrics.csv"))

    net_long <- pivot_longer(net_df,
    cols = c("accuracy", "nll", "ece"),
    names_to = "metric", values_to = "value"
    )
    ens_cal_long <- pivot_longer(ens_df_cal,
    cols = c("accuracy", "nll", "ece"),
    names_to = "metric", values_to = "value"
    )
    ens_pwc_long <- pivot_longer(ens_df_pwc,
    cols = c("accuracy", "nll", "ece"),
    names_to = "metric", values_to = "value"
    )

    networks <- net_df$network

    comb_stats_df <- data.frame(matrix(
    ncol = 14, nrow = 0,
    dimnames = list(NULL, c(
        "combination_size", "combination_id",
        "acc_min", "acc_max", "acc_avg", "acc_var",
        "nll_min", "nll_max", "nll_avg", "nll_var",
        "ece_min", "ece_max", "ece_avg", "ece_var"
    ))
    ))

    list[ens_df_cal, ens_df_pwc] <- add_combination_metrics(net_df = net_df, ens_df_cal = ens_df_cal, ens_df_pwc = ens_df_pwc)

    nets_plot <- ggplot(data = net_long) +
        geom_col(mapping = aes(x = network, y = value)) +
        facet_grid(metric ~ ., labeller = labeller(metric = metric_names), scales = "free") +
        xlab("sieť") +
        ylab("hodnota") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90)) +
        ggtitle("Metriky sietí")

    plot_name <- paste0("evaluation_sk/C", cifar, "_net_metrics.pdf")
    ggsave(plot = nets_plot, filename = plot_name, device = cairo_pdf)


    ens_pwc_plt_df <- ens_df_pwc # %>% filter(combining_method != "lda")
    ens_cal_plt_df <- ens_df_cal

    comb_methods <- c(
    "average", "prob_average",
    "cal_average", "cal_prob_average",
    "logreg", "logreg_sweep_C",
    "logreg_no_interc", "logreg_no_interc_sweep_C",
    "grad_m1", "grad_m2", "grad_bc"
    )
    comb_methods <- c(sapply(X = comb_methods, FUN = {
    function(cm) c(cm, paste(cm, "uncert", sep = "."))
    }))

    ens_pwc_plt_df$combining_method <- factor(ens_pwc_plt_df$combining_method,
    levels = comb_methods
    )

    for (sss in max(ens_cal_plt_df$combination_size))
    {
        for (ssi in unique(ens_cal_plt_df %>%
            filter(combination_size == sss) %>%
            pull(combination_id)))
        {
            cur_ens_cal <- ens_cal_plt_df %>% filter(combination_size == sss &
            combination_id == ssi)
            cur_ens_pwc <- ens_pwc_plt_df %>% filter(combination_size == sss &
            combination_id == ssi)
            cur_nets_vec <- to_vec(
            for (net in networks) {
                if (str_replace_all(net, "-", ".") %in% colnames(cur_ens_cal) &&
                cur_ens_cal[[str_replace_all(net, "-", ".")]][1] == "True") {
                net
                }
            }
            )
            cur_nets <- net_df %>% filter(network %in% cur_nets_vec)

            acc_plot <- ggplot() +
            (
                geom_hline(
                data = cur_nets,
                mapping = aes(yintercept = accuracy, colour1 = network),
                linetype = "dashed"
                ) %>%
                rename_geom_aes(new_aes = c("colour" = "colour1"))
            ) +
            geom_hline(
                data = cur_ens_cal,
                mapping = aes(yintercept = accuracy, color = "TemperatureScaling")
            ) +
            (
                geom_hpline(
                data = cur_ens_pwc,
                mapping = aes(
                    x = combining_method, y = accuracy,
                    colour2 = coupling_method
                ),
                size = 0.8, width = 0.11,
                position = position_dodge(width = 0.65)
                ) %>%
                rename_geom_aes(new_aes = c("colour" = "colour2"))
            ) +
            scale_colour_brewer(
                aesthetics = "colour1", palette = 1,
                name = "sieť", type = "qual"
            ) +
            scale_colour_brewer(
                aesthetics = "colour2", palette = 2,
                name = "párová zväzovacia metóda", type = "qual"
            ) +
            ylab("presnosť") +
            scale_color_manual(values = c("black"), name = "baseline") +
            scale_x_discrete(labels = 1:length(comb_methods)) +
            theme_classic() +
            theme(
                axis.text.x = element_text(angle = 90),
                axis.title.x = element_blank()
            )

            if (cifar == 100)
            {
                acc_plot <- acc_plot + coord_cartesian(ylim = c(0.75, 0.87))
            }

            y_limits <- layer_scales(acc_plot)$y$get_limits()
            x_limits <- layer_scales(acc_plot)$x$get_limits()
            all_y_lim <- c(y_limits[1], cur_ens_cal$all_cor)

            nll_plot <- ggplot() +
            (
                geom_hline(
                data = cur_nets,
                mapping = aes(yintercept = nll, colour1 = network),
                linetype = "dashed"
                ) %>%
                rename_geom_aes(new_aes = c("colour" = "colour1"))
            ) +
            geom_hline(
                data = cur_ens_cal,
                mapping = aes(yintercept = nll, color = "TemperatureScaling")
            ) +
            (
                geom_hpline(
                data = cur_ens_pwc,
                mapping = aes(
                    x = combining_method, y = nll,
                    colour2 = coupling_method
                ),
                size = 0.8, width = 0.11,
                position = position_dodge(width = 0.65)
                ) %>%
                rename_geom_aes(new_aes = c("colour" = "colour2"))
            ) +
            scale_colour_brewer(
                aesthetics = "colour1", palette = 1,
                name = "sieť", type = "qual"
            ) +
            scale_colour_brewer(
                aesthetics = "colour2", palette = 2,
                name = "párová zväzovacia metóda", type = "qual"
            ) +
            ylab("NLL") +
            scale_color_manual(values = c("black"), name = "baseline") +
            scale_y_reverse() +
            scale_x_discrete(labels = 1:length(comb_methods)) +
            theme_classic() +
            theme(
                axis.text.x = element_text(angle = 90),
                axis.title.x = element_blank()
            )

            nums <- c(1:length(comb_methods))
            names(nums) <- comb_methods
            max_len <- max(unlist(lapply(X = comb_methods, FUN = nchar)))
            x_labs <- lapply(
                X = nums,
                FUN = function(i) paste0(str_pad(
                    string = comb_methods[i],
                    width = 1.8 * max_len - 0.8 * nchar(comb_methods[i]) + 4,
                    side = "both",
                    pad = " "), i))

            ece_plot <- ggplot() +
            (
                geom_hline(
                data = cur_nets,
                mapping = aes(yintercept = ece, colour1 = network),
                linetype = "dashed"
                ) %>%
                rename_geom_aes(new_aes = c("colour" = "colour1"))
            ) +
            geom_hline(
                data = cur_ens_cal,
                mapping = aes(yintercept = ece, color = "TemperatureScaling")
            ) +
            (
                geom_hpline(
                data = cur_ens_pwc,
                mapping = aes(
                    x = combining_method, y = ece,
                    colour2 = coupling_method
                ),
                size = 0.8, width = 0.11,
                position = position_dodge(width = 0.65)
                ) %>%
                rename_geom_aes(new_aes = c("colour" = "colour2"))
            ) +
            scale_colour_brewer(
                aesthetics = "colour1", palette = 1,
                name = "sieť", type = "qual"
            ) +
            scale_colour_brewer(
                aesthetics = "colour2", palette = 2,
                name = "párová zväzovacia metóda", type = "qual"
            ) +
            ylab("ECE") +
            xlab("kombinačná metóda") +
            scale_color_manual(values = c("black"), name = "baseline") +
            scale_x_discrete(labels = x_labs) +
            scale_y_reverse() +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 90))

            res_plot <- acc_plot / nll_plot / ece_plot + plot_layout(guides = "collect") +
                plot_annotation(title = paste(
                    "Metriky ansámblov",
                    paste(
                    c("Nekonzistencia chýb", signif(cur_ens_cal$err_incons[[1]], 5)),
                    collapse = " "
                    ),
                    paste(
                    c("Priemerný rozptyl párových presností", signif(cur_ens_cal$mean_pwa_var[[1]], 5)),
                    collapse = " "
                    ),
                    sep = "\n"
                ))

            plot_name <- paste0("evaluation_sk/C", cifar, "_ensemble_metrics.pdf")
            ggsave(plot = res_plot, filename = plot_name, device = cairo_pdf)
        }
    }

    xax <- c(
    "err_incons", "mean_pwa_var"
    )
    yax <- c("acc_imp_avg")
    comb_ms <- c("cal_average", "cal_average.uncert", "logreg", "logreg.uncert",
                "logreg_no_interc", "logreg_no_interc.uncert", "logreg_no_interc_sweep_C",
                "logreg_no_interc_sweep_C.uncert", "logreg_sweep_C", "logreg_sweep_C.uncert",
                "grad_bc", "grad_m1", "grad_m2", "grad_m2.uncert")

    rel_ens_pwc_plt_df <- ens_pwc_plt_df %>% filter(combining_method %in% comb_ms)

    for (xa in xax)
    {
        for (ya in yax)
        {
            x_lab <- if (xa == "err_incons") "nekonzistencia chýb sietí" else if (xa == "mean_pwa_var") "priemerný rozptyl párových presností sietí"
            y_lab <- if (ya == "acc_imp_max") "zlepšenie presnosti ansámblu oproti najpresnejšej sieti"
                else if (ya == "acc_imp_avg") "zlepšenie presnosti ansámblu oproti priemeru sietí"
            cur_plot <- ggplot() +
            geom_point(
                data = ens_cal_plt_df,
                mapping = aes_string(x = xa, y = ya, shape = shQuote("baseline")),
                alpha = 0.5
            ) +
            geom_point(
                data = ens_pwc_plt_df,
                mapping = aes_string(x = xa, y = ya, color = "coupling_method", shape = shQuote("párový ansámbel")),
                alpha = 0.5
            ) +
            facet_grid(rows = vars(combining_method), scales = "free") +
            ggtitle(sprintf(
                "Závislosť zlepšenia presnosti ansámblom oproti %s sietí a %s.",
                if (ya == "acc_imp_max") "maxima" else "priemeru",
                if (xa == "err_incons") "nekonzistencie chýb sietí" else if (xa == "mean_pwa_var") "priemerného rozptylu párových presností sietí"
            )) +
            xlab(x_lab) +
            ylab(y_lab) +
            theme_bw() +
            scale_shape_manual(name = "druh ansámblu", values = c(1, 17)) +
            scale_color_discrete(name = "párová zväzovacia metóda") +
            theme(strip.text.y = element_text(size = 8, angle = 0))

            plot_name <- paste0("evaluation_sk/C", cifar, "_relation_", ya, "~", xa, ".pdf")
            ggsave(plot = cur_plot, filename = plot_name, device = cairo_pdf, height = 40)
        }
    }

    avg_imp_table <- rbind(
        ens_pwc_plt_df %>%
            mutate(method = paste(combining_method, toupper(coupling_method), sep = " + ")) %>%
            group_by(method) %>%
            summarise(
                acc_imp_o_avg = mean(acc_imp_avg), acc_imp_o_best = mean(acc_imp_max),
                nll_imp_o_avg = mean(nll_imp_avg), nll_imp_o_best = mean(nll_imp_best),
                ece_imp_o_avg = mean(ece_imp_avg), ece_imp_o_best = mean(ece_imp_best)),
        ens_cal_plt_df %>%
            mutate(method = paste0("baseline - ", calibrating_method)) %>%
            group_by(method) %>%
            summarise(
                acc_imp_o_avg = mean(acc_imp_avg), acc_imp_o_best = mean(acc_imp_max),
                nll_imp_o_avg = mean(nll_imp_avg), nll_imp_o_best = mean(nll_imp_best),
                ece_imp_o_avg = mean(ece_imp_avg), ece_imp_o_best = mean(ece_imp_best))
        )
    
    avg_imp_table <- tibble::rownames_to_column(avg_imp_table, var = "rank")
    avg_imp_table$rank <- as.numeric(avg_imp_table$rank)

    table_name <- paste0("evaluation_sk/C", cifar, "_improvements_table.csv")
    write.csv(avg_imp_table, file = table_name, row.names = FALSE, na = "")
}


plot_plots(base_dir = base_dir_C10, cifar = 10)
plot_plots(base_dir = base_dir_C100, cifar = 100)
