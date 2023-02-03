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

metrics <- c("accuracy", "nll", "ece")
metric_names <- c(accuracy = "presnosť", nll = "NLL", ece = "ECE",
    accuracy1 = "presnosť top-1", accuracy5 = "presnosť top-5")

load_ens_dfs <- function(base_dir, comb_methods = NULL)
{
    net_df <- read.csv(file.path(base_dir, "net_metrics.csv"))
    ens_df_cal <- read.csv(file.path(base_dir, "ens_cal_metrics.csv"))
    ens_df_pwc <- read.csv(file.path(base_dir, "ens_pwc_metrics.csv"))

    networks <- net_df$network

    list[ens_df_cal, ens_df_pwc] <- add_combination_metrics(net_df = net_df, ens_df_cal = ens_df_cal, ens_df_pwc = ens_df_pwc)

    ens_pwc_plt_df <- ens_df_pwc # %>% filter(combining_method != "lda")
    ens_cal_plt_df <- ens_df_cal

    if (is.null(comb_methods))
    {
        comb_methods <- c(
        "average", "prob_average",
        "cal_average", "cal_prob_average",
        "logreg", "logreg_sweep_C",
        "logreg_no_interc", "logreg_no_interc_sweep_C",
        "grad_m1", "grad_m2", "grad_bc", "logreg_torch",
        "random"
        )
        comb_methods <- c(sapply(X = comb_methods, FUN = {
        function(cm) c(cm, paste(cm, "uncert", sep = "."))
        }))
    }

    ens_pwc_plt_df <- ens_pwc_plt_df %>% filter(combining_method %in% comb_methods)

    ens_pwc_plt_df$combining_method <- factor(ens_pwc_plt_df$combining_method,
    levels = comb_methods
    )
    ens_pwc_plt_df <- droplevels(ens_pwc_plt_df)

    return(list(ens_cal_plt_df, ens_pwc_plt_df))
}

plot_nets <- function(base_dir, dtset, output_dir = "evaluation_sk")
{
    print("Plotting nets")
    net_df <- read.csv(file.path(base_dir, "net_metrics.csv"))

    metric_cols <- c("nll", "ece")
    if ("accuracy" %in% colnames(net_df))
    {
        metric_cols <- c("accuracy", metric_cols)
    }
    else 
    {
       metric_cols <- c("accuracy1", "accuracy5", metric_cols)
    }

    if (dtset == "IMNET")
    {
        net_df <- net_df %>% mutate(network = str_remove(network, "_IM2012"))
    }

    net_long <- pivot_longer(
        net_df,
        cols = all_of(metric_cols),
        names_to = "metric", values_to = "value"
    )

    nets_plot <- ggplot(data = net_long) +
        geom_col(mapping = aes(x = network, y = value)) +
        facet_grid(metric ~ ., labeller = labeller(metric = metric_names), scales = "free") +
        xlab("sieť") +
        ylab("hodnota") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90)) +
        ggtitle("Metriky sietí")

    plot_name <- file.path(output_dir, paste0(dtset, "_net_metrics.pdf"))
    ggsave(plot = nets_plot, filename = plot_name, device = cairo_pdf)
}

plot_ensembles <- function(base_dir, dtset, co_m_subset = NULL, constit_num = NULL, cust_name = NULL, size = c(7, 7),
output_dir = "evaluation_sk", topl_strat = NULL)
{
    print(paste0("Plotting ensembles", ifelse(is.null(topl_strat), "", paste0(" topl_strat: ", topl_strat))))
    net_df <- read.csv(file.path(base_dir, "net_metrics.csv"))
    list[ens_cal_plt_df, ens_pwc_plt_df] <- load_ens_dfs(base_dir = base_dir)

    if (dtset == "IMNET")
    {
        orig_nets <- net_df$network
        for (net in orig_nets)
        {
            names(ens_cal_plt_df)[names(ens_cal_plt_df) == net] <- str_remove(net, "_IM2012")
            names(ens_pwc_plt_df)[names(ens_pwc_plt_df) == net] <- str_remove(net, "_IM2012")
        }
        net_df <- net_df %>% mutate(network = str_remove(network, "_IM2012"))
    }

    ens_pwc_plt_df <- ens_pwc_plt_df %>% mutate(
                    combining_method = recode(combining_method, logreg_torch = "logreg"))

    networks <- net_df$network

    if (!is.null(topl_strat))
    {
        if (topl_strat == "full")
        {
            ens_pwc_plt_df <- ens_pwc_plt_df %>% filter(topl == max(topl))
        }
        else if(topl_strat == "fast")
        {
            ens_pwc_plt_df <- ens_pwc_plt_df %>% filter(topl < max(topl))
        }
    }

    if (!is.null(co_m_subset))
    {
        ens_pwc_plt_df <- ens_pwc_plt_df %>% filter(combining_method %in% co_m_subset)
    }

    hp_line_width <- 0.11
    hp_line_size <- 0.8
    hp_line_dodge <- 0.65

    if (is.null(constit_num))
    {
        iter_cons_num <- unique(ens_cal_plt_df$combination_size)
    }
    else 
    {
        iter_cons_num <- constit_num
    }

    processing_top5_acc <- "accuracy5" %in% colnames(net_df)
    if (!processing_top5_acc)
    {
        acc_plots_params <- list(list(metric = "accuracy", name = "presnosť"))
    }
    else 
    {
        acc_plots_params <- list(
            list(metric = "accuracy1", name = "presnosť top-1"),
            list(metric = "accuracy5", name = "presnosť top-5")
        )
    }

    for (sss in iter_cons_num)
    {
        for (ssi in unique(ens_cal_plt_df %>%
            filter(combination_size == sss) %>%
            pull(combination_id)))
        {
            cur_ens_cal <- ens_cal_plt_df %>% filter(combination_size == sss &
            combination_id == ssi)
            cur_ens_pwc <- ens_pwc_plt_df %>% filter(combination_size == sss &
            combination_id == ssi)
            cur_nets_mask <- as.logical(t(cur_ens_cal[1, str_replace_all(networks, "-", ".")]))
            cur_nets_vec <- networks[cur_nets_mask]
            print(cur_nets_vec)
            cur_nets <- net_df %>% filter(network %in% cur_nets_vec)

            acc_plots <- list()
            for (i in seq_along(acc_plots_params))
            {
                acc_plots[[i]] <- ggplot() +
                (
                    geom_hline(
                    data = cur_nets,
                    mapping = aes(yintercept = !! sym(acc_plots_params[[i]][["metric"]]), colour1 = network),
                    linetype = "dashed"
                    ) %>%
                    rename_geom_aes(new_aes = c("colour" = "colour1"))
                ) +
                geom_hline(
                    data = cur_ens_cal,
                    mapping = aes(
                        yintercept = !! sym(acc_plots_params[[i]][["metric"]]),
                        color = "TemperatureScaling")
                ) +
                (
                    geom_hopline(
                    data = cur_ens_pwc,
                    mapping = aes(
                        x = combining_method, y = !! sym(acc_plots_params[[i]][["metric"]]),
                        colour2 = coupling_method
                    ),
                    linewidth = hp_line_size, width = hp_line_width,
                    position = position_dodge(width = hp_line_dodge)
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
                ylab(acc_plots_params[[i]][["name"]]) +
                scale_color_manual(values = c("black"), name = "baseline") +
                scale_x_discrete(labels = 1:length(unique(cur_ens_pwc$combining_method))) +
                theme_classic() +
                theme(
                    axis.text.x = element_text(angle = 90),
                    axis.title.x = element_blank()
                )
            }

            if (processing_top5_acc)
            {
                acc_plot <- acc_plots[[1]] / acc_plots[[2]]
            }
            else
            {
                acc_plot <- acc_plots[[1]]
            }

            if (dtset == "C100")
            {
                acc_plot <- acc_plot + coord_cartesian(ylim = c(0.75, 0.87))
            }

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
                geom_hopline(
                data = cur_ens_pwc,
                mapping = aes(
                    x = combining_method, y = nll,
                    colour2 = coupling_method
                ),
                linewidth = hp_line_size, width = hp_line_width,
                position = position_dodge(width = hp_line_dodge)
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
            scale_x_discrete(labels = 1:length(unique(cur_ens_pwc$combining_method))) +
            theme_classic() +
            theme(
                axis.text.x = element_text(angle = 90),
                axis.title.x = element_blank()
            )

            present_co_m <- levels(droplevels(cur_ens_pwc$combining_method))
            nums <- c(1:length(present_co_m))
            names(nums) <- present_co_m
            max_len <- max(unlist(lapply(X = present_co_m, FUN = nchar)))
            x_labs <- lapply(
                X = nums,
                FUN = function(i) paste0(str_pad(
                    string = present_co_m[i],
                    width = 1.8 * max_len - 0.8 * nchar(present_co_m[i]) + 4,
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
                geom_hopline(
                data = cur_ens_pwc,
                mapping = aes(
                    x = combining_method, y = ece,
                    colour2 = coupling_method
                ),
                linewidth = hp_line_size, width = hp_line_width,
                position = position_dodge(width = hp_line_dodge)
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

            res_plot <- acc_plot / nll_plot / ece_plot + plot_layout(guides = "collect")

            if (is.null(cust_name))
            {
                res_plot <- res_plot + plot_annotation(title = paste(
                    "Metriky ansámblov",
                    paste0("Topl ", ifelse(is.null(topl_strat), "full", topl_strat)),
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
            }

            if (is.null(cust_name))
            {
                plot_name <- file.path(output_dir, paste0(
                    dtset, "_ensemble_metrics_",
                    paste(cur_nets_vec, collapse = "+"),
                    "_topl_", ifelse(is.null(topl_strat), "full", topl_strat), ".pdf"))
            }
            else
            {
                plot_name <- file.path(output_dir, cust_name)
            }
            ggsave(plot = res_plot, filename = plot_name, device = cairo_pdf, width = size[1], height = size[2])
        }
    }
}

plot_dependencies <- function(base_dir, dtset, output_dir = "evaluation_sk")
{
    net_df <- read.csv(file.path(base_dir, "net_metrics.csv"))

    list[ens_cal_plt_df, ens_pwc_plt_df] <- load_ens_dfs(base_dir = base_dir)

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

            plot_name <- file.path(output_dir, paste0(dtset, "_relation_", ya, "~", xa, ".pdf"))
            ggsave(plot = cur_plot, filename = plot_name, device = cairo_pdf, height = 40)
        }
    }
}

comp_tables <- function(base_dir, dtset, output_dir = "evaluation_sk", topl_strat = NULL)
{
    print(paste0("Printing tables", ifelse(is.null(topl_strat), "", paste0(" topl_strat: ", topl_strat))))
    list[ens_cal_plt_df, ens_pwc_plt_df] <- load_ens_dfs(base_dir = base_dir)
    net_df <- read.csv(file.path(base_dir, "net_metrics.csv"))
    if (dtset == "IMNET")
    {
        orig_nets <- net_df$network
        for (net in orig_nets)
        {
            names(ens_cal_plt_df)[names(ens_cal_plt_df) == net] <- str_remove(net, "_IM2012")
            names(ens_pwc_plt_df)[names(ens_pwc_plt_df) == net] <- str_remove(net, "_IM2012")
        }
        net_df <- net_df %>% mutate(network = str_remove(network, "_IM2012"))
    }

    ens_pwc_plt_df <- ens_pwc_plt_df %>% mutate(
                    combining_method = recode(combining_method, logreg_torch = "logreg"))

    processing_acc5 <- "accuracy5" %in% colnames(ens_pwc_plt_df)

    if (!is.null(topl_strat))
    {
        if (topl_strat == "full")
        {
            ens_pwc_plt_df <- ens_pwc_plt_df %>% filter(topl == max(topl))
        }
        else if (topl_strat == "fast")
        {
            ens_pwc_plt_df <- ens_pwc_plt_df %>% filter(topl < max(topl))
        }
    }

    if (!processing_acc5)
    {
        avg_imp_table <- rbind(
            ens_pwc_plt_df %>%
                mutate(method = paste(combining_method, coupling_method, sep = " + ")) %>%
                group_by(method) %>%
                summarise(
                    acc_imp_o_avg = mean(acc_imp_avg), acc_imp_o_best = mean(acc_imp_best),
                    nll_imp_o_avg = mean(nll_imp_avg), nll_imp_o_best = mean(nll_imp_best),
                    ece_imp_o_avg = mean(ece_imp_avg), ece_imp_o_best = mean(ece_imp_best)),
            ens_cal_plt_df %>%
                mutate(method = paste0("baseline - ", calibrating_method)) %>%
                group_by(method) %>%
                summarise(
                    acc_imp_o_avg = mean(acc_imp_avg), acc_imp_o_best = mean(acc_imp_best),
                    nll_imp_o_avg = mean(nll_imp_avg), nll_imp_o_best = mean(nll_imp_best),
                    ece_imp_o_avg = mean(ece_imp_avg), ece_imp_o_best = mean(ece_imp_best))
            )
    }
    else 
    {
         avg_imp_table <- rbind(
            ens_pwc_plt_df %>%
                mutate(method = paste(combining_method, coupling_method, sep = " + ")) %>%
                group_by(method) %>%
                summarise(
                    acc1_imp_o_avg = mean(acc1_imp_avg), acc1_imp_o_best = mean(acc1_imp_best),
                    acc5_imp_o_avg = mean(acc5_imp_avg), acc5_imp_o_best = mean(acc5_imp_best),
                    nll_imp_o_avg = mean(nll_imp_avg), nll_imp_o_best = mean(nll_imp_best),
                    ece_imp_o_avg = mean(ece_imp_avg), ece_imp_o_best = mean(ece_imp_best)),
            ens_cal_plt_df %>%
                mutate(method = paste0("baseline - ", calibrating_method)) %>%
                group_by(method) %>%
                summarise(
                    acc1_imp_o_avg = mean(acc1_imp_avg), acc1_imp_o_best = mean(acc1_imp_best),
                    acc5_imp_o_avg = mean(acc5_imp_avg), acc5_imp_o_best = mean(acc5_imp_best),
                    nll_imp_o_avg = mean(nll_imp_avg), nll_imp_o_best = mean(nll_imp_best),
                    ece_imp_o_avg = mean(ece_imp_avg), ece_imp_o_best = mean(ece_imp_best))
            )
    }

    avg_imp_table <- tibble::rownames_to_column(avg_imp_table, var = "rank")
    avg_imp_table$rank <- as.numeric(avg_imp_table$rank)

    table_name <- file.path(
        output_dir,
        paste0(dtset, "_improvements_table_topl_", ifelse(is.null(topl_strat), "full", topl_strat), ".csv"))
    write.csv(avg_imp_table, file = table_name, row.names = FALSE, na = "")
}

plot_improvements <- function(
    base_dir, dtset, over = "best", comb_methods = NULL,
    size = c(7, 7), acc_lim = NULL, nll_lim = NULL, ece_lim = NULL,
    output_dir = "evaluation_sk", topl_strat = NULL)
{
    print(paste0("Plotting improvements", ifelse(is.null(topl_strat), "", paste0(" topl_strat: ", topl_strat))))
    list[ens_cal_plt_df, ens_pwc_plt_df] <- load_ens_dfs(base_dir = base_dir, comb_methods = comb_methods)
    if (!is.null(topl_strat))
    {
        if (topl_strat == "full")
        {
            ens_pwc_plt_df <- ens_pwc_plt_df %>% filter(topl == max(topl))
        }
        else if (topl_strat == "fast")
        {
            ens_pwc_plt_df <- ens_pwc_plt_df %>% filter(topl < max(topl))
        }
        ens_pwc_plt_df <- droplevels(ens_pwc_plt_df)
    }

    ens_pwc_plt_df <- ens_pwc_plt_df %>% mutate(
                    combining_method = recode(combining_method, logreg_torch = "logreg"))

    net_df <- read.csv(file.path(base_dir, "net_metrics.csv"))
    if (dtset == "IMNET")
    {
        orig_nets <- net_df$network
        for (net in orig_nets)
        {
            names(ens_cal_plt_df)[names(ens_cal_plt_df) == net] <- str_remove(net, "_IM2012")
            names(ens_pwc_plt_df)[names(ens_pwc_plt_df) == net] <- str_remove(net, "_IM2012")
        }
        net_df <- net_df %>% mutate(network = str_remove(network, "_IM2012"))
    }

    small_box_width <- 0.4
    small_box_size <- 0.9
    big_box_width <- length(levels(ens_pwc_plt_df$combining_method))
    big_box_x <- 1 + (length(levels(ens_pwc_plt_df$combining_method)) - 1) / 2

    processing_top5_acc <- "accuracy5" %in% colnames(ens_pwc_plt_df)

    if (!processing_top5_acc)
    {
        acc_plot_params <- list(list(metric = "acc_imp_", name = "presnosť"))
    }
    else
    {
        acc_plot_params <- list(
            list(metric = "acc1_imp_", name = "presnosť top-1"),
            list(metric = "acc5_imp_", name = "presnosť top-5")
        )
    }
    acc_plots <- list()
    for (i in seq_along(acc_plot_params))
    {
        acc_plots[[i]] <- ggplot() +
            geom_hline(mapping = aes(yintercept = 0.0), color = "red") +
            geom_boxplot(
                data = ens_cal_plt_df,
                mapping = aes(y = !! sym(paste0(acc_plot_params[[i]][["metric"]], over)), color = "TemperatureScaling",
                    x = big_box_x),
                width = big_box_width
                ) +
            (
                geom_boxplot(
                data = ens_pwc_plt_df,
                mapping = aes(
                    x = combining_method, y = !! sym(paste0(acc_plot_params[[i]][["metric"]], over)),
                    colour2 = coupling_method
                ),
                size = small_box_size, width = small_box_width,
                position = position_dodge(width = 0.65)
                ) %>%
                rename_geom_aes(new_aes = c("colour" = "colour2"))
            ) +
            scale_colour_brewer(
                aesthetics = "colour2", palette = 2,
                name = "párová zväzovacia metóda", type = "qual"
            ) +
            ylab(acc_plot_params[[i]][["name"]]) +
            scale_color_manual(values = c("black"), name = "baseline") +
            scale_x_discrete(labels = 1:length(levels(ens_pwc_plt_df$combining_method))) +
            theme_classic() +
            theme(
                axis.text.x = element_text(angle = 90),
                axis.title.x = element_blank()
            )
    }

    if (processing_top5_acc)
    {
        acc_plot <- acc_plots[[1]] / acc_plots[[2]]
    }
    else 
    {
        acc_plot <- acc_plots[[1]]
    }

    if (!is.null(acc_lim))
    {
        acc_plot <- acc_plot + coord_cartesian(ylim = acc_lim)
    }

    nll_plot <- ggplot() +
    geom_hline(mapping = aes(yintercept = 0.0), color = "red") +
    geom_boxplot(
        data = ens_cal_plt_df,
        mapping = aes(y = !! sym(paste0("nll_imp_", over)), color = "TemperatureScaling",
            x = big_box_x),
        width = big_box_width
    ) +
    (
        geom_boxplot(
        data = ens_pwc_plt_df,
        mapping = aes(
            x = combining_method, y = !! sym(paste0("nll_imp_", over)),
            colour2 = coupling_method
        ),
        size = small_box_size, width = small_box_width,
        position = position_dodge(width = 0.65)
        ) %>%
        rename_geom_aes(new_aes = c("colour" = "colour2"))
    ) +
    scale_colour_brewer(
        aesthetics = "colour2", palette = 2,
        name = "párová zväzovacia metóda", type = "qual"
    ) +
    ylab("NLL") +
    scale_color_manual(values = c("black"), name = "baseline") +
    scale_x_discrete(labels = 1:length(unique(ens_pwc_plt_df$combining_method))) +
    theme_classic() +
    theme(
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank()
    )

    if (!is.null(nll_lim))
    {
        nll_plot <- nll_plot + coord_cartesian(ylim = nll_lim)
    }

    nums <- c(1:length(unique(ens_pwc_plt_df$combining_method)))
    names(nums) <- levels(ens_pwc_plt_df$combining_method)
    max_len <- max(unlist(lapply(X = levels(ens_pwc_plt_df$combining_method), FUN = nchar)))
    x_labs <- lapply(
        X = nums,
        FUN = function(i) paste0(str_pad(
            string = levels(ens_pwc_plt_df$combining_method)[i],
            width = 1.8 * max_len - 0.8 * nchar(levels(ens_pwc_plt_df$combining_method)[i]) + 4,
            side = "both",
            pad = " "), i))

    ece_plot <- ggplot() +
    geom_hline(mapping = aes(yintercept = 0.0), color = "red") +
    geom_boxplot(
        data = ens_cal_plt_df,
        mapping = aes(y = !! sym(paste0("ece_imp_", over)), color = "TemperatureScaling",
            x = big_box_x),
        width = big_box_width
    ) +
    (
        geom_boxplot(
        data = ens_pwc_plt_df,
        mapping = aes(
            x = combining_method, y = !! sym(paste0("ece_imp_", over)),
            colour2 = coupling_method
        ),
        size = small_box_size, width = small_box_width,
        position = position_dodge(width = 0.65)
        ) %>%
        rename_geom_aes(new_aes = c("colour" = "colour2"))
    ) +
    scale_colour_brewer(
        aesthetics = "colour2", palette = 2,
        name = "párová zväzovacia metóda", type = "qual"
    ) +
    ylab("ECE") +
    xlab("kombinačná metóda") +
    scale_color_manual(values = c("black"), name = "baseline") +
    scale_x_discrete(labels = x_labs) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90))

    if (!is.null(ece_lim))
    {
        ece_plot <- ece_plot + coord_cartesian(ylim = ece_lim)
    }

    res_plot <- acc_plot / nll_plot / ece_plot + plot_layout(guides = "collect") #+
        #plot_annotation(title = paste0(
        #    "Zlepšenia ansámblov oproti ", ifelse(over == "best", "najlepšej", "priemeru"), " zo sietí")
        #)

    plot_name <- file.path(
        output_dir,
        paste0(
            dtset, "_ensemble_improvements_over_", over,
            "_topl_", ifelse(is.null(topl_strat), "full", topl_strat), ".pdf"))
    ggsave(plot = res_plot, filename = plot_name, device = cairo_pdf, width = size[1], height = size[2])
}

plot_improvements_topls <- function(base_dir, dtset, over = "best", comb_methods = NULL, output_dir = "imagenet_topl")
{
    list[ens_cal_df, ens_pwc_df] <- load_ens_dfs(base_dir = base_dir, comb_methods = comb_methods)
    ens_pwc_df$topl <- as.factor(ens_pwc_df$topl)
    small_box_width <- 0.4
    small_box_size <- 0.9
    big_box_width <- length(levels(ens_pwc_df$topl))
    big_box_x <- 1 + (length(levels(ens_pwc_df$topl)) - 1) / 2

    for (co_m in unique(ens_pwc_df$combining_method))
    {
        ens_cal_plt_df <- ens_cal_df
        ens_pwc_plt_df <- ens_pwc_df %>% filter(combining_method == co_m)
        acc_plot <- ggplot() +
        geom_hline(mapping = aes(yintercept = 0.0), color = "red") +
        geom_boxplot(
            data = ens_cal_plt_df,
            mapping = aes(y = !! sym(paste0("acc1_imp_", over)), color = "TemperatureScaling",
                x = big_box_x),
            width = big_box_width
            ) +
        (
            geom_boxplot(
            data = ens_pwc_plt_df,
            mapping = aes(
                x = topl, y = !! sym(paste0("acc1_imp_", over)),
                colour2 = coupling_method
            ),
            size = small_box_size, width = small_box_width,
            position = position_dodge(width = 0.65)
            ) %>%
            rename_geom_aes(new_aes = c("colour" = "colour2"))
        ) +
        scale_x_discrete() +
        scale_colour_brewer(
            aesthetics = "colour2", palette = 2,
            name = "párová zväzovacia metóda", type = "qual"
        ) +
        ylab("presnosť") +
        scale_color_manual(values = c("black"), name = "baseline") +
        theme_classic() +
        theme(
            axis.text.x = element_text(angle = 90),
            axis.title.x = element_blank()
        )

        nll_plot <- ggplot() +
        geom_hline(mapping = aes(yintercept = 0.0), color = "red") +
        geom_boxplot(
            data = ens_cal_plt_df,
            mapping = aes(y = !! sym(paste0("nll_imp_", over)), color = "TemperatureScaling",
                x = big_box_x),
            width = big_box_width
        ) +
        (
            geom_boxplot(
            data = ens_pwc_plt_df,
            mapping = aes(
                x = topl, y = !! sym(paste0("nll_imp_", over)),
                colour2 = coupling_method
            ),
            size = small_box_size, width = small_box_width,
            position = position_dodge(width = 0.65)
            ) %>%
            rename_geom_aes(new_aes = c("colour" = "colour2"))
        ) +
        scale_x_discrete() +
        scale_colour_brewer(
            aesthetics = "colour2", palette = 2,
            name = "párová zväzovacia metóda", type = "qual"
        ) +
        ylab("NLL") +
        scale_color_manual(values = c("black"), name = "baseline") +
        theme_classic() +
        theme(
            axis.text.x = element_text(angle = 90),
            axis.title.x = element_blank()
        )

        ece_plot <- ggplot() +
        geom_hline(mapping = aes(yintercept = 0.0), color = "red") +
        geom_boxplot(
            data = ens_cal_plt_df,
            mapping = aes(y = !! sym(paste0("ece_imp_", over)), color = "TemperatureScaling",
                x = big_box_x),
            width = big_box_width
        ) +
        (
            geom_boxplot(
            data = ens_pwc_plt_df,
            mapping = aes(
                x = topl, y = !! sym(paste0("ece_imp_", over)),
                colour2 = coupling_method
            ),
            size = small_box_size, width = small_box_width,
            position = position_dodge(width = 0.65)
            ) %>%
            rename_geom_aes(new_aes = c("colour" = "colour2"))
        ) +
        scale_colour_brewer(
            aesthetics = "colour2", palette = 2,
            name = "párová zväzovacia metóda", type = "qual"
        ) +
        ylab("ECE") +
        xlab("topl") +
        scale_color_manual(values = c("black"), name = "baseline") +
        scale_x_discrete() +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90))

        res_plot <- acc_plot / nll_plot / ece_plot + plot_layout(guides = "collect") # +
            #plot_annotation(title = paste0(
            #    "Zlepšenia ansámblov oproti ", ifelse(over == "best", "najlepšej", "priemeru"), " zo sietí")
            #)

        plot_name <- file.path(output_dir, paste0(dtset, "_ensemble_improvements_over_", over, "_co_m_", co_m, ".pdf"))
        ggsave(plot = res_plot, filename = plot_name, device = cairo_pdf, width = 9, height = 5)

    }
}

plot_plots <- function(base_dir, cifar)
{
    subs_comb_m <- c(
        "cal_average",
        "cal_average.uncert",
        "logreg",
        "logreg.uncert",
        "logreg_no_interc_sweep_C",
        "logreg_no_interc_sweep_C.uncert",
        "grad_m2",
        "grad_m2.uncert"
    )

    subs_c10 <- c(
        "cal_average",
        "cal_average.uncert",
        "logreg_no_interc",
        "logreg_no_interc.uncert",
        "logreg",
        "grad_m1",
        "grad_m2",
        "grad_bc"
    )

    subs_c100 <- c(
        "cal_average",
        "logreg",
        "logreg_no_interc",
        "logreg_sweep_C",
        "logreg_no_interc_sweep_C",
        "grad_m1",
        "grad_m2"
    )
    subs <- list("10" = subs_c10, "100" = subs_c100)

    plot_nets(base_dir = base_dir, dtset = paste0("C", cifar))
    plot_ensembles(base_dir = base_dir, dtset = paste0("C", cifar))
    plot_dependencies(base_dir = base_dir, dtset = paste0("C", cifar))
    comp_tables(base_dir = base_dir, dtset = paste0("C", cifar))
    plot_improvements(
        base_dir = base_dir, dtset = paste0("C", cifar),
        comb_methods = subs[[as.character(cifar)]])
}


#plot_plots(base_dir = base_dir_C10, cifar = 10)
#plot_plots(base_dir = base_dir_C100, cifar = 100)

plot_imnet_eval <- function()
{
    source_dir <- "/home/mordechaj/school/disertation/data/imagenet/eval"
    source_dir_topl <- "/home/mordechaj/school/disertation/data/imagenet/topl"
    dest_dir <- "imagenet_evaluation"
    plot_improvements_topls(base_dir = source_dir_topl, dtset = "IMNET")
    plot_nets(base_dir = source_dir, dtset = "IMNET", output_dir = dest_dir)
    plot_ensembles(base_dir = source_dir, dtset = "IMNET", output_dir = dest_dir, topl_strat = "full")
    plot_ensembles(base_dir = source_dir, dtset = "IMNET", output_dir = dest_dir, topl_strat = "fast")
    #plot_dependencies(base_dir = source_dir, dtset = "IMNET", output_dir = dest_dir)
    comp_tables(base_dir = source_dir, dtset = "IMNET", output_dir = dest_dir, topl_strat = "full")
    comp_tables(base_dir = source_dir, dtset = "IMNET", output_dir = dest_dir, topl_strat = "fast")
    plot_improvements(base_dir = source_dir, dtset = "IMNET", output_dir = dest_dir, topl_strat = "full", ece_lim = c(-0.2, 0.05))
    plot_improvements(base_dir = source_dir, dtset = "IMNET", output_dir = dest_dir, topl_strat = "fast")
}

plot_imnet_eval()