library(ggplot2)
library(dplyr)
library(tidyr)
library(Cairo)
library(relayer)
library(patchwork)
library(stringr)

plot_CEs <- function(cal_df_file, pwc_df_file, size = c(7, 7))
{
    cal_df <- read.csv(cal_df_file)
    pwc_df <- read.csv(pwc_df_file)

    cal_df <- cal_df %>% mutate(imp_mCE = best_net_mCE - mCE, imp_relCE = best_net_relCE - relCE)
    pwc_df <- pwc_df %>% mutate(imp_mCE = best_net_mCE - mCE, imp_relCE = best_net_relCE - relCE)

    pwc_df <- pwc_df %>% mutate(combining_method = factor(combining_method))

    max_topl <- max(pwc_df$topl)

    pwc_full <- pwc_df %>% filter(topl == max_topl) %>% droplevels()
    pwc_fast <- pwc_df %>% filter(topl < max_topl) %>% droplevels()

    small_box_width <- 0.4
    small_box_size <- 0.9

    ce_metrics <- list(
        list(metric = "imp_mCE", name = "zlepšenie mCE"),
        list(metric = "imp_relCE", name = "zlepšenie relCE")
    )

    plot_topl_strategy <- function(pwc_plot_df, plot_name)
    {
        print(levels(pwc_plot_df$combining_method))
        big_box_width <- length(levels(pwc_plot_df$combining_method))
        big_box_x <- 1 + (length(levels(pwc_plot_df$combining_method)) - 1) / 2

        ce_plots <- list()
        for (i in seq_along(ce_metrics))
        {
            nums <- c(1:length(unique(pwc_plot_df$combining_method)))
            if (i == length(ce_metrics))
            {
                names(nums) <- levels(pwc_plot_df$combining_method)
                max_len <- max(unlist(lapply(X = levels(pwc_plot_df$combining_method), FUN = nchar)))
                x_labs <- lapply(
                    X = nums,
                    FUN = function(i) paste0(str_pad(
                        string = levels(pwc_plot_df$combining_method)[i],
                        width = 1.8 * max_len - 0.8 * nchar(levels(pwc_plot_df$combining_method)[i]) + 4,
                        side = "both",
                        pad = " "), i))
            }
            else
            {
                x_labs <- nums
            }


            ce_plots[[i]] <- ggplot() +
                geom_hline(mapping = aes(yintercept = 0.0), color = "red") +
                geom_boxplot(
                    data = cal_df,
                    mapping = aes(y = !! sym(ce_metrics[[i]][["metric"]]), color = "TemperatureScaling",
                        x = big_box_x),
                    width = big_box_width
                    ) +
                (
                    geom_boxplot(
                    data = pwc_plot_df,
                    mapping = aes(
                        x = combining_method, y = !! sym(ce_metrics[[i]][["metric"]]),
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
                ylab(ce_metrics[[i]][["name"]]) +
                scale_color_manual(values = c("black"), name = "baseline") +
                scale_x_discrete(labels = x_labs) +
                theme_classic() +
                theme(
                    axis.text.x = element_text(angle = 90),
                    axis.title.x = element_blank()
                )
        }
        ce_plot <- ce_plots[[1]] / ce_plots[[2]]
        ce_plot <- ce_plot + plot_layout(guides = "collect")

        ggsave(plot = ce_plot, filename = file.path("imagenetC", plot_name), device = cairo_pdf, width = size[1], height = size[2])
    }

    plot_topl_strategy(pwc_full, "corruption_errors_full.pdf")
    plot_topl_strategy(pwc_fast, "corruption_errors_fast.pdf")
}


cal_file <- "/home/mordechaj/school/disertation/data/imagenet/imagenetC/cal_CE_relative.csv"
pwc_file <- "/home/mordechaj/school/disertation/data/imagenet/imagenetC/pwc_CE_relative.csv"

plot_CEs(cal_df_file = cal_file, pwc_df_file = pwc_file)