library(ggplot2)
library(dplyr)
library(Cairo)

base_dir <- "/home/mordechaj/school/disertation/data/imagenet/topl/"

plot_inf_time <- function(
    dir, ens_sizes = NULL, co_m = "logreg_no_interc",
    file_name = "IMNET_topl_time_dependency.pdf", width = 5, height = 2)
{
    pwc_df <- read.csv(file.path(dir, "ens_pwc_metrics.csv"))
    pwc_df <- pwc_df %>% 
        filter(computational_precision == "float") %>% 
        mutate(
            prediction_time = prediction_time / 1000,
            topl = as.factor(topl),
            combining_method = recode(
                combining_method,
                logreg_torch = "logreg",
                logreg_torch_no_interc = "logreg_no_interc"))
    
    if (!is.null(ens_sizes))
    {
        pwc_df <- pwc_df %>% filter(combination_size %in% ens_sizes)
    }
    pwc_df <- pwc_df %>% filter(combining_method == co_m, topl %in% c(5, 10, 25, 100, 500, 1000))

    small_box_width <- 0.4
    small_box_size <- 0.9
    small_box_dodge <- 0.65

    plot <- ggplot() +
            geom_boxplot(
                data = pwc_df,
                mapping = aes(x = topl, y = prediction_time, color = coupling_method)) +
            scale_y_log10() +
            ylab("Čas predikcie [s]") +
            scale_color_discrete(name = "Párová zväzovacia metóda") +
            theme_classic() +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())

    ggsave(
        filename = file.path("imagenet_topl", file_name),
        device = cairo_pdf,
        width = width,
        height = height
    )
}

plot_inf_time(dir = base_dir)