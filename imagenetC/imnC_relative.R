library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(Cairo)

compute_relative_CEs <- function(clean_dir, corrupt_dir)
{
    nets_clean <- read.csv(file.path(clean_dir, "net_metrics.csv"))
    pwc_clean <- read.csv(file.path(clean_dir, "ens_pwc_metrics.csv"))
    cal_clean <- read.csv(file.path(clean_dir, "ens_cal_metrics.csv"))

    networks <- sort(unique(nets_clean$network))

    create_nets_col <- function(df)
    {
        get_true_cols <- function(row) {
            true_cols <- names(row)[as.logical(row)]
            paste(true_cols, collapse = "+")
        }

        df$nets <- apply(df[networks], 1, get_true_cols)
        return(df)
    }

    cal_clean <- create_nets_col(cal_clean)
    pwc_clean <- create_nets_col(pwc_clean)

    nets_corrupt <- read.csv(file.path(corrupt_dir, "nets_CE_detailed.csv"))
    pwc_corrupt <- read.csv(file.path(corrupt_dir, "pwc_CE_detailed.csv"))
    cal_corrupt <- read.csv(file.path(corrupt_dir, "cal_CE_detailed.csv"))
    alexn_bsl <- read.csv(file.path(corrupt_dir, "alexnet_baseline.csv"))

    # Aggregate over corruption levels
    nets_corrupt <- nets_corrupt %>% 
        group_by(network, corruption_type) %>% 
        summarize(absolute_CE = mean(corruption_error), .groups = "drop")

    # Add clean error
    nets_corrupt <- left_join(
        nets_corrupt,
        nets_clean %>% mutate(error_clean = 1.0 - accuracy1) %>% select(network, error_clean),
        by = c(network)
    )

    # Add alexnet clean and corrupt error
    nets_corrupt <- left_join(
        nets_corrupt,
        alexn_bsl,
        by = c(corruption_type)
    )

    # Compute corruption errors
    nets_corrupt <- nets_corrupt %>%
        mutate(
            CE = corruption_error / alexnet_corruption_error,
            rel_CE = (corruption_error - error_clean) / (alexnet_corruption_error - alexnet_error_clean)
        )
    
    write.csv(nets_corrupt, file.path(corrupt_dir, "nets_CE_relative.csv"), row.names = F)

    
}

cl_dir <- "/home/mordechaj/school/disertation/data/imagenet/eval"
cor_dir <- "/home/mordechaj/school/disertation/data/imagenet/imagenetC"

compute_relative_CEs(clean_dir = cl_dir, corrupt_dir = cor_dir)
