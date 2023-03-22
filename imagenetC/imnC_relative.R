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

    add_best_net <- function(df, net_df)
    {
        get_best_net <- function(row) {
            present_nets <- strsplit(row["nets"], split = "+", fixed = T)
            present_df <- net_df %>% filter(network %in% present_nets$nets)
            best_mCE <- min(present_df$mCE)
            best_relCE <- min(present_df$relCE)
            return(c(best_mCE, best_relCE))
        }
        df[, c("best_net_mCE", "best_net_relCE")] <- t(apply(df, 1, get_best_net))
        return(df)
    }

    cal_missing <- networks[!(networks %in% names(cal_clean))]
    pwc_missing <- networks[!(networks %in% names(pwc_clean))]
    cal_clean[, cal_missing] <- F
    pwc_clean[, pwc_missing] <- F

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
        by = c("network")
    )

    # Add alexnet clean and corrupt error
    nets_corrupt <- left_join(
        nets_corrupt,
        alexn_bsl,
        by = c("corruption_type")
    )

    # Compute corruption errors
    nets_corrupt <- nets_corrupt %>%
        mutate(
            mCE = absolute_CE / alexnet_corruption_error,
            relCE = (absolute_CE - error_clean) / (alexnet_corruption_error - alexnet_error_clean)
        )

    write.csv(nets_corrupt, file.path(corrupt_dir, "nets_CE_relative_detailed.csv"), row.names = F)

    # Summarize over corruption types
    nets_corrupt_summ <- nets_corrupt %>%
        group_by(network) %>%
        summarize(mCE = mean(mCE), relCE = mean(relCE), .groups = "drop")

    write.csv(nets_corrupt_summ, file.path(corrupt_dir, "nets_CE_relative.csv"), row.names = F)

    # Aggregate over corruption levels
    cal_corrupt <- cal_corrupt %>% 
        group_by(nets, corruption_type, calibrating_method, comb_size) %>%
        summarize(absolute_CE = mean(corruption_error), .groups = "drop")

    # Add clean error
    cal_corrupt <- left_join(
        cal_corrupt,
        cal_clean %>% mutate(error_clean = 1.0 - accuracy1) %>% select(nets, error_clean),
        by = c("nets")
    )

    # Add alexnet clean and corrupt error
    cal_corrupt <- left_join(
        cal_corrupt,
        alexn_bsl,
        by = c("corruption_type")
    )

    # Compute corruption errors
    cal_corrupt <- cal_corrupt %>%
        mutate(
            mCE = absolute_CE / alexnet_corruption_error,
            relCE = (absolute_CE - error_clean) / (alexnet_corruption_error - alexnet_error_clean)
        )
    
    write.csv(cal_corrupt, file.path(corrupt_dir, "cal_CE_relative_detailed.csv"), row.names = F)

    cal_corrupt_summ <- cal_corrupt %>%
        group_by(nets, calibrating_method, comb_size) %>%
        summarize(
            mCE = mean(mCE), relCE = mean(relCE), .groups = "drop")

    cal_corrupt_summ <- add_best_net(cal_corrupt_summ, nets_corrupt_summ)

    write.csv(cal_corrupt_summ, file.path(corrupt_dir, "cal_CE_relative.csv"), row.names = F)

    # Aggregate over corruption levels
    pwc_corrupt <- pwc_corrupt %>%
        group_by(nets, combining_method, coupling_method, corruption_type, comb_size, topl) %>%
        summarize(absolute_CE = mean(corruption_error), .groups = "drop")

    # Add clean error
    pwc_corrupt <- left_join(
        pwc_corrupt,
        pwc_clean %>% mutate(error_clean = 1.0 - accuracy1) %>% select(nets, combining_method, coupling_method, topl, error_clean),
        by = c("nets", "combining_method", "coupling_method", "topl")
    )

    # Add alexnet clean and corrupt error
    pwc_corrupt <- left_join(
        pwc_corrupt,
        alexn_bsl,
        by = c("corruption_type")
    )

    # Compute corruption errors
    pwc_corrupt <- pwc_corrupt %>%
        mutate(
            mCE = absolute_CE / alexnet_corruption_error,
            relCE = (absolute_CE - error_clean) / (alexnet_corruption_error - alexnet_error_clean)
        )
    
    write.csv(pwc_corrupt, file.path(corrupt_dir, "pwc_CE_relative_detailed.csv"), row.names = F)

    # Aggregate over corruption types
    pwc_corrupt_summ <- pwc_corrupt %>%
        group_by(nets, combining_method, coupling_method, topl, comb_size) %>%
        summarize(
            mCE = mean(mCE), relCE = mean(relCE), .groups = "drop")

    pwc_corrupt_summ <- add_best_net(pwc_corrupt_summ, nets_corrupt_summ)

    write.csv(pwc_corrupt_summ, file.path(corrupt_dir, "pwc_CE_relative.csv"), row.names = F)
}

cl_dir <- "/home/mordechaj/school/disertation/data/imagenet/eval2"
cor_dir <- "/home/mordechaj/school/disertation/data/imagenet/imagenetC"

compute_relative_CEs(clean_dir = cl_dir, corrupt_dir = cor_dir)
