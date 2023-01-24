library(ggplot2)
library(plotly)
library(reticulate)
library(dplyr)
library(tibble)
library(htmlwidgets)

np <- import("numpy")

output_type <- "test"
net_folder <- "/home/mordechaj/school/disertation/data/imagenet/network_outputs"
all_classes <- "imnet_classes.txt"
subset_classes <- "imagenet-50.csv"
networks <- c("B16", "M_B16", "S16")

get_triplets_stats <- function(networks, net_folder, all_classes, subset_classes)
{
    subs_cl <- read.csv(subset_classes)
    label <- np$load(file.path(net_folder, networks[1], paste0(output_type, "_labels.npy")))
    label <- data.frame(label = label + 1)
    all_cl <- read.table(all_classes)
    names(all_cl)[names(all_cl) == "V1"] <- "class_name"
    all_cl <- rownames_to_column(all_cl) %>% mutate(rowname = as.integer(rowname))
    lab_names <- left_join(
        label,
        all_cl,
        by = c("label" = "rowname"))
    
    im50_lab_names <- lab_names[lab_names$class_name %in% subs_cl$class, ]

    get_predictions <- function(net)
    {
        out <- np$load(file.path(net_folder, net, paste0(output_type, "_outputs.npy")))
        out <- data.frame(out)
        colnames(out) <- all_cl$class_name
        out <- out %>% select(subs_cl$class)
        out <- out[lab_names$class_name %in% subs_cl$class, ]
        preds <- colnames(out)[apply(out, 1, which.max)]
        return(preds)
    }

    outputs <- lapply(networks, get_predictions)
    outputs <- data.frame(do.call(cbind, outputs))
    colnames(outputs) <- networks

    triplets <- combn(subs_cl$class, 3, simplify = FALSE)
    class_cols <- c("Class1", "Class2", "Class3")

    triplets_data <- function(triplet)
    {
        rel_preds <- outputs[im50_lab_names$class_name %in% triplet, ]
        rel_preds <- rel_preds %>% rowwise() %>% mutate(
            all_diff = length(unique(c(!!!syms(networks)))) == 3,
            two_diff = length(unique(c(!!!syms(networks)))) == 2,
            all_same = length(unique(c(!!!syms(networks)))) == 1,
            "o_{networks[1]}" := length(unique(c(!!!syms(networks[c(2, 3)])))) == 1 && !!sym(networks[1]) != !!sym(networks[2]),
            "o_{networks[2]}" := length(unique(c(!!!syms(networks[c(1, 3)])))) == 1 && !!sym(networks[1]) != !!sym(networks[2]),
            "o_{networks[3]}" := length(unique(c(!!!syms(networks[c(1, 2)])))) == 1 && !!sym(networks[1]) != !!sym(networks[3])
        ) %>% ungroup()

        stats <- rel_preds %>% summarize(
            all_diff = mean(all_diff),
            two_diff =  mean(two_diff),
            all_same = mean(all_same),
            "o_{networks[1]}" := mean(!!sym(paste0("o_", networks[1]))),
            "o_{networks[2]}" := mean(!!sym(paste0("o_", networks[2]))),
            "o_{networks[3]}" := mean(!!sym(paste0("o_", networks[3]))),
        )
        stats[class_cols] <- as.list(triplet)
        return(stats)
    }

    triplets_stats <- lapply(triplets, triplets_data)
    triplets_stats <- data.frame(do.call(rbind, triplets_stats))
    return(triplets_stats)
}

plot_triplets_stats <- function(triplets_stats, networks)
{
    triplets_stats <- triplets_stats %>% mutate(
        info = paste0(
            "classes: ", paste(Class1, Class2, Class3), "\n",
            "overvote ratios: ", paste(!!!syms(paste0("o_", networks)), sep = "; ")))

    max_overvote <- max(triplets_stats[, paste0("o_", networks)])
    triplets_stats[, paste0("o_", networks)] <- triplets_stats[, paste0("o_", networks)] / max_overvote

    jitter <- position_jitter(width = 0.002, height = 0.01)
    plot <- triplets_stats %>%
        ggplot(mapping = aes(
            x = all_diff, y = two_diff, color = rgb(!!!syms(paste0("o_", networks))),
            label = info)) +
        geom_point(position = jitter) +
        scale_colour_identity() +
        xlab("Three different predictions ratio") +
        ylab("Two different predictions ratio") +
        theme_classic() +
        theme(legend.position = "none")

    saveWidget(ggplotly(plot), file = "triplets_plot.html")
}


triplets_stats <- get_triplets_stats(
    networks = networks, net_folder = net_folder,
    all_classes = all_classes, subset_classes = subset_classes)

plot_triplets_stats(triplets_stats, networks)
