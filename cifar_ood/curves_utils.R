np <- import("numpy")

source("utils.R")

curve_axes <- c(
    roc = c(x = "FPR", y = "TPR"),
    prc = c(x = "recall", y = "precision")
)

load_curve <- function(file_info, file_col, nms, dir)
{
    curve <- read.csv(file.path(dir, file_info[file_col]))
    for (name_i in seq_along(nms))
    {
        curve[names(nms)[[name_i]]] <- file_info[nms[[name_i]]]
    }
    return(curve)
}

load_curves <- function(files_info, file_col, nms, dir)
{
    curves <- apply(
        files_info,
        MARGIN = 1,
        FUN = load_curve,
        file_col = file_col,
        nms = nms,
        dir = dir)

    return(dplyr::bind_rows(curves))
}

get_net_curves <- function(dir)
{
    net_files <- find_files_by_ptrn(
        dir = dir,
        ptrns = list(
            roc = "net_msp_roc",
            prc = "net_msp_prc")
    )
    curves <- c("roc", "prc")
    net_curves <- lapply(
        X = curves,
        FUN = function(curve, dir) {
            load_curves(files_info = net_files, file_col = curve, nms = list(net = "net"), dir = dir)
        },
        dir = dir
    )
    names(net_curves) <- curves
    return(net_curves)
}


get_cal_files <- function(dir)
{
    cal_files <- find_files_by_ptrn(
        dir = dir,
        ptrns = list(
            cal_msp_roc = "cal_ens_msp_roc",
            cal_msp_prc = "cal_ens_msp_prc"
        )
    )
    return(cal_files)
}

get_pwc_files <- function(dir)
{
    pwc_files <- find_files_by_ptrn(
        dir = dir,
        ptrns = list(
            pwc_msp_roc = "pwc_ens_msp_roc",
            pwc_msp_prc = "pwc_ens_msp_prc",
            pwc_unc_roc = "pwc_ens_unc_roc",
            pwc_unc_prc = "pwc_ens_unc_prc"
        )
    )

    pwc_files <- pwc_files %>% tidyr::pivot_longer(
        cols = c(pwc_msp_roc, pwc_msp_prc, pwc_unc_roc, pwc_unc_prc),
        names_to = c("det_met", "curve"),
        names_pattern = "pwc_(.*?)_(.*?)$",
        values_to = "file"
    )
    return(pwc_files)
}

get_cal_curves <- function(dir)
{
    cal_files <- get_cal_files(dir = dir)

    curves <- c("roc", "prc")
    cal_curves <- lapply(
        X = curves,
        FUN = function(curve, dir) {
            load_curves(
                files_info = cal_files,
                file_col = paste0("cal_msp_", curve),
                nms = list(calibrating_method = "calibrating_method", nets = "nets"),
                dir = dir)
        },
        dir = dir
    )
    names(cal_curves) <- curves
    return(cal_curves)
}

get_pwc_curves <- function(dir)
{
    pwc_files <- get_pwc_files(dir = dir)

    curves <- c("roc", "prc")
    pwc_curves <- lapply(
        X = curves,
        FUN = function(load_curve, dir) {
            pwc_roc <- load_curves(
                files_info = pwc_files %>% filter(curve == load_curve),
                file_col = "file",
                nms = list(combining_method = "combining_method", coupling_method = "coupling_method", det_met = "det_met", nets = "nets"),
                dir = dir)
        },
        dir = dir
    )
    names(pwc_curves) <- curves
    return(pwc_curves)
}