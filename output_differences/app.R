library(shiny)
library(shinyFiles)
library(readr)
library(stringr)
library(reticulate)
library(kit)
library(gridExtra)
library(ggplotify)
library(ggpubr)
library(ggplot2)
library(grid)

np <- import("numpy")
tv <- import("torchvision")

ui <- fluidPage(
    fluidRow(
        column(
            4,
            textInput("dir_path", "Folder path",
                value = "D:\\skola\\1\\weighted_ensembles\\tests\\test_cifar_2021\\data\\data_tv_5000_c100\\0\\exp_ensemble_evaluation")
        ),
        column(
            4,
            actionButton("load_nets", "Load networks")
        ),
        column(
            4,
            verbatimTextOutput("dir_test")
        )
    ),
    fluidRow(
        column(
            4,
            textInput("cifar_path", "Cifar path",
                value = "D:\\skola\\1\\weighted_ensembles\\tests\\test_cifar_2021\\data\\cifar_data")
        ),
        column(
            4,
            actionButton("load_cifar", "Load cifar")
        ),
        column(
            4,
            verbatimTextOutput("cif_dir_test")
        )
    ),
    fluidRow(
        column(
            4,
            actionButton("add", "Add filter"),
        ),
        column(
            4,
            actionButton("display", "Display images")
        )
    ),
    fluidRow(
        tags$div(id = "filters_plc")
    ),
    fluidRow(
        tags$div(id = "images_plc")
    )
)

server <- function(input, output) {

    pwc_ens_file <- "ens_pwc_metrics.csv"
    cal_ens_file <- "ens_cal_metrics.csv"
    networks_file <- "networks_order.txt"

    global <- reactiveValues(
        filterID = 0,
        networks = c(),
        co_m = c(),
        cp_m = c(),
        cal_m = c(),
        cal_ens_metrics = NULL,
        pwc_ens_metrics = NULL,
        cifar = NULL)

    filter_files <- list()

    observeEvent(input$add, {
        curID <- global$filterID
        global$filterID <- global$filterID + 1
        rmv_btn_id <- paste0("rem_", curID)
        div_id <- paste0("filter_", curID)
        checkbox_id <- paste0("checkbox_", curID)
        radio_id <- paste0("radio_", curID)
        co_m_id <- paste0("rad_co_", curID)
        cp_m_id <- paste0("rad_cp_", curID)
        cal_m_id <- paste0("rad_cal_", curID)
        cor_id <- paste0("rad_cor_", curID)
        check_id <- paste0("check_", curID)

        insertUI(
            selector = "#filters_plc",
            where = "beforeBegin",
            ui = tags$div(
                id = div_id,
                column(
                    3,
                    h3(paste0("Filter ", curID)),
                    label = "Filter",
                    actionButton(rmv_btn_id, "x"),
                    checkboxGroupInput(
                        checkbox_id,
                        label = "Ensembled networks",
                        choices = global$networks
                    ),
                    radioButtons(
                        radio_id,
                        label = "Ensemble type",
                        choices = list(
                            "calibrating" = "cal", "weighted" = "pwc"),
                        selected = "pwc"),
                    conditionalPanel(
                        condition =
                            paste0("input.", radio_id, " == \"cal\""),
                        radioButtons(
                            cal_m_id,
                            label = "Calibrating method",
                            choices = global$cal_m
                        )
                    ),
                    conditionalPanel(
                        condition =
                            paste0("input.", radio_id, " == \"pwc\""),
                        radioButtons(
                            co_m_id,
                            label = "Combining method",
                            choices = global$co_m
                        ),
                        radioButtons(
                            cp_m_id,
                            label = "Coupling method",
                            choices = global$cp_m
                        )
                    ),
                    radioButtons(
                        cor_id,
                        label = "Classification is",
                        choices = list("correct" = T, "incorrect" = F)
                    ),
                    verbatimTextOutput(
                        check_id
                    )
                )
            )
        )

        observeEvent(input[[rmv_btn_id]], {
            filter_files[[toString(curID)]] <- NULL
            removeUI(
                selector = paste0("#", div_id)
            )
        })

        filter_files[[toString(curID)]] <<- reactive({
            if (input[[radio_id]] == "cal") {
                ptrn <- paste0(
                    "^(.*?)_ens_test_outputs_cal_",
                    input[[cal_m_id]], "_prec_float.npy$")
            } else if (input[[radio_id]] == "pwc") {
                ptrn <- paste0(
                    "^(.*?)_ens_test_outputs_co_",
                    input[[co_m_id]], "_cp_",
                    input[[cp_m_id]], "_prec_float.npy$")
            }

            sel_nets <- input[[checkbox_id]]
            files <- list.files(input$dir_path, pattern = ptrn)
            file_ind <- which(
                unlist(
                    lapply(
                        strsplit(
                            str_match(files, ptrn)[, 2],
                            split = "+",
                            fixed = T),
                            function(x) setequal(x, sel_nets))))
            return(file.path(input$dir_path, files[file_ind]))
        })

        output[[check_id]] <- renderText({
            validate(
                need(
                    length(filter_files[[toString(curID)]]()) != 0,
                    "Output file with specified conditions does not exist"
                )
            )
        })
    })

    observeEvent(input$load_nets, {
        global$pwc_metrics <- read.csv(file.path(input$dir_path, pwc_ens_file))
        global$cal_metrics <- read.csv(file.path(input$dir_path, cal_ens_file))
        global$networks <- read_lines(file.path(input$dir_path, networks_file))
        global$cp_m <- unique(global$pwc_metrics$coupling_method)
        global$co_m <- unique(global$pwc_metrics$combining_method)
        global$cal_m <- unique(global$cal_metrics$calibrating_method)

        output$dir_test <- renderText(
            "Classifier info loaded"
        )
    })

    output$dir_test <- renderText({
        validate(
            need(file.exists(file.path(input$dir_path, pwc_ens_file)),
                "Enter folder which contains weighted ensemble metrics file"),
            need(file.exists(file.path(input$dir_path, cal_ens_file)),
                "Enter folder which contains calibrating ensemble metrics file"),
            need(file.exists(file.path(input$dir_path, networks_file)),
                "Enter folder which contains networks order file")
        )
    })

    output$cif_dir_test <- renderText({
        validate(
            need(dir.exists(input$cifar_path),
            "Enter existing folder")
        )
    })

    observeEvent(input$load_cifar, {
        global$cifar <- tv$datasets$CIFAR100(
            root = input$cifar_path, train = F, download = T)
        output$cif_dir_test <- renderText({
            "Cifar 100 loaded"
        })
    })

    observeEvent(input$display, {
        n_for_top <- 5

        correct_labels <- global$cifar$targets + 1
        masks <- matrix(ncol = 0, nrow = length(correct_labels))
        topn_inds <- list()
        topn_probs <- list()
        for (filterID in names(filter_files)) {
            correct <- input[[paste0("rad_cor_", filterID)]]
            predictions <- np$load(filter_files[[filterID]]())
            top_inds <- t(apply(
                predictions, 1,
                {function (row) topn(row, n = n_for_top)}))
            top_probs <- t(apply(
                predictions, 1,
                {function (row) topn(row, n = n_for_top, index = F)}))

            topn_inds[[length(topn_inds) + 1]] <- top_inds
            topn_probs[[length(topn_probs) + 1]] <- top_probs
            mask <- (correct_labels == top_inds[, 1]) == correct
            masks <- cbind(masks, mask)
        }

        res_mask <- apply(masks, 1, prod) == 1
        num_images <- sum(res_mask)
        correct_classes <- global$cifar$classes[correct_labels[res_mask]]
        topn_inds_selected <- lapply(topn_inds, function(mat) mat[res_mask, ])
        topn_classes_selected <- lapply(
            topn_inds_selected,
            function(mat) matrix(global$cifar$classes[mat], ncol = n_for_top))
        topn_probs_selected <- lapply(topn_probs, function(mat) mat[res_mask, ])
        images <- global$cifar$data[res_mask, , , ] / 255

        n_plot_imgs <- min(100, num_images)
        disp_images <- images[1:n_plot_imgs, , , ]
        disp_correct_classes <- correct_classes[1:n_plot_imgs]
        disp_topn_classes <- lapply(
            topn_classes_selected,
            function(mat) mat[1:n_plot_imgs, ])
        disp_topn_probs <- lapply(
            topn_probs_selected,
            function(mat) mat[1:n_plot_imgs, ])

        texts <- lapply(
            seq(n_plot_imgs),
            function(i) {
                paste(
                    paste0("Correct class ", disp_correct_classes[[i]]),
                    paste(
                        lapply(
                            seq(length(disp_topn_classes)),
                            function(j) {
                                paste(
                                    paste0("Filter ", names(filter_files)[j]),
                                    paste(
                                        lapply(
                                            seq(n_for_top),
                                            function(k) {
                                                paste0(
                                                    disp_topn_classes[[j]][i, k],
                                                    " (",
                                                    round(
                                                        100 * disp_topn_probs[[j]][i, k],
                                                        digits = 2),
                                                    "%)")
                                            }
                                        ),
                                        collapse = "<br/>"
                                    ),
                                    sep = "<br/>"
                                )
                            }
                        ),
                        collapse = "<br/>"
                    ),
                    sep = "<br/>"
                )
            }
        )

        insertUI(
            selector = "#images_plc",
            where = "beforeBegin",
            ui = tags$div(
                id = "images",
                lapply(
                    seq(n_plot_imgs),
                    function(i) {
                        column(
                            2,
                            plotOutput(
                                paste0("im_", toString(i))
                            ),
                            p(HTML(texts[[i]]))
                        )
                    }
                )
            )
        )

        lapply(seq(n_plot_imgs), function(i) {
            output[[paste0("im_", toString(i))]] <-
            renderPlot(plot(as.raster(disp_images[i, , , ])))
        })

    })
}

shinyApp(ui = ui, server = server)