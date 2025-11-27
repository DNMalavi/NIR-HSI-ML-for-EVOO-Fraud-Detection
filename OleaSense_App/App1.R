# ============================================================
# Author: Derick Malavi
# Date: November 20, 2025
# OleaSense: HSI EVOO classifier
#
# Description:
# OleaSense is an interactive R Shiny application that showcases how
# near-infrared hyperspectral imaging (HSI), chemometrics, and
# machine learning can be used to authenticate extra-virgin olive oil (EVOO).
#
# Core functionality:
# - Upload spectral data (rows = samples, columns = wavelengths)
# - Apply MSC or SNV followed by Savitzky–Golay 2nd derivative
# - Use PCA scores for LDA and penalised logistic regression
# - Use full preprocessed spectra for PLS-DA and RF classification
# - Visualise:
#     * Class prediction distribution (counts or %)
#     * Raw vs preprocessed spectra (toggle)
#     * PCA score plots (interactive PC selection)
#     * PCA loadings (with top wavelengths highlighted)
#     * Variable importance (top 20 predictors with wavelength info)
#     * PLS cross-validation curve (Accuracy vs. ncomp) with best component
#     * RF cross-validation curve (Accuracy vs. mtry) with best mtry
# - Download prediction tables, example test data, feedback logs,
#   and publication-ready plots
#
# The app is designed as a transparent, research-ready interface to demonstrate
# EVOO classification models based on HSI and machine learning.
# The classes correspond to authentic EVOO and EVOO adulterated with
# refined olive oil, olive pomace oil, or hazelnut oil.
# ============================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(mdatools)
library(shinythemes)
library(bslib)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(readxl)
library(tools)
library(caret)
library(tidyr)
library(caret)
library(pls)
library(randomForest)

# ===========================
# Load EVOO models, PCA, and X-column names
# ===========================

# LDA Models
fit_lda_msc_sg_2d  <- readRDS("fit_lda_msc_sg_2d.rds")
fit_lda_snv_sg_2d  <- readRDS("fit_lda_snv_sg_2d.rds")

# Logistic Regression Models
fit_logistic_regression_msc_sg_2d <- readRDS("fit_logistic_regression_msc_sg_2d.rds")
fit_logistic_regression_snv_sg_2d <- readRDS("fit_logistic_regression_snv_sg_2d.rds")

# PLS-DA Models
fit_pls_msc_sg_2d <- readRDS("fit_pls_msc_sg_2d.rds")
fit_pls_snv_sg_2d <- readRDS("fit_pls_snv_sg_2d.rds")

# Random Forest Models (full spectra like PLS)
fit_rf_msc_sg_2d  <- readRDS("fit_rf_msc_sg_2d.rds")
fit_rf_snv_sg_2d  <- readRDS("fit_rf_snv_sg_2d.rds")

# PCA Preprocessing 
pca_Preprocess_msc_sg_2d <- readRDS("pca_Preprocess_msc_sg_2d.rds")
pca_Preprocess_snv_sg_2d <- readRDS("pca_Preprocess_snv_sg_2d.rds")

# X variable names (wavelengths used during training)
X_column_names <- readRDS("X_column_names.rds")

# True EVOO test set from Excel (used as example/sample data)
# Make sure evoo_true_test_set.xlsx is in the same app folder when running/deploying
evoo_true_test_set <- read_excel("evoo_true_test_set.xlsx")

# ===========================
# Helper functions
# ===========================

get_model <- function(model_choice, preproc_choice) {
  key <- paste(model_choice, preproc_choice, sep = "_")
  switch(
    key,
    "lda_MSC_SG2"      = fit_lda_msc_sg_2d,
    "lda_SNV_SG2"      = fit_lda_snv_sg_2d,
    "logreg_MSC_SG2"   = fit_logistic_regression_msc_sg_2d,
    "logreg_SNV_SG2"   = fit_logistic_regression_snv_sg_2d,
    "pls_MSC_SG2"      = fit_pls_msc_sg_2d,
    "pls_SNV_SG2"      = fit_pls_snv_sg_2d,
    "rf_MSC_SG2"       = fit_rf_msc_sg_2d,
    "rf_SNV_SG2"       = fit_rf_snv_sg_2d,
    stop("Selected combination of model and preprocessing is not available.")
  )
}

get_pca <- function(preproc_choice) {
  switch(
    preproc_choice,
    "MSC_SG2" = pca_Preprocess_msc_sg_2d,
    "SNV_SG2" = pca_Preprocess_snv_sg_2d,
    NULL
  )
}

# Retrieve PLS CV results (Accuracy vs ncomp) from caret objects
get_pls_cv <- function(preproc_choice) {
  pls_model <- if (preproc_choice == "MSC_SG2") {
    fit_pls_msc_sg_2d
  } else {
    fit_pls_snv_sg_2d
  }
  if (is.null(pls_model) || is.null(pls_model$results)) return(NULL)
  
  df <- pls_model$results
  if (!"ncomp" %in% names(df) || !"Accuracy" %in% names(df)) return(NULL)
  
  best_ncomp <- pls_model$bestTune$ncomp
  list(
    results    = df,
    best_ncomp = best_ncomp
  )
}

# Retrieve RF CV results (Accuracy vs mtry) from caret objects
get_rf_cv <- function(preproc_choice) {
  rf_model <- if (preproc_choice == "MSC_SG2") {
    fit_rf_msc_sg_2d
  } else {
    fit_rf_snv_sg_2d
  }
  if (is.null(rf_model) || is.null(rf_model$results)) return(NULL)
  
  df <- rf_model$results
  if (!"mtry" %in% names(df) || !"Accuracy" %in% names(df)) return(NULL)
  
  best_mtry <- rf_model$bestTune$mtry
  list(
    results   = df,
    best_mtry = best_mtry
  )
}

# ===========================
# Preprocessing functions
# ===========================

apply_msc_sg2 <- function(df) {
  m  <- as.matrix(df)
  m1 <- prep.msc(m)
  m2 <- prep.savgol(m1, width = 7, porder = 2, dorder = 2)
  as.data.frame(m2)
}

apply_snv_sg2 <- function(df) {
  m  <- as.matrix(df)
  m1 <- prep.snv(m)
  m2 <- prep.savgol(m1, width = 7, porder = 2, dorder = 2)
  as.data.frame(m2)
}

apply_preprocessing <- function(df, preproc_choice) {
  if (preproc_choice == "MSC_SG2") {
    apply_msc_sg2(df)
  } else if (preproc_choice == "SNV_SG2") {
    apply_snv_sg2(df)
  } else {
    df
  }
}

# ===========================
# UI
# ===========================

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5),
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f7f8;
      }
      .app-header {
        background: linear-gradient(to right, #111827, #1f2937, #4f46e5);
        padding: 14px 20px 4px 20px;
        color: white;
        font-weight: 700;
        font-size: 24px;
        border-radius: 0 0 18px 18px;
        margin-bottom: 10px;
      }
      .app-header-inner {
        display: flex;
        align-items: center;
        justify-content: space-between;
      }
      .app-title-main {
        display: flex;
        flex-direction: column;
      }
      .app-title-row {
        display: flex;
        align-items: center;
        gap: 8px;
      }
      .app-subtitle {
        font-size: 13px;
        font-weight: 400;
        opacity: 0.95;
        margin-top: 4px;
      }
      .app-ribbon {
        background: rgba(15, 23, 42, 0.85);
        border-radius: 999px;
        padding: 4px 12px;
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: 0.06em;
        border: 1px solid rgba(148, 163, 184, 0.7);
      }
      .hsi-strip {
        margin-top: 8px;
        width: 100%;
        height: 6px;
        border-radius: 999px;
        background: linear-gradient(
          to right,
          #00004f,
          #001f7f,
          #0044ff,
          #00b3ff,
          #00ff8c,
          #a0ff00,
          #ffdd00,
          #ff8800,
          #ff0044
        );
        opacity: 0.9;
      }
      .sidebarPanel {
        background-color: #ffffff;
        border-radius: 16px;
        box-shadow: 0 4px 16px rgba(15,23,42,0.08);
      }
      .main-panel-card {
        background-color: #ffffff;
        border-radius: 16px;
        padding: 12px 16px;
        box-shadow: 0 4px 16px rgba(15,23,42,0.08);
        margin-bottom: 15px;
      }
      .nav-tabs > li > a {
        font-weight: 500;
      }
    "))
  ),
  
  div(
    class = "app-header",
    div(
      class = "app-header-inner",
      div(
        class = "app-title-main",
        div(
          class = "app-title-row",
          span("🌈 OleaSense: HSI EVOO Classifier 💧🍾")
        ),
        span(
          class = "app-subtitle",
          HTML("Near-Infrared Hyperspectral Imaging (900-1700 nm) · Chemometrics · Machine Learning")
        ),
        div(class = "hsi-strip")
      ),
      div(
        class = "app-ribbon",
        "Research prototype · Not for routine QC"
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Upload data"),
      tags$small("Upload a spectral dataset where each row is a sample and columns are wavelengths."),
      fileInput(
        "test_file",
        "Upload spectral data file",
        accept = c(".csv", ".txt", ".tsv", ".xls", ".xlsx")
      ),
      hr(),
      h4("2. Model configuration"),
      selectInput(
        "model_choice",
        "Choose classifier:",
        choices = c(
          "LDA (PCA scores)"            = "lda",
          "Logistic regression (PCA)"   = "logreg",
          "PLS-DA (full spectra)"       = "pls",
          "Random Forest (full spectra)"= "rf"
        )
      ),
      radioButtons(
        "preproc_choice",
        "Preprocessing for models:",
        choices = c(
          "MSC + SG 2nd derivative" = "MSC_SG2",
          "SNV + SG 2nd derivative" = "SNV_SG2"
        ),
        selected = "MSC_SG2"
      ),
      helpText("LDA/logistic use PCA scores. PLS-DA and RF use full preprocessed spectra."),
      selectInput(
        "pca_x",
        "PCA X-axis:",
        choices = c("PC1" = 1, "PC2" = 2, "PC3" = 3),
        selected = 1
      ),
      selectInput(
        "pca_y",
        "PCA Y-axis:",
        choices = c("PC1" = 1, "PC2" = 2, "PC3" = 3),
        selected = 2
      ),
      radioButtons(
        "bar_type",
        "Prediction bar scale:",
        choices = c("Counts" = "count", "Percentages" = "percent"),
        selected = "count",
        inline = TRUE
      ),
      hr(),
      h4("3. Appearance and export"),
      selectInput(
        "theme_choice",
        "Theme:",
        choices = c(
          "Default"   = "default",
          "Cerulean"  = "cerulean",
          "Cosmo"     = "cosmo",
          "Darkly"    = "darkly",
          "Flatly"    = "flatly",
          "Journal"   = "journal",
          "Litera"    = "litera",
          "Lumen"     = "lumen",
          "Minty"     = "minty",
          "Pulse"     = "pulse",
          "Sandstone" = "sandstone",
          "Spacelab"  = "spacelab",
          "United"    = "united",
          "Yeti"      = "yeti"
        ),
        selected = "flatly"
      ),
      textInput("filename", "Custom filename", "OleaSense_predictions"),
      br(),
      actionButton("predict_btn", "Run prediction", class = "btn btn-success btn-block"),
      actionButton("reset_btn",   "Reset app",      class = "btn btn-warning btn-block"),
      hr(),
      downloadButton("download_preds", "Download predictions", class = "btn btn-primary btn-block")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "📘 User guide",
          br(),
          div(class = "main-panel-card",
              h3("How to use OleaSense"),
              tags$ol(
                tags$li("Prepare your spectral data so that each row is a sample and each column is a wavelength variable."),
                tags$li("Upload the file using the \"Upload spectral data file\" control on the left."),
                tags$li("Choose the model and preprocessing options under \"Model configuration\"."),
                tags$li("Click \"Run prediction\" to generate class predictions."),
                tags$li("Explore the tabs for spectra, PCA scores, PCA loadings, variable importance, and cross-validation (PLS-DA or RF)."),
                tags$li("Download prediction tables, plots, and feedback logs for documentation or reporting.")
              ),
              p("The classification models distinguish authentic extra-virgin olive oil from EVOO adulterated with refined olive oil, olive pomace oil, or hazelnut oil.")
          )
        ),
        tabPanel(
          "📊 Overview",
          br(),
          div(class = "main-panel-card",
              h4("Uploaded data preview"),
              tags$small("The first five rows of the uploaded file are shown. Only numeric columns are used as spectral variables."),
              withSpinner(DTOutput("preview_data"))
          ),
          div(class = "main-panel-card",
              h4("Prediction summary"),
              tags$small("Counts or percentages of samples per predicted class. Classes correspond to authentic EVOO or EVOO adulterated with refined olive oil, olive pomace oil, or hazelnut oil."),
              withSpinner(DTOutput("results_summary")),
              br(),
              withSpinner(plotOutput("bar_distribution", height = "360px")),
              br(),
              downloadButton("download_bar_plot", "Download prediction distribution plot")
          )
        ),
        tabPanel(
          "📡 Spectra",
          br(),
          div(class = "main-panel-card",
              h4("Spectral profiles"),
              tags$small("Visualise sample spectra as raw or after MSC/SNV + Savitzky–Golay preprocessing."),
              radioButtons(
                "spectra_view",
                "Spectra shown as:",
                choices = c(
                  "Raw (unpreprocessed)"        = "raw",
                  "MSC + SG 2nd derivative"     = "MSC_SG2",
                  "SNV + SG 2nd derivative"     = "SNV_SG2"
                ),
                selected = "raw",
                inline = TRUE
              ),
              withSpinner(plotOutput("spectra_plot", height = "360px"))
          )
        ),
        tabPanel(
          "📈 PCA scores",
          br(),
          div(class = "main-panel-card",
              h4("PCA score plot"),
              tags$small("PCA is applied to preprocessed spectra. LDA and logistic regression use these PCA scores as input."),
              withSpinner(plotOutput("pca_dynamic_plot", height = "360px")),
              br(),
              downloadButton("download_pca_scores", "Download PCA score plot")
          )
        ),
        tabPanel(
          "📉 PCA loadings",
          br(),
          div(class = "main-panel-card",
              h4("PCA loadings for model input space"),
              tags$small("These loadings link principal components used by LDA/logistic regression back to the original wavelengths, supporting chemical interpretation."),
              selectInput(
                "pc_loading",
                "Select principal component:",
                choices = c("PC1" = 1, "PC2" = 2, "PC3" = 3),
                selected = 1
              ),
              withSpinner(plotOutput("pca_loadings_plot", height = "360px")),
              br(),
              downloadButton("download_pca_loadings", "Download PCA loadings plot"),
              br(), br(),
              h5("Top wavelengths driving the selected principal component"),
              withSpinner(DTOutput("pca_loadings_table"))
          )
        ),
        tabPanel(
          "📐 Cross-validation",
          br(),
          div(class = "main-panel-card",
              h4("Cross-validation (training results)"),
              tags$small("For PLS-DA: accuracy versus number of PLS components. For Random Forest: accuracy versus mtry. The optimal setting is indicated by the red marker."),
              withSpinner(plotOutput("pls_cv_plot", height = "360px")),
              br(),
              downloadButton("download_pls_cv", "Download CV plot")
          )
        ),
        tabPanel(
          "⭐ Variable importance",
          br(),
          div(class = "main-panel-card",
              h4("Top 20 most important variables"),
              tags$small("Variable importance is computed for the selected classifier and preprocessing. Where possible, variables are linked to wavelengths in nm."),
              withSpinner(plotOutput("varimp_plot", height = "400px")),
              br(),
              downloadButton("download_varimp", "Download variable importance plot"),
              br(), br(),
              withSpinner(DTOutput("varimp_table"))
          )
        ),
        tabPanel(
          "📋 Predictions",
          br(),
          div(class = "main-panel-card",
              h4("Prediction details"),
              tags$small("Predicted class labels and, where available, class probabilities for each sample."),
              withSpinner(DTOutput("prediction_table")),
              br(),
              verbatimTextOutput("class_summary")
          )
        ),
        tabPanel(
          "🧪 Sample data",
          br(),
          div(class = "main-panel-card",
              h4("Example test set"),
              p("Download an example spectral test set based on the EVOO true test set. This allows you to explore OleaSense predictions with a realistic dataset."),
              downloadButton("download_sample", "Download sample CSV")
          )
        ),
        tabPanel(
          "⚙️ Preprocessing info",
          br(),
          div(class = "main-panel-card",
              h3("Preprocessing pipeline"),
              p("OleaSense applies the same preprocessing pipeline used in the EVOO case study:"),
              tags$ul(
                tags$li("Multiplicative scatter correction (MSC): Corrects multiplicative scatter and path length effects between spectra."),
                tags$li("Standard normal variate (SNV): Standardises each spectrum to reduce scatter variation (alternative to MSC)."),
                tags$li("Savitzky–Golay derivative: Smoothing and second order derivative (window width 7, polynomial order 2, derivative order 2) to enhance subtle spectral features.")
              )
          )
        ),
        tabPanel(
          "✉️ Feedback",
          br(),
          div(class = "main-panel-card",
              h3("Feedback"),
              p("Your feedback helps improve this tool for both research and practical use."),
              textAreaInput(
                "user_feedback",
                "Share comments, suggestions, or issues:",
                "",
                width = "100%",
                height = "120px"
              ),
              actionButton("send_feedback", "Submit feedback", class = "btn btn-primary"),
              br(), br(),
              h4("Developer tools"),
              tags$small("Enter the developer key to unlock feedback download (for internal use only)."),
              passwordInput(
                "dev_key",
                "Developer key:",
                placeholder = "Enter developer key"
              ),
              uiOutput("feedback_download_ui")
          )
        ),
        tabPanel(
          "🔍 Diagnostics",
          br(),
          div(class = "main-panel-card",
              h3("Diagnostics"),
              p("Basic checks on the uploaded file and app state, useful during development or debugging."),
              verbatimTextOutput("diagnostics")
          )
        ),
        tabPanel(
          "ℹ️ About",
          br(),
          div(class = "main-panel-card",
              h3("About OleaSense"),
              p("OleaSense is an interactive R Shiny application that demonstrates how near infrared hyperspectral imaging, chemometrics, and machine learning can be used to assess the authenticity of extra virgin olive oil."),
              tags$ul(
                tags$li("Models: Linear discriminant analysis (LDA), penalised logistic regression, PLS-DA, and Random Forest, trained on NIR HSI data."),
                tags$li("Preprocessing: MSC or SNV combined with Savitzky–Golay second-order derivatives, with PCA used for LDA and logistic regression."),
                tags$li("Outputs: Class predictions, prediction summaries, PCA score plots, cross-validation curves, PCA loadings, raw vs preprocessed spectra, and variable importance visualisations.")
              ),
              p("The models distinguish authentic extra-virgin olive oil from EVOO adulterated with refined olive oil, olive pomace oil, or hazelnut oil."),
              p("The app is intended for demonstration, educational, and research use and should be interpreted alongside proper method validation and domain expertise.")
          )
        ),
        tabPanel(
          "📩 Contact",
          br(),
          div(class = "main-panel-card",
              h3("Developer"),
              p("Name: Derick Malavi"),
              p("Email: ", a(href = "mailto:malaviderick@gmail.com", "malaviderick@gmail.com")),
              p("LinkedIn: ",
                a(href = "https://www.linkedin.com/in/derick-malavi-64742643/",
                  "linkedin.com/in/derick-malavi")),
              p("GitHub: ",
                a(href = "https://github.com/DNMalavi", "github.com/DNMalavi"))
          )
        )
      )
    )
  )
)

# ===========================
# Server
# ===========================

server <- function(input, output, session) {
  pred_results     <- reactiveVal()
  input_preview    <- reactiveVal()
  pca_scores       <- reactiveVal()
  pca_predictions  <- reactiveVal()
  spectra_raw      <- reactiveVal()
  dev_mode         <- reactiveVal(FALSE)
  
  # ---- dynamic theme ----
  observe({
    req(input$theme_choice)
    if (input$theme_choice == "default") {
      session$setCurrentTheme(bs_theme(version = 5))
    } else {
      session$setCurrentTheme(bs_theme(bootswatch = input$theme_choice))
    }
  })
  
  # ---- dynamic PCA choices (if more PCs) ----
  observe({
    pca_obj <- get_pca(input$preproc_choice)
    if (!is.null(pca_obj) && !is.null(pca_obj$rotation)) {
      ncomp <- ncol(pca_obj$rotation)
      if (ncomp >= 1) {
        pc_choices <- setNames(seq_len(ncomp), paste0("PC", seq_len(ncomp)))
        updateSelectInput(session, "pca_x",      choices = pc_choices, selected = min(1, ncomp))
        updateSelectInput(session, "pca_y",      choices = pc_choices, selected = min(2, ncomp))
        updateSelectInput(session, "pc_loading", choices = pc_choices, selected = min(1, ncomp))
      }
    }
  })
  
  # ---- developer password for feedback log ----
  observeEvent(input$dev_key, {
    # Set your dev key here
    valid_key <- "Nyabera@1990"  # your secret key
    
    if (!is.null(input$dev_key) && nzchar(input$dev_key) && identical(input$dev_key, valid_key)) {
      dev_mode(TRUE)
      showNotification("Developer mode unlocked. Feedback download enabled.", type = "message")
    } else {
      dev_mode(FALSE)
      if (nzchar(input$dev_key)) {
        showNotification("Invalid developer key.", type = "error")
      }
    }
  })
  
  # ---- run prediction ----
  observeEvent(input$predict_btn, {
    req(input$test_file)
    
    ext <- tools::file_ext(input$test_file$name)
    new_data <- tryCatch(
      {
        switch(
          ext,
          csv  = read_csv(input$test_file$datapath, show_col_types = FALSE),
          txt  = read_delim(input$test_file$datapath, delim = "\t", show_col_types = FALSE),
          tsv  = read_tsv(input$test_file$datapath, show_col_types = FALSE),
          xls  = read_excel(input$test_file$datapath),
          xlsx = read_excel(input$test_file$datapath),
          stop("Unsupported file type.")
        )
      },
      error = function(e) {
        showNotification(paste("Failed to read file:", e$message), type = "error")
        return(NULL)
      }
    )
    req(new_data)
    
    # preview head
    input_preview(head(new_data, 5))
    output$preview_data <- renderDT({
      datatable(input_preview(), options = list(scrollX = TRUE))
    })
    
    # numeric columns as spectra
    numeric_cols <- sapply(new_data, is.numeric)
    spectra_df   <- new_data[, numeric_cols, drop = FALSE]
    
    # If column names are missing/blank, assign something sensible
    if (is.null(colnames(spectra_df)) || all(colnames(spectra_df) == "")) {
      if (ncol(spectra_df) == length(X_column_names)) {
        colnames(spectra_df) <- X_column_names
      } else {
        colnames(spectra_df) <- paste0("X", seq_len(ncol(spectra_df)))
      }
    }
    
    # Align columns with training X_column_names
    if (ncol(spectra_df) == length(X_column_names)) {
      spectra_df <- spectra_df[, seq_len(length(X_column_names)), drop = FALSE]
      colnames(spectra_df) <- X_column_names
    } else {
      missing_cols <- setdiff(X_column_names, colnames(spectra_df))
      if (length(missing_cols) > 0) {
        showNotification(
          paste(
            "Uploaded data are missing",
            length(missing_cols),
            "required spectral variables. Example:",
            paste(head(missing_cols, 5), collapse = ", "), "..."
          ),
          type = "error",
          duration = 8
        )
        return(NULL)
      }
      spectra_df <- spectra_df[, X_column_names, drop = FALSE]
    }
    
    # Store raw spectra for plotting
    spectra_raw(spectra_df)
    
    # 1. preprocessing for modelling
    processed_df <- apply_preprocessing(spectra_df, input$preproc_choice)
    
    # 2. PCA (for LDA/logreg and visualisation)
    pca_obj <- get_pca(input$preproc_choice)
    if (!is.null(pca_obj)) {
      X_pca <- predict(pca_obj, processed_df)
      X_pca <- as.data.frame(X_pca)
      colnames(X_pca) <- paste0("PC", seq_len(ncol(X_pca)))
      pca_scores(X_pca)
    } else {
      pca_scores(NULL)
    }
    
    # 3. select model
    model <- get_model(input$model_choice, input$preproc_choice)
    
    # 4. choose input for model
    if (input$model_choice %in% c("lda", "logreg")) {
      newdata_model <- pca_scores()    # LDA & logistic use PCA scores
    } else {                           # PLS-DA & RF use full preprocessed spectra
      newdata_model <- processed_df
    }
    
    # 5. predictions
    preds_class <- predict(model, newdata = newdata_model, type = "raw")
    preds_prob <- tryCatch(
      {
        as.data.frame(predict(model, newdata = newdata_model, type = "prob"))
      },
      error = function(e) NULL
    )
    
    result_df <- data.frame(
      Sample_ID  = paste0("Sample", seq_len(nrow(new_data))),
      Prediction = preds_class,
      stringsAsFactors = FALSE
    )
    if (!is.null(preds_prob)) {
      result_df <- cbind(result_df, preds_prob)
    }
    
    pred_results(result_df)
    pca_predictions(preds_class)
    
    # ---- outputs ----
    output$prediction_table <- renderDT({
      datatable(result_df, options = list(scrollX = TRUE))
    })
    
    output$class_summary <- renderPrint({
      table(result_df$Prediction)
    })
    
    # Summary table (always counts)
    output$results_summary <- renderDT({
      as.data.frame(table(result_df$Prediction)) |>
        dplyr::rename(Class = Var1, Count = Freq)
    })
    
    # Bar distribution (counts or %)
    output$bar_distribution <- renderPlot({
      df <- result_df
      
      if (input$bar_type == "percent") {
        df_plot <- df %>%
          count(Prediction) %>%
          mutate(Percent = n / sum(n) * 100)
        
        ggplot(df_plot, aes(x = Prediction, y = Percent, fill = Prediction)) +
          geom_col(width = 0.7, color = "black") +
          theme_bw(base_size = 14) +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
            axis.title = element_text(color = "black"),
            axis.text  = element_text(color = "black")
          ) +
          labs(
            title = "Distribution of predicted EVOO classes (percent)",
            x = "Predicted class",
            y = "Percentage of samples"
          )
      } else {
        ggplot(df, aes(x = Prediction, fill = Prediction)) +
          geom_bar(width = 0.7, color = "black") +
          theme_bw(base_size = 14) +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
            axis.title = element_text(color = "black"),
            axis.text  = element_text(color = "black")
          ) +
          labs(
            title = "Distribution of predicted EVOO classes (counts)",
            x = "Predicted class",
            y = "Number of samples"
          )
      }
    })
    
    # PCA scores plot
    output$pca_dynamic_plot <- renderPlot({
      req(pca_scores())
      df <- as.data.frame(pca_scores())
      if (ncol(df) < 2) return(NULL)
      df$Prediction <- pca_predictions()
      
      x_pc <- paste0("PC", input$pca_x)
      y_pc <- paste0("PC", input$pca_y)
      
      df_plot <- df
      df_plot$x_val <- df_plot[[x_pc]]
      df_plot$y_val <- df_plot[[y_pc]]
      
      ggplot(df_plot, aes(x = x_val, y = y_val, color = Prediction)) +
        geom_point(size = 3, alpha = 0.9) +
        theme_bw(base_size = 14) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
          axis.title = element_text(color = "black"),
          axis.text  = element_text(color = "black")
        ) +
        labs(
          title = paste("PCA score plot:", x_pc, "vs", y_pc),
          x = x_pc,
          y = y_pc,
          color = "Prediction"
        )
    })
    
    showNotification("Prediction complete.", type = "message")
  })
  
  # ========= Spectra plot (raw vs preprocessed) =========
  output$spectra_plot <- renderPlot({
    req(spectra_raw())
    mat <- spectra_raw()
    
    # Apply view-specific preprocessing for plotting only
    if (input$spectra_view == "MSC_SG2") {
      mat <- as.matrix(apply_msc_sg2(mat))
    } else if (input$spectra_view == "SNV_SG2") {
      mat <- as.matrix(apply_snv_sg2(mat))
    } else {
      mat <- as.matrix(mat)  # raw
    }
    
    # Limit number of spectra to avoid clutter
    n_samp <- min(30, nrow(mat))
    mat <- mat[seq_len(n_samp), , drop = FALSE]
    
    df_long <- as.data.frame(mat)
    df_long$Sample <- factor(paste0("S", seq_len(nrow(df_long))))
    
    df_long <- df_long %>%
      pivot_longer(
        cols = -Sample,
        names_to = "Var",
        values_to = "Intensity"
      )
    
    # Map variable names to numeric wavelengths
    wl_num <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", colnames(mat))))
    if (length(wl_num) == ncol(mat) && !any(is.na(wl_num))) {
      wl_map <- data.frame(Var = colnames(mat), Wavelength = wl_num)
      df_long <- df_long %>%
        left_join(wl_map, by = "Var")
      x_lab <- "Wavelength (nm)"
      
      ggplot(df_long, aes(x = Wavelength, y = Intensity, group = Sample, color = Sample)) +
        geom_line(alpha = 0.6) +
        theme_bw(base_size = 13) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
          axis.title = element_text(color = "black"),
          axis.text  = element_text(color = "black"),
          legend.position = "none"
        ) +
        labs(
          title = "Sample spectra",
          x = x_lab,
          y = "Intensity"
        )
    } else {
      df_long$WavelengthIndex <- as.numeric(gsub("X", "", df_long$Var))
      ggplot(df_long, aes(x = WavelengthIndex, y = Intensity, group = Sample, color = Sample)) +
        geom_line(alpha = 0.6) +
        theme_bw(base_size = 13) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
          axis.title = element_text(color = "black"),
          axis.text  = element_text(color = "black"),
          legend.position = "none"
        ) +
        labs(
          title = "Sample spectra",
          x = "Wavelength index",
          y = "Intensity"
        )
    }
  })
  
  # ========= Cross-Validation Plot (PLS-DA or RF) =========
  output$pls_cv_plot <- renderPlot({
    req(input$preproc_choice)
    
    validate(
      need(input$model_choice %in% c("pls", "rf"),
           "Cross-validation plot is available for the PLS-DA and Random Forest models. Please select one of these in the sidebar.")
    )
    
    if (input$model_choice == "pls") {
      # ----- PLS-DA CV -----
      cv_info <- get_pls_cv(input$preproc_choice)
      validate(
        need(!is.null(cv_info), "PLS cross-validation results are not available for this model object.")
      )
      
      df <- cv_info$results
      best_ncomp <- cv_info$best_ncomp
      
      validate(
        need(all(c("ncomp", "Accuracy") %in% names(df)),
             "PLS model does not contain ncomp and Accuracy information.")
      )
      
      df_plot <- df %>%
        group_by(ncomp) %>%
        summarise(
          Accuracy   = mean(Accuracy,   na.rm = TRUE),
          AccuracySD = if ("AccuracySD" %in% names(df))
            mean(AccuracySD, na.rm = TRUE) else 0,
          .groups = "drop"
        )
      
      best_acc <- df_plot$Accuracy[df_plot$ncomp == best_ncomp]
      
      ggplot(df_plot, aes(x = ncomp, y = Accuracy)) +
        geom_line(color = "#4f46e5", linewidth = 1) +
        geom_point(color = "#111827", size = 2.5) +
        annotate(
          "segment",
          x = best_ncomp, xend = best_ncomp,
          y = 0.993, yend = best_acc,
          linetype = "dotted", color = "red", linewidth = 0.8
        ) +
        annotate(
          "point",
          x = best_ncomp, y = best_acc,
          color = "red", size = 3
        ) +
        annotate(
          "text",
          x = best_ncomp, y = best_acc,
          label = paste0("Best ncomp = ", best_ncomp),
          vjust = -1,
          color = "red",
          size = 3.5
        ) +
        scale_x_continuous(breaks = df_plot$ncomp) +
        coord_cartesian(ylim = c(0.993, 1)) +
        theme_bw(base_size = 13) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
          axis.title = element_text(color = "black"),
          axis.text  = element_text(color = "black")
        ) +
        labs(
          title = "PLS-DA cross-validation accuracy by number of components",
          x = "Number of PLS components (ncomp)",
          y = "Cross-validated accuracy"
        )
      
    } else {
      # ----- RF CV -----
      cv_info <- get_rf_cv(input$preproc_choice)
      validate(
        need(!is.null(cv_info), "RF cross-validation results are not available for this model object.")
      )
      
      df <- cv_info$results
      best_mtry <- cv_info$best_mtry
      
      validate(
        need(all(c("mtry", "Accuracy") %in% names(df)),
             "RF model does not contain mtry and Accuracy information.")
      )
      
      df_plot <- df %>%
        group_by(mtry) %>%
        summarise(
          Accuracy   = mean(Accuracy,   na.rm = TRUE),
          AccuracySD = if ("AccuracySD" %in% names(df))
            mean(AccuracySD, na.rm = TRUE) else 0,
          .groups = "drop"
        )
      
      best_acc <- df_plot$Accuracy[df_plot$mtry == best_mtry]
      
      ggplot(df_plot, aes(x = mtry, y = Accuracy)) +
        geom_line(color = "#4f46e5", linewidth = 1) +
        geom_point(color = "#111827", size = 2.5) +
        annotate(
          "segment",
          x = best_mtry, xend = best_mtry,
          y = 0.993, yend = best_acc,
          linetype = "dotted", color = "red", linewidth = 0.8
        ) +
        annotate(
          "point",
          x = best_mtry, y = best_acc,
          color = "red", size = 3
        ) +
        annotate(
          "text",
          x = best_mtry, y = best_acc,
          label = paste0("Best mtry = ", best_mtry),
          vjust = -1,
          color = "red",
          size = 3.5
        ) +
        scale_x_continuous(breaks = df_plot$mtry) +
        coord_cartesian(ylim = c(0.993, 1)) +
        theme_bw(base_size = 13) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
          axis.title = element_text(color = "black"),
          axis.text  = element_text(color = "black")
        ) +
        labs(
          title = "Random Forest cross-validation accuracy by mtry",
          x = "mtry (number of variables tried at each split)",
          y = "Cross-validated accuracy"
        )
    }
  })
  
  # ========= PCA Loadings (data + plot + top wavelengths) =========
  pca_loadings_data <- reactive({
    pca_obj <- get_pca(input$preproc_choice)
    validate(
      need(!is.null(pca_obj) && !is.null(pca_obj$rotation),
           "PCA loadings are not available for this preprocessing option.")
    )
    req(input$pc_loading)
    
    load_mat <- pca_obj$rotation
    pc_index <- as.integer(input$pc_loading)
    validate(
      need(pc_index >= 1 && pc_index <= ncol(load_mat),
           "Selected principal component is not available.")
    )
    
    load_vec <- load_mat[, pc_index]
    
    # Parse wavelengths from X_column_names; fall back to index if needed
    wl <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", X_column_names)))
    if (length(wl) != length(load_vec) || any(is.na(wl))) {
      wl <- seq_along(load_vec)
      xlab <- "Wavelength index"
    } else {
      xlab <- "Wavelength (nm)"
    }
    
    df <- data.frame(
      Wavelength = wl,
      Loading    = as.numeric(load_vec)
    )
    list(df = df, xlab = xlab, pc_index = pc_index)
  })
  
  output$pca_loadings_plot <- renderPlot({
    ld <- pca_loadings_data()
    df <- ld$df
    xlab <- ld$xlab
    pc_index <- ld$pc_index
    
    # Highlight top 10 by absolute loading
    n_highlight <- min(10, nrow(df))
    top_idx <- order(abs(df$Loading), decreasing = TRUE)[seq_len(n_highlight)]
    df$Top <- FALSE
    df$Top[top_idx] <- TRUE
    
    ggplot(df, aes(x = Wavelength, y = Loading)) +
      geom_line(linewidth = 0.7, color = "#2563eb") +
      geom_point(
        data = df[df$Top, ],
        aes(x = Wavelength, y = Loading),
        color = "red",
        size = 2
      ) +
      theme_bw(base_size = 13) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
        axis.title = element_text(color = "black"),
        axis.text  = element_text(color = "black")
      ) +
      labs(
        title = paste0("PCA loadings for PC", pc_index),
        x = xlab,
        y = "Loading weight"
      )
  })
  
  output$pca_loadings_table <- renderDT({
    ld <- pca_loadings_data()
    df <- ld$df
    # Top 10 by absolute loading
    n_highlight <- min(10, nrow(df))
    top_idx <- order(abs(df$Loading), decreasing = TRUE)[seq_len(n_highlight)]
    df_top <- df[top_idx, , drop = FALSE]
    df_top$Rank <- seq_len(nrow(df_top))
    df_top <- df_top[order(df_top$Rank), ]
    datatable(
      df_top[, c("Rank", "Wavelength", "Loading")],
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # ========= Variable importance =========
  varimp_data <- reactive({
    req(input$model_choice, input$preproc_choice)
    model <- get_model(input$model_choice, input$preproc_choice)
    
    imp <- varImp(model, scale = TRUE)
    df  <- as.data.frame(imp$importance)
    df$Variable <- rownames(df)
    
    if ("Overall" %in% names(df)) {
      df$Importance <- df$Overall
    } else {
      score_cols <- setdiff(names(df), "Variable")
      df$Importance <- apply(df[, score_cols, drop = FALSE], 1, mean, na.rm = TRUE)
    }
    
    # Try to parse wavelength from variable name
    wl <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", df$Variable)))
    if (length(wl) == nrow(df) && !all(is.na(wl))) {
      df$Wavelength_nm <- wl
    } else {
      df$Wavelength_nm <- NA_real_
    }
    
    df <- df[order(-df$Importance), ]
    head(df, 20)
  })
  
  output$varimp_plot <- renderPlot({
    df <- varimp_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = reorder(Variable, Importance),
                   y = Importance,
                   fill = Importance)) +
      geom_col(width = 0.8, color = "black") +
      coord_flip() +
      scale_fill_distiller(palette = "Spectral") +
      theme_bw(base_size = 13) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
        axis.title = element_text(color = "black"),
        axis.text  = element_text(color = "black")
      ) +
      labs(
        x = "Variable",
        y = "Importance",
        title = "Top 20 important variables"
      )
  })
  
  output$varimp_table <- renderDT({
    df <- varimp_data()
    cols <- c("Variable", "Wavelength_nm", "Importance")
    datatable(df[, cols],
              options = list(pageLength = 20, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # ---- diagnostics ----
  output$diagnostics <- renderPrint({
    tryCatch(
      {
        list(
          "Columns in uploaded file"              = names(input_preview()),
          "First few expected spectral variables" = head(X_column_names),
          "Number of preview rows"                = if (is.null(input_preview())) 0 else nrow(input_preview()),
          "Spectra loaded"                        = if (is.null(spectra_raw())) 0 else nrow(spectra_raw())
        )
      },
      error = function(e) {
        list("Diagnostics error" = e$message)
      }
    )
  })
  
  # ---- download predictions ----
  output$download_preds <- downloadHandler(
    filename = function() {
      paste0(input$filename, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(pred_results(), file, row.names = FALSE)
    }
  )
  
  # ---- download bar plot ----
  output$download_bar_plot <- downloadHandler(
    filename = function() {
      paste0("oleasense_prediction_distribution_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(pred_results())
      df <- pred_results()
      
      if (input$bar_type == "percent") {
        df_plot <- df %>%
          count(Prediction) %>%
          mutate(Percent = n / sum(n) * 100)
        
        p <- ggplot(df_plot, aes(x = Prediction, y = Percent, fill = Prediction)) +
          geom_col(width = 0.7, color = "black") +
          theme_bw(base_size = 14) +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
            axis.title = element_text(color = "black"),
            axis.text  = element_text(color = "black")
          ) +
          labs(
            title = "Distribution of predicted EVOO classes (percent)",
            x = "Predicted class",
            y = "Percentage of samples"
          )
      } else {
        p <- ggplot(df, aes(x = Prediction, fill = Prediction)) +
          geom_bar(width = 0.7, color = "black") +
          theme_bw(base_size = 14) +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
            axis.title = element_text(color = "black"),
            axis.text  = element_text(color = "black")
          ) +
          labs(
            title = "Distribution of predicted EVOO classes (counts)",
            x = "Predicted class",
            y = "Number of samples"
          )
      }
      ggsave(file, p, width = 7, height = 5, dpi = 300)
    }
  )
  
  # ---- download PCA scores plot ----
  output$download_pca_scores <- downloadHandler(
    filename = function() {
      paste0("oleasense_pca_scores_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(pca_scores(), pca_predictions())
      df <- as.data.frame(pca_scores())
      df$Prediction <- pca_predictions()
      
      x_pc <- paste0("PC", input$pca_x)
      y_pc <- paste0("PC", input$pca_y)
      
      df_plot <- df
      df_plot$x_val <- df_plot[[x_pc]]
      df_plot$y_val <- df_plot[[y_pc]]
      
      p <- ggplot(df_plot, aes(x = x_val, y = y_val, color = Prediction)) +
        geom_point(size = 3, alpha = 0.9) +
        theme_bw(base_size = 14) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
          axis.title = element_text(color = "black"),
          axis.text  = element_text(color = "black")
        ) +
        labs(
          title = paste("PCA score plot:", x_pc, "vs", y_pc),
          x = x_pc,
          y = y_pc,
          color = "Prediction"
        )
      ggsave(file, p, width = 7, height = 5, dpi = 300)
    }
  )
  
  # ---- download PCA loadings plot ----
  output$download_pca_loadings <- downloadHandler(
    filename = function() {
      paste0("oleasense_pca_loadings_PC", input$pc_loading, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ld <- pca_loadings_data()
      df <- ld$df
      xlab <- ld$xlab
      pc_index <- ld$pc_index
      
      n_highlight <- min(10, nrow(df))
      top_idx <- order(abs(df$Loading), decreasing = TRUE)[seq_len(n_highlight)]
      df$Top <- FALSE
      df$Top[top_idx] <- TRUE
      
      p <- ggplot(df, aes(x = Wavelength, y = Loading)) +
        geom_line(linewidth = 0.7, color = "#2563eb") +
        geom_point(
          data = df[df$Top, ],
          aes(x = Wavelength, y = Loading),
          color = "red",
          size = 2
        ) +
        theme_bw(base_size = 13) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
          axis.title = element_text(color = "black"),
          axis.text  = element_text(color = "black")
        ) +
        labs(
          title = paste0("PCA loadings for PC", pc_index),
          x = xlab,
          y = "Loading weight"
        )
      ggsave(file, p, width = 7, height = 5, dpi = 300)
    }
  )
  
  # ---- download CV plot (PLS-DA or RF) ----
  output$download_pls_cv <- downloadHandler(
    filename = function() {
      model_label <- if (input$model_choice == "rf") "rf" else "pls"
      paste0("oleasense_", model_label, "_cv_", input$preproc_choice, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(input$model_choice %in% c("pls", "rf"))
      
      if (input$model_choice == "pls") {
        cv_info <- get_pls_cv(input$preproc_choice)
        validate(
          need(!is.null(cv_info), "PLS cross-validation results are not available for this model object.")
        )
        
        df <- cv_info$results
        best_ncomp <- cv_info$best_ncomp
        
        df_plot <- df %>%
          group_by(ncomp) %>%
          summarise(
            Accuracy   = mean(Accuracy,   na.rm = TRUE),
            AccuracySD = if ("AccuracySD" %in% names(df))
              mean(AccuracySD, na.rm = TRUE) else 0,
            .groups = "drop"
          )
        
        best_acc <- df_plot$Accuracy[df_plot$ncomp == best_ncomp]
        
        p <- ggplot(df_plot, aes(x = ncomp, y = Accuracy)) +
          geom_line(color = "#4f46e5", linewidth = 1) +
          geom_point(color = "#111827", size = 2.5) +
          annotate(
            "segment",
            x = best_ncomp, xend = best_ncomp,
            y = 0.993, yend = best_acc,
            linetype = "dotted", color = "red", linewidth = 0.8
          ) +
          annotate(
            "point",
            x = best_ncomp, y = best_acc,
            color = "red", size = 3
          ) +
          annotate(
            "text",
            x = best_ncomp, y = best_acc,
            label = paste0("Best ncomp = ", best_ncomp),
            vjust = -1,
            color = "red",
            size = 3.5
          ) +
          scale_x_continuous(breaks = df_plot$ncomp) +
          coord_cartesian(ylim = c(0.993, 1)) +
          theme_bw(base_size = 13) +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
            axis.title = element_text(color = "black"),
            axis.text  = element_text(color = "black")
          ) +
          labs(
            title = "PLS-DA cross-validation accuracy by number of components",
            x = "Number of PLS components (ncomp)",
            y = "Cross-validated accuracy"
          )
      } else {
        cv_info <- get_rf_cv(input$preproc_choice)
        validate(
          need(!is.null(cv_info), "RF cross-validation results are not available for this model object.")
        )
        
        df <- cv_info$results
        best_mtry <- cv_info$best_mtry
        
        df_plot <- df %>%
          group_by(mtry) %>%
          summarise(
            Accuracy   = mean(Accuracy,   na.rm = TRUE),
            AccuracySD = if ("AccuracySD" %in% names(df))
              mean(AccuracySD, na.rm = TRUE) else 0,
            .groups = "drop"
          )
        
        best_acc <- df_plot$Accuracy[df_plot$mtry == best_mtry]
        
        p <- ggplot(df_plot, aes(x = mtry, y = Accuracy)) +
          geom_line(color = "#4f46e5", linewidth = 1) +
          geom_point(color = "#111827", size = 2.5) +
          annotate(
            "segment",
            x = best_mtry, xend = best_mtry,
            y = 0.993, yend = best_acc,
            linetype = "dotted", color = "red", linewidth = 0.8
          ) +
          annotate(
            "point",
            x = best_mtry, y = best_acc,
            color = "red", size = 3
          ) +
          annotate(
            "text",
            x = best_mtry, y = best_acc,
            label = paste0("Best mtry = ", best_mtry),
            vjust = -1,
            color = "red",
            size = 3.5
          ) +
          scale_x_continuous(breaks = df_plot$mtry) +
          coord_cartesian(ylim = c(0.993, 1)) +
          theme_bw(base_size = 13) +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
            axis.title = element_text(color = "black"),
            axis.text  = element_text(color = "black")
          ) +
          labs(
            title = "Random Forest cross-validation accuracy by mtry",
            x = "mtry (number of variables tried at each split)",
            y = "Cross-validated accuracy"
          )
      }
      ggsave(file, p, width = 7, height = 5, dpi = 300)
    }
  )
  
  # ---- sample data (use ALL rows from evoo_true_test_set) ----
  output$download_sample <- downloadHandler(
    filename = function() { "OleaSense_sample_spectral_data.csv" },
    content = function(file) {
      # Start from the true EVOO test set loaded at startup
      df <- evoo_true_test_set
      
      # Keep only numeric spectral columns (as the app uses)
      numeric_cols <- sapply(df, is.numeric)
      spectra_df   <- df[, numeric_cols, drop = FALSE]
      
      # Use all rows (e.g. 24 samples)
      # Ensure column names match training X_column_names where possible
      if (ncol(spectra_df) == length(X_column_names)) {
        colnames(spectra_df) <- X_column_names
      }
      
      # Add a Sample_ID column
      sample_data <- cbind(
        Sample_ID = paste0("Sample", seq_len(nrow(spectra_df))),
        spectra_df
      )
      
      # Write as CSV
      write.csv(sample_data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # ---- feedback logging ----
  observeEvent(input$send_feedback, {
    req(input$user_feedback)
    feedback <- data.frame(
      timestamp = Sys.time(),
      feedback  = input$user_feedback,
      stringsAsFactors = FALSE
    )
    feedback_file <- "oleasense_feedback_log.csv"
    if (file.exists(feedback_file)) {
      write.table(
        feedback,
        file  = feedback_file,
        append = TRUE,
        sep    = ",",
        row.names = FALSE,
        col.names = FALSE
      )
    } else {
      write.table(
        feedback,
        file  = feedback_file,
        append = FALSE,
        sep    = ",",
        row.names = FALSE,
        col.names = TRUE
      )
    }
    showNotification("Thank you. Your feedback has been saved.", type = "message")
  })
  
  # ---- download feedback log (dev only, via UI gating) ----
  output$download_feedback_log <- downloadHandler(
    filename = function() {
      paste0("oleasense_feedback_log_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (file.exists("oleasense_feedback_log.csv")) {
        file.copy("oleasense_feedback_log.csv", file)
      } else {
        write.csv(
          data.frame(
            timestamp = character(0),
            feedback  = character(0)
          ),
          file,
          row.names = FALSE
        )
      }
    }
  )
  
  # ---- show feedback download button only for dev ----
  output$feedback_download_ui <- renderUI({
    req(dev_mode())
    if (!dev_mode()) {
      return(NULL)
    }
    
    tagList(
      br(),
      tags$small("Developer only: Download full feedback log."),
      br(),
      downloadButton("download_feedback_log", "Download feedback log", class = "btn btn-secondary")
    )
  })
  
  # ---- reset app ----
  observeEvent(input$reset_btn, {
    pred_results(NULL)
    input_preview(NULL)
    pca_scores(NULL)
    pca_predictions(NULL)
    spectra_raw(NULL)
    dev_mode(FALSE)
    
    output$prediction_table      <- renderDT(NULL)
    output$class_summary         <- renderPrint(NULL)
    output$results_summary       <- renderDT(NULL)
    output$bar_distribution      <- renderPlot(NULL)
    output$pca_dynamic_plot      <- renderPlot(NULL)
    output$preview_data          <- renderDT(NULL)
    output$pls_cv_plot           <- renderPlot(NULL)
    output$pca_loadings_plot     <- renderPlot(NULL)
    output$pca_loadings_table    <- renderDT(NULL)
    output$varimp_plot           <- renderPlot(NULL)
    output$varimp_table          <- renderDT(NULL)
    output$spectra_plot          <- renderPlot(NULL)
    output$feedback_download_ui  <- renderUI(NULL)
    
    updateFileInput(session, "test_file", NULL)
    updateTextInput(session, "user_feedback", value = "")
    updateTextInput(session, "dev_key", value = "")
  })
}

shinyApp(ui = ui, server = server)
# =========================================================
