#' Create server for the Rapids Shiny App
#'
#' This function sets up the server logic for the RapidsShiny app.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @return None. Sets up Shiny outputs and observers.
#' @export
#'
#' @import shiny
#' @import shinydashboard
#' @import shinyBS
#' @importFrom shinyjs toggle disable enable
#' @importFrom DT renderDT
#' @import ggplot2
#' @import dplyr
#' @importFrom dplyr select
#' @importFrom gridExtra grid.arrange
#' @import rapids
#' @import MASS

create_server <- function(input, output, session) {

  ##### Initial set-up #########################################################

  sim_data <- reactiveVal(NULL)
  sim_data_tte <- reactiveVal(NULL)
  real_data_tte <- reactiveVal(NULL)
  real_data_binary <- reactiveVal(NULL)
  res_cvrs <- reactiveVal(NULL)
  res_cvrs_tte <- reactiveVal(NULL)
  res_cvasd <- reactiveVal(NULL)
  res_real_tte <- reactiveVal(NULL)
  res_real_binary <- reactiveVal(NULL)
  cvasd_running <- reactiveVal(FALSE)
  cvrs_running <- reactiveVal(FALSE)
  cvrs_tte_running <- reactiveVal(FALSE)
  real_tte_running <- reactiveVal(FALSE)
  real_binary_running <- reactiveVal(FALSE)
  duration_cvrs <- reactiveVal(NULL)
  duration_cvrs_tte <- reactiveVal(NULL)
  duration_cvasd <- reactiveVal(NULL)
  duration_real_tte <- reactiveVal(NULL)
  duration_real_binary <- reactiveVal(NULL)
  every_safe_tte <- reactiveVal(NULL)
  every_safe_binary <- reactiveVal(NULL)
  nperm_safe_tte <- reactiveVal(NULL)
  nperm_safe_binary <- reactiveVal(NULL)

  # Toggle Simulated Data panel
  observeEvent(input$toggle_sim, {
    shinyjs::toggle(id = "sim_panel")
    shinyjs::toggleClass(id = "toggle_sim", class = "active")
    shinyjs::toggleClass(id = "toggle_sim", class = "nav-btn") # rotates arrow
  })

  # Toggle Real Data panel
  observeEvent(input$toggle_real, {
    shinyjs::toggle(id = "real_panel")
    shinyjs::toggleClass(id = "toggle_real", class = "active")
  })

  ##### Binary: Simulate  Data ###############################################

  observeEvent(input$simulate, {
    datalist <- rapids::simulate.data(
      N = input$N, K = input$K, L = input$L,
      rho1 = input$rho1, rho2 = input$rho2, rho0 = input$rho0,
      mu1 = input$mu1, mu2 = input$mu2, mu0 = input$mu0, sigma1 = input$sigma1,
      sigma2 = input$sigma2, sigma0 = input$sigma0,
      perc.sp = input$perc.sp,
      rr.sp.treat = input$rr.sp.treat,
      rr.nsp.treat = input$rr.nsp.treat,
      rr.con = input$rr.con,
      seed = input$seed
    )

    # store in a list
    sim_data(list(
      patients = datalist$patients,
      covar = datalist$covar,
      response = datalist$response
    ))

    # display simulated data
    display_sim_data(output = output,
      sim_data_reactive = sim_data,
      patients_table_id = "patients_table",
      patients_header_id = "patients_header",
      covar_table_id = "covar_table",
      covar_header_id = "covar_header",
      download_simdata_ui_id = "download_simdata_ui",
      download_simdata_id = "download_simdata",
      outcome_type = "binary"
    )

    # show analyse cvrs button after simulation
    output$analyse_cvrs_button_ui <- renderUI({
      actionButton("analyse_cvrs", "Analyse with CVRS")
    })

    # show analyse cvasdbutton after simulation
    output$analyse_cvasd_button_ui <- renderUI({
      actionButton("analyse_cvasd", "Analyse with CVASD")
    })
    updateTabsetPanel(session, "sim_tabs", selected = "Data")
  })

  # Show the download button (only when data is available)
  output$download_simdata_ui <- renderUI({
    req(sim_data())
    downloadButton("download_simdata", "Download simulated data")
  })

  output$download_simdata <- downloadHandler(
    filename = function() {
      "simdata.rds" # name of the downloaded file
    },
    content = function(file) {
      saveRDS(sim_data(), file) # save the object for download
    }
  )

  ##### Binary: CVRS for simulated data  ######################################

  observeEvent(input$analyse_cvrs, {
    if (cvrs_running()) {
      return()
    } # stop if already running
    cvrs_running(TRUE)
    shinyjs::disable("analyse_cvrs")
    updateTabsetPanel(session, "sim_tabs", selected = "Results CVRS")

    req(sim_data())
    start_time <- Sys.time()

    withProgress(message = "Analysing...", value = 0, {
      incProgress(0.2, detail = "")
      result <- rapids::analyse.simdata(
        sim_data(),
        method = "cvrs",
        seed = input$seed
      )
      res_cvrs(result)
      incProgress(0.7, detail = "")
    })

    duration_cvrs(round(as.numeric(Sys.time() - start_time, units = "secs"), 2))
    cvrs_running(FALSE)
    shinyjs::enable("analyse_cvrs")
  })

  # Display the results
  display_sim_results(output = output,
    duration_id = "analysis_cvrs_time",
    res_id = "analysis_cvrs_out",
    predicted_sensitivity_id = "cvrs_predicted_sensitivity",
    sens_status_table = "cvrs_table",
    duration_reactive = duration_cvrs,
    res_reactive = res_cvrs
  )

  output$plot_risk_scores <- renderPlot({
    req(res_cvrs())
    plot_risk_scores(
      res_cvrs()$cvrs,
      res_cvrs()$sens.pred,
      res_cvrs()$patients$sens.true
    )
  })

  output$results_cvrs_panel <- renderUI({
    req(res_cvrs())
    tagList(
      wellPanel(h4("Analysis time"), textOutput("analysis_cvrs_time")),
      wellPanel(
        h4("Analysis summary"),
        verbatimTextOutput("analysis_cvrs_out")
      ),
      wellPanel(h4("Plot of risk scores"), plotOutput("plot_risk_scores")),
      wellPanel(
        h4("Sensitivity status count"),
        tableOutput("cvrs_predicted_sensitivity")
      ),
      wellPanel(h4("Sensitivity status list"), DTOutput("cvrs_table")),
      selectInput(
        "download_res_cvrs_format",
        "Download format",
        choices = c("RDS" = "rds", "JSON" = "json")
      ),
      uiOutput("download_res_cvrs_ui")
    )
  })

  # Show the download button (only when data is available)
  output$download_res_cvrs_ui <- renderUI({
    req(res_cvrs())
    downloadButton("download_res_cvrs", "Download CVRS results")
  })

  output$download_res_cvrs <- downloadHandler(
    filename = function() {
      paste0(
        "res_cvrs.",
        input$download_res_cvrs_format
      )
    },
    content = function(file) {
      res <- res_cvrs()  # resolve reactive ONCE
      if (input$download_res_cvrs_format == "rds") {
        saveRDS(res, file)
      } else if (input$download_res_cvrs_format == "json") {
        res <- unclass(res)
        rjson::write_json(res, file, pretty = TRUE)
      }
    }
  )

  ##### Binary: CVASD for simulated data #####################################

  observeEvent(input$analyse_cvasd, {
    if (cvasd_running()) {
      return()
    } # stop if already running
    shinyjs::toggle("extra_cvasd_params")
  })

  # Validate input parameters
  eta_vec <- reactive({
    if (is.null(input$eta) || input$eta == "") {
      return(NULL)
    }
    parts <- trimws(strsplit(input$eta, ",")[[1]])
    is_num <- grepl("^-?\\d*\\.?\\d+$", parts) # numeric
    if (!all(is_num)) {
      return(NA)
    }
    as.numeric(parts)
  })

  r_vec <- reactive({
    if (is.null(input$R) || input$R == "") {
      return(NULL)
    }
    parts <- trimws(strsplit(input$R, ",")[[1]])
    is_num <- grepl("^-?\\d*\\.?\\d+$", parts) # numeric
    if (!all(is_num)) {
      return(NA)
    }
    as.numeric(parts)
  })

  g_vec <- reactive({
    if (is.null(input$G) || input$G == "") {
      return(NULL)
    }
    parts <- trimws(strsplit(input$G, ",")[[1]])
    is_num <- grepl("^-?\\d*\\.?\\d+$", parts) # numeric
    if (!all(is_num)) {
      return(NA)
    }
    as.numeric(parts)
  })

  observeEvent(input$confirm_cvasd, {
    eta <- eta_vec()
    R <- r_vec()
    G <- g_vec()

    if (is.null(eta) || is.null(R) || is.null(G)) {
      showModal(
        modalDialog(
          title = "Input Error",
          "All inputs must have at least one value. None can be empty.",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
      return()
    }

    # Check for NA values (non-numeric entries)
    if (any(is.na(c(eta, R, G)))) {
      showModal(
        modalDialog(
          title = "Input Error",
          "All values must be numeric. Please correct your inputs.",
          type = "error.",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
      return()
    }

    # Check that all vectors have the same length
    if (!length(eta) == length(R) || !length(R) == length(G)) {
      showModal(
        modalDialog(
          title = "Input Error",
          "The input parameter vectors must have the same length.",
          type = "error.",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
      return()
    }

    removeModal()

    cvasd_running(TRUE)
    shinyjs::disable("analyse_cvasd")
    shinyjs::disable("confirm_cvasd")
    updateTabsetPanel(session, "sim_tabs", selected = "Results CVASD")

    start_time <- Sys.time()

    withProgress(message = "Analysing...", value = 0, {
      incProgress(0.2, detail = "")
      result <- rapids::analyse.simdata(
        sim_data(),
        method = "cvasd",
        eta = eta_vec(),
        R = r_vec(),
        G = g_vec(),
        seed = input$seed
      )
      res_cvasd(result)
      incProgress(0.7, detail = "")
    })

    duration_cvasd(round(as.numeric(Sys.time() - start_time, units = "secs"),
                         2))

    cvasd_running(FALSE)
    shinyjs::enable("analyse_cvasd")
    shinyjs::enable("confirm_cvasd")
  })

  # Display the results
  display_sim_results(output,
    duration_id = "analysis_cvasd_time",
    res_id = "analysis_cvasd_out",
    predicted_sensitivity_id = "cvasd_predicted_sensitivity",
    sens_status_table = "cvasd_table",
    duration_reactive = duration_cvasd,
    res_reactive = res_cvasd
  )

  output$results_cvasd_panel <- renderUI({
    req(res_cvasd())
    tagList(
      wellPanel(h4("Analysis time"), textOutput("analysis_cvasd_time")),
      wellPanel(
        h4("Analysis summary"),
        verbatimTextOutput("analysis_cvasd_out")
      ),
      wellPanel(
        h4("Sensitivity status count"),
        tableOutput("cvasd_predicted_sensitivity")
      ),
      wellPanel(h4("Sensitivity status list"), DTOutput("cvasd_table")),
      selectInput(
        "download_res_cvasd_format",
        "Download format",
        choices = c("RDS" = "rds", "JSON" = "json")
      ),
      uiOutput("download_res_cvasd_ui")
    )
  })

  # Show the download button (only when data is available)
  output$download_res_cvasd_ui <- renderUI({
    req(res_cvasd())
    downloadButton("download_res_cvasd", "Download CVASD results")
  })

  output$download_res_cvasd <- downloadHandler(
    filename = function() {
      paste0(
        "res_cvasd.",
        input$download_res_cvasd_format
      )
    },
    content = function(file) {
      res <- res_cvasd()  # resolve reactive ONCE
      if (input$download_res_cvasd_format == "rds") {
        saveRDS(res, file)
      } else if (input$download_res_cvasd_format == "json") {
        res <- unclass(res)
        rjson::write_json(res, file, pretty = TRUE)
      }
    }
  )

  ### TTE: Simulate Data  ######################################################

  observeEvent(input$simulate_tte, {

    datalist <- simulate_data_tte(n_size = input$N_tte,
      n_covar = input$L_tte, n_sens_covar = input$K_tte, rho1 = input$rho1_tte,
      rho2 = input$rho2_tte, rho0 = input$rho0_tte, mu1 = input$mu1_tte,
      mu2 = input$mu2_tte, mu0 = input$mu0_tte, sigma1 = input$sigma1_tte,
      sigma2 = input$sigma2_tte, sigma0 = input$sigma0_tte,
      perc_sp = input$perc.sp_tte, lambda_nsp_treat = input$lambda.nsp.treat,
      lambda_sp_treat = input$lambda.sp.treat, lambda_con = input$lambda.con,
      pcens = input$pcens_tte, seed = input$seed_tte
    )

    sim_data_tte(list(
      patients = datalist$patients,
      covar = datalist$covar,
      event = datalist$event
    ))

    # display simulated data
    display_sim_data(output = output,
      sim_data_reactive = sim_data_tte,
      patients_table_id = "patients_table_tte",
      patients_header_id = "patients_header_tte",
      covar_table_id = "covar_table_tte", covar_header_id = "covar_header_tte",
      download_simdata_ui_id = "download_simdata_ui_tte",
      download_simdata_id = "download_simdata_tte", outcome_type = "tte"
    )

    # show analyse cvrs button after simulation
    output$analyse_cvrs_tte_button_ui <- renderUI({
      actionButton("analyse_cvrs_tte", "Analyse with CVRS", width = "100%")
    })

    updateTabsetPanel(session, "sim_tte_tabs", selected = "Data")

  })

  ##### TTE: CVRS for simulated data###########################################

  observeEvent(input$analyse_cvrs_tte, {

    if (cvrs_tte_running()) {
      return()
    } # stop if already running
    cvrs_tte_running(TRUE)
    shinyjs::disable("analyse_cvrs_tte")
    updateTabsetPanel(session, "sim_tte_tabs", selected = "Results CVRS")

    req(sim_data_tte())
    start_time <- Sys.time()

    withProgress(message = "Analysing...", value = 0, {
      incProgress(0.2, detail = "")

      result <- analyse_simulated(sim_data_tte(),
        seed = input$seed, "tte"
      )

      res_cvrs_tte(result)
      incProgress(0.7, detail = "")
    })

    duration_cvrs_tte(round(as.numeric(Sys.time() - start_time, units = "secs"),
                            2))
    cvrs_tte_running(FALSE)
    shinyjs::enable("analyse_cvrs_tte")
  })

  output$plot_risk_scores_tte <- renderPlot({
    req(res_cvrs_tte())
    plot_risk_scores(
      res_cvrs_tte()$risk_scores,
      res_cvrs_tte()$sens_pred,
      sim_data_tte()$patients$sens.true
    )
  })

  # Display the results
  display_sim_results_tte(output,
    duration_id = "analysis_cvrs_tte_time", res_id = "analysis_cvrs_tte_out",
    predicted_sensitivity_id = "cvrs_predicted_sensitivity_tte",
    sens_status_table = "cvrs_table_tte", duration_reactive = duration_cvrs_tte,
    res_reactive = res_cvrs_tte, sim_data_reactive = sim_data_tte
  )

  output$results_cvrs_panel_tte <- renderUI({
    req(res_cvrs_tte())
    tagList(
      wellPanel(h4("Analysis time"), textOutput("analysis_cvrs_tte_time")),
      wellPanel(h4("Analysis summary"),
                verbatimTextOutput("analysis_cvrs_tte_out")),
      wellPanel(h4("Distribution of the risk scores"),
                plotOutput("plot_risk_scores_tte")),
      wellPanel(
        h4("Sensitivity status count"),
        tableOutput("cvrs_predicted_sensitivity_tte")
      ),
      wellPanel(h4("Sensitivity status list"), DTOutput("cvrs_table_tte")),
      selectInput(
        "download_res_cvrs_format_tte",
        "Download format",
        choices = c("RDS" = "rds", "JSON" = "json")
      ),
      uiOutput("download_res_cvrs_tte_ui")
    )
  })

  # Show the download button (only when data is available)
  output$download_res_cvrs_tte_ui <- renderUI({
    req(res_cvrs_tte())
    downloadButton("download_res_cvrs_tte", "Download CVRS results")
  })

  output$download_res_cvrs_tte <- downloadHandler(
    filename = function() {
      paste0(
        "res_cvrs_tte.",
        input$download_res_cvrs_format_tte
      )
    },
    content = function(file) {
      res <- res_cvrs_tte()  # resolve reactive ONCE
      if (input$download_res_cvrs_format_tte == "rds") {
        saveRDS(res, file)
      } else if (input$download_res_cvrs_format_tte == "json") {
        res <- unclass(res)
        rjson::write_json(res, file, pretty = TRUE)
      }
    }
  )

  ##### Real Data TTE   ########################################################

  real_data_tte <- reactive({
    req(input$realdata_file_tte)

    file <- input$realdata_file_tte$datapath
    ext <- tools::file_ext(input$realdata_file_tte$name)
    if (!ext %in% c("rds", "csv")) {
      showModal(
        modalDialog(
          title = "Input Error",
          "Unsupported file type. Please upload a CSV or RDS file.",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
      return(NULL)  # Stop further processing
    }
    switch(ext, "rds" = {
      obj <- readRDS(file)
      # Validate required elements
      required_elements <- c("patients", "covar", "event")
      missing_elements <- setdiff(required_elements, names(obj))
      if (length(missing_elements) > 0) {
        showModal(modalDialog(title = "Input Error",
          paste("Missing required elements in RDS:",
                paste(missing_elements, collapse = ", ")),
          easyClose = TRUE
        ))
        return(NULL)  # stop further processing
      }
      required_patient_cols <- c("FID", "IID", "treat", "eventdate")
      missing_patient_cols <- setdiff(required_patient_cols,
                                      names(obj$patients))
      if (length(missing_patient_cols) > 0) {
        showModal(modalDialog(title = "Input Error",
          paste("Missing columns in patients data frame:",
                paste(missing_patient_cols, collapse = ", ")),
          easyClose = TRUE
        ))
        return(NULL)
      }
      if (ncol(obj$covar) == 0) {
        showModal(modalDialog(title = "Input Error",
          paste("Missing columns in covar data frame"), easyClose = TRUE
        ))
        return(NULL)
      }
      obj  # return validated RDS object
    },
    "csv" = {
      csv_file = read.csv(file, stringsAsFactors = FALSE)
      required_cols <- c("FID", "IID", "treat", "eventdate", "event")
      missing_cols <- setdiff(required_cols, names(csv_file))
      if (length(missing_cols) > 0) {
        showModal(modalDialog(title = "Input Error",
          paste("Missing required columns in CSV:",
                paste(missing_cols, collapse = ", ")), easyClose = TRUE
        ))
        return(NULL)
      }
      covar_col = csv_file[, setdiff(names(csv_file), c("FID", "IID", "treat",
                                                        "event", "eventdate"))]
      if (length(covar_col) == 0) {
        showModal(modalDialog(title = "Input Error",
          "The CSV file does not contain any covariate columns.",
          easyClose = TRUE
        ))
        return(NULL)
      }
      list(patients = data.frame(IID = csv_file$FID,
                                 FID = csv_file$IID,  treat = csv_file$treat,
                                 eventdate = csv_file$eventdate),
           event = as.vector(csv_file$event),
           covar = covar_col)
    }
    )
  })

  observeEvent(input$realdata_file_tte, {
    output$main_content_tte <- renderUI({})
    updateTabsetPanel(session, "real_output_tabs_tte", selected = "Data")
  })

  display_realdata_input(output,
    patients_real_table = "patients_real_table_tte",
    patients_real_header = "patients_real_header_tte",
    covar_real_table = "covar_real_table_tte",
    covar_real_header = "covar_real_header_tte",
    res_reactive = real_data_tte
  )

  ##### Analise real data TTE with CVRS  #####################################

  output$analyse_cvrs_real_tte_ui <- renderUI({
    req(real_data_tte())
    actionButton("analyse_cvrs_real_tte", "Analyse CVRS")
  })

  observeEvent(input$analyse_cvrs_real_tte, {
    if (real_tte_running()) {
      return()
    } # stop if already running
    shinyjs::toggle("params_real_tte")
  })

  # Run analysis
  observeEvent(input$run_cvrs_real_tte, {

    if (real_tte_running()) {
      return()
    } # stop if already running


    validate_input(input = input, output = output,
      reps_validation_msg_id = "reps_tte_validation_msg",
      nfolds_validation_msg_id = "nfolds_tte_validation_msg",
      nperm_validation_msg_id = "nperm_tte_validation_msg",
      thresh_validation_msg_id = "thresh_tte_validation_msg",
      reps_id = "reps_tte", nfolds_id = "nfolds_tte",
      nperm_id = "nperm_tte", every_id  = "every_tte", thresh_id = "thresh_tte",
      every_safe = every_safe_tte, nperm_safe = nperm_safe_tte
    )

    req(real_data_tte(), input$reps_tte >= 1, input$nfolds_tte >= 2,
        input$thresh_tte >= 0, input$thresh_tte <= 1)
    if (nperm_safe_tte() > 1) {
      req(every_safe_tte() <= nperm_safe_tte())
    }

    real_tte_running(TRUE)
    shinyjs::disable("analyse_cvrs_real_tte")
    shinyjs::disable("run_cvrs_real_tte")
    updateTabsetPanel(session, "real_output_tabs_tte", selected = "Results")

    # Update timestamp
    start_time <- Sys.time()

    output$start_time_real_tte <- renderText({
      paste(
        "Analysis started at:",
        format(start_time, "%Y-%m-%d %H:%M:%S")
      )
    })

    withProgress(message = "Analysing...", value = 0, {

      result <- analyse_real(real_data_tte(), input$nfolds_tte,
        input$reps_tte, input$thresh_tte, nperm_safe_tte(), every_safe_tte(),
        progress_rcv = update_rcv_fun, start_perm = start_perm,
        progress_perm = update_perm_fun, "tte"
      )
      res_real_tte(result)

      duration_real_tte(round(as.numeric(Sys.time() - start_time,
                                         units = "secs"), 2))
      real_tte_running(FALSE)
      shinyjs::enable("analyse_cvrs_real_tte")
      shinyjs::enable("run_cvrs_real_tte")

      showModal(
        modalDialog(
          title = "Analysis Complete",
          "Your CVRS analysis has successfully finished.",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )

      # Update timestamp
      output$end_time_real_tte <- renderText({
        paste(
          "Analysis completed at:",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      })
    }) #end withProgress
  }) # end observer event

  # predicted sensitivity status (count)
  output$sens_count_real_tte <- renderTable({
    req(res_real_tte())
    df <- as.data.frame(table(res_real_tte()$sens_index))
    colnames(df) <- c("Predicted sensitivity status", "Count")
    df
  })

  output$summary_real_tte <- renderUI({
    req(duration_real_tte())
    HTML(paste0(
      "Analysis completed in ", duration_real_tte(), " seconds.<br>",
      "Number of successful cross-validation repeats: ",
      res_real_tte()$reps_ok,
      "<br>Number of successful permutations: ",
      res_real_tte()$num_perm
    ))
  })

  output$mod_coeff_real_tte <- renderTable({
    req(res_real_tte())
    df <- res_real_tte()$mod_coeff
    df <- cbind(" " = rownames(df), df) # add row names as their own column
    df
  }, rownames = FALSE)

  # Permutation p-value
  output$pval_perm_tte <- renderText({
    req(res_real_tte()$pval_perm)
    paste("Permutation p-value for interaction effect: ",
          signif(res_real_tte()$pval_perm, 4), sep = "")
  })

  # Plot risk scores
  output$plot_risk_scores_real_tte <- renderPlot({
    req(res_real_tte())
    plot_risk_scores_real(res_real_tte()$risk_scores,
      real_data_tte()$patients$treat
    )
  })

  # Plot probability of sensitive group
  output$plot_prob_sensitive_real_tte <- renderPlot({
    req(res_real_tte()$sens_rcv)
    plot_prob_sensitive(res_real_tte()$sens_rcv)
  })

  # Survival plots
  output$plots_surv_per_group_real <- renderPlot({
    req(res_real_tte(), input$thresh_tte >= 0, input$thresh_tte <= 1)
    plots_surv_per_group(real_data_tte(),
      res_real_tte()$sens_group, input$thresh_tte
    )
  })

  # Display predicted sensitivity status
  display_pred_sens(output = output,
    sens_count_id = "sens_count_real_tte",
    sens_status_table_id = "pred_sens_real_tte",
    data_reactive = real_data_tte, res_reactive = res_real_tte
  )

  # Output panel
  output$results_real_panel_tte <- renderUI({
    req(res_real_tte())
    tagList(
      wellPanel(h4("Analysis summary"), textOutput("start_time_real_tte"),
                textOutput("end_time_real_tte"), uiOutput("summary_real_tte")),
      wellPanel(
        h4("Effect of the sensitive group"),
        tableOutput("mod_coeff_real_tte"), textOutput("pval_perm_tte")
      ),
      wellPanel(h4("Distribution of the risk scores"),
                plotOutput("plot_risk_scores_real_tte")),
      wellPanel(h4("Probability of belonging to sensitive group"),
                plotOutput("plot_prob_sensitive_real_tte")),
      wellPanel(h4("Kaplan-Meier plots"),
                plotOutput("plots_surv_per_group_real")),
      wellPanel(
        h4("Sensitivity status count"),
        tableOutput("sens_count_real_tte")
      ),
      wellPanel(h4("Sensitivity status list"), DTOutput("pred_sens_real_tte")),
      selectInput(
        "download_res_real_tte_format",
        "Download format",
        choices = c("RDS" = "rds", "JSON" = "json")
      ),
      uiOutput("download_res_real_tte_ui")
    )
  })

  # Show the download button (only when data is available)
  output$download_res_real_tte_ui <- renderUI({
    req(res_real_tte())
    downloadButton("download_res_real_tte", "Download results")
  })

  output$download_res_real_tte <- downloadHandler(
    filename = function() {
      paste0(
        "res_real_tte.",
        input$download_res_real_tte_format
      )
    },
    content = function(file) {
      res <- res_real_tte()  # resolve reactive ONCE
      if (input$download_res_real_tte_format == "rds") {
        saveRDS(res, file)
      } else if (input$download_res_real_tte_format == "json") {
        rjson::write_json(res, file, pretty = TRUE)
      }
    }
  )

  ##### Real Data Binary   ####################################################

  # Read in input file
  real_data_binary <- reactive({
    req(input$realdata_file_binary)

    file <- input$realdata_file_binary$datapath
    ext <- tools::file_ext(input$realdata_file_binary$name)
    if (!ext %in% c("rds", "csv")) {
      showModal(
        modalDialog(
          title = "Input Error",
          "Unsupported file type. Please upload a CSV or RDS file.",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
      return(NULL)  # Stop further processing
    }
    switch(ext, {
      obj <- readRDS(file)
      # Validate required elements
      required_elements <- c("patients", "covar", "response")
      missing_elements <- setdiff(required_elements, names(obj))
      if (length(missing_elements) > 0) {
        showModal(modalDialog(title = "Input Error",
          paste("Missing required elements in RDS:",
                paste(missing_elements, collapse = ", ")),
          easyClose = TRUE
        ))
        return(NULL)  # stop further processing
      }
      required_patient_cols <- c("FID", "IID", "treat")
      missing_patient_cols <- setdiff(required_patient_cols,
                                      names(obj$patients))
      if (length(missing_patient_cols) > 0) {
        showModal(modalDialog(title = "Input Error",
          paste("Missing columns in patients data frame:",
                paste(missing_patient_cols, collapse = ", ")),
          easyClose = TRUE
        ))
        return(NULL)
      }
      if (ncol(obj$covar) == 0) {
        showModal(modalDialog(title = "Input Error",
          paste("Missing columns in covar data frame"), easyClose = TRUE
        ))
        return(NULL)
      }
      obj
    },
    "csv" = {
      csv_file = read.csv(file, stringsAsFactors = FALSE)
      required_cols <- c("FID", "IID", "treat", "response")
      missing_cols <- setdiff(required_cols, names(csv_file))
      if (length(missing_cols) > 0) {
        showModal(modalDialog(title = "Input Error",
                              paste("Missing required columns in CSV:",
                                    paste(missing_cols, collapse = ", ")),
                              easyClose = TRUE)
        )
        return(NULL)
      }
      covar_col = csv_file[, setdiff(names(csv_file),
                                     c("FID", "IID", "treat", "response"))]
      if (length(covar_col) == 0) {
        showModal(modalDialog(title = "Input Error",
                              "The CSV file does not contain
                              any covariate columns.",
                              easyClose = TRUE)
        )
        return(NULL)
      }
      list(patients = data.frame(IID = csv_file$FID, FID = csv_file$IID,
                                 treat = csv_file$treat),
           response = as.vector(csv_file$response), covar = covar_col)
    }
    )
  })

  observeEvent(input$realdata_file_binary, {
    output$main_content_binary <- renderUI({})
    updateTabsetPanel(session, "real_output_tabs_binary", selected = "Data")
  })

  display_realdata_input(output,
    patients_real_table = "patients_real_table_binary",
    patients_real_header = "patients_real_header_binary",
    covar_real_table = "covar_real_table_binary",
    covar_real_header = "covar_real_header_binary",
    res_reactive = real_data_binary
  )

  output$analyse_cvrs_real_binary_ui <- renderUI({
    req(real_data_binary())
    actionButton("analyse_cvrs_real_binary", "Analyse CVRS")
  })

  ##### Analise real data Binary with CVRS  ###################################

  observeEvent(input$analyse_cvrs_real_binary, {
    if (real_binary_running()) {
      return()
    } # stop if already running
    shinyjs::toggle("params_real_binary")
  })


  # Run analysis
  observeEvent(input$run_cvrs_real_binary, {
    validate_input(input = input, output = output,
      reps_validation_msg_id = "reps_binary_validation_msg",
      nfolds_validation_msg_id = "nfolds_binary_validation_msg",
      nperm_validation_msg_id = "nperm_binary_validation_msg",
      thresh_validation_msg_id = "thresh_binary_validation_msg",
      reps_id = "reps_binary", nfolds_id = "nfolds_binary",
      nperm_id = "nperm_binary", every_id  = "every_binary",
      thresh_id = "thresh_binary", every_safe = every_safe_binary,
      nperm_safe = nperm_safe_binary
    )

    req(real_data_binary(), input$reps_binary >= 1, input$nfolds_binary >= 2,
        input$thresh_binary >= 0, input$thresh_binary <= 1)
    if (nperm_safe_binary() > 1) {
      req(every_safe_binary() <= nperm_safe_binary())
    }

    if (real_binary_running()) {
      return()
    } # stop if already running
    real_binary_running(TRUE)
    shinyjs::disable("analyse_cvrs_real_binary")
    shinyjs::disable("run_cvrs_real_binary")
    updateTabsetPanel(session, "real_output_tabs_binary", selected = "Results")

    # Update timestamp
    start_time <- Sys.time()

    output$start_time_real_binary <- renderText({
      paste(
        "Analysis started at:",
        format(start_time, "%Y-%m-%d %H:%M:%S")
      )
    })

    withProgress(message = "Analysing...", value = 0, {

      result <- analyse_real(real_data_binary(),
        input$nfolds_binary, input$reps_binary, input$thresh_binary,
        nperm_safe_binary(), every_safe_binary(), progress_rcv = update_rcv_fun,
        start_perm = start_perm, progress_perm = update_perm_fun, "binary"
      )
      res_real_binary(result)

      duration_real_binary(round(as.numeric(Sys.time() - start_time,
                                            units = "secs"), 2))

      real_binary_running(FALSE)
      shinyjs::enable("analyse_cvrs_real_binary")
      shinyjs::enable("run_cvrs_real_binary")

      showModal(
        modalDialog(
          title = "Analysis Complete",
          "Your CVRS analysis has successfully finished.", easyClose = TRUE,
          footer = modalButton("Close")
        )
      )

      # Update timestamp
      output$end_time_real_binary <- renderText({
        paste(
          "Analysis completed at:",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      })
    }) #end withProgress
  }) # end observer event

  # Display predicted sensitivity status
  display_pred_sens(output = output,
    sens_count_id = "sens_count_real_binary",
    sens_status_table_id = "pred_sens_real_binary",
    data_reactive = real_data_binary, res_reactive = res_real_binary
  )

  # Display analysis Summary
  output$summary_real_binary <- renderUI({
    req(duration_real_binary())
    HTML(paste0(
      "Analysis completed in ", duration_real_tte(), " seconds.<br>",
      "Number of successful cross-validation repeats: ",
      res_real_binary()$reps_ok,
      "<br>Number of successful permutations: ",
      res_real_binary()$num_perm
    ))
  })

  # Interaction effect
  output$mod_coeff_real_binary <- renderTable({
    req(res_real_binary())
    df <- res_real_binary()$mod_coeff
    df <- cbind(" " = rownames(df), df) # add row names as their own column
    df
  }, rownames = FALSE)

  # Permutation p-value
  output$pval_perm_binary <- renderText({
    req(res_real_binary()$pval_perm)
    paste("Permutation p-value for interaction effect: ",
          signif(res_real_binary()$pval_perm, 4), sep = "")
  })

  # Plot risk scores
  output$plot_risk_scores_real_binary <- renderPlot({
    req(res_real_binary())
    plot_risk_scores_real(res_real_binary()$risk_scores,
      real_data_binary()$patients$treat
    )
  })

  # Plot probability of sensitive group
  output$plot_prob_sensitive_real_binary <- renderPlot({
    req(res_real_binary()$sens_rcv)
    plot_prob_sensitive(res_real_binary()$sens_rcv)
  })

  # Output panel
  output$results_real_panel_binary <- renderUI({
    req(res_real_binary())
    tagList(
      wellPanel(h4("Analysis summary"), textOutput("start_time_real_binary"),
                textOutput("end_time_real_binary"),
                uiOutput("summary_real_binary")),
      wellPanel(
        h4("Effect of the sensitive group"),
        tableOutput("mod_coeff_real_binary"), textOutput("pval_perm_binary")
      ),
      wellPanel(h4("Distribution of the risk scores"),
                plotOutput("plot_risk_scores_real_binary")),
      wellPanel(h4("Probability of belonging to sensitive group"),
                plotOutput("plot_prob_sensitive_real_binary")),
      wellPanel(
        h4("Sensitivity status count"),
        tableOutput("sens_count_real_binary")
      ),
      wellPanel(h4("Sensitivity status list"),
                DTOutput("pred_sens_real_binary")),
      selectInput(
        "download_res_real_binary_format",
        "Download format",
        choices = c("RDS" = "rds", "JSON" = "json")
      ),
      uiOutput("download_res_real_binary_ui")
    )
  })

  # Show the download button (only when data is available)
  output$download_res_real_binary_ui <- renderUI({
    req(res_real_binary())
    downloadButton("download_res_real_binary", "Download results")
  })

  output$download_res_real_binary <- downloadHandler(

    filename = function() {
      paste0(
        "res_real_binary.",
        input$download_res_real_binary_format
      )
    },
    content = function(file) {
      res <- res_real_binary()  # resolve reactive ONCE
      if (input$download_res_real_binary_format == "rds") {
        saveRDS(res, file)
      } else if (input$download_res_real_binary_format == "json") {
        rjson::write_json(res, file, pretty = TRUE)
      }
    }
  )
}
