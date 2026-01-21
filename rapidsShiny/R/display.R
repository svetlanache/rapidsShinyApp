#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Display simulated Data
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

validate_input <- function(output, input,
                           reps_validation_msg_id,
                           nfolds_validation_msg_id,
                           nperm_validation_msg_id,
                           thresh_validation_msg_id,
                           reps_id, nfolds_id, nperm_id, every_id, thresh_id,
                           every_safe, nperm_safe) {

  if (is.null(input[[nperm_id]]) || is.na(input[[nperm_id]]) ||
        input[[nperm_id]] < 0) {
    nperm_safe(0)
  } else {
    nperm_safe(input[[nperm_id]])
  }

  if (is.null(input[[every_id]]) || is.na(input[[every_id]]) ||
        input[[every_id]] < 0) {
    every_safe(0)
  } else {
    every_safe(input[[every_id]])
  }

  # Validate input parameters
  output[[reps_validation_msg_id]] <- renderUI({
    if (is.null(input[[reps_id]]) || is.na(input[[reps_id]]) ||
          input[[reps_id]] < 1) {
      tags$div(style = "color:red;", "Please enter a number of repeats ≥ 1.")
    } else {
      NULL
    }
  })

  output[[nfolds_validation_msg_id]] <- renderUI({
    if (is.null(input[[nfolds_id]]) || is.na(input[[nfolds_id]]) ||
          input[[nfolds_id]] < 2) {
      tags$div(style = "color:red;", "Please enter a number of folds ≥ 2.")
    } else {
      NULL
    }
  })


  observeEvent(input[[nperm_id]], {
    if (is.null(input[[nperm_id]]) ||
          is.na(input[[nperm_id]]) ||
          input[[nperm_id]] < 0) {
      nperm_safe(0)
    } else {
      nperm_safe(input[[nperm_id]])
    }
  }, ignoreInit = FALSE)

  output[[nperm_validation_msg_id]] <- renderUI({
    req(input[[every_id]], input[[nperm_id]] > 1)
    if (input[[every_id]] > input[[nperm_id]]) {
      tags$div(style = "color:red;", "The number of permutations for updating
      the progress display cannot exceed
      the total number of permutations.")
    } else {
      NULL
    }
  })

  output[[thresh_validation_msg_id]] <- renderUI({
    if ((is.null(input[[thresh_id]])) || (is.na(input[[thresh_id]])) ||
          (input[[thresh_id]] < 0) || (input[[thresh_id]] > 1)) {
      tags$div(style = "color:red;", "Please enter a threshold betwee 0 and 1.")
    } else {
      NULL
    }
  })



}
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Display simulated Data
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

display_sim_data <- function(output,
                             sim_data_reactive,
                             patients_table_id,
                             patients_header_id,
                             covar_table_id,
                             covar_header_id,
                             download_simdata_ui_id,
                             download_simdata_id,
                             outcome_type) {

  patients_display <- reactive({
    req(sim_data_reactive()$patients)
    df <- sim_data_reactive()$patients
    if (outcome_type == "binary") {
      df$Response <- sim_data_reactive()$response
    } else if (outcome_type == "tte") {
      df$Event <- sim_data_reactive()$event
    }
    if ("resp1" %in% names(df)) {
      names(df)[names(df) == "resp1"] <- "Response"
    }
    if ("eventdate" %in% names(df)) {
      names(df)[names(df) == "eventdate"] <- "Event date"
    }
    if ("hazard" %in% names(df)) {
      names(df)[names(df) == "hazard"] <- "Hazard"
    }
    if ("rr" %in% names(df)) {
      df$rr <- round(as.numeric(df$rr), 2)
      names(df)[names(df) == "rr"] <- "Prob. of response"
    }
    if ("sens.true" %in% names(df)) {
      names(df)[names(df) == "sens.true"] <- "Sensitivity"
    }
    if ("treat" %in% names(df)) {
      names(df)[names(df) == "treat"] <- "Treatment"
    }
    df
  })

  output[[patients_table_id]] <- DT::renderDT({
    datatable(
      patients_display(),
      options = list(
        pageLength = 50,
        # number of rows per page
        scrollY = "300px",
        # vertical scroll
        scrollX = TRUE,
        # horizontal scroll
        scrollCollapse = TRUE,
        paging = TRUE
      )
    )
  })

  output[[patients_header_id]] <- renderUI({
    req(sim_data_reactive()$patients) # only show after data is available
    h4("Patients' data")
  })

  covar_display <- reactive({
    req(sim_data_reactive()$covar)
    df <- round(sim_data_reactive()$covar, 3)
    df
  })

  output[[covar_table_id]] <- DT::renderDT({
    datatable(
      covar_display(),
      options = list(
        pageLength = 50,
        # number of rows per page
        scrollY = "300px",
        # vertical scroll
        scrollX = TRUE,
        # horizontal scroll
        scrollCollapse = TRUE,
        paging = TRUE
      )
    )
  })

  output[[covar_header_id]] <- renderUI({
    req(sim_data_reactive()$covar) # only show after data is available
    h4("Covariates' data")
  })

  # Show the download button (only when data is available)
  output[[download_simdata_ui_id]] <- renderUI({
    req(sim_data_reactive())
    downloadButton("download_simdata", "Download simulated data")
  })

  output[[download_simdata_id]] <- downloadHandler(
    filename = function() {
      "simdata.rds" # name of the downloaded file
    },
    content = function(file) {
      saveRDS(sim_data_reactive(), file) # save the object for download
    }
  )
}
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Display simulation results for binary outcome
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

display_sim_results <- function(output,
                                duration_id,
                                res_id,
                                predicted_sensitivity_id,
                                sens_status_table_id,
                                duration_reactive,
                                res_reactive) {
  # analysis time
  output[[duration_id]] <- renderText({
    req(duration_reactive())
    paste("Analysis completed in", duration_reactive(), "seconds.")
  })

  # display results
  output[[res_id]] <- renderPrint({
    req(res_reactive())
    res_reactive()
  })

  # predicted sensitivity status (count)
  output[[predicted_sensitivity_id]] <- renderTable({
    req(res_reactive())
    df <- as.data.frame(table(
      res_reactive()$patients$sens.true,
      as.numeric(res_reactive()$sens.pred)
    ))
    names(df) <- c(
      "True sensitivity status",
      "Predicted sensitivity status",
      "Count"
    )
    df
  })

  # individual sensitivity status
  sens_status <- reactive({
    req(res_reactive())
    df <- data.frame(
      res_reactive()$patients$FID,
      res_reactive()$patients$IID,
      as.numeric(res_reactive()$sens.pred)
    )
    names(df) <- c("FID", "IID", "Predicted sensitivity")
    df
  })

  output[[sens_status_table_id]] <- DT::renderDT({
    datatable(
      sens_status(),
      options = list(
        pageLength = 50, # number of rows per page
        scrollY = "300px", # vertical scroll
        scrollX = TRUE, # horizontal scroll
        scrollCollapse = TRUE,
        paging = TRUE
      )
    )
  })
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Display simulation results for TTE outcome
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

display_sim_results_tte <- function(output,
                                    duration_id,
                                    res_id,
                                    predicted_sensitivity_id,
                                    sens_status_table_id,
                                    duration_reactive,
                                    res_reactive,
                                    sim_data_reactive) {
  # analysis time
  output[[duration_id]] <- renderText({
    req(duration_reactive())
    paste("Analysis completed in", duration_reactive(), "seconds.")
  })

  # display results
  output[[res_id]] <- renderPrint({
    req(res_reactive())
    cat(res_reactive()$toprint)
  })

  # predicted sensitivity status (count)
  output[[predicted_sensitivity_id]] <- renderTable({
    req(res_reactive())
    df <- as.data.frame(table(
      sim_data_reactive()$patients$sens.true,
      as.numeric(res_reactive()$sens_pred)
    ))
    names(df) <- c(
      "True sensitivity status",
      "Predicted sensitivity status",
      "Count"
    )
    df
  })

  # individual sensitivity status
  sens_status <- reactive({
    req(res_reactive())
    df <- data.frame(
      sim_data_reactive()$patients$FID,
      sim_data_reactive()$patients$IID,
      as.numeric(res_reactive()$sens_pred)
    )
    names(df) <- c("FID", "IID", "Predicted sensitivity")
    df
  })

  output[[sens_status_table_id]] <- DT::renderDT({
    datatable(
      sens_status(),
      options = list(
        pageLength = 50, # number of rows per page
        scrollY = "300px", # vertical scroll
        scrollX = TRUE, # horizontal scroll
        scrollCollapse = TRUE,
        paging = TRUE
      )
    )
  })
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Predicted sensitivity status
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

display_pred_sens <- function(output,
                              sens_count_id,
                              sens_status_table_id,
                              data_reactive,
                              res_reactive) {

  output[[sens_count_id]] <- renderTable({
    req(res_reactive())
    df <- as.data.frame(table(res_reactive()$sens_index))
    colnames(df) <- c("Predicted sensitivity status", "Count")
    df
  })


  sens_status <- reactive({
    req(res_reactive())
    df <- data.frame(
      data_reactive()$patients$FID,
      data_reactive()$patients$IID,
      as.numeric(res_reactive()$sens_index)
    )
    names(df) <- c("FID", "IID", "Predicted sensitivity")
    df
  })

  output[[sens_status_table_id]] <- DT::renderDT({
    datatable(
      sens_status(),
      options = list(
        pageLength = 50, # number of rows per page
        scrollY = "300px", # vertical scroll
        scrollX = TRUE, # horizontal scroll
        scrollCollapse = TRUE,
        paging = TRUE
      )
    )
  })

}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Display Real Data Input ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

display_realdata_input  <- function(output,
                                    patients_real_table,
                                    patients_real_header,
                                    covar_real_table,
                                    covar_real_header,
                                    res_reactive) {


  patients_real_display <- reactive({
    req(res_reactive()$patients)
    if ("event" %in% names(res_reactive())) {
      df <- cbind(res_reactive()$patients, as.data.frame(res_reactive()$event))
      names(df) [ncol(df)] <- "Event"
    }
    if ("response" %in% names(res_reactive())) {
      df <- cbind(res_reactive()$patients,
                  as.data.frame(res_reactive()$response))
      names(df) [ncol(df)] <- "Response"
    }
    if ("eventdate" %in% names(df)) {
      names(df)[names(df) == "eventdate"] <- "Event Date"
    }
    if ("sens.true" %in% names(df)) {
      names(df)[names(df) == "sens.true"] <- "Sensitivity"
    }
    if ("treat" %in% names(df)) {
      names(df)[names(df) == "treat"] <- "Treatment"
    }
    df
  })

  output[[patients_real_table]] <- DT::renderDT({
    datatable(
      patients_real_display(),
      options = list(
        pageLength = 50,
        # number of rows per page
        scrollY = "300px",
        # vertical scroll
        scrollX = TRUE,
        # horizontal scroll
        scrollCollapse = TRUE,
        paging = TRUE
      )
    )
  })

  output[[patients_real_header]] <- renderUI({
    req(res_reactive()$patients) # only show after data is available
    h4("Patients' data")
  })

  covar_real_display <- reactive({
    req(res_reactive()$covar)
    df <- round(res_reactive()$covar, 3)
    df
  })

  output[[covar_real_table]] <- DT::renderDT({
    datatable(
      covar_real_display(),
      options = list(
        pageLength = 50,
        # number of rows per page
        scrollY = "300px",
        # vertical scroll
        scrollX = TRUE,
        # horizontal scroll
        scrollCollapse = TRUE,
        paging = TRUE
      )
    )
  })

  output[[covar_real_header]] <- renderUI({
    req(res_reactive()$covar) # only show after data is available
    h4("Covariates' data")
  })
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Functions for progress update ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

update_rcv_fun <- function(i, total) {
  incProgress((0.2 / total),
    detail = paste("Repeated cross-validation ", i, "of", total)
  )
}

start_perm <- function() {
  setProgress(detail = "Performing permutations...")
}


update_perm_fun <- function(i, total, every) {
  if (i %% every == 0 || i == total) {
    incProgress((0.8 / (total / every)),
      detail = paste("Permutation", i, "of", total)
    )
  }
}
