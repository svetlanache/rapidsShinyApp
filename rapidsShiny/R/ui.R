#' Create UI for the Rapids Shiny App
#'
#' This function generates the dashboardPage UI for the RapidsShiny app.
#' @return A shiny dashboardPage object
#' @export

#' @import shiny
#' @import shinydashboard
#' @import shinyBS
#' @importFrom DT DTOutput datatable
#' @importFrom shinyjs useShinyjs hidden

create_ui <- function() {
  dashboardPage(
    # Dashboard: Header --------------------------------------------------------
    dashboardHeader(title = "RAPIDS"),
    # Dashboard: Sidebar -------------------------------------------------------
    dashboardSidebar(
      useShinyjs(),
      sidebarMenu(id = "tabs",
        selected = "home_page",
        menuItem("Home",
          tabName = "home_page", icon = icon("home"), startExpanded = FALSE
        ),
        menuItem("Simulated Data",
          tabName = "sim_data", icon = icon("database"), startExpanded = FALSE,
          menuSubItem("Binary outcome", tabName = "sim_binary"),
          menuSubItem("Time-to-event outcome", tabName = "sim_tte")
        ),
        menuItem("Real Data",
          tabName = "real_data", icon = icon("database"), startExpanded = FALSE,
          menuSubItem("Binary outcome", tabName = "real_binary"),
          menuSubItem("Time-to-event outcome", tabName = "real_tte")
        )
      ),
      tags$hr(), # horizontal line
      tags$div(
        style = "padding: 10px; font-weight: bold;", icon("github"),
        tags$a("Source Code", href = "https://github.com/svetlanache/rapidsShinyApp",
               target = "_blank"),
        tags$sup(icon("external-link-alt"))
      )
    ),
    # Dashboard: Body ----------------------------------------------------------
    dashboardBody(
      tabItems(
        # Tab: Home ------------------------------------------------------------
        tabItem(tabName = "home_page",
          fluidRow(box(
            title = "RAPIDS: Risk-Score Adaptive Design",
            status = "primary", solidHeader = TRUE, width = 12,
            p("This",
              tags$a(href = "https://www.r-project.org/",
                target = "_blank",
                "R",
                tags$sup(icon("external-link-alt"))
              ),
              tags$a(href = "https://shiny.posit.co/",
                target = "_blank",
                "Shiny",
                tags$sup(icon("external-link-alt"))
              ), "application implements the Risk-Score Adaptive Design (RAPIDS)
              for randomised clinical trials."),
            p("Use the navigation panel on the left to simulate data or to
                upload your own dataset for subsiquent analysis."),
            p("For both simulated and real data, the application supports
                the Cross-Validation Risk-Score (CVRS) method of",
              tags$a(href = "https://onlinelibrary.wiley.com/doi/full/10.1002/
                       sim.8665?msockid=29a83da939df6dc901e12b0138b46c68",
                target = "_blank", "Cherlin and Wason (2020)",
                tags$sup(icon("external-link-alt"))
              ), "implemented for binary and time-to-event outcomes.
                For simulated data with a binary outcome, the Cross-Validated
                Adaptive Signature Design (CVASD) method of ",
              tags$a(href = "https://aacrjournals.org/clincancerres/
                       article/16/2/691/75866/
                       The-Cross-Validated-Adaptive-Signature-DesignCross",
                target = "_blank", "Freidlin and Simon (2010)",
                tags$sup(icon("external-link-alt"))
              ), "is also implemented.")
          )) #end box/fluidrow
        ), #end tabItem home_page
        # Tab: Simulated Data, TTE ---------------------------------------------
        tabItem(
          tabName = "sim_tte",
          tabItem(
            # Inner Tab: Input Simulated Data, TTE -----------------------------
            tabName = "sim_tte_input",
            fluidRow(box(
              title = "Input Parameters", status = "primary",
              solidHeader = TRUE, width = 12,
              fluidRow(
                column(width = 6,
                  numericInput("N_tte", "Sample size:", 400, min = 50,
                               max = 2000),
                  numericInput("L_tte", "Number of covariates:", 100,
                               min = 1),
                  numericInput("perc.sp_tte",
                               "Prevalence of sensitive group:", 0.1, min = 0,
                               max = 1, step = 0.05)
                ),
                column(width = 6,
                  numericInput("K_tte", "Number of sensitive covariates:",
                               10, min = 1),
                  numericInput("pcens_tte", "Censoring rate", value = 0.2),
                  numericInput("seed_tte", "Random seed:", 123, min = 1),
                )
              ),
              bsCollapse(
                id = "collapse_inputs",
                bsCollapsePanel("Hazard", fluidRow(
                  column(width = 4, numericInput("lambda.sp.treat",
                                                 "Sensitive group", 0.05)),
                  column(width = 4, numericInput("lambda.nsp.treat",
                                                 "Non-sensitive group", 0.1)),
                  column(width = 4, numericInput("lambda.con", "Control
                                                 (both groups)", 0.1))
                ), style = "info"),
                bsCollapsePanel("Sensitive Covariates", fluidRow(
                  column(width = 6,
                    wellPanel(h4("Sensitive group"),
                      numericInput("mu1_tte", "Mean", 1),
                      numericInput("sigma1_tte", "Standard Deviation", 0.5,
                                   min = 0.001),
                      numericInput("rho1_tte", "Correlation", 0, min = 0,
                                   max = 1)
                    )
                  ), column(width = 6,
                    wellPanel(h4("Non-sensitive group"),
                      numericInput("mu2_tte", "Mean", 0),
                      numericInput("sigma2_tte", "Standard Deviation", 0.1,
                                   min = 0.001),
                      numericInput("rho2_tte", "Correlation", 0, min = 0,
                                   max = 1)
                    )
                  )
                ), style = "info"),
                bsCollapsePanel(
                  "Non-Sensitive Covariates", fluidRow(
                    column(width = 4, numericInput("mu0_tte", "Mean", 0)),
                    column(width = 4, numericInput("sigma0_tte",
                                                   "Standard Deviation", 0.5,
                                                   min = 0.001)),
                    column(width = 4, numericInput("rho0_tte", "Correlation",
                                                   0, min = 0, max = 1))
                  ), style = "info"
                )
              )
            )), #end box/fluidRow
            fluidRow(
              column(width = 4, align = "center",
                     actionButton("simulate_tte", "Simulate Data",
                                  width = "100%")),
              column(width = 4, align = "center",
                     uiOutput("analyse_cvrs_tte_button_ui"))
            )
          ),
          # Inner Tab: Output Simulated Data, TTE ------------------------------
          tabItem(
            tabName = "sim_tte_output",
            fluidRow(box(
              title = "Output", status = "primary", solidHeader = TRUE,
              width = 12,
              tabsetPanel(id = "sim_tte_tabs",
                tabPanel("Data",
                  uiOutput("patients_header_tte"),
                  DTOutput("patients_table_tte"),
                  uiOutput("covar_header_tte"),
                  DTOutput("covar_table_tte"),
                  uiOutput("download_simdata_ui_tte")
                ),
                tabPanel("Results CVRS", uiOutput("results_cvrs_panel_tte"))
              )
            ))
          )
        ), #end tabItem
        # Tab: Simulated Data, Binary ------------------------------------------
        tabItem(
          tabName = "sim_binary",
          # Input parameters ---------------------------------------------------
          fluidRow(box(
            title = "Input Parameters", status = "primary", solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(width = 6,
                numericInput("N", "Sample size:", 400, min = 50, max = 2000),
                numericInput("perc.sp", "Prevalence of sensitive group:", 0.1,
                             min = 0, max = 1, step = 0.01),
                numericInput("seed", "Random seed:", 123, min = 1)
              ),
              column(width = 6,
                numericInput("L", "Number of covariates:", 100, min = 1),
                numericInput("K", "Number of sensitive covariates:", 10,
                             min = 1)
              )
            ),
            bsCollapse(
              id = "collapse_inputs",
              bsCollapsePanel("Response rates", fluidRow(
                column(width = 4, numericInput("rr.sp.treat", "Sensitive group",
                                               0.8, min = 0, max = 1,
                                               step = 0.01)),
                column(width = 4, numericInput("rr.nsp.treat",
                                               "Non-sensitive group", 0.25,
                                               min = 0, max = 1, step = 0.01)),
                column(width = 4, numericInput("rr.con", "Control
                                               (both groups)", 0.25, min = 0,
                                               max = 1, step = 0.01))
              ), style = "info"),
              bsCollapsePanel("Sensitive Covariates",
                fluidRow(
                  column(width = 6,
                    wellPanel(h4("Sensitive group"),
                      numericInput("mu1", "Mean", 1),
                      numericInput("sigma1", "Standard Deviation", 0.5,
                                   min = 0.001),
                      numericInput("rho1", "Correlation", 0, min = 0, max = 1)
                    )
                  ),
                  column(width = 6,
                    wellPanel(h4("Non-sensitive group"),
                      numericInput("mu2", "Mean", 0),
                      numericInput("sigma2", "Standard Deviation", 0.1,
                                   min = 0.001),
                      numericInput("rho2", "Correlation", 0, min = 0, max = 1)
                    )
                  )
                ), style = "info"
              ),
              bsCollapsePanel("Non-Sensitive Covariates",
                fluidRow(
                  column(width = 4, numericInput("mu0", "Mean", 0)),
                  column(width = 4, numericInput("sigma0", "Standard Deviation",
                                                 0.5, min = 0.001)),
                  column(width = 4, numericInput("rho0", "Correlation", 0,
                                                 min = 0, max = 1))
                ), style = "info"
              )
            ),
            # CVRS / CVASD buttons ---------------------------------------------
            fluidRow(
              column(width = 4, align = "center", actionButton("simulate",
                                                               "Simulate Data",
                                                               width = "100%")),
              column(width = 4, align = "center",
                     uiOutput("analyse_cvrs_button_ui")),
              column(width = 4, align = "center",
                     uiOutput("analyse_cvasd_button_ui"))
            ),
            # Extra CVASD Parameters -------------------------------------------
            hidden(
              div(
                id = "extra_cvasd_params",
                style = "margin-top: 20px; padding: 15px;
                        background-color: #f7f7f7; border: 1px solid #ddd;
                        border-radius: 6px;",
                h4("Extra CVASD Parameters"),
                fluidRow(
                  column(width = 12,
                    textInput("eta", "Significance level",
                              "0.005, 0.05, 0.1, 0.2, 0.5"),
                    helpText("Enter a single numeric value or comma-separated
                             list. Example: 0.005,0.05,0.1"),
                  ),
                  column(width = 12,
                    textInput("R", "Threshold of the odds ratio",
                              "2, 3, 4, 3, 2"),
                    helpText("Enter a single numeric value or comma-separated
                             list. Example: 2, 3, 4, 3, 2"),
                  ),
                  column(width = 12,
                    textInput("G", "Threshold for the number of covariates",
                              "5, 6, 5, 6, 5"),
                    helpText("Enter a single numeric value or comma-separated
                             list. Example: 5, 6, 5, 6, 5"),
                  )
                ),
                actionButton("confirm_cvasd", "Run CVASD Analysis",
                             width = "100%")
              )
            ) #end hidden
          )), # end input box and fluidrow
          # Inner Tab: Output, Simulated Data, Binary --------------------------
          tabItem(
            tabName = "sim_output",
            fluidRow(box(
              title = "Output", status = "primary", solidHeader = TRUE,
              width = 12,
              tabsetPanel(id = "sim_tabs",
                tabPanel("Data",
                  uiOutput("patients_header"),
                  DTOutput("patients_table"),
                  uiOutput("covar_header"),
                  DTOutput("covar_table"),
                  uiOutput("download_simdata_ui")
                ),
                tabPanel("Results CVRS", uiOutput("results_cvrs_panel")),
                tabPanel("Results CVASD", uiOutput("results_cvasd_panel"))
              )
            ))
          )
        ), # end tabItem sim_binary
        #  Tab: Real Data TTE --------------------------------------------------
        tabItem(
          tabName = "real_tte",
          # Upload Real Data  --------------------------------------------------
          fluidRow(
            box(
              title = "Upload dataset", status = "primary", solidHeader = TRUE,
              width = 12,
              fileInput("realdata_file_tte", "Choose CSV or RDS file"),
              fluidRow(column(width = 12, align = "center",
                              uiOutput("analyse_cvrs_real_tte_ui"))),
              hidden(div(
                id = "params_real_tte",
                style = "margin-top: 20px; padding: 15px;
                        background-color: #f7f7f7; border: 1px solid #ddd;
                        border-radius: 6px;",
                h4("Input parameters"),
                fluidRow(
                  column(width = 6,
                    numericInput("nfolds_tte", "Number of folds", 10, min = 2,
                                 step = 1),
                    helpText("Enter a number of folds for cross validation")
                  ),
                  column(width = 6,
                    numericInput("nperm_tte", "Number of permutations", 50,
                                 min = 1, step = 1),
                    helpText("Enter a number of permutations")
                  ),
                  column(width = 6,
                    numericInput("reps_tte", "Number of repeats", 20, min = 1,
                                 step = 1),
                    helpText("Enter a number of repeats for repeated for cross
                             validation")
                  ),
                  column(width = 6,
                    numericInput("thresh_tte", "Sensitive group threshold",
                                 0.5, min = 0, max = 1, step = 0.000001),
                    helpText("Enter a threshold for the probability of the
                             sensitive group")
                  ),
                  column(width = 12,
                    div(
                      style = "display: flex; align-items: center; gap: 10px;",
                      span("Update progress display every"),
                      numericInput("every_tte", label = NULL, value = 5,
                                   min = 1, step = 1, width = "80px"),
                      span("permutations")
                    ),
                    helpText("Enter the number of permutations after which
                             the progress bar should update.")
                  )
                ),
                actionButton("run_cvrs_real_tte", "Run CVRS", width = "100%"),
                uiOutput("reps_tte_validation_msg"),
                uiOutput("nfolds_tte_validation_msg"),
                uiOutput("nperm_tte_validation_msg"),
                uiOutput("thresh_tte_validation_msg")
              ))
            ) # end box
          ), #end fluidRow

          ##### Inner tab: Output Real Data, TTE ######################

          tabItem(
            tabName = "real_output_tte",
            fluidRow(
              box(
                title = "Output", status = "primary", solidHeader = TRUE,
                width = 12,
                tabsetPanel(id = "real_output_tabs_tte",
                  tabPanel("Data",
                    uiOutput("patients_real_header_tte"),
                    DTOutput("patients_real_table_tte"),
                    uiOutput("covar_real_header_tte"),
                    DTOutput("covar_real_table_tte")
                  ),
                  tabPanel(
                    "Results", uiOutput("results_real_panel_tte")
                  )
                )
              )
            )
          ) # End tabItem real_output
        ), # End tabItem real_tte

        #####  Tab: Real Data Binary ######################
        tabItem(
          tabName = "real_binary",
          fluidRow(box(
            title = "Upload dataset", status = "primary", solidHeader = TRUE,
            width = 12,
            fileInput("realdata_file_binary", "Choose CSV or RDS file"),
            fluidRow(column(
              width = 12, align = "center",
              uiOutput("analyse_cvrs_real_binary_ui")
            )),
            hidden(
              div(
                id = "params_real_binary",
                style = "
                margin-top: 20px;
                padding: 15px;
                background-color: #f7f7f7;
                border: 1px solid #ddd;
                border-radius: 6px;
                ",
                h4("Input parameters"),
                fluidRow(
                  column(width = 6,
                    numericInput("nfolds_binary", "Number of folds", 10,
                                 min = 1, step = 1),
                    helpText("Enter a number of folds for cross validation")
                  ),
                  column(width = 6,
                    numericInput("nperm_binary", "Number of permutations", 50,
                                 min = 1, step = 1),
                    helpText("Enter a number of permutations")
                  ),
                  column(width = 6,
                    numericInput("reps_binary", "Number of repeats", 20,
                                 min = 1, step = 1),
                    helpText("Enter a number of repeats for repeated for
                             cross validation")
                  ),
                  column(width = 6,
                    numericInput("thresh_binary", "Sensitive group threshold",
                                 0.5, min = 0, max = 1, step = 0.000001),
                    helpText("Enter a threshold for the probability of the
                             sensitive group")
                  ),
                  column(width = 12,
                    div(
                      style = "display: flex; align-items: center; gap: 10px;",
                      span("Update progress display every"),
                      numericInput("every_binary", label = NULL, value = 5,
                                   min = 1, step = 1, width = "80px"),
                      span("permutations")
                    ),
                    helpText("Enter the number of permutations after which the
                             progress bar should update.")
                  )
                ),
                actionButton("run_cvrs_real_binary", "Run CVRS",
                             width = "100%"),
                uiOutput("reps_binary_validation_msg"),
                uiOutput("nfolds_binary_validation_msg"),
                uiOutput("nperm_binary_validation_msg"),
                uiOutput("thresh_binary_validation_msg")
              )
            )
          )), # end box/fluidrow
          #####  Inner Tab: Real Data Binary output ######################

          tabItem(
            tabName = "real_output_binary",
            fluidRow(box(
              title = "Output", status = "primary", solidHeader = TRUE,
              width = 12,
              tabsetPanel(
                id = "real_output_tabs_binary",
                tabPanel(
                  "Data",
                  uiOutput("patients_real_header_binary"),
                  DTOutput("patients_real_table_binary"),
                  uiOutput("covar_real_header_binary"),
                  DTOutput("covar_real_table_binary")
                ),
                tabPanel(
                  "Results", uiOutput("results_real_panel_binary")
                )
              )
            ))
          ) #End tabItem real_binary_output
        ) # END tabItem  real_binary
      ) # end tabItems
    ) # End dashboard body
  ) # end Dashboard page
}
