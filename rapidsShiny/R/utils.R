
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# find_sensitive_group ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

find_sensitive_group <- function(datalist, nfolds, seed, outcome_type) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (outcome_type == "tte") {
    outcome <- datalist$event
  } else if (outcome_type == "binary") {
    outcome <- datalist$response
  }

  covar <- datalist$covar
  patients <- datalist$patients

  # Divide into folds and keep prevalence of events within folds

  outcome1 <- outcome[outcome == 1]
  outcome0 <- outcome[outcome == 0]

  covar1 <- covar[outcome == 1, ]
  covar0 <- covar[outcome == 0, ]

  patients1 <- patients[outcome == 1, ]
  patients0 <- patients[outcome == 0, ]

  foldid1 <- sample(rep(seq(nfolds), length = length(outcome1)))
  foldid0 <- sample(rep(seq(nfolds), length = length(outcome0)))

  df <- data.frame()

  # CV loop

  for (i in seq(nfolds)) {
    which1 <- foldid1 == i
    which0 <- foldid0 == i

    patients_train <- rbind(patients1[!which1, ], patients0[!which0, ])
    patients_test <- rbind(patients1[which1, ], patients0[which0, ])

    covar_train <- rbind(covar1[!which1, ], covar0[!which0, ])
    covar_test <- rbind(covar1[which1, ], covar0[which0, ])

    outcome_train <- c(outcome1[!which1], outcome0[!which0])

    # Fit a single-covariate regression model to training data

    beta_hat <- apply(as.matrix(covar_train), 2, function(x) {

      if (outcome_type == "tte") {
        df <- data.frame(edate = patients_train$eventdate,
                         event = outcome_train,
                         treat = patients_train$treat, x = x)
        names(df) <- c("edate", "event", "treat", "x")
        mod <- survival::coxph(Surv(edate, event) ~ treat + x + treat:x,
                               data = df)
      } else if (outcome_type == "binary") {
        df <- data.frame(outcome = outcome_train, treat = patients_train$treat,
                         x = x)
        names(df) <- c("outcome", "treat", "x")
        mod <- glm(outcome ~ treat + x + treat:x, data = df,
                   family = "binomial")
      }
      if (is.na(mod$coeff[length(mod$coeff)])) {
        0
      } else {
        mod$coeff[length(mod$coeff)]
      }
    })
    beta_hat[is.na(beta_hat)] <- 0

    # Compute  risk scores for test data

    cvrs <- apply(as.matrix(covar_test), 1, function(x) {
      t(as.matrix(x)) %*% (as.matrix(beta_hat))
    })
    df_folds <- data.frame(FID = patients_test$FID, IID = patients_test$IID,
                           cvrs = cvrs, fold = i)

    # Divide test data into 2 clusters by (fold-wise k-means)

    df_folds$sens.pred <- FALSE

    km <- kmeans(df_folds$cvrs, 2)

    # For time to event outcome, smaller risk scores correspond to sensitive
    # group (smaller risk of event)
    if (outcome_type == "tte") {
      if (km$centers[1] < km$centers[2]) {
        df_folds$sens.pred[km$cluster == 1] <- TRUE
      } else {
        df_folds$sens.pred[km$cluster == 2] <- TRUE
      }
    } else if (outcome_type == "binary") {
      if (km$centers[1] > km$centers[2]) {
        df_folds$sens.pred[km$cluster == 1] <- TRUE
      } else {
        df_folds$sens.pred[km$cluster == 2] <- TRUE
      }
    }
    df <- rbind(df, df_folds)
  } # End CV loop

  m <- match(
    paste(as.character(patients$FID), as.character(patients$IID), sep = ":"),
    paste(as.character(df$FID), as.character(df$IID), sep = ":")
  )
  df <- df[m, ]

  ret <- list(sens.pred = df$sens.pred, cvrs = df$cvrs)
  ret
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# permutation ---
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

permutation <- function(datalist, nperm, progress_perm = NULL, outcome_type,
                        every) {
  pval_vector <- NULL
  warn_iter <- integer(0)
  msg <- vector()
  if (is.na(every) || is.null(every) || every < 1 || nperm == 1) {
    every = 1
  }
  for (i in 1:nperm) {
    set.seed(i)
    treat <- sample(datalist$patients$treat, replace = FALSE)
    nfolds <- 10
    withCallingHandlers(
      {
        sens <- find_sensitive_group(datalist, nfolds, i, outcome_type)
      },
      warning = function(w) {
        msg <<- c(msg, paste("Iteration", i, ", Warning: ", conditionMessage(w),
                             sep = " "))
        warn_iter <<- c(warn_iter, i)
        invokeRestart("muffleWarning")
      }
    )

    if (outcome_type == "tte") {
      df <- data.frame(edate = datalist$patients$eventdate,
                       event = datalist$event, treat = treat,
                       sens_group = as.numeric(sens$sens.pred))
    } else if (outcome_type == "binary") {
      df <- data.frame(outcome = datalist$response, treat = treat,
                       sens_group = as.numeric(sens$sens.pred))
      names(df) <- c("outcome", "treat", "sens_group")
    }
    withCallingHandlers(
      {
        if (outcome_type == "tte") {
          modperm <- survival::coxph(Surv(edate, event) ~ treat + sens_group +
                                       treat:sens_group, data = df)
          modsum <- summary(modperm)$coeff
        } else if (outcome_type == "binary") {
          modperm <- glm(outcome ~ treat + sens_group + treat:sens_group,
                         data = df, family = "binomial")
          modsum <- summary(modperm)$coeff
          modsum <- modsum[rownames(modsum) != "(Intercept)", ]
        }

      },
      warning = function(w) {
        msg <<- c(msg, paste("Iteration", i, ", Warning: ", conditionMessage(w),
                             sep = " "))
        message(" Cox model: Warning at iteration ", i)
        warn_iter <<- c(warn_iter, i)
        invokeRestart("muffleWarning")
      }
    )

    pval <- modsum[nrow(modsum), colnames(modsum) == "Pr(>|z|)"]
    pval_vector <- c(pval_vector, pval)

    if (!is.null(progress_perm)) {
      progress_perm(i, nperm, every)
    }
  }
  if (length(warn_iter) > 0) {
    pval_vector <- pval_vector[-warn_iter]
  }
  list(pval_vector = pval_vector, msg = msg)
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Analyse Real Data (analyse_real---------)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

analyse_real <- function(datalist, nfolds, reps, thresh, nperm, every,
                         progress_rcv = NULL, start_perm, progress_perm = NULL,
                         outcome_type) {
  cvrs_rcv <- NULL
  sens_rcv <- NULL
  warn_rep <- integer(0)
  msg <- vector()
  warning_ind <- FALSE
  res <- NULL
  num_perm <- 0
  perm_msg <- NULL
  pval_perm <- NA

  for (j in 1:reps) { # repeated CV
    seed <- j
    withCallingHandlers(
      {
        sens <- find_sensitive_group(datalist, nfolds, seed, outcome_type)
      },
      warning = function(w) {
        msg <<- c(msg, paste("Repetition", j, ", Warning: ",
                             conditionMessage(w), sep = " "))
        warn_rep <<- c(warn_rep, j)
        warning_ind <<- TRUE
        invokeRestart("muffleWarning")
      }
    )

    # only keep repetitions without warnings
    if (!warning_ind && !is.null(sens)) {
      sens_rcv <- cbind(sens_rcv, sens$sens.pred)
      cvrs_rcv <- cbind(cvrs_rcv, sens$cvrs)
    }

    if (!is.null(progress_rcv)) {
      progress_rcv(j, reps)
    }
  }

  sens_group <- rowMeans(sens_rcv)
  sens_index <- as.integer(sens_group >= thresh)

  if (outcome_type == "tte") {
    df <- data.frame(edate = datalist$patients$eventdate,
                     event = datalist$event, treat = datalist$patients$treat,
                     sens_group = sens_group)
    names(df) <- c("edate", "event", "treat", "sens_group")
    mod <- survival::coxph(Surv(edate, event) ~ treat + sens_group +
                             treat:sens_group, data = df)
    mod_coeff <- signif(summary(mod)$coeff, 3)
  } else if (outcome_type == "binary") {
    df <- data.frame(outcome = datalist$response,
                     treat = datalist$patients$treat, sens_group = sens_group)
    names(df) <- c("outcome", "treat", "sens_group")
    mod <- glm(outcome ~ treat + sens_group + treat:sens_group, data = df,
               family = "binomial")
    mod_coeff <- signif(summary(mod)$coeff, 3)
    mod_coeff <- mod_coeff[rownames(mod_coeff) != "(Intercept)", ]
  }
  rownames(mod_coeff) <- c("Treatment", "Sensitive group", "Interaction")
  pval_orig <- mod_coeff[nrow(mod_coeff), colnames(mod_coeff) == "Pr(>|z|)"]

  # Permutation
  if (nperm > 0) {
    start_perm()
    res <- permutation(datalist, nperm, progress_perm, outcome_type, every)

    pval_perm <- (1 + sum(res$pval_vector < pval_orig)) /
      (1 + length(res$pval_vector))
    num_perm <- length(res$pval_vector)
    perm_msg <- res$msg
  }
  list(risk_scores = cvrs_rcv, sens_rcv = sens_rcv,
       sens_group = rowMeans(sens_rcv), mod_coeff = mod_coeff,
       pval_orig = pval_orig, pval_perm = pval_perm,
       sens_index = sens_index, num_perm = num_perm, reps_msg = msg,
       reps_ok = reps - length(warn_rep), perm_msg = perm_msg)
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# plot_risk_scores------
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

plot_risk_scores <- function(cvrs, sens_pred, sens_true = NULL) {

  sens_pred_color <- factor(as.numeric(sens_pred) + 1,
                            labels = c("Non-Sensitive", "Sensitive"))
  sens_true_color <- factor(as.numeric(sens_true) + 1,
                            labels = c("Non-Sensitive", "Sensitive"))
  sens_pred <- factor(as.vector(sens_pred),
                      labels = c("Non-Sensitive", "Sensitive"))
  cvrs_vec <- as.vector(cvrs)
  sens_true <- as.factor(as.numeric(sens_true))
  dat <- data.frame(cvrs_vec, sens_pred, sens_true)
  plot_pred <- ggplot(dat, aes(x = seq_along(cvrs_vec), y = cvrs_vec,
                               color = sens_pred_color)) +
    geom_point(fill = "transparent", shape = 21, size = 2, stroke = 0.5) +
    labs(x = "Patients", y = "Risk scores",
         title = "Risk scores coloured by the predicted sensitivity status") +
    theme(legend.position = "bottom") +
    scale_colour_discrete(name = "Predicted sensitivity status") +
    guides(colour = guide_legend(override.aes = list(shape = 15)))

  plot_true <- ggplot(dat, aes(x = seq_along(cvrs_vec), y = cvrs_vec,
                               color = sens_true_color)) +
    geom_point(fill = "transparent", shape = 21, size = 2, stroke = 0.5) +
    labs(x = "Patients", y = "Risk scores",
         title = "Risk scores coloured by the true sensitivity status") +
    theme(legend.position = "bottom") +
    scale_colour_discrete(name = "True sensitivity status") +
    guides(colour = guide_legend(override.aes = list(shape = 15)))

  plotcvrs <- grid.arrange(plot_pred, plot_true, ncol = 1)
  plotcvrs
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# plot_risk_scores_real -----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

plot_risk_scores_real <- function(risk_scores, treat) {
  df <- as.data.frame(risk_scores)
  df$id <- factor(seq_len(nrow(df)))
  df$treatment <- as.factor(treat)

  df_long <- df |>
    tidyr::pivot_longer(
      cols = -c(.data$id, .data$treatment),
      names_to = "Variable",
      values_to = "Value"
    )

  output_plot <- ggplot(df_long, aes(x = .data$id, y = .data$Value,
                                     fill = .data$treatment)) +
    geom_boxplot() +
    labs(
      x = "Patients",
      y = "Risk scores"
    ) +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    guides(fill = guide_legend(title = NULL))

  output_plot
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# plot_prob_sensitive -------
# Plot probability of belonging to sensitive group
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

plot_prob_sensitive <- function(sens_rcv) {
  sens_group <- rowMeans(sens_rcv)

  # Plot probability of belonging to sensitive group

  output_plot <- ggplot(data.frame(sens_group = sens_group),
                        aes(x = sens_group)) +
    geom_histogram(bins = 40) +
    labs(
      x = "Probability of belonging to sensitive group"
    )

  output_plot
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# plots_surv_per_group---------
# Kaplan-Meier plots per group
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

plots_surv_per_group <- function(datalist, sens_group, threshold) {

  sens_index <- vector(length = length(sens_group))
  sens_index[] <- as.numeric(sens_group >= threshold)

  # Survival plot for sensitive group

  df1 <- data.frame(edate = datalist$patients$eventdate[sens_index == 1],
                    event = datalist$event[sens_index == 1],
                    treat = datalist$patients$treat[sens_index == 1])
  if (nrow(df1) > 0) {
    fit1 <- survival::survfit(Surv(edate, event) ~ treat, data = df1)
    plot1 <- survminer::ggsurvplot(fit1, data = df1, legend.title = "",
                                   legend.labs = c("Control", "Treatment"),
                                   title = "Sensitive group")
  }

  # Survival plot for non-sensitive group
  df0 <- data.frame(edate = datalist$patients$eventdate[sens_index == 0],
                    event = datalist$event[sens_index == 0],
                    treat = datalist$patients$treat[sens_index == 0])
  if (nrow(df0) > 0) {
    fit0 <- survival::survfit(Surv(edate, event) ~ treat, data = df0)
    plot0 <- survminer::ggsurvplot(fit0, data = df0, legend.title = "",
                                   legend.labs = c("Control", "Treatment"),
                                   title = "Non-sensitive group")
  }

  if ((nrow(df1) > 0) && (nrow(df0) == 0)) {
    #everyone is in the sensitive group
    output_plot = plot1 #
  } else if ((nrow(df0) > 0) && (nrow(df1) == 0)) {
    #everyone is in the non-sensitive group
    output_plot = plot0
  } else if ((nrow(df0) > 0) && (nrow(df1) > 0)) {
    output_plot <- gridExtra::grid.arrange(plot1$plot, plot0$plot, nrow = 1)
  }
  output_plot
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Analyse Simulated Data (analyse_simulated) ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

analyse_simulated <- function(datalist, seed, outcome_type) {
  msg <- vector()
  pred_rr_sens <- NULL
  nfolds <- 10
  withCallingHandlers(
    {
      sens <- find_sensitive_group(datalist, nfolds, seed, outcome_type)
    },
    warning = function(w) {
      msg <<- c(msg, paste("Warning: ", conditionMessage(w), sep = " "))
      invokeRestart("muffleWarning")
    }
  )

  if (outcome_type == "tte") {

    # p-value in the overall population
    df <- data.frame(edate = datalist$patients$eventdate,
                     event = datalist$event, treat = datalist$patients$treat)
    df$treat <- factor(df$treat)
    mod_overall <- survival::coxph(Surv(edate, event) ~ treat, data = df)
    pval_overall <- summary(mod_overall)$coeff[5]

    # interaction p-value
    df <- data.frame(edate = datalist$patients$eventdate,
                     event = datalist$event, treat = datalist$patients$treat,
                     sens_group = sens$sens.pred)
    names(df) <- c("edate", "event", "treat", "sens_group")
    mod <- survival::coxph(Surv(edate, event) ~ treat + sens_group +
                             treat:sens_group, data = df)
    mod_coeff <- signif(summary(mod)$coeff, 3)

    # p-value in the sensitive group
    df <- data.frame(edate = datalist$patients$eventdate[sens$sens.pred == 1],
                     event = datalist$event[sens$sens.pred == 1],
                     treat = datalist$patients$treat[sens$sens.pred == 1])
    names(df) <- c("edate", "event", "treat")
    mod <- survival::coxph(Surv(edate, event) ~ treat, data = df)
    mod_sens <- signif(summary(mod)$coeff, 3)
    pval_sens <- mod_sens[5]
  } else if (outcome_type == "binary") {
    # p-value for the overall population
    df <- data.frame(outcome = datalist$response,
                     treat = datalist$patients$treat)
    names(df) <- c("outcome", "treat")
    mod <- glm(outcome ~ treat, data = df, family = "binomial")
    mod_overall <- signif(summary(mod)$coeff, 3)
    mod_overall <- mod_overall[rownames(mod_overall) != "(Intercept)", ]
    pval_overall <- mod_overall[4]

    # interaction p-value
    df <- data.frame(outcome = datalist$response,
                     treat = datalist$patients$treat,
                     sens_group = sens$sens.pred)
    names(df) <- c("outcome", "treat", "sens_group")
    mod <- glm(outcome ~ treat + sens_group + treat:sens_group, data = df,
               family = "binomial")
    mod_coeff <- signif(summary(mod)$coeff, 3)
    mod_coeff <- mod_coeff[rownames(mod_coeff) != "(Intercept)", ]

    # p-value in the sensitive group
    df <- data.frame(outcome = datalist$response[sens$sens.pred == 1],
                     treat = datalist$patients$treat[sens$sens.pred == 1])
    names(df) <- c("outcome", "treat")
    mod <- glm(outcome ~ treat, data = df, family = "binomial")
    mod_sens <- signif(summary(mod)$coeff, 3)
    mod_sens <- mod_sens[rownames(mod_sens) != "(Intercept)", ]
    pval_sens <- mod_sens[4]

    # response rate in the sensitive group
    sens_group <- sens$sens.pred[sens$sens.pred == 1]
    #treatment allocation in the sensitive group#response in the sensitive group
    treat_group <- datalist$patients$treat[sens$sens.pred == 1]
    pred_rr_sens <- sum(sens_group & treat_group) / length(treat_group)
  }

  rownames(mod_coeff) <- c("Treatment", "Sensitive group", "Interaction")
  pval_inter <- mod_coeff[nrow(mod_coeff), colnames(mod_coeff) == "Pr(>|z|)"]
  ret <- list(risk_scores = sens$cvrs, sens_pred = sens$sens.pred,
              pval_inter = pval_inter, pval_overall = pval_overall,
              mod_coeff = mod_coeff)

  ## Sensitivity and specificity of the sensitive group selection algorithm

  sens_pred <- sens$sens.pred
  sens_true <- datalist$patients$sens.true

  if (with(datalist$patients, exists("sens.true"))) {
    conf <- matrix(nrow = 2, ncol = 2,
                   data = c(sum(!sens_pred & !sens_true),
                            sum(!sens_pred & sens_true),
                            sum(sens_pred & !sens_true),
                            sum(sens_pred & sens_true)),
                   byrow = TRUE)
    psens <- conf[2, 2] / (conf[2, 2] + conf[1, 2])
    pspec <- conf[1, 1] / (conf[1, 1] + conf[2, 1])
    ret[["psens"]] <- psens
    ret[["pspec"]] <- pspec
  }

  toprint <- paste(
    paste0("P-value for the test in the overall population: ",
           signif(pval_overall, 3)),

    paste0("P-value for the test in the sensitive group: ",
           signif(pval_sens, 3)),

    paste0("Sensitivity of identifying the sensitive group: ",
           signif(psens, 3)),

    paste0("Specificity of identifying the sensitive group: ",
           signif(pspec, 3)),

    sep = "\n"
  )

  if (!is.null(pred_rr_sens)) {
    toprint = paste0(toprint, "Response rate in the sensitive group on the
                     treatment arm: ", signif(pred_rr_sens, 3), sep = "\n")
  }
  ret[["toprint"]] <- toprint
  ret
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Simulate data #######################
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

simulate_data_tte <- function(n_size = 1000, n_covar = 100, n_sens_covar = 10,
                              rho1 = 0, rho2 = 0, rho0 = 0,
                              mu1 = 1, mu2 = 0, mu0 = 0, sigma1 = 0.5,
                              sigma2 = 0.1, sigma0 = 0.5,
                              perc_sp = 0.1,
                              lambda_nsp_treat = 0.1,
                              lambda_sp_treat = 0.05,
                              lambda_con = 0.1,
                              pcens = 0.2,
                              seed = 123) {

  set.seed(seed)
  lambda <- log(lambda_nsp_treat / lambda_con) # main treatment effect
  inter_scaling <- log(lambda_sp_treat / lambda_con) - lambda #interact. scaling

  #sensitive covariates, sensitive patients
  m1 <- numeric(length = n_sens_covar)
  m1[] <- mu1
  sigma1_mtx <- matrix(nrow = n_sens_covar, ncol = n_sens_covar,
                       data = sigma1 * sigma1 * rho1)
  diag(sigma1_mtx) <- sigma1^2

  #sensitive covariates, non-sensitive patients
  m2 <- numeric(length = n_sens_covar)
  m2[] <- mu2
  sigma2_mtx <- matrix(nrow = n_sens_covar, ncol = n_sens_covar,
                       data = sigma2 * sigma2 * rho2)
  diag(sigma2_mtx) <- sigma2^2

  #non-sensitive covariates, all patients
  m0 <- numeric(length = n_covar - n_sens_covar)
  m0[0] <- mu0
  sigma0_mtx <- matrix(nrow = n_covar - n_sens_covar,
                       ncol = n_covar - n_sens_covar,
                       data = sigma0 * sigma0 * rho0)
  diag(sigma0_mtx) <- sigma0^2

  ## Simulate covariates for sensitive patients
  #sensitive covariates
  scovar_sp <- mvrnorm(n = n_size *  perc_sp, m1, sigma1_mtx, tol = 1e-6)
  #non-sensitive covariates
  nscovar_sp <- mvrnorm(n = n_size * perc_sp, m0, sigma0_mtx, tol = 1e-6)
  sp <- cbind(scovar_sp, nscovar_sp)
  sp <- as.data.frame(sp)
  sp$sens.true <- 1

  ## Simulate covariates for non-sensitive patients
  #sensitive covariates
  scovar_nsp <- mvrnorm(n = n_size * (1 - perc_sp), m2, sigma2_mtx, tol = 1e-6)
  #non-sensitive covariates
  nscovar_nsp <- mvrnorm(n = n_size * (1 - perc_sp), m0, sigma0_mtx, tol = 1e-6)
  nsp <- cbind(scovar_nsp, nscovar_nsp)
  nsp <- as.data.frame(nsp)
  nsp$sens.true <- 0

  ## Equal randomisation to control/treatment arm for sensitive patients
  ind <- seq_len(nrow(sp))
  control_sp <- sp[ind %% 2 == 0, ]
  treatment_sp <- sp[ind %% 2 == 1, ]

  ## Equal randomisation to control/treatment arm for non-sensitive patients
  ind <- seq_len(nrow(nsp))
  control_nsp <- nsp[ind %% 2 == 0, ]
  treatment_nsp <- nsp[ind %% 2 == 1, ]

  ## Combine control and treatment
  control <- rbind(control_sp, control_nsp)
  treatment <- rbind(treatment_sp, treatment_nsp)
  control$treat <- 0   #control/treatment indicator
  treatment$treat <- 1 #control/treatment indicator
  patients_data <-  rbind(control, treatment)
  rownames(patients_data) <- seq(1:n_size)

  covar <- as.data.frame(patients_data[, 1:n_covar])
  colnames(covar) <- paste("Covar", 1:n_covar, sep = "")
  patients <- data.frame(FID = seq(1:n_size), IID = seq(1:n_size),
                         sens.true = patients_data$sens.true,
                         treat =  patients_data$treat)

  sens_covar <- as.matrix(covar[, 1:n_sens_covar])
  ##covariate-treatment interaction
  gamma <- sapply(m1, function(x) {
                                   ifelse(x == 0, 0, inter_scaling /
                                            (n_sens_covar * x))})
  sens_covar_levels <- sens_covar %*% as.matrix(gamma, nrow = n_sens_covar)

  # hazard for each patient
  linpred <- patients$treat * lambda + patients$treat * sens_covar_levels
  patients$hazard <- signif(lambda_con * exp(linpred), 3)

  # simulate event times from exponential distribution: T = -log(U)/hazard
  u <- runif(n_size)
  patients$eventdate <- -log(u) / patients$hazard

  nu <- mean(patients$hazard) * pcens / (1 - pcens)
  cdate <- rexp(n_size, rate = nu)

  event <- as.numeric(patients$eventdate <= cdate)
  patients$eventdate  <- signif(pmin(patients$eventdate, cdate), 3)

  list(patients = patients, covar = covar, event = event)

}
