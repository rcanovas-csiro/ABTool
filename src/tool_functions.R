rm(list =ls())
library(data.table)
library(tidyverse)
library(pROC)


########################################
# FOR TAB: Prevalence by Age         #
#######################################

#' Print plot for Tab percentage age
#' 
prev_cent_age_plot <- function(df, gp.list, diagnosis, prev=TRUE) {
  df <- df[df$Diagnosis==diagnosis, ]
  y.val <- "Centiloid"
  y.min <- "Cen_low"
  y.max <- "Cen_high"
  if(prev) {
    y.val <- "Prevalence"
    y.min <- "Prev_low"
    y.max <- "Prev_high"
  }
  color.co <- diagnosis
  color.aponeg <- paste0(diagnosis, "-APOE e4 non-Carrier")
  color.apopos <- paste0(diagnosis, "-APOE e4 Carrier")
  gp.list[[1]] <- gp.list[[1]] + 
     geom_line(data=df[df$Apoe4=="All", ], 
               aes(x=Age, y=!!sym(y.val), colour=color.co), linewidth=1) +
     geom_ribbon(data=df[df$Apoe4=="All", ], 
                 aes(x=Age, ymin=!!sym(y.min), ymax=!!sym(y.max), fill=color.co), alpha=0.4)  
   gp.list[[2]] <- gp.list[[2]] + 
     geom_line(data=df[df$Apoe4=="POS", ], 
               aes(x=Age, y=!!sym(y.val), colour=color.apopos), linewidth=1) +
     geom_ribbon(data=df[df$Apoe4=="POS", ], 
                 aes(x=Age, ymin=!!sym(y.min), ymax=!!sym(y.max), fill=color.apopos), alpha=0.4) + 
     geom_line(data=df[df$Apoe4=="NEG", ], 
               aes(x=Age, y=!!sym(y.val), colour=color.aponeg), linewidth=1) +
     geom_ribbon(data=df[df$Apoe4=="NEG", ], 
                 aes(x=Age, ymin=!!sym(y.min), ymax=!!sym(y.max), fill=color.aponeg), alpha=0.4) 
  return(gp.list)
}

#fix theme of plots
fix_theme <- function(gp, colors.gp, x.lab, y.lab, x.inter, y.inter, x.title = TRUE, y.title=TRUE) {
  gp <- gp + geom_vline(xintercept = x.inter, linewidth = 1, linetype="dotted", color='black') +
    geom_hline(yintercept = y.inter, linewidth = 1, linetype="dotted", color='black') +  
    scale_colour_manual(name="lines", values=colors.gp, guide = "none") +
    scale_fill_manual(name="filling", values=colors.gp) +
    xlab(x.lab) + ylab(y.lab) + 
    theme(axis.title = element_text(size = 15, face="bold"),
          axis.text = element_text(size = 12))
  if(!x.title) {
    gp <- gp + theme(axis.title.x = element_blank())
  }
  if(!y.title) {
    gp <- gp + theme(axis.title.y = element_blank())
  }
  return(gp)
}

##########################################
# FOR TAB:  TAB: Simulation - Reference #
#########################################

#' upload reference data
#' 
#' @param path Indicates the path from where to get the reference dataset, If NULL, load the default cohort
#' 
#' @return upload cohort
#' 
upload_file <- function(path) {
  df <- NULL
  df <- read.csv(path)
  #check that the columns required are available (Centiloid and Diagnosis)
  #cols <- c('Centiloid', 'Diagnosis')
  cols <- c('Diagnosis')
  if(!all(cols %in% colnames(df))) {
    cols <- setdiff(cols, colnames(df))  #variables that are missing
    df <- paste0("The following columns are missing: ", paste(cols, collapse = ", "))
    return(df)
  }
  return(df)
}


#' Run Analysis over reference data
#' 
#' @param ab.marker Name Column to be used as AB measure (must be a continuous variable)
#' @param dir.thr Indicates the direction of how ab.marker is to be used
#' @param marker1 Name Column to be used as predictor (must be a continuous variable)
#' @param marker2 Name Column to be used as denominator of predictor if ratio is selected
#' @param participants Vector with the participant diagnoses to be include (CD and/or MRI and/or AD)
#' @param centiloid Centiloid threshold used to define AB- and AB+
#' @param path Indicates the path from where to get the reference dataset, If NULL, load the default cohort
#' 
#' @return Cohort stats given the response and predictor choose
#' 
run_analysis <- function(ab.param, dir.thr, marker1, marker2, participants, path = NULL) {
  df <- NULL
  if(is.null(path)) 
    return("Please upload a  valid path to the data.")
  df <- read.csv(path) #all the checks over the file were done in the upload check
  #now load and compute the data
  if(!is.null(df)) {
    df <- df[df$Diagnosis %in% participants, ] #still need to check this
    #df[  ,c(marker1)] <- replace_outliers(df[  ,c(marker1)])
    df$Pred <- df[ ,c(marker1)]
    if(marker2 != "") {
      #df[  ,c(marker2)] <- replace_outliers(df[  ,c(marker2)]) 
      #check that the marker does not contain zeros
      if(any(df[  ,c(marker2)]  == 0, na.rm = TRUE))
         return("Denominator cannot have zero values")
      df$Pred <- df$Pred / df[ ,c(marker2)]
    }
    #df$AB.PARAM <- replace_outliers(df[  ,c(ab.param)])
    df$AB.PARAM <- df[  ,c(ab.param)]
    df <- df[ , c("AB.PARAM", "Pred", "Diagnosis")]
    df <- na.omit(df) 
    if(nrow(df) < 20) #The dataframe must have at least 10 AB+ and 10 AB-
      return("File has less than the minimum 20 participant requiered after filtering.")
    param.values <- sort(df$AB.PARAM)
    mi.param <- param.values[1]
    ma.param <- param.values[length(param.values)]
    #check that is it possible to get 10 ab- and 10 ab+ for some value
    ab_neg <- param.values[10] 
    ab_pos <- param.values[length(param.values) - 10]
    if(ab_pos < ab_neg)
      return("It is not possible to select an Aβ parameter threshold such that report at least 10 Aβ+ and 10 Aβ- for the selected participants.")
    
    if(length(unique(param.values)) > 2) {
      thr <- param.values[round(length(param.values)/2, 0)] #middle point
      if(thr < ab_neg)
        thr <- ab_neg
      else if(thr > ab_pos)
        thr <- ab_pos
    }
    else {
      if(dir.thr == "low_neg")
        thr <- ma.param
      else
        thr <- mi.param
    }
    param.info <- c(mi.param, ma.param, thr)
    stats <- compute_roc(df, thr, dir.thr)
    if(is.character(stats)) #returning an error message
      return(stats)
    return(append(stats, list(param.info)))
  }
  return(NULL)
}



#' Replace outliers with median
#' 
#' @param x Vector of values to check 
#' @param na.rm Indicates if we consider NAs or not
#' 
#' @return New vector where outliers are replaced by the median of the original raw vector
#' 
replace_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.05, .95), na.rm = TRUE)
  H <- 1 * IQR(x, na.rm = TRUE)
  y <- x
  y[!is.na(x) & ((x < (qnt[1] - H)) | (x > (qnt[2] + H)))] <- median(x, na.rm = TRUE)
  return(y)
}

#' Load reference data
#' 
#' @param df DataFrame containing the containing the columns Pred and AB.PARAM
#' @param thr Threshold to be used to compute AB- and AB+
#' @param dir.thr Direction of the value to define negative or positive
#' 
#' @return Cohort stats given the response and predictor choose
#' 
compute_roc <- function(df, thr, dir.thr) {
  if(is.null(df))
    return("DataFrame is NULL")
  if(dir.thr == "low_neg")
    df$Res <- ifelse(df$AB.PARAM < thr, 0, 1) #AB- -> 0  AB+ -> 1
  else
    df$Res <- ifelse(df$AB.PARAM > thr, 0, 1)
  #size test
  m0 <-  nrow(df[df$Res==0, ])
  m1 <-  nrow(df[df$Res==1, ])
  stest <- min(m1, m0)
  if(stest < 10) {
    error <- paste0("Not enough participant, there must be at least 10 AB- and 10 Aβ+ cases. Currently, using ", 
                    thr, " as threshold, we have ")
    if(dir.thr == "low_neg") 
      error <- paste0(error, m0, " Aβ- and ", m1, " Aβ+.")
    else 
      error <- paste0(error, m0, " Aβ+ and ", m1, " Aβ-.")
    return(error)
  }
  df$FitPred <- glm(Res ~ Pred, family="binomial", data=df)$fitted
  ro <- pROC::roc(response = df$Res, predictor = df$FitPred)
  auc <- as.numeric(pROC::auc(ro))
  auc <- format(round(auc, 2), nsmall = 2)
  auc_ci <- as.numeric(pROC::ci(ro))
  auc_l <- format(round(auc_ci[1], 2), nsmall = 2)
  auc_h <- format(round(auc_ci[3], 2), nsmall = 2)
  auc <- paste0(auc, " (", auc_l, "-", auc_h, ")")
  stats <- pROC::coords(ro, ro$thresholds, ret=c('threshold','sensitivity','specificity',
                                                 'ppv','npv','youden', 'tn','tp','fn','fp'))
  return(list(df, stats, auc))
}




#' Calculate the TN, TP, FN, FP, and sample size data given a prevalence, sample size and a reference sens and spec
#' 
#' @param prev Prevalence of Cohort
#' @param n Size of the Cohort
#' @param sens Reference Sensitivity 
#' @param spec Reference Specificity
#' 
#' @return The simulated TN, TP, FN, FP, and sample size for the Cohort
#' 
 simulate_data_sample <- function(prev, n, sens, spec) {
   all_cases <- floor(n * prev)
   all_controls <- n - all_cases
   tp <- floor(all_cases * sens)
   fn <- all_cases - tp
   tn <- floor(all_controls * spec)
   fp <- all_controls - tn
   values <- c(n, tp, fp, fn, tn)
   return(values)
 }
 
 #' Calculate the TN, TP, FN, FP, and sample size data given a prevalence, tp and a reference sens and spec
 #' 
 #' @param prev Prevalence of Cohort
 #' @param tp Number of desired True Positive cases
 #' @param sens Reference Sensitivity 
 #' @param spec Reference Specificity
 #' 
 #' @return The simulated TN, TP, FN, FP, and sample size for the Cohort
 #' 
 simulate_data_tp <- function(prev, tp, sens, spec) {
   n <- ceiling(tp / (sens * prev))
   all_cases <- floor(n * prev)
   all_controls <- n - all_cases
   fn <- all_cases - tp
   tn <- floor(all_controls * spec)
   fp <- all_controls - tn
   values <- c(n, tp, fp, fn, tn)
   return(values)
 }

 
 ##########################################
 # FOR TAB:  Compute Risk           #
 #########################################
 
 compute_model <- function(df, fmla, thr, dir.thr) {
   if(is.null(df))
     return("DataFrame is NULL after applying filters")
   if(dir.thr == "low_neg")
     df$Res <- ifelse(df$AB.PARAM < thr, 0, 1) #AB- -> 0  AB+ -> 1
   else
     df$Res <- ifelse(df$AB.PARAM > thr, 0, 1)
   #size test
   m0 <-  nrow(df[df$Res==0, ])
   m1 <-  nrow(df[df$Res==1, ])
   stest <- min(m1, m0)
   if(stest < 10) {
     error <- paste0("Not enough participant, there must be at least 10 AB- and 10 Aβ+ cases. Currently, using ", 
                     thr, " as threshold, we have ")
     if(dir.thr == "low_neg") 
       error <- paste0(error, m0, " Aβ- and ", m1, " Aβ+.")
     else 
       error <- paste0(error, m0, " Aβ+ and ", m1, " Aβ-.")
     return(error)
   }
   model <- glm(as.formula(fmla), data = df, family="binomial")
   #TODO: check that model is valid
   pred <-  predict(model, type = "response")
   df$pred.results <- pred
   ro <- pROC::roc(response = df$Res, predictor = pred)
   auc <- as.numeric(pROC::auc(ro))
   auc_ci <- as.numeric(pROC::ci(ro))
   auc <- format(round(auc, 2), nsmall = 2)
   auc_l <- format(round(auc_ci[1], 2), nsmall = 2)
   auc_h <- format(round(auc_ci[3], 2), nsmall = 2)
   auc <- paste0(auc, " (", auc_l, "-", auc_h, ")")
   stats <- pROC::coords(ro, ro$thresholds, ret=c('threshold','sensitivity','specificity',
                                                  'ppv','npv','youden', 'tn','tp','fn','fp'))
   #Marker ROC
   ro <- pROC::roc(response = df$Res, predictor = df$Pred)
   m.stats <- pROC::coords(ro, ro$thresholds, ret=c('threshold','sensitivity','specificity'))
   
   return(list(df, model, auc, stats, m.stats))
 }

 
 
 #' Create Model 
 #' 
 #' @param ab.marker Name Column to be used as AB measure (must be a continuous variable)
 #' @param dir.thr Indicates the direction of how ab.marker is to be used
 #' @param marker1 Name Column to be used as predictor (must be a continuous variable)
 #' @param marker2 Name Column to be used as denominator of predictor if ratio is selected
 #' @param participants Vector with the participant diagnoses to be include (CD and/or MRI and/or AD)
 #' @param centiloid Centiloid threshold used to define AB- and AB+
 #' @param covar_list Column names to be used as adjusting covariables for the model
 #' @param path Indicates the path from where to get the reference dataset, If NULL, load the default cohort
 #' 
 #' @return Cohort stats given the response and predictor choose
 #' 
 create_model <- function(ab.param, dir.thr, marker1, marker2, participants, covar_list, path=NULL) {
   df <- NULL
   stats <- NULL
   if(is.null(path))
     return("Please upload a  valid path to the data.")
   df <- read.csv(path)
   #now load and compute the data
   if(!is.null(df)) {
     #check that the columns required are available (marker1, marker2, Centiloid and Diagnosis)
     df_cols <- c(ab.param, marker1, 'Diagnosis', covar_list)
     df_cols <- unique(df_cols) #in case that Diagnosis is repeated
     if(marker2 != "")
       df_cols <- c(df_cols, marker2)
     if(!all(df_cols %in% colnames(df))) {
       df_cols <- setdiff(df_cols, colnames(df))  #variables that are missing
       df <- paste0("The following columns are missing: ", paste(df_cols, collapse = ", "))
       return(df)
     }
     #now load and compute the data
     df <- df[df$Diagnosis %in% participants, ]
     #df[  , c(marker1)] <- replace_outliers(df[  ,c(marker1)]) #THIS I CHANGED
     df$Pred <- df[ ,c(marker1)]
     if(marker2 != "") {
       #df[  ,c(marker2)] <- replace_outliers(df[  ,c(marker2)])
       if(any(df[  ,c(marker2)]  == 0, na.rm = TRUE)) #check that the marker does not contain zeros
         return("Denominator cannot have zero values")
       df$Pred <- df$Pred / df[ ,c(marker2)]
     }
     
     #df$AB.PARAM <- replace_outliers(df[  ,c(ab.param)]) #THIS I CHANGED
     df$AB.PARAM <- df[  , c(ab.param)]
     #get columns of interest and remove any rows with NAs
     df_cols <- c('AB.PARAM', 'Diagnosis', 'Pred', covar_list)
     df_cols <- unique(df_cols)
     df <- na.omit(df[ , c(df_cols)])
     fmla <- "Res ~ Pred"
     if(length(covar_list) > 0)
       fmla <- paste0(fmla," + ", paste(covar_list, collapse = " + "))
     if(nrow(df) < 20) #The dataframe must have at least 10 Ab+ and 10 AB-
       return("File has less than the minimum 20 participant requiered after filtering.")
     param.values <- sort(df$AB.PARAM)
     mi.param <- param.values[1]
     ma.param <- param.values[length(param.values)]
     #check that is it possible to get 10 ab- and 10 ab+ for some value
     ab_neg <- param.values[10] 
     ab_pos <- param.values[length(param.values) - 10]
     if(ab_pos < ab_neg)
       return("It is not possible to select an Aβ parameter threshold such that report at least 10 Aβ+ and 10 Aβ- for the selected participants.")
     
     if(length(unique(param.values)) > 2) {
       thr <- param.values[round(length(param.values)/2, 0)] #middle point
       if(thr < ab_neg)
         thr <- ab_neg
       else if(thr > ab_pos)
         thr <- ab_pos
     }
     else {
       if(dir.thr == "low_neg")
         thr <- ma.param
       else
         thr <- mi.param
     }
     param.info <- c(mi.param, ma.param, thr)
     model_data <- compute_model(df, fmla, thr, dir.thr)  #return(list(df, model, auc, stats, best.thr))
     
     if(is.character(model_data))
       return(model_data)
     #get sliders info
     sl_pred <- get_min_max_vector(df$Pred)
     model_data <- append(model_data, list(fmla, param.info, sl_pred))
     if ('Age' %in% covar_list)
       model_data <- append(model_data, list(get_min_max_vector(df[ ,c('Age')])))
     return(model_data)
  } #Close is.null(df)
  return(NULL)
}
 
 
decimalplaces <- function(x) {
  if ((x %% 1) != 0) {
    strs <- strsplit(as.character(format(x, scientific = F)), "\\.")
    if(length(strs[[1]]) < 2)
      n <- 0
    else
      n <- nchar(strs[[1]][2])
  } 
  else
    n <- 0
  return(n) 
 }

min.decimalplaces <- function(x, min.d=3) {
  n <- 0
  if(x == 0)
    return(n)
  while(abs(x) < 1) { #ie. number is of the type 0.00.. or -0.00...
    x <- x * 10
    n <- n + 1
  }
  n <-  n + min.d - 1
  return(n)
}
 

 get_min_max_vector <- function(vect, md=3) {
   #dec <- as.numeric(lapply(vect, function(x) decimalplaces(x)))
   dec <- as.numeric(lapply(vect, function(x) min.decimalplaces(x)))
   maxdec <- max(dec, na.rm = T)
   maxdec <- max(maxdec, md) #maximum 3 decimals
   min_v <- round(min(vect, na.rm = T), digits = maxdec)
   max_v <- round(max(vect, na.rm = T), digits = maxdec)
   if(maxdec == 0)
     return(c(min_v, max_v, 1))
   else
     return(c(min_v, max_v, 10^(-maxdec)))
 }
 
annotation_compass <- function(label,
                               position = c('N','NE','E','SE','S','SW','W','NW'),
                               padding = grid::unit(c(0.5,0.5),"line"),...){
  position <- match.arg(position)
  x <- switch (position, N = 0.5, NE = 1, E = 1, SE = 1,
               S = 0.5, SW = 0, W = 0, NW = 0)
  y <- switch (position, N = 1, NE = 1, E = 0.5, SE = 0,
               S = 0, SW = 0, W = 0.5, NW = 1)
  hjust <- switch (position, N = 0.5, NE = 1, E = 1, SE = 1,
                   S = 0.5, SW = 0, W = 0, NW = 0)
  vjust <- switch (position, N = 1, NE = 1, E = 0.5, SE = 0,
                   S = 0, SW = 0, W = 0.5, NW = 1)
  f1 <- switch (position, N = 0, NE = -1, E = -1, SE = -1, 
                S = 0, SW = 1, W = 1, NW = 1)
  f2 <- switch (position, N = -1, NE = -1, E = 0, SE = 1,
                S = 1, SW = 1, W = 0,  NW = -1)
  annotation_custom(grid::textGrob(label, 
                                   x=grid::unit(x,"npc") + f1*padding[1] , 
                                   y=grid::unit(y,"npc") + f2*padding[2],
                                   hjust=hjust,vjust=vjust,gp=grid::gpar(cex=1), ...))
}
 

 