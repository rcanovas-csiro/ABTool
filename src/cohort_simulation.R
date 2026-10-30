##################################################
############### TAB: Simulation - Reference
##################################################


#### global variables
CS.data.ref <- reactiveValues(df = NULL, path = NULL, stats = NULL,
                              stat_index = 1,
                              threshold = 0, dir.thr = "low_neg",
                              min.ab = 0, max.ab = 100,
                              ab.param = "", marker="",
                              auc = "",
                              error_msg="")

CS.data.sim <- reactiveValues(mode= NULL,
                              value = 0,
                              prev = 0,
                              error_msg="")


#Uploading a cohort
observeEvent(input$CS.ref.file, {
  showModal(modalDialog("Loading...", footer=NULL, size = 's'))
  CS.data.ref$error_msg <- ""
  path <- NULL
  file <- isolate(input$CS.ref.file)
  if (is.null(file)) {
    CS.data.ref$error_msg <- "Please upload a csv file"
  }
  else {
    ext <- tools::file_ext(file$datapath) #get extension
    if(ext != "csv")
      CS.data.ref$error_msg <- "File must have be a csv"
    else {
      ldf <- upload_file(file$datapath)
      if(is.character(ldf))
        CS.data.ref$error_msg <- ldf
      else {
        CS.data.ref$df <- ldf
        CS.data.ref$path <- file$datapath
        markers <- as.data.frame(select_if(ldf, is.numeric))
        all_fields <- sort(colnames(markers))
        if(length(all_fields) == 0) #still check that it is not empty
          CS.data.ref$error_msg <- "There are no numerical columns to use as markers in the uploaded file"
        updateSelectInput(session, "CS.marker.sin", choices= unique(all_fields), selected = NULL)
        updateSelectInput(session, "CS.marker.rat", choices= unique(all_fields), selected = NULL)
        updateSelectInput(session, "CS.ab.param", choices= unique(all_fields), selected = NULL)
      }
    }
  }
  #whatever happened we need to restart the results tabs
  CS.data.sim$mode= NULL
  CS.data.sim$value = 0
  CS.data.sim$prev = 0
  CS.data.sim$error_msg=""
  if(CS.data.ref$error_msg != "") {
    showModal(modalDialog(title = "Error", CS.data.ref$error_msg,
                          easyClose = TRUE, size = 's'))
    CS.data.ref$df <- NULL
    CS.data.ref$stats <- NULL
    CS.data.ref$path <- NULL
    CS.data.ref$auc <- ""
    CS.data.ref$threshold <- 0
    CS.data.ref$min.ab <- 0
    CS.data.ref$max.ab <- 0
    CS.data.ref$dir.thr <- "low_neg"
    CS.data.ref$ab.param <- ""
    CS.data.ref$marker <- ""
    updateSelectInput(session, "CS.marker.sin", choices= NULL, selected = NULL)
    updateSelectInput(session, "CS.marker.rat", choices= NULL, selected = NULL)
  }
  else
    removeModal()
},
ignoreInit = TRUE)


# Filter selected data and check if it is valid
observeEvent(input$CS.ref.compute, { 
  showModal(modalDialog("Loading...", footer=NULL, size = 's'))
  CS.data.ref$error_msg <- ""
  if(is.null(CS.data.ref$path))
    CS.data.ref$error_msg <- "Please upload a csv file"
  else {
    participants <- isolate(input$CS.Diag.input)
    if(length(participants) == 0)
      CS.data.ref$error_msg <- "Please select at least one of the participant Diagnosis groups"
    else {
      #get ab parameter
      ab.param <- isolate(input$CS.ab.param)
      CS.data.ref$ab.param <- ab.param
      CS.data.ref$dir.thr <- isolate(input$CS.ab.dir)
      #get marker
      type_marker <- isolate(input$CS_marker_type) #isolate avoid reaction when element changes
      m1 <- isolate(input$CS.marker.sin)
      CS.data.ref$marker <- m1
      m2 <- ""
      if(type_marker == "ratio") {
        m2 <- isolate(input$CS.marker.rat)
        CS.data.ref$marker <- paste0(CS.data.ref$marker, "/", m2)
      }
      if(ab.param %in% c(m1,m2))    
        CS.data.ref$error_msg <- "Marker can not contain same variable as the Aβ paramater"
      if(m1 == m2)
        CS.data.ref$error_msg <- "The denominator marker cannot be the same as the numerator"
      if(CS.data.ref$error_msg == "") { #no error so far
        ldf <- run_analysis(ab.param, CS.data.ref$dir.thr, m1, m2, participants, CS.data.ref$path)
        if(is.character(ldf))
          CS.data.ref$error_msg <- ldf
        else{
          CS.data.ref$df <- ldf[[1]]
          CS.data.ref$stats <- ldf[[2]]
          CS.data.ref$auc <- ldf[[3]]
          thr.info <- ldf[[4]]
          CS.data.ref$threshold <- thr.info[3]
          CS.data.ref$min.ab <- thr.info[1]
          CS.data.ref$max.ab <- thr.info[2]
          mylab <-  paste0("Aβ Parameter Threshold (", ab.param,")")
          updateNumericInput(session, "CS.slider.ab.param", 
                             label = mylab, 
                             value = thr.info[3], 
                             min = thr.info[1], max = thr.info[2])
          #find stat_index of sensitivity ~0.95
          CS.data.ref$stat_index <- which.min(abs((CS.data.ref$stats)$sensitivity - 0.95))
          update_cs_sens(FALSE)
          updateSliderInput(session, "CS.slider.mes.sens", value = ((CS.data.ref$stats)$sensitivity)[CS.data.ref$stat_index])
          update_cs_sens(FALSE)
          updateSliderInput(session, "CS.slider.mes.spec", value = ((CS.data.ref$stats)$specificity)[CS.data.ref$stat_index])
        }
      }
    }
  } #close path is valid
  #whatever happened we need to restart the results tabs
  CS.data.sim$mode= NULL
  CS.data.sim$value = 0
  CS.data.sim$prev = 0
  CS.data.sim$error_msg=""
  if(CS.data.ref$error_msg != "") {
    showModal(modalDialog(title = "Error", CS.data.ref$error_msg,
                          easyClose = TRUE, size = 's'))
    #Note that the path is not removed as still is valid
    CS.data.ref$df <- NULL
    CS.data.ref$stats <- NULL
    CS.data.ref$auc <- ""
    CS.data.ref$marker <- ""
    CS.data.ref$stat_index <- 1
    CS.data.ref$threshold <- 0
    CS.data.ref$min.ab <- 0
    CS.data.ref$max.ab <- 100
    CS.data.ref$dir.thr <- "low_neg"
    CS.data.ref$ab.param <- ""
    update_cs_sens(FALSE)
    updateSliderInput(session, "CS.slider.mes.sens", value = 0.95)
    update_cs_spec(FALSE)
    updateSliderInput(session, "CS.slider.mes.spec", value = 0.95)
  }
  else
    removeModal()
},
ignoreInit = TRUE
)

#Add Validator for the threshold field 
CS.iv <- InputValidator$new()
CS.iv$add_rule("CS.slider.ab.param", sv_required())
CS.iv$add_rule("CS.slider.ab.param", function(value) {
  if(!is.numeric(value))
    return("Value must be a number")
  else if(value < CS.data.ref$min.ab | value > CS.data.ref$max.ab)
    return(paste0("Value must be between ", CS.data.ref$min.ab, " and ", CS.data.ref$max.ab))
  else
    return(NULL)
})
CS.iv$enable()


update_cs_ab <- reactiveVal(TRUE)  # weird trick to avoid double reacting
update_cs_sens <- reactiveVal(TRUE)  
update_cs_spec <- reactiveVal(TRUE)
 

observeEvent(input$CS.slider.ab.param, {
  thr <-  input$CS.slider.ab.param
  if(update_cs_ab()) {
    if(is.null(CS.data.ref$df)) {
      update_cs_ab(FALSE) #to avoid re triggering this observe
      updateNumericInput(session, "CS.slider.ab.param", value=CS.data.ref$threshold)
      update_cs_sens(FALSE)
      updateSliderInput(session, "CS.slider.mes.sens", value = 0.95)
      update_cs_sens(FALSE)
      updateSliderInput(session, "CS.slider.mes.spec", value = 0.95)
    }
    else {
      if (CS.iv$is_valid()) {   #check that threshold is valid first
        da <- compute_roc(CS.data.ref$df, thr, CS.data.ref$dir.thr)
        if(is.character(da)) {
          showModal(modalDialog(title = "Error", da, easyClose = TRUE, size = 's'))
          update_cs_ab(FALSE)
          updateNumericInput(session, "CS.slider.ab.param", value=CS.data.ref$threshold)
        }
        else {
          CS.data.ref$df <- da[[1]]
          CS.data.ref$stats <- da[[2]]
          CS.data.ref$auc <- da[[3]]
          CS.data.ref$threshold <- thr
          #Keep sensitivity change specificity if required
          value <- isolate(input$CS.slider.mes.sens) 
          CS.data.ref$stat_index <- which.min(abs((CS.data.ref$stats)$sensitivity - value))
          #update sensitivity and specificity sliders
          update_cs_sens(FALSE)
          updateSliderInput(session, "CS.slider.mes.sens", 
                            value = ((CS.data.ref$stats)$sensitivity)[CS.data.ref$stat_index])
          update_cs_sens(FALSE)
          updateSliderInput(session, "CS.slider.mes.spec", 
                            value = ((CS.data.ref$stats)$specificity)[CS.data.ref$stat_index])
        }
      }
    }
  }
  else
    update_cs_ab(TRUE) #open again
 },
 ignoreInit = TRUE
)


observeEvent(input$CS.slider.mes.sens, {
  value.sens <-  input$CS.slider.mes.sens
  if(update_cs_sens()) {
    if(is.null(CS.data.ref$df)) {
      update_cs_sens(FALSE) #to avoid re triggering this observe
      updateSliderInput(session, "CS.slider.mes.sens", value=0.95)
      update_cs_spec(FALSE)
      updateSliderInput(session, "CS.slider.mes.spec", value=0.95)
    }
    else {
      CS.data.ref$stat_index <- which.min(abs((CS.data.ref$stats)$sensitivity - value.sens))
      update_cs_spec(FALSE)
      updateSliderInput(session, "CS.slider.mes.spec", 
                        value = ((CS.data.ref$stats)$specificity)[CS.data.ref$stat_index])
    }
  }
  else
    update_cs_sens(TRUE) #open again
},
ignoreInit = TRUE
)
 
   
observeEvent(input$CS.slider.mes.spec, {
  value.spec <-  input$CS.slider.mes.spec
  if(update_cs_spec()) {
    if(is.null(CS.data.ref$df)) {
      update_cs_sens(FALSE) #to avoid re triggering this observe
      updateSliderInput(session, "CS.slider.mes.sens", value=0.95)
      update_cs_spec(FALSE)
      updateSliderInput(session, "CS.slider.mes.spec", value=0.95)
    }
    else {
      CS.data.ref$stat_index <- which.min(abs((CS.data.ref$stats)$specificity - value.spec))
      update_cs_sens(FALSE)
      updateSliderInput(session, "CS.slider.mes.sens", 
                        value = ((CS.data.ref$stats)$sensitivity)[CS.data.ref$stat_index])
    }
  }
  else
    update_cs_spec(TRUE) #open again
},
ignoreInit = TRUE
)
 
 
#General stats Confusion Matrix
output$CS.gens.confM <- renderPlot({
  if(!is.null(CS.data.ref$stats)) {
    i_ss <- CS.data.ref$stat_index
    c_sens <- ((CS.data.ref$stats)$sensitivity)[i_ss]
    c_sens <- format(round(c_sens, 2), nsmall = 2)
    c_spec <- ((CS.data.ref$stats)$specificity)[i_ss]
    c_spec <- format(round(c_spec, 2), nsmall = 2)
    tp <- ((CS.data.ref$stats)$tp)[i_ss]
    tn <- ((CS.data.ref$stats)$tn)[i_ss]
    fp <- ((CS.data.ref$stats)$fp)[i_ss]
    fn <- ((CS.data.ref$stats)$fn)[i_ss]
    labels <- c(paste0('TP: ', tp, " (", format(round(100*tp/(tp+fn), 1), nsmall=1),  "%)"),
                paste0('FP: ', fp, " (", format(round(100*fp/(tn+fp), 1), nsmall=1),"%)"),
                paste0('FN: ', fn, " (", format(round(100*fn/(tp+fn), 1), nsmall=1),"%)"),
                paste0('TN: ', tn, " (", format(round(100*tn/(tn+fp), 1), nsmall=1),"%)"))
    df_plot <- data.frame(x = factor(c('Aβ+', 'Aβ+', 'Aβ-', 'Aβ-'),
                                     ordered=FALSE,
                                     levels = c('Aβ+', 'Aβ-')),
                          y = c('Aβ+', 'Aβ-', 'Aβ+', 'Aβ-'),
                          color = c('A', 'B', 'B', 'A'),
                          labs= labels)
    g <- ggplot(df_plot, aes(x = x, y = y, fill = color)) +
      geom_tile() + geom_text(aes(label=labs), size=4) +
      scale_fill_manual(values=c("#feb24c", "#e5f5e0")) +
      xlab("Predicted Class") + ylab("True Class") +
      scale_x_discrete(position = "top") +
      theme(axis.text.y = element_text(angle=90, size = 12, hjust = 0.5),
            axis.text.x = element_text(size = 12),
            axis.ticks = element_blank(),
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.title = element_text(size = 13, face="bold"))
    g
  }
},
height = function() {min(session$clientData$output_CS.gens.confM_width, 300)}
)
 
 
# Sensitivity plot (ROC curve)
output$CS.PlotSens <- renderPlot({
  df <- CS.data.ref$stats
  if(is.null(df))
    return(NULL)
  df <- data.frame(Sensitivity = df$sensitivity,
                   Specificity = df$specificity)
  i_ss <- CS.data.ref$stat_index
  c_sens <- (df$Sensitivity)[i_ss]
  c_spec <- (df$Specificity)[i_ss]
  g <- ggplot(df, aes(x=1-Specificity, y=Sensitivity)) +
    geom_line(colour = "cadetblue4", size=0.25) +
    geom_abline(colour = "red", linetype = "solid", alpha = 0.3, size = 1) +
    geom_segment(aes(x = (1-c_spec), y = 0, xend = (1-c_spec), yend = c_sens),
                 linetype = "dashed", color = "black", size = 0.5) +
    geom_segment(aes(x = 0, y = c_sens, xend = (1-c_spec), yend = c_sens),
                 linetype = "dashed", color = "black", size = 0.5) +
    geom_point(aes(x= (1- c_spec), y = c_sens), colour="cadetblue4", size = 3) +
    labs(title = paste0("AUC (95% CI): ", CS.data.ref$auc)) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 13, face="bold"),
      axis.text = element_text(size = 12)
    )
  g
},
height = function() {min(session$clientData$output_CS.PlotSens_width, 300)}
)
 
 
##################################################
############### TAB: Simulation - Hypothetical
##################################################

observeEvent(input$CS.sim.compute, {
  CS.data.sim$error_msg <- ""
  showModal(modalDialog("Calculating...", footer=NULL, size = 's'))
  #check that a valid reference is already loaded
  if(is.null(CS.data.ref$stats)) {CS.data.sim$error_msg <- "You nee to load a valid reference first"}
  else {
    #check if values are valid
    prev <- isolate(input$CS.prevalence)
    if (!is.numeric(prev)){CS.data.sim$error_msg <- "Prevalence must be a numer in (0,1)"}
    else if(prev <= 0) {CS.data.sim$error_msg <- "Prevalence must be higher than 0"}
    else if (prev >= 1) {CS.data.sim$error_msg <- "Prevalence must be lower than 1"}
    else {
      choise <- isolate(input$CS.sim.choise)
      value <- isolate(input$CS.sample.size)
      if (!is.integer(value)){CS.data.sim$error_msg <- "Value must be an integer"}
      else if(choise == "sam_size"){
        if (prev * value < 10){CS.data.sim$error_msg <- "Sample size is too small for the given prevalence"}
        else {
          CS.data.sim$prev <- prev
          CS.data.sim$value <- value
          CS.data.sim$mode <- "sam_size"
        }
      }
      else {
        if (value < 10){CS.data.sim$error_msg <- "Number of cases is too small, it must be higher than 10"}
        else {
          CS.data.sim$prev <- prev
          CS.data.sim$value <- value
          CS.data.sim$mode <- "min_cases"
        }
      }
    }
  }
  if(CS.data.sim$error_msg != "")
    showModal(modalDialog(title = "Error", CS.data.sim$error_msg,
                          easyClose = TRUE, size = 's'))
  else
    removeModal()
},
ignoreInit = TRUE
)

 
# Confusion Matrix Sim
output$CS.res.confM <- renderPlot({
  stats <- CS.data.ref$stats
  #we have a valid reference
  if(is.null(stats))
    return(NULL)
  #we have valid inputs
  if(is.null(CS.data.sim$mode) | CS.data.sim$error_msg != "")
    return(NULL)
  #calculate the stats
  df <- data.frame(Sensitivity = stats$sensitivity,
                   Specificity = stats$specificity)  #this is just to be sure that it is a df

  #get closer value
  i_ss <- CS.data.ref$stat_index
  c_sens <- (df$Sensitivity)[i_ss]
  c_spec <- (df$Specificity)[i_ss]
  if(CS.data.sim$mode == "sam_size") {
    results <- simulate_data_sample(CS.data.sim$prev, CS.data.sim$value, c_sens, c_spec)
  }
  else {
    results <- simulate_data_tp(CS.data.sim$prev, CS.data.sim$value, c_sens, c_spec)
  }
  tp <- results[2]
  fp <- results[3]
  fn <- results[4]
  tn <- results[5]
  labels <- c(paste0('TP: ', tp, " (", format(round(100*tp/(tp+fn), 1), nsmall=1),  "%)"),
              paste0('FP: ', fp, " (", format(round(100*fp/(tn+fp), 1), nsmall=1),"%)"),
              paste0('FN: ', fn, " (", format(round(100*fn/(tp+fn), 1), nsmall=1),"%)"),
              paste0('TN: ', tn, " (", format(round(100*tn/(tn+fp), 1), nsmall=1),"%)"))
  df_plot <- data.frame(x = factor(c('Aβ+', 'Aβ+', 'Aβ-', 'Aβ-'),
                                   ordered=FALSE,
                                   levels = c('Aβ+', 'Aβ-')),
                        y = c('Aβ+', 'Aβ-', 'Aβ+', 'Aβ-'),
                        color = c('A', 'B', 'B', 'A'),
                        labs= labels)
  g <- ggplot(df_plot, aes(x = x, y = y, fill = color)) +
    geom_tile() + geom_text(aes(label=labs), size=4) +
    scale_fill_manual(values=c("#feb24c", "#e5f5e0")) +
    xlab("Predicted Class") + ylab("True Class") +
    scale_x_discrete(position = "top") +
    theme(axis.text.y = element_text(angle=90, size = 12, hjust = 0.5),
          axis.text.x = element_text(size = 12),
          axis.ticks = element_blank(),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_text(size = 14, face="bold"))
  text <- ggdraw() + draw_label(paste0("Total Sample size: ", results[1]), fontface='bold', size=20)
  plot_grid(g, text, ncol=1, rel_heights=c(1, 0.1))
},
height = function() {min(session$clientData$output_CS.res.confM_width, 300)}
)
 
 
##################################################
############### TAB: Summary Results
##################################################

#Data Table General Stats
output$CS.summ.stats <- renderUI({
  df <- data.frame(ST=c("Cohort Size", "CN", "MCI", "AD", "Marker", "Aβ Param. Threshold",
                        "Aβ+ Prevalence", "(Aβ+/Aβ-)", "Pred. (Aβ+/Aβ-)" ,
                        "Sensitivity", "Specifity", "PPV/NPV (%)"))
  df$RC <- rep("-", 12)
  df$SC <- rep("-", 12)
  if(!is.null(CS.data.ref$stats)) {  #reference data
    cohort <- CS.data.ref$df
    CN <- nrow(cohort[cohort$Diagnosis == 'CN', ])
    MCI <- nrow(cohort[cohort$Diagnosis == 'MCI', ])
    AD <- nrow(cohort[cohort$Diagnosis == 'AD', ])
    i_ss <- CS.data.ref$stat_index
    r_sens <- ((CS.data.ref$stats)$sensitivity)[i_ss]
    r_spec <- ((CS.data.ref$stats)$specificity)[i_ss]
    tp <- ((CS.data.ref$stats)$tp)[i_ss]
    tn <- ((CS.data.ref$stats)$tn)[i_ss]
    fp <- ((CS.data.ref$stats)$fp)[i_ss]
    fn <- ((CS.data.ref$stats)$fn)[i_ss]
    N <- tp + tn + fp + fn
    ppv <-  format(100 * round(tp/(tp+fp), 2), nsmall = 2)
    npv <-  format(100 * round(tn/(tn+fn), 2), nsmall = 2)
    prev <- format(round((tp+fn)/N, 3), nsmall = 3)
    refer <- c(N, CN, MCI, AD, CS.data.ref$marker, 
               CS.data.ref$threshold, prev, paste0(tp+fn,"/",tn+fp),
               paste0(tp+fp,"/",tn+fn),
               paste0("aprox. ", format(round(r_sens, 2), nsmall = 2)),
               paste0("aprox. ", format(round(r_spec, 2), nsmall = 2)),
               paste0(ppv,"/", npv))
    if(!is.null(CS.data.sim$mode)) { # Simulated cohort
      if(CS.data.sim$mode == "sam_size")
        results <- simulate_data_sample(CS.data.sim$prev, CS.data.sim$value, r_sens, r_spec)
      else
        results <- simulate_data_tp(CS.data.sim$prev, CS.data.sim$value, r_sens, r_spec)
      tp <- results[2]
      fp <- results[3]
      fn <- results[4]
      tn <- results[5]
      sens <- tp/(tp+fn)
      spec <- tn/(tn+fp)
      ppv <-  format(round(100 * tp/(tp+fp), 2), nsmall = 2)
      npv <-  format(round(100 * tn/(tn+fn), 2), nsmall = 2)
      sim <- c(results[1], "-", "-","-", 
               CS.data.ref$marker, CS.data.ref$threshold, 
               CS.data.sim$prev, paste0(tp+fn,"/",tn+fp),
               paste0(tp+fp,"/",tn+fn),
               paste0("aprox. ", format(round(sens, 2), nsmall = 2)),
               paste0("aprox. ", format(round(spec, 2), nsmall = 2)),
               paste0(ppv,"/", npv))
      df$SC <- sim
    }
    df$RC <- refer
  }
  flextable(df) %>%
    width(j=(1:3), width=3) %>%
    set_header_labels(ST="Stats", RC="Reference Cohort", SC="Simulated Cohort") %>%
    border_remove() %>%  hline_top(border=fp_border(width=2, color="black"), part="header") %>%
    hline(i=c(1), border=fp_border(width=2, color="#007FA6"), part="header") %>%
    hline(i=c(nrow(df)), border=fp_border(width=2, color="black"), part="body") %>%
    flextable::align(align = "center", j = c(2:3), part = "all")  %>%
    style(pr_t=fp_text(font.family= "roboto")) %>%
    fontsize(i = 1, size = 9, part = "header") %>%   # adjust font size of header
    bold(i = 1, bold = TRUE, part = "header") %>%     # adjust bold face of header
    fontsize(part="body", size = 11) %>%
    bold(i = c(1, 5:nrow(df)), j = 1, bold = TRUE, part = "body") %>%
    htmltools_value()
})
