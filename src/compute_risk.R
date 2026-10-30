
# #### global variables
RC.data.model <- reactiveValues(df = NULL, model = NULL, path = NULL,
                             stats = NULL, #threshold,sensitivity,specificity,ppv,npv,youden,tn,tp,fn,fp
                             index.sens = 1, index.spec = 1,
                             fmla = "", marker="", auc = 0,
                             marker.stats = NULL, 
                             marker.sens.idx = 1, marker.spec.idx = 1, marker.dir = "low_neg",
                             threshold = 0, dir.thr = "low_neg",
                             min.ab = 0, max.ab = 100,
                             ab.param = "",
                             covars = NULL,
                             age.range = NULL,
                             error_msg="")

RC.data.ind <- reactiveValues(risk ="",
                              value="")


reset.data.model <- function(path=NULL) {
  RC.data.model$df <- NULL
  RC.data.model$stats <- NULL
  RC.data.model$index.sens <- 1
  RC.data.model$index.spec <- 1
  RC.data.model$model <- NULL
  RC.data.model$path <- path
  RC.data.model$fmla <- ""
  RC.data.model$auc <- 0
  RC.data.model$marker <- ""
  RC.data.model$covars <- NULL
  RC.data.model$marker.stats <- NULL
  RC.data.model$marker.sens.idx <- 1
  RC.data.model$marker.spec.idx <- 1
  RC.data.model$marker.dir <- "low_neg"
  RC.data.model$threshold <- 0
  RC.data.model$min.ab <- 0
  RC.data.model$max.ab <- 0
  RC.data.model$dir.thr <- "low_neg"
  RC.data.model$ab.param <- ""
  RC.data.model$age.range <- NULL
}

#Uploading a cohort
observeEvent(input$RC.ref.file, {
  showModal(modalDialog("Loading...", footer=NULL, size = 's'))
  RC.data.model$error_msg <- ""
  path <- NULL
  file <- isolate(input$RC.ref.file)
  if (is.null(file)) {
    RC.data.model$error_msg <- "Please upload a csv file"
  }
  else {
    ext <- tools::file_ext(file$datapath) #get extension
    if(ext != "csv")
      RC.data.model$error_msg <- "File must have be a csv"
    else {
      ldf <- upload_file(file$datapath)
      if(is.character(ldf))
        RC.data.model$error_msg <- ldf
      else {
        RC.data.model$df <- ldf
        RC.data.model$path <- file$datapath
        markers <- as.data.frame(select_if(ldf, is.numeric))
        all_fields <- sort(colnames(markers))
        if(length(all_fields) == 0) #still check that it is not empty
          RC.data.model$error_msg <- "There are no numerical columns to use as markers in the uploaded file"
        updateSelectInput(session, "RC.marker.sin", choices= unique(all_fields), selected = NULL)
        updateSelectInput(session, "RC.marker.rat", choices= unique(all_fields), selected = NULL)
        updateSelectInput(session, "RC.ab.param", choices= unique(all_fields), selected = NULL)
      }
    }
  }
  if(RC.data.model$error_msg != "") {
    showModal(modalDialog(title = "Error", RC.data.model$error_msg,
                          easyClose = TRUE, size = 's'))
    reset.data.model()
    updateSelectInput(session, "RC.marker.sin", choices= NULL, selected = NULL)
    updateSelectInput(session, "RC.marker.rat", choices= NULL, selected = NULL)
  }
  else
    removeModal()
},
ignoreInit = TRUE)

# Filter selected data and check if it is valid
observeEvent(input$RC.create.model, {
  showModal(modalDialog("Loading...", footer=NULL, size = 's'))
  RC.data.model$error_msg <- ""
  if(is.null(RC.data.model$path))
    RC.data.model$error_msg <- "Please upload a csv file"
  else {
    participants <- isolate(input$RC.Diag.input)
    if(length(participants) == 0)
      RC.data.model$error_msg <- "Please select at least one of the participant Diagnosis groups"
    else {
      #get ab parameter
      ab.param <- isolate(input$RC.ab.param)
      RC.data.model$ab.param <- ab.param
      RC.data.model$dir.thr <- isolate(input$RC.ab.dir)
      #get marker
      type_marker <- isolate(input$RC_marker_type) #isolate avoid reaction when element changes
      m1 <- isolate(input$RC.marker.sin)
      RC.data.model$marker <- m1
      m2 <- ""
      if(type_marker == "ratio") {
        m2 <- isolate(input$RC.marker.rat)
        RC.data.model$marker <- paste0(RC.data.model$marker, "/", m2)
      }
      if(ab.param %in% c(m1,m2))    
        RC.data.model$error_msg <- "Marker can not contain same variable as the Aβ paramater"
      if(m1 == m2)
        RC.data.model$error_msg <- "The denominator marker cannot be the same as the numerator"
      if(RC.data.model$error_msg == "") { #no error so far
        #get covariables to be used
        covars <- isolate(input$RC.covars)
        covars <- replace(covars, covars=="APOE e4", "apoe4")
        covars <- covars[!covars %in% c(m1, m2, ab.param)]  #avoid using as covar part of the marker
        ldf <- create_model(ab.param, RC.data.model$dir.thr, m1, m2, 
                            participants, covars, path=RC.data.model$path)
        if(is.character(ldf))
          RC.data.model$error_msg <- ldf
        else{
          RC.data.model$covar <- covars
          RC.data.model$df <- ldf[[1]]
          RC.data.model$model <- ldf[[2]]
          RC.data.model$auc <- ldf[[3]]
          RC.data.model$stats <- ldf[[4]]
          RC.data.model$marker.stats <- ldf[[5]]
          RC.data.model$fmla <- ldf[[6]]
          thr.info <- ldf[[7]]
          RC.data.model$threshold <- thr.info[3]
          RC.data.model$min.ab <- thr.info[1]
          RC.data.model$max.ab <- thr.info[2]
          mylab <-  paste0("Aβ Parameter Threshold (", ab.param,")")
          updateNumericInput(session, "RC.slider.ab.param", 
                             label = mylab, 
                             value = thr.info[3], 
                             min = thr.info[1], max = thr.info[2])
          #change sliders to values depending on the data imputed (force them to not have more that 2 decimals)
          sl_marker <- ldf[[8]]
          #COMPUTE direction of marker Pred
          df <- RC.data.model$df
          RC.data.model$marker.dir <- "low_neg"
          if(RC.data.model$dir.thr == "low_neg") {
            m.abneg <- mean(df[df$AB.PARAM < RC.data.model$threshold, ]$Pred)
            m.abpos <- mean(df[df$AB.PARAM >= RC.data.model$threshold, ]$Pred)
          }
          else{
            m.abneg <- mean(df[df$AB.PARAM > RC.data.model$threshold, ]$Pred)
            m.abpos <- mean(df[df$AB.PARAM <= RC.data.model$threshold, ]$Pred)
          }
          if(m.abpos < m.abneg)
            RC.data.model$marker.dir <- "high_neg"
          updateSliderInput(session, "RC.marker.val",
                            label = paste0(RC.data.model$marker, " Value"),
                            value = sl_marker[1],
                            min = sl_marker[1], max = sl_marker[2], step = sl_marker[3])
      
          #Marker Sens and Spec
          pred <- sort((RC.data.model$df)$Pred)
          stats <- RC.data.model$marker.stats
          qu <- quantile(pred)
          RC.data.model$marker.sens.idx <-  which.min(abs(stats$threshold - qu[2])) #25%
          RC.data.model$marker.spec.idx <-  which.min(abs(stats$threshold - qu[4])) #75%
          if(RC.data.model$marker.dir == "high_neg") {
            idx.aux <- RC.data.model$marker.sens.idx
            RC.data.model$marker.sens.idx <- RC.data.model$marker.spec.idx
            RC.data.model$marker.spec.idx <- idx.aux
          }
          RC.upd.marker.sens(FALSE)
          updateSliderInput(session, "RC.marker.sens", 
                            value = (stats$sensitivity)[RC.data.model$marker.sens.idx])
          RC.upd.marker.spec(FALSE)
          updateSliderInput(session, "RC.marker.spec",
                            value = (stats$specificity)[RC.data.model$marker.spec.idx])
          #Model Sens and Spec
          pred <- sort((RC.data.model$df)$pred.results)
          stats <- RC.data.model$stats
          qu <- quantile(pred)
          RC.data.model$index.sens <-  which.min(abs(stats$threshold - qu[2])) #25%
          RC.data.model$index.spec <-  which.min(abs(stats$threshold - qu[4])) #75%
          RC.upd.sens(FALSE)
          updateSliderInput(session, "RC.sens", 
                            value = (stats$sensitivity)[RC.data.model$index.sens])
          RC.upd.spec(FALSE)
          updateSliderInput(session, "RC.spec", 
                            value = (stats$specificity)[RC.data.model$index.spec])
          
          #enable and disable or hide/show options
          if("Age" %in% covars){
            RC.data.model$age.range <- ldf[[9]]
            sl_age <- ldf[[9]]
            updateNumericInput(session, "RC.age.ind",
                               value = sl_age[1],
                               min = sl_age[1], max = sl_age[2])
            shinyjs::show("RC.age.ind")
          }
          else{shinyjs::hide("RC.age.ind")}
          if("Gender" %in% covars){shinyjs::show("RC.gender.ind")} #we assume that we always have males and females
          else{shinyjs::hide("Rs_gender_ind")}
          if("apoe4" %in% covars){shinyjs::show("RC.apoe.ind")}  # we assumed that we always have apoe 0 and 1
          else{shinyjs::hide("RC.apoe.ind")}
        }
      }
    }
  } #close is.null(data.model$df)

  if(RC.data.model$error_msg != "") {
    showModal(modalDialog(title = "Error", RC.data.model$error_msg, easyClose = TRUE, size = 's'))
    reset.data.model(path = RC.data.model$path)
    updateSliderInput(session, "RC.sens", value = 0.9)
    updateSliderInput(session, "RC.specs", value = 0.9)
  }
  else {
    shinyjs::show("RC.marker.val")
    removeModal()
  }
},
ignoreInit = TRUE
)


RC.upd.ab <- reactiveVal(TRUE)

RC.iv <- InputValidator$new()
RC.iv$add_rule("RC.slider.ab.param", sv_required())
RC.iv$add_rule("RC.slider.ab.param", function(value) {
  if(!is.numeric(value))
    return("Value must be a number")
  else if(value < RC.data.model$min.ab | value > RC.data.model$max.ab)
    return(paste0("Value must be between ", RC.data.model$min.ab, " and ", RC.data.model$max.ab))
  else
    return(NULL)
})

RC.iv$enable()



observeEvent(input$RC.slider.ab.param, {
  thr <-  isolate(input$RC.slider.ab.param)
  if(RC.upd.ab()) {
    if(is.null(RC.data.model$df)) {
      RC.upd.ab(FALSE) #to avoid re triggering this observe
      updateNumericInput(session, "RC.slider.ab.param", value=RC.data.model$threshold)
    }
    else {
      if (RC.iv$is_valid()) { 
        dm <- compute_model(RC.data.model$df, RC.data.model$fmla, thr, RC.data.model$dir.thr)
        if(is.character(dm)) {
          showModal(modalDialog(title = "Error", dm, easyClose = TRUE, size = 's'))
          RC.upd.ab(FALSE)
          updateNumericInput(session, "RC.slider.ab.param", value=RC.data.model$threshold)
        }
        else {
          RC.data.model$df <- dm[[1]]
          RC.data.model$model <- dm[[2]]
          RC.data.model$auc <- dm[[3]]
          RC.data.model$stats <- dm[[4]]
          RC.data.model$marker.stats <- dm[[5]]
          RC.data.model$threshold <- thr
          #Update Marker Sens and Spec
          pred <- sort((RC.data.model$df)$Pred)
          stats <- RC.data.model$marker.stats
          qu <- quantile(pred)
          RC.data.model$marker.sens.idx <-  which.min(abs(stats$threshold - qu[2])) #25%
          RC.data.model$marker.spec.idx <-  which.min(abs(stats$threshold - qu[4])) #75%
          #update marker direction
          df <- dm[[1]]
          RC.data.model$marker.dir <- "low_neg"
          if(RC.data.model$dir.thr == "low_neg") {
            m.abneg <- mean(df[df$AB.PARAM < RC.data.model$threshold, ]$Pred)
            m.abpos <- mean(df[df$AB.PARAM >= RC.data.model$threshold, ]$Pred)
          }
          else{
            m.abneg <- mean(df[df$AB.PARAM > RC.data.model$threshold, ]$Pred)
            m.abpos <- mean(df[df$AB.PARAM <= RC.data.model$threshold, ]$Pred)
          }
          if(m.abpos < m.abneg)
            RC.data.model$marker.dir <- "high_neg"
          
          
          if(RC.data.model$marker.dir == "high_neg") {
            idx.aux <- RC.data.model$marker.sens.idx
            RC.data.model$marker.sens.idx <- RC.data.model$marker.spec.idx
            RC.data.model$marker.spec.idx <- idx.aux
          }
          RC.upd.marker.sens(FALSE)
          updateSliderInput(session, "RC.marker.sens",
                            value = (stats$sensitivity)[RC.data.model$marker.sens.idx])
          RC.upd.marker.spec(FALSE)
          updateSliderInput(session, "RC.marker.spec",
                            value = (stats$specificity)[RC.data.model$marker.spec.idx])
          #Model Sens and Spec
          pred <- sort((RC.data.model$df)$pred.results)
          stats <- RC.data.model$stats
          qu <- quantile(pred)
          RC.data.model$index.sens <-  which.min(abs(stats$threshold - qu[2])) #25%
          RC.data.model$index.spec <-  which.min(abs(stats$threshold - qu[4])) #75%
          RC.upd.sens(FALSE)
          updateSliderInput(session, "RC.sens",
                            value = (stats$sensitivity)[RC.data.model$index.sens])
          RC.upd.spec(FALSE)
          updateSliderInput(session, "RC.spec",
                            value = (stats$specificity)[RC.data.model$index.spec])
        }
      }
    }
  }
  else
    RC.upd.ab(TRUE) #open again
},
ignoreInit = TRUE
)


#### Sensitivity and Specificity Slider

RC.upd.sens <- reactiveVal(TRUE)  
RC.upd.spec <- reactiveVal(TRUE)
RC.upd.marker.sens <- reactiveVal(TRUE)
RC.upd.marker.spec <- reactiveVal(TRUE)

observeEvent(input$RC.sens, {
  value.sens <-  input$RC.sens
  if(RC.upd.sens()) {
    if(is.null(RC.data.model$model)) {
      RC.upd.sens(FALSE)
      updateSliderInput(session, "RC.sens", value = 0.9)
    }
    else {#risk plot defined areas
      thr.spec <- ((RC.data.model$stats)$threshold)[RC.data.model$index.spec]
      idx.thr <- which.min(abs((RC.data.model$stats)$sensitivity - value.sens))
      thr.sens <- ((RC.data.model$stats)$threshold)[idx.thr]
      error <- ""
      #biomarker plot defined areas
      if (thr.sens > thr.spec) {
        error <- "Specificity and Sensitivity selected cutoff overlap on APS Density Plot"
        RC.upd.sens(FALSE)
        updateSliderInput(session, "RC.sens", 
                          value = ((RC.data.model$stats)$sensitivity)[RC.data.model$index.sens])
      }
      if(error == "") 
        RC.data.model$index.sens <- idx.thr       
      else 
        showModal(modalDialog(title = "Error", error, easyClose = TRUE, size = 's'))
    }
  }
  else
    RC.upd.sens(TRUE) #open again
}, 
ignoreInit = TRUE
)


observeEvent(input$RC.spec, {
  value.spec <-  input$RC.spec
  if(RC.upd.spec()) {
    if(is.null(RC.data.model$model)) {
      RC.upd.spec(FALSE)
      updateSliderInput(session, "RC.spec", value = 0.9)
    }
    else {
      thr.sens <- ((RC.data.model$stats)$threshold)[RC.data.model$index.sens]
      idx.thr <- which.min(abs((RC.data.model$stats)$specificity - value.spec))
      thr.spec <- ((RC.data.model$stats)$threshold)[idx.thr]
      error <- ""
      #biomarker plot defined areas
      if (thr.sens > thr.spec) {
        error <- "Specificity and Sensitivity selected cutoff overlap on APS Density Plot"
        RC.upd.spec(FALSE)
        updateSliderInput(session, "RC.spec", 
                          value = ((RC.data.model$stats)$specificity)[RC.data.model$index.spec])
      }
      if(error == "") 
        RC.data.model$index.spec <- idx.thr
      else 
        showModal(modalDialog(title = "Error", error, easyClose = TRUE, size = 's'))
    }
  }
  else
    RC.upd.spec(TRUE) #open again
},
ignoreInit = TRUE
)


observeEvent(input$RC.marker.sens, {
  value.sens <-  input$RC.marker.sens
  if(RC.upd.marker.sens()) {
    if(is.null(RC.data.model$model)) {
      RC.upd.marker.sens(FALSE)
      updateSliderInput(session, "RC.marker.sens", value = 0.9)
    }
    else {#risk plot defined areas
      dir <- RC.data.model$marker.dir
      thr.spec <- ((RC.data.model$marker.stats)$threshold)[RC.data.model$marker.spec.idx]
      idx.thr <- which.min(abs((RC.data.model$marker.stats)$sensitivity - value.sens))
      thr.sens <- ((RC.data.model$marker.stats)$threshold)[idx.thr]
      error <- ""
      #biomarker plot defined areas
      if ((dir == "low_neg" & (thr.sens > thr.spec)) |
          (dir == "high_neg" & (thr.sens < thr.spec))){
        error <- "Specificity and Sensitivity selected cutoff overlap on Biomarker Density Plot"
        RC.upd.marker.sens(FALSE)
        updateSliderInput(session, "RC.marker.sens", 
                          value = ((RC.data.model$marker.stats)$sensitivity)[RC.data.model$marker.sens.idx])
      }
      if(error == "") 
        RC.data.model$marker.sens.idx <- idx.thr       
      else 
        showModal(modalDialog(title = "Error", error, easyClose = TRUE, size = 's'))
    }
  }
  else
    RC.upd.marker.sens(TRUE) #open again
}, 
ignoreInit = TRUE
)


observeEvent(input$RC.marker.spec, {
  value.spec <-  input$RC.marker.spec
  if(RC.upd.marker.spec()) {
    if(is.null(RC.data.model$model)) {
      RC.upd.marker.spec(FALSE)
      updateSliderInput(session, "RC.marker.spec", value = 0.9)
    }
    else {
      dir <- RC.data.model$marker.dir
      thr.sens <- ((RC.data.model$marker.stats)$threshold)[RC.data.model$marker.sens.idx]
      idx.thr <- which.min(abs((RC.data.model$marker.stats)$specificity - value.spec))
      thr.spec <- ((RC.data.model$marker.stats)$threshold)[idx.thr]
      error <- ""
      #biomarker plot defined areas
      if ((dir == "low_neg" & (thr.sens > thr.spec)) |
          (dir == "high_neg" & (thr.sens < thr.spec))){
        error <- "Specificity and Sensitivity selected cutoff overlap on Biomarker Density Plot"
        RC.upd.marker.spec(FALSE)
        updateSliderInput(session, "RC.marker.spec", 
                          value = ((RC.data.model$marker.stats)$specificity)[RC.data.model$marker.spec.idx])
      }
      if(error == "") 
        RC.data.model$marker.spec.idx <- idx.thr
      else 
        showModal(modalDialog(title = "Error", error, easyClose = TRUE, size = 's'))
    }
  }
  else
    RC.upd.marker.spec(TRUE) #open again
},
ignoreInit = TRUE
)


#### PLOTS 

RC.iv.age <- InputValidator$new()
RC.iv.age$add_rule("RC.age.ind", sv_required())
RC.iv.age$add_rule("RC.age.ind", function(value) {
  if(!is.null(RC.data.model$age.range)) {
    if(!is.numeric(value))
      return("Value must be a number")
    else if(value < RC.data.model$age.range[1] | value > RC.data.model$age.range[2])
      return(paste0("Value must be between ", RC.data.model$age.range[1], " and ", RC.data.model$age.range[2]))
  }
  return(NULL)
})
RC.iv.age$enable()

 
#Marker Plot
output$RC.marker.plot <- renderPlot({
  df <- RC.data.model$df
  stats <- RC.data.model$marker.stats
  if(is.null(RC.data.model$model))
    return(NULL)

  #Risk cutoff depending on Sens and Spec
  sens.cutoff <- round(100 * (stats$sensitivity)[RC.data.model$marker.sens.idx], 0)
  spec.cutoff <- round(100 * (stats$specificity)[RC.data.model$marker.spec.idx], 0)
  sens_line <- (stats$threshold)[RC.data.model$marker.sens.idx]
  spec_line <-(stats$threshold)[RC.data.model$marker.spec.idx]

  min.m <- min(df$Pred)
  max.m <- max(df$Pred)

  dens <- density(df$Pred, from=min.m, to=max.m)
  plot_data <- data.frame(x=dens$x, y=dens$y)
  y.text <- (max(plot_data$y) + min(plot_data$y)) / 2 #so text is within the plot
  #get direction of the marker
  col.sens <- '#99d8c9'
  col.spec <- '#fc9272'
  co.plot <- c("#99d8c9", "grey", "#fc9272")
  if(RC.data.model$marker.dir == "high_neg") {
    co.plot <- c("#fc9272", "grey", "#99d8c9")
    plot_data$quant <- factor(findInterval(plot_data$x, c(spec_line, sens_line)))
  }
  else
    plot_data$quant <- factor(findInterval(plot_data$x, c(sens_line, spec_line)))
  
  dec <- min.decimalplaces(min(sens_line, spec_line), min.d=3)
  se.line <- round(sens_line, dec)
  sp.line <- round(spec_line, dec)
  greyzone <- paste0(min(se.line , sp.line), " - ", max(se.line , sp.line),", ",
                    round((abs(sens_line - spec_line))/(max.m - min.m)*100, 2), "%")
  
  padding4 = grid::unit(c(0.5,0.5),"line")
  
  g <- ggplot(plot_data, aes(x,y)) +
     geom_line() +
    geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) +
    scale_fill_manual(values=co.plot, guide="none") +
    labs(title = paste0("Model: Aβ Status ~ ", RC.data.model$marker)) + xlab(RC.data.model$marker) + ylab("Density") +
    geom_vline(xintercept = sens_line, linewidth = 1, color=col.sens) +
    annotate("text", x=sens_line, y=y.text, angle=90, fontface = "bold", label= paste0("Sens. ", sens.cutoff, "%")) +
    geom_vline(xintercept = spec_line, linewidth = 1, color=col.spec) +
    annotate("text", x=spec_line, y=y.text, angle=90, fontface = "bold", label= paste0("Spec. ", spec.cutoff, "%")) +
    geom_vline(xintercept = input$RC.marker.val, linewidth = 1, color='#428bca') +
    annotation_compass(position = "NE", label = paste("Grey zone: ",greyzone,sep=""), padding = padding4) +			
    theme_bw() +
    theme(
      axis.title = element_text(size = 15, face="bold"),
      axis.text = element_text(size = 12),
      axis.text.y = element_blank(),
      plot.title = element_text(size = 15, face="bold")
    )
  g

},
#height = function() {min(session$clientData$output_PlotSens_width, 400)}
)
 

# Density Scores plot
output$RC.density.plot <- renderPlot({
  model <- RC.data.model$model
  df <- RC.data.model$df
  stats <- RC.data.model$stats
  if(is.null(model))
    return(NULL)

  if(!is.null(RC.data.model$age.range) & !RC.iv.age$is_valid()) #checking that age is valid
    return(NULL)
  
  pred <- df$pred.results
  model_se <- sd(pred) / sqrt(length(pred))  # se = sd/sqrt(sample_size)
  #Calculate the value for the individual data
  ind <- data.frame(Pred=character(), Age=character(),
                    Gender=character(), apoe4=character())
  #load all variables
  ind[nrow(ind) +1, ] <- c(input$RC.marker.val, input$RC.age.ind,
                           input$RC.gender.ind, input$RC.apoe.ind)

  ind$apoe4 <- ifelse(ind$apoe4 == "Carrier", 1, 0)
  #be sure that the numeric values are numeric
  ind[, c(1,2,4)] <- ind[, c(1,2,4)] %>% mutate_all(as.numeric)
  ind_pred <- predict(model, ind, type = "response")

  #95% of individual prediction
  critval <- 1.96 ## approx 95% CI
  upr <- ind_pred + (critval * model_se)
  lwr <- ind_pred - (critval * model_se)

  #Risk cutoff depending on Sens and Spec
  sens.cutoff <- round(100 * (stats$sensitivity)[RC.data.model$index.sens], 0)
  spec.cutoff <- round(100 * (stats$specificity)[RC.data.model$index.spec], 0)
  lr_line <- (stats$threshold)[RC.data.model$index.sens]
  rr_line <-(stats$threshold)[RC.data.model$index.spec]
  min_pred <-floor(100 * min(pred)) / 100
  max_pred <-ceiling(100 * max(pred)) / 100
  dens <- density(pred, from=min_pred, to=max_pred)
  plot_data <- data.frame(x=dens$x, y=dens$y)
  plot_data$quant <- factor(findInterval(plot_data$x, c(lr_line, rr_line)))

  if(!is.null(ind_pred)) {
    RC.data.ind$value <- paste0(format(round(ind_pred, 2), nsmall = 2),
                             " (",
                             format(round(lwr, 2), nsmall = 2),
                             ", ",
                             format(round(upr, 2), nsmall = 2),
                             ")")
    if(ind_pred < lr_line)
      RC.data.ind$risk <- "Low"
    else if(ind_pred < rr_line)
      RC.data.ind$risk <- "Medium"
    else
      RC.data.ind$risk <- "High"
  }
  else {
    RC.data.ind$risk <- ""
    RC.data.ind$vale <- ""
  }
  y.text <- (max(plot_data$y) + min(plot_data$y)) / 2 #so text is within the plot
  fmla <- RC.data.model$fmla
  fmla <- gsub("Res", "Aβ Status", fmla)
  fmla <- gsub("Pred", RC.data.model$marker, fmla)
  
  dec <- min.decimalplaces(min(lr_line, rr_line), min.d=2)
  l.line <- round(lr_line, dec)
  r.line <- round(rr_line, dec)
 
  greyzone <- paste0(min(l.line , r.line), " - ", max(l.line , r.line),", ",
                     round((abs(lr_line - rr_line))/(max_pred - min_pred)*100,2), "%")
  padding4 = grid::unit(c(0.5,0.5),"line")
  
  g <- ggplot(plot_data, aes(x,y)) +
    geom_line() +
    geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) +
    scale_fill_manual(values=c("#99d8c9", "grey", "#fc9272"), guide="none") +
    labs(title = paste0("Model: ",fmla)) + xlab("Amyloid probability Score (APS)") + ylab("Density") +

    geom_vline(xintercept = lr_line, linewidth = 1, color='#99d8c9') +
    annotate("text", x=lr_line, y=y.text, angle=90, fontface = "bold", label= paste0("Sens. ", sens.cutoff, "%")) +
    geom_vline(xintercept = rr_line, linewidth = 1, color='#fc9272') +
    annotate("text", x=rr_line, y=y.text, angle=90, fontface = "bold", label= paste0("Spec. ", spec.cutoff, "%")) +
    annotation_compass(position = "NE", label = paste("Grey zone: ",greyzone,sep=""), padding = padding4) +			
    theme_bw() +
    theme(
      axis.title = element_text(size = 15, face="bold"),
      axis.text = element_text(size = 12),
      axis.text.y = element_blank(),
      plot.title = element_text(size = 15, face="bold")
    )
  if(!is.null(ind_pred)){
    g <- g +
      geom_vline(xintercept = ind_pred, linewidth = 1, color='#428bca') +
      geom_vline(xintercept = lwr, linewidth = 1, linetype="dotted", color='#428bca') +
      geom_vline(xintercept = upr, linewidth = 1, linetype="dotted", color='#428bca') +
      annotate("rect", xmin=lwr, xmax=upr, ymin=0, ymax=Inf, alpha=0.3, fill="#428bca")
  }
  g
},
#height = function() {min(session$clientData$output_PlotSens_width, 400)}
)

output$RC.model.results <- renderText({
  if(!is.null(RC.data.model$model)) {
    cohort <- RC.data.model$df
    n <- nrow(cohort)
    if(RC.data.model$dir.thr == "low_neg")
      abpos <- nrow(cohort[cohort$AB.PARAM >= RC.data.model$threshold, ]) 
    else
      abpos <- nrow(cohort[cohort$AB.PARAM <= RC.data.model$threshold, ])
    CN <- nrow(cohort[cohort$Diagnosis == 'CN', ])
    MCI <- nrow(cohort[cohort$Diagnosis == 'MCI', ])
    AD <- nrow(cohort[cohort$Diagnosis == 'AD', ])
    str_mod <- paste0("<b>Model Stats:</b>&ensp; ",  #<br> is a jump line
                      "&emsp;<b>Sample Size:</b>&ensp;", n, 
                      "&ensp;(CN:", CN, "&ensp;MCI:", MCI, "&ensp;AD:", AD, ")",
                      "&emsp;<b> Aβ+/Aβ- (", RC.data.model$ab.param, 
                      " Thr.: ", RC.data.model$threshold,
                      "):</b>&ensp;", abpos , "/", n-abpos, " (prev. ", round(abpos/n, 3), ")",
                      "&emsp;<b>AUC (95% CI):</b>&ensp;", RC.data.model$auc)
    str_mod
  }
})
 
 
output$RC.results <- renderText({
  if(!is.null(RC.data.model$age.range) & !RC.iv.age$is_valid()) #checking that age is valid
    return(NULL)
  
  if(RC.data.ind$risk != "") {
    stats <- RC.data.model$stats
    value1 <- round((stats$threshold)[RC.data.model$index.sens], 2)
    value2 <- round((stats$threshold)[RC.data.model$index.spec], 2)
    value3 <- RC.data.ind$value
    str_ind <- paste0("Dual threshold model given a set sensitivity and specificity ",
                      "at the chosen levels. Shown are the modelled probabilities ",
                      "(Amyloid probability Score (APS)) for the Generalised Linear ",
                      "Model. Values range from 0 through to 1 (depending on the model ",
                      "this range may be reduced), with 0 being 100% Amyloid negative, ",
                      "and 1 being 100% Amyloid Positive.  The APS Threshold at ",
                      "the set sensitivity is ", value1,
                      ". The APS Threshold at the set specificity is ", value2,
                      ". Vertical blue line indicates the probability of the person ",
                      "being Amyloid positive given the model prediction from the ",
                      "participant value added. The current person has a APS (95% CI), ",
                      "of ", value3, ". Values to the right of the orange ",
                      "line are classified as Amyloid positive, whilst participant ",
                      "values to the left of the green line are classified as ",
                      "Amyloid negative. Values in between the two lines are in ",
                      "the grey zone and will need follow up testing to ascertain ",
                      "Amyloid status.")
    str_ind
  }
})


output$RC.results2 <- renderText({
  if(RC.data.ind$risk != "") {
     stats <- RC.data.model$marker.stats
    #Risk cutoff depending on Sens and Spec
    sens_line <- (stats$threshold)[RC.data.model$marker.sens.idx]
    spec_line <-(stats$threshold)[RC.data.model$marker.spec.idx]
    dec <- min.decimalplaces(min(sens_line, spec_line), min.d=3)
    sens_line <- round(sens_line, dec)
    spec_line <- round(spec_line, dec)
    str_ind <- paste0("Dual threshold model given a set sensitivity and specificity at the chosen levels. ",
                      "Shown is the real data values for the biomarker of interest on the x-axis. The ",
                      "Biomarker Threshold at the set sensitivity is ", sens_line,
                      ". The Biomarker Threshold at the set specificity is ", spec_line,
                      ". Vertical blue line indicates the participant value added. Participant values on ",
                      "the orange area are classified as Amyloid positive, whilst participant ",
                      "values on the green area classified as Amyloid negative. Values in ",
                      "between the two lines are in the grey zone and will need follow up testing to ",
                      "ascertain Amyloid status.")
    str_ind
  }
})


