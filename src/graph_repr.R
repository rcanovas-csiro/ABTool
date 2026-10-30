##################################################
############### TAB: Simulation - Reference
##################################################


#### global variables
GR.data.ref <- reactiveValues(df = NULL, stats = NULL, path = NULL,
                              threshold = 0, dir.thr = "low_neg",
                              min.ab = 0, max.ab = 100,
                              ab.param = "", marker="",
                              auc = "",
                              error_msg="")
 
#Uploading a cohort
observeEvent(input$GR.ref.file, {
  showModal(modalDialog("Loading...", footer=NULL, size = 's'))
  GR.data.ref$error_msg <- ""
  path <- NULL
  file <- isolate(input$GR.ref.file)
  if (is.null(file)) {
    GR.data.ref$error_msg <- "Please upload a csv file"
  }
  else {
    ext <- tools::file_ext(file$datapath) #get extension
    if(ext != "csv")
      GR.data.ref$error_msg <- "File must have be a csv"
    else {
      ldf <- upload_file(file$datapath)
      if(is.character(ldf))
        GR.data.ref$error_msg <- ldf
      else {
        GR.data.ref$df <- ldf
        GR.data.ref$path <- file$datapath
        markers <- as.data.frame(select_if(ldf, is.numeric))
        all_fields <- sort(colnames(markers))
        if(length(all_fields) == 0) #still check that it is not empty
          GR.data.ref$error_msg <- "There are no numerical columns to use as markers in the uploaded file"
        updateSelectInput(session, "GR.marker.sin", choices= unique(all_fields), selected = NULL)
        updateSelectInput(session, "GR.marker.rat", choices= unique(all_fields), selected = NULL)
        updateSelectInput(session, "GR.ab.param", choices= unique(all_fields), selected = NULL)
      }
    }
  }
  if(GR.data.ref$error_msg != "") {
    showModal(modalDialog(title = "Error", GR.data.ref$error_msg,
                          easyClose = TRUE, size = 's'))
    GR.data.ref$df <- NULL
    GR.data.ref$stats <- NULL
    GR.data.ref$path <- NULL
    GR.data.ref$auc <- ""
    GR.data.ref$threshold <- 0
    GR.data.ref$min.ab <- 0
    GR.data.ref$max.ab <- 0
    GR.data.ref$dir.thr <- "low_neg"
    GR.data.ref$ab.param <- ""
    GR.data.ref$marker <- ""
    updateSelectInput(session, "GR.marker.sin", choices= NULL, selected = NULL)
    updateSelectInput(session, "GR.marker.rat", choices= NULL, selected = NULL)
  }
  else
    removeModal()
  #also update sliders

},
ignoreInit = TRUE)


# Filter selected data and check if it is valid
observeEvent(input$GR.ref.compute, {
  showModal(modalDialog("Loading...", footer=NULL, size = 's'))
  GR.data.ref$error_msg <- ""
  if(is.null(GR.data.ref$path))
    GR.data.ref$error_msg <- "Please upload a csv file"
  else {
    participants <- isolate(input$GR.Diag.input)
    if(length(participants) == 0)
      GR.data.ref$error_msg <- "Please select at least one of the participant Diagnosis groups"
    else {
      #get ab parameter
      ab.param <- isolate(input$GR.ab.param)
      GR.data.ref$ab.param <- ab.param
      GR.data.ref$dir.thr <- isolate(input$GR.ab.dir)
      #get marker
      type_marker <- isolate(input$GR_marker_type)
      m1 <- isolate(input$GR.marker.sin)
      GR.data.ref$marker <- m1
      m2 <- ""
      if(type_marker == "ratio") {
        m2 <- isolate(input$GR.marker.rat)
        GR.data.ref$marker <- paste0(GR.data.ref$marker, "/", m2)
      }
      if(ab.param %in% c(m1,m2))    
        GR.data.ref$error_msg <- "Marker can not contain same variable as the Aβ paramater"
      if(m1 == m2)
        GR.data.ref$error_msg <- "The denominator marker cannot be the same as the numerator"
      if(GR.data.ref$error_msg == "") { #no error so far
        ldf <- run_analysis(ab.param, GR.data.ref$dir.thr, m1, m2, participants, GR.data.ref$path)
        if(is.character(ldf))
          GR.data.ref$error_msg <- ldf
        else{
          GR.data.ref$df <- ldf[[1]]
          GR.data.ref$stats <- ldf[[2]]
          GR.data.ref$auc <- ldf[[3]]
          thr.info <- ldf[[4]]
          GR.data.ref$threshold <- thr.info[3]
          GR.data.ref$min.ab <- thr.info[1]
          GR.data.ref$max.ab <- thr.info[2]
          mylab <-  paste0("Aβ Parameter Threshold (", ab.param,")")
          updateNumericInput(session, "GR.slider.ab.param", 
                             label = mylab, 
                             value = thr.info[3], 
                             min = thr.info[1], max = thr.info[2])
        }
      } # Close else error message 
    } # Close else participants
  } # Close else path is valid
  if(GR.data.ref$error_msg != "") {
    showModal(modalDialog(title = "Error", GR.data.ref$error_msg,
                          easyClose = TRUE, size = 's'))
    #Note that the path is not removed as still is valid
    GR.data.ref$df <- NULL
    GR.data.ref$stats <- NULL
    GR.data.ref$auc <- ""
    GR.data.ref$marker <- ""
    GR.data.ref$threshold <- 0
    GR.data.ref$min.ab <- 0
    GR.data.ref$max.ab <- 100
    GR.data.ref$dir.thr <- "low_neg"
    GR.data.ref$ab.param <- ""
  }
  else
    removeModal()
  #also update sliders
  updateNumericInput(session, "GR.slider.ab.param", value=GR.data.ref$threshold)
},
ignoreInit = TRUE
)


#Add Validator for the threshold field 
GR.iv <- InputValidator$new()
GR.iv$add_rule("GR.slider.ab.param", sv_required())
GR.iv$add_rule("GR.slider.ab.param", function(value) {
  if(!is.numeric(value))
    return("Value must be a number")
  else if(value < GR.data.ref$min.ab | value > GR.data.ref$max.ab)
    return(paste0("Value must be between ", GR.data.ref$min.ab, " and ", GR.data.ref$max.ab))
  else
    return(NULL)
  })
GR.iv$enable()


update_ab_silder <- reactiveVal(TRUE)  # weird trick to avoid double reacting

observeEvent(input$GR.slider.ab.param, {
  thr <-  input$GR.slider.ab.param
  if(update_ab_silder()) {
    if(is.null(GR.data.ref$df)) {
      update_ab_silder(FALSE) #to avoid re triggering this observe
      updateNumericInput(session, "GR.slider.ab.param", value=GR.data.ref$threshold)
    }
    else {
      if (GR.iv$is_valid()) {   #check that threshold is valid first
        #check if I can compute the roc
        da <- compute_roc(GR.data.ref$df, thr, GR.data.ref$dir.thr)
        if(is.character(da)) {
          showModal(modalDialog(title = "Error", da, easyClose = TRUE, size = 's'))
          update_ab_silder(FALSE)
          updateNumericInput(session, "GR.slider.ab.param", value=GR.data.ref$threshold)
        }
        else {
          GR.data.ref$df <- da[[1]]
          GR.data.ref$stats <- da[[2]]
          GR.data.ref$auc <- da[[3]]
          GR.data.ref$threshold <- thr
        }
      } 
    }
  }
  else
    update_ab_silder(TRUE) #open again
},
ignoreInit = TRUE
)


# DensityPlot
output$GR.PlotDen <- renderPlot({
  stats <- GR.data.ref$stats
  cohort <- GR.data.ref$df
  if(is.null(stats))
    return(NULL)
  N <- nrow(cohort)
  if(GR.data.ref$dir.thr == "low_neg")
    cohort$AB <- ifelse(cohort$AB.PARAM < GR.data.ref$threshold, 0, 1) #AB- -> 0  AB+ -> 1
  else
    cohort$AB <- ifelse(cohort$AB.PARAM > GR.data.ref$threshold, 0, 1)
  cohort$AB <- as.factor(cohort$AB)
  #check number if levels==1
  lev <- unique(cohort$AB) # there are AB+ and - or only one of them

  cols_pl <- c()
  labs_pb <- c()
  den <- ggplot()  #density plot
  if(0 %in% lev){
    den <- den + geom_density(data=cohort[cohort$AB==0,], aes(x=Pred, fill="AB-"), alpha=0.4)
    cols_pl <- c(cols_pl, "cadetblue4")
    labs_pb <- c(labs_pb, "Aβ-")
  }
  if(1 %in% lev){
    den <- den + geom_density(data=cohort[cohort$AB==1,], aes(x=Pred, fill="AB+"), alpha=0.4)
    cols_pl <- c(cols_pl, "#e41a1c")
    labs_pb <- c(labs_pb, "Aβ+")
  }
  den <- den + xlab(GR.data.ref$marker) +
    scale_fill_manual(name="Status", values=cols_pl, labels=labs_pb) +
    theme_bw() +
    theme(
      legend.position="top",
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      axis.title = element_text(size = 15, face="bold"),
      axis.text = element_text(size = 12)
    )
  den
},
height = function() {min(session$clientData$output_GR.PlotDen_width, 350)}
)


# Boxplot 
output$GR.PlotBox <- renderPlot({
  stats <- GR.data.ref$stats
  cohort <- GR.data.ref$df
  if(is.null(stats))
    return(NULL)
  N <- nrow(cohort)
  if(GR.data.ref$dir.thr == "low_neg")
    cohort$AB <- ifelse(cohort$AB.PARAM < GR.data.ref$threshold, 0, 1) #AB- -> 0  AB+ -> 1
  else
    cohort$AB <- ifelse(cohort$AB.PARAM > GR.data.ref$threshold, 0, 1)
  cohort$AB <- as.factor(cohort$AB)
  #check number if levels==1
  lev <- unique(cohort$AB) # there are AB+ and - or only one of them
  cols_pl <- c()
  labs_pb <- c()
  cohort$Diagnosis <- factor(cohort$Diagnosis, levels = c("CN","MCI","AD"))
  bo <- ggplot(data=cohort, aes(y=Pred, x=AB, color=Diagnosis)) # box plot
  num_diag <- length(unique(cohort$Diagnosis))
  if(num_diag > 1) {
    bo <- bo +
      geom_boxplot(fill=NA, size=1) + ### replaced Pred with marker
      geom_point(size=1,
                 position=position_jitterdodge(jitter.width = 0.25, dodge.width=0.9), alpha = 0.5)
    bo <- bo +
      scale_color_manual(name="", values=c("CN"="cadetblue4","MCI"="#d8b365","AD"="#e41a1c"))
  }
  else {
    if(0 %in% lev){
      bo <- bo +
        geom_boxplot(data=cohort[cohort$AB==0,], aes(y=Pred, x=AB, color="AB-"), fill=NA, size=1) + ### replaced Pred with marker
        geom_point(data=cohort[cohort$AB==0,], aes(y=Pred, x=AB, color="AB-"), size=1,
                   position=position_jitterdodge(jitter.width = 0.25, dodge.width=0.9), alpha = 0.5)
      cols_pl <- c(cols_pl, "cadetblue4")
      labs_pb <- c(labs_pb, "Aβ-")
    }
    if(1 %in% lev){
      bo <- bo +
        geom_boxplot(data=cohort[cohort$AB==1,], aes(y=Pred, x=AB, color="AB+"), fill=NA, size=1) +
        geom_point(data=cohort[cohort$AB==1,], aes(y=Pred, x=AB, color="AB+"), size=1,
                   position=position_jitterdodge(jitter.width = 0.25, dodge.width=0.9), alpha = 0.5)
      cols_pl <- c(cols_pl, "#e41a1c")
      labs_pb <- c(labs_pb, "Aβ+")
    }
    bo <- bo + scale_color_manual(name="", values=cols_pl, labels=labs_pb)
  }
  bo <- bo + ylab(GR.data.ref$marker) + xlab("Aβ Status") +
    scale_x_discrete(labels=c("0" = "Aβ-", "1" = "Aβ+")) +
    theme_bw() +
    theme(
      legend.position="top",
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      axis.title = element_text(size = 15, face="bold"),
      axis.text = element_text(size = 12)
    )
  bo
},
height = function() {min(session$clientData$output_GR.PlotBox_width, 350)}
)


#Data Table General Stats
output$GR.summ.stats <- renderUI({
  df <- data.frame(ST=c("Cohort Size", "CN", "MCI", "AD",
                        "Marker", "Aβ Param. Threshold",  "AUC (CI 95%)", 
                        "Aβ+ Real Prevalence",
                        "Real Aβ+/Aβ-"))
  df$RC <- rep("-", 9)
  cohort <- GR.data.ref$df
  if(!is.null(GR.data.ref$stats)) {  #reference data
    N <- nrow(cohort)
    CN <- nrow(cohort[cohort$Diagnosis == 'CN', ])
    MCI <- nrow(cohort[cohort$Diagnosis == 'MCI', ])
    AD <- nrow(cohort[cohort$Diagnosis == 'AD', ])
    #just take the first point
    tp <- ((GR.data.ref$stats)$tp)[1]
    fn <- ((GR.data.ref$stats)$fn)[1]
    ab.pos <- tp + fn
    r_prev <- format(round((ab.pos)/N, 3), nsmall = 3)
    refer <- c(N, CN, MCI, AD,
               GR.data.ref$marker, GR.data.ref$threshold, 
               GR.data.ref$auc,
               r_prev, paste0(ab.pos,"/", N - ab.pos))
    df$RC <- refer
  }

  flextable(df) %>%
    width(j=(1:2), width=3) %>%
    set_header_labels(ST="Stats", RC="Reference Cohort") %>%
    border_remove() %>%  hline_top(border=fp_border(width=2, color="black"), part="header") %>%
    hline(i=c(1), border=fp_border(width=2, color="#007FA6"), part="header") %>%
    hline(i=c(nrow(df)), border=fp_border(width=2, color="black"), part="body") %>%
    flextable::align(align = "center", j = c(2), part = "all")  %>%
    style(pr_t=fp_text(font.family= "roboto")) %>%
    fontsize(i = 1, size = 9, part = "header") %>%   # adjust font size of header
    bold(i = 1, bold = TRUE, part = "header") %>%     # adjust bold face of header
    fontsize(part="body", size = 11) %>%
    bold(i = c(1, 5:nrow(df)), j = 1, bold = TRUE, part = "body") %>%
    htmltools_value()
})

