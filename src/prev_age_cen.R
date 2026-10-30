##################################################
############### TAB: Prevalance by Age
##################################################


#### global variables


observe({
  if(length(input$Ref_Cohort) < 1){
    updateCheckboxGroupInput(session, "Ref_Cohort", selected= "CN")
  }
  if(length(input$Ind_Cohort) < 1){
    updateCheckboxGroupInput(session, "Ind_Cohort", selected= "All")
  }
})



# Plots
output$PlotPrevAgeCen <- renderPlot({
  cohort <- input$Ref_Cohort
  samples <- input$Ind_Cohort
  age <- input$age_slider
  cent <- input$cent_slider
  prev <- input$prev_slider
  #prev vs age
  gpa <- list(ggplot() + coord_cartesian(ylim=c(5, 95), xlim=c(50,90)) + theme_bw(),
              ggplot() + coord_cartesian(ylim=c(5, 95), xlim=c(50,90)) + theme_bw())
  colors.gp <- list(c(),c())
  if('CN' %in% cohort) {
    colors.gp[[1]] <- c(colors.gp[[1]], "CN"="#43a2ca")
    colors.gp[[2]] <- c(colors.gp[[2]], "CN-APOE e4 non-Carrier"="#a6bddb", "CN-APOE e4 Carrier"="#3182bd")
    gpa <- prev_cent_age_plot(dpa, gpa, 'CN', prev=TRUE) 
  }
  if('MCI' %in% cohort) {
    colors.gp[[1]] <- c(colors.gp[[1]], "MCI"="#fde725ff")
    colors.gp[[2]] <- c(colors.gp[[2]], "MCI-APOE e4 non-Carrier"="#e3e418ff", "MCI-APOE e4 Carrier"="#75d054ff")
    gpa <- prev_cent_age_plot(dpa, gpa, 'MCI', prev=TRUE) 
  }
  if('AD' %in% cohort) {
    colors.gp[[1]] <- c(colors.gp[[1]], "AD"="#e6550d")
    colors.gp[[2]] <- c(colors.gp[[2]], "AD-APOE e4 non-Carrier"="#fa9fb5", "AD-APOE e4 Carrier"="#de2d26")
    gpa <- prev_cent_age_plot(dpa, gpa, 'AD', prev=TRUE) 
  }
  #fix themes
  gpa[[1]] <- fix_theme(gpa[[1]], colors.gp[[1]], "Age (years)", "Aβ+ Prevalence (%)", age, prev, FALSE)
  gpa[[2]] <- fix_theme(gpa[[2]], colors.gp[[2]], "Age (years)", "Aβ+ Prevalence (%)", age, prev, FALSE, FALSE)
  #get legend, then removed and then added to plot
  legend <- get_legend(gpa[[1]] + theme(legend.title = element_blank(), 
                                        legend.position = "top", 
                                        legend.text = element_text(size = 10))) 
  legend2 <- get_legend(gpa[[2]] + theme(legend.title = element_blank(), 
                                         legend.position = "top", 
                                         legend.text = element_text(size = 10))) 
  gpa[[1]] <- gpa[[1]] + theme(legend.position="none")
  gpa[[2]] <- gpa[[2]] + theme(legend.position="none")
  gpa <- plot_grid(gpa[[1]], gpa[[2]], nrow = 1)
  g <- gpa #new part
  
  t <- ggdraw() + draw_label("Whole Cohort", fontface='bold')
  t2 <- ggdraw() + draw_label("APOE e4 Stratified", fontface='bold')
  t <-  plot_grid(t, t2, nrow = 1)
  legend <- plot_grid(legend, legend2, nrow = 1)
  g <- plot_grid(legend, g, ncol = 1, rel_heights = c(0.1, 1))
  plot_grid(t, g, ncol = 1, rel_heights = c(0.1, 1))
},
height = function() {min(session$clientData$output_PlotPrevAgeCen_width, 500)}
)
