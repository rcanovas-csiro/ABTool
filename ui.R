
ui <- dashboardPage( 
  dashboardHeader(title = 'Aβ Toolbox', titleWidth = 450),
  dashboardSidebar(collapsed = TRUE,
    sidebarMenu( #search for icon
      menuItem("About", tabName = "about", icon = icon("question-circle")),
      menuItem("Statistical Design", tabName = "stat_design", icon = icon("brain")),
      menuItem("Aβ Prevalence", tabName = "prev_age_cen", icon = icon("chart-line")),
      menuItem("Graphical Representation", tabName = "graph_rep", icon = icon("chart-bar")),
      menuItem("Cohort Simulation", tabName = "simulation", icon = icon("chart-area")),
      menuItem("Compute Risk", tabName = "risk_comp", icon = icon("hand-holding-medical"))
    ),
    tags$a(href = "https://aibl.csiro.au/", 
           img(src = "img/aibl-logo.png", width='95%',  # height='50', width='150',
               style="display: block; margin-left: auto; margin-right: auto; margin-top: 50px")),        
    tags$a(href = "https://aehrc.com/", 
           img(src = "img/EHealth_logo.png",
               title = "The Australian eHealth Research Centre Homepage", 
               width='65%', #height='100', width='150',
               style="display: block; margin-left: auto; margin-right: auto; margin-top: 50px")),
    tags$a(href='https://www.csiro.au/', 
           img(src="img/CSIRO_Logo.png", height='120', width='120',
               style="display: block; margin-left: auto; margin-right: auto; margin-top: 50px"))
  ), #close Sidebar
  
  dashboardBody(
    useShinyjs(), # to disable an activate areas
    #add reference to css
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "my.css")),
    
    tabItems(
      ###############################################################
      ############### TAB: ABOUT
      ###############################################################
      tabItem(tabName = "about", 
              fluidRow(
                box(title = NULL, width = "100%",
                    fluidRow(includeMarkdown("./md/About.md"))
                ), #close box
                tags$div(
                  tags$p("This tool was created by ", tags$em("Rodrigo Canovas & James Doecke"),
                         style="text-align:right; color:gray"),
                  tags$p("Contact: rodrigo.canovas@csiro.au", style="text-align:right; color:gray"),
                  tags$p("Beta Version, October 2023", style="text-align:right; color:gray")
                )
              )
      ), # close tab about
      
      ###############################################################
      ############### TAB: STAT_DESIGN
      ###############################################################
      tabItem(tabName = "stat_design", 
              fluidRow(
                withMathJax(),
                box(title = NULL, width = "100%", 
                    fluidRow(includeMarkdown("./md/Stats_Design.md"))
                ))
      ), # close tab stat_design
      
      ###############################################################
      ############### TAB: Prevalence by Age
      ###############################################################
      tabItem(tabName = "prev_age_cen", 
              fluidRow(
                box(width = NULL, collapsible = TRUE, 
                    title = tagList(
                      tags$span("Aβ+ Prevalence"),
                      tags$span(icon("info-circle"), id = "icon_ab_prev", style = "color:#428bca;")),
                    bsPopover("icon_ab_prev", "Study", placement = 'bottom', trigger = 'click',
                              content = PrevPredText),
                    tags$br(),
                    fluidRow(
                      column(width = 4, offset = 4,
                             checkboxGroupInput(inputId = "Ref_Cohort", label = "Cognitive Cohort to Display:",
                                                choices = c("CN", "MCI", "AD"),
                                                selected = "CN", inline = TRUE)
                      )
                    ),
                    fluidRow(plotOutput(outputId = "PlotPrevAgeCen", width="100%", height = "auto")),
                    fluidRow(
                      column(width = 4, offset = 2,
                             sliderInput(inputId = "age_slider", label="Age", min=50, max=90, step=1, value=55)),
                      column(width = 4, 
                             sliderInput(inputId = "prev_slider", label="Aβ+ Prev.", min=0, max=100, step=1, value=25))
                    )
              ) #close Box
            ) #Close main fluidRow
      ), # Close Prevalence by Age Tab
      
      
      ###############################################################
      ############### TAB: Graphical Representation
      ###############################################################
      tabItem(tabName = "graph_rep",
              fluidRow(
                box(width = NULL, collapsible = TRUE,
                    title = tagList(
                      tags$span("1. Reference"),
                      tags$span(icon("info-circle"), id = "icon_ref2", style = "color:#428bca;")),
                    bsPopover("icon_ref2", "Step 1", placement = 'right', trigger = 'click',
                              content = Reference_content),
                    tags$br(),
                    column(width = 4,
                           fileInput(inputId = "GR.ref.file",
                                     label = "Upload your reference (.csv)",
                                     accept = ".csv")
                    ),
                    column(width = 6,
                           fluidRow(
                             column(width = 4,
                                    radioButtons(inputId = "GR_marker_type", label = "Marker Type: ",
                                                 choices = c("Single" = "single", "Ratio" = "ratio"),
                                                 selected = "single", inline = TRUE)
                            ),
                            column(width = 4,
                                   selectizeInput(inputId = "GR.marker.sin", label="Marker",
                                                  choices= NULL, selected = NULL, multiple = FALSE)
                            ),
                            column(width = 4,
                                   conditionalPanel(condition = "input.GR_marker_type == 'ratio'",
                                                    selectizeInput(inputId = "GR.marker.rat", label = "Denominator",
                                                                   choices= NULL, selected = NULL, multiple = FALSE))
                            )
                           ), # Close first fluidrow
                           fluidRow(
                             column(width = 4,
                                    selectizeInput(inputId = "GR.ab.param", label="Aβ Parameter",
                                                   choices= NULL, selected = NULL, multiple = FALSE)
                             ),
                             column(width = 6,
                                    radioButtons(inputId = "GR.ab.dir", label = "Direction: ",
                                                 choices = c("Low is Aβ-" = "low_neg", "High is Aβ-" = "high_neg"),
                                                 selected = "low_neg", inline = TRUE)
                             )
                           )
                    ), # Close column 6
                    column(width = 2,
                           checkboxGroupInput(inputId = "GR.Diag.input", label = "Participant to use:",
                                              choices = c("CN", "MCI", "AD"),
                                              selected = "CN", inline = FALSE),
                           fluidRow(actionButton(inputId = "GR.ref.compute", label = "Run Model",
                                                 class = "rightAlign"))
                    )
                ), # GR Close Box Reference
                box(width = NULL, collapsible = TRUE,
                    title = tagList(
                      tags$span("2. Stats"),
                      tags$span(icon("info-circle"), id = "icon_plots", style = "color:#428bca;")),
                    bsPopover("icon_plots", "Step 2", placement = 'right', trigger = 'click',
                              content = Step2_GraphRep),
                    tags$br(),
                    fluidRow(
                      column(width = 4,
                             numericInput(inputId = "GR.slider.ab.param", label="Aβ Param Threshold",
                                          min=0, max=100, value=0),
                             uiOutput(outputId = "GR.summ.stats")
                      ),
                      column(width = 8,
                             column(width = 6,
                                    plotOutput(outputId = "GR.PlotDen", width="100%", height = "auto")
                             ),
                             column(width = 6,
                                    plotOutput(outputId = "GR.PlotBox", width="100%", height = "auto")
                             )
                      )
                    )
                ) # GR Close Box Stats
              ) # GR Close Main Fluidrow 
      ), #Close Graphical Representation Tab


      ##################################################
      ############### TAB:  Cohort Simulation
      ##################################################
      tabItem(tabName = "simulation",
              fluidRow(
                box(width = NULL, collapsible = TRUE,
                    title = tagList(
                      tags$span("1. Reference"),
                      tags$span(icon("info-circle"), id = "icon_ref", style = "color:#428bca;")),
                    bsPopover("icon_ref", "Step 1", placement = 'right', trigger = 'click',
                              content = Reference_content),
                    tags$br(),
                    column(width = 4,
                           fileInput(inputId = "CS.ref.file",
                                     label = "Upload your reference (.csv)",
                                     accept = ".csv")
                    ),
                    column(width = 6,
                           fluidRow(
                             column(width = 4,
                                    radioButtons(inputId = "CS_marker_type", label = "Marker Type: ",
                                                 choices = c("Single" = "single", "Ratio" = "ratio"),
                                                 selected = "single", inline = TRUE)
                             ),
                             column(width = 4,
                                    selectizeInput(inputId = "CS.marker.sin", label="Marker",
                                                   choices= NULL, selected = NULL, multiple = FALSE)
                             ),
                             column(width = 4,
                                    conditionalPanel(condition = "input.CS_marker_type == 'ratio'",
                                                     selectizeInput(inputId = "CS.marker.rat", label = "Denominator",
                                                                    choices= NULL, selected = NULL, multiple = FALSE))
                             )
                           ), # Close first fluidrow
                           fluidRow(
                             column(width = 4,
                                    selectizeInput(inputId = "CS.ab.param", label="Aβ Parameter",
                                                   choices= NULL, selected = NULL, multiple = FALSE)
                             ),
                             column(width = 6,
                                    radioButtons(inputId = "CS.ab.dir", label = "Direction: ",
                                                 choices = c("Low is Aβ-" = "low_neg", "High is Aβ-" = "high_neg"),
                                                 selected = "low_neg", inline = TRUE)
                             )
                           )
                    ), # Close column 6
                    column(width = 2,
                           checkboxGroupInput(inputId = "CS.Diag.input", label = "Participant to use:",
                                              choices = c("CN", "MCI", "AD"),
                                              selected = "CN", inline = FALSE),
                           fluidRow(actionButton(inputId = "CS.ref.compute", label = "Run Model",
                                                 class = "rightAlign"))
                    )
                ), # CS Close Box Reference
                box(width = NULL, collapsible = TRUE,
                    title = tagList(
                      tags$span("2. Dynamic Exploration"),
                      tags$span(icon("info-circle"), id = "icon_dyn", style = "color:#428bca;")),
                    bsPopover("icon_dyn", "Step 2", placement = 'right', trigger = 'click',
                              content = Dynamic_content),
                    tags$br(),
                    fluidRow(
                      column(width = 4,
                             numericInput(inputId = "CS.slider.ab.param", label="Aβ Param Threshold",
                                         min=0, max=100, value=0)
                      ),
                      column(width = 4,
                             sliderInput(inputId = "CS.slider.mes.sens", label="Sensitivity",
                                         min=0.01, max=0.99, step=0.01, value=0.95)
                      ),
                      column(width = 4,
                             sliderInput(inputId = "CS.slider.mes.spec", label="Specificity",
                                         min=0.01, max=0.99, step=0.01, value=0.95)
                      )
                    ), #Close dynamic sliders
                    fluidRow(
                      column(width = 4,
                             fluidRow(align="center", uiOutput(outputId = "CS.summ.stats"))
                      ),
                      column(width = 8,
                             box(width = NULL, collapsible = TRUE,
                                 title = tagList(tags$span("Reference Cohort")),
                                 tags$br(),
                                 fluidRow(
                                   column(width = 6, align="center",
                                          fluidRow(plotOutput(outputId = "CS.gens.confM",
                                                              width="100%", height = "auto")) #close second fluid
                                   ), # close column width 6
                                   column(width = 6,
                                          plotOutput(outputId = "CS.PlotSens", width="100%", height = "auto")
                                   ) # close column width 6
                                 ), # close fluidrow
                             ), #close box
                             box(width = NULL, collapsible = TRUE, collapsed = TRUE,
                                 title = tagList(tags$span("3. Simulation Cohort"),
                                                 tags$span(icon("info-circle"), id = "icon_simu", style = "color:#428bca;")),
                                 bsPopover("icon_simu", "Step 4", placement = 'bottom', trigger = 'click',
                                           content = Simulated_content),
                                 tags$br(),
                                 column(width = 6,
                                        fluidRow(align="center", plotOutput(outputId = "CS.res.confM",
                                                                            width="100%", height = "300px"))
                                 ),
                                 column(width = 6,
                                        numericInput(inputId = "CS.prevalence", label = "Aβ+ Prevalence",
                                                     min = 0, max = 1, value = 0.28, step = 1e-7),
                                        radioButtons(inputId = "CS.sim.choise", label = "Use:",
                                                     choices = c("Sample Size" = "sam_size", "# of predicted Aβ+" = "min_cases"),
                                                     selected = "sam_size",  inline = TRUE),
                                        numericInput(inputId = "CS.sample.size", label = "Value", min = 1, value = 100, step = 1),
                                        fluidRow(actionButton(inputId = "CS.sim.compute", label = "Calculate", class = "rightAlign"))
                                 )
                             ) # close second box
                      ) # My new column 8
                    )
                ) # close box Dynamic Section
              ) #Close main fluidrow
      ), # close tab SIMULATIONS


      ###############################################################
      ############### TAB: Risk Comp
      ###############################################################
      tabItem(tabName = "risk_comp",
              fluidRow(
                box(width = NULL, collapsible = TRUE,
                    title = tagList(
                      tags$span("1. Model to Use"),
                      tags$span(icon("info-circle"), id = "icon_mod_use", style = "color:#428bca;")),
                    bsPopover("icon_mod_use", "Step 1", placement = 'right', trigger = 'click',
                              content = Model_load_content),
                    tags$br(),
                    column(width = 4,
                           fluidRow(
                             fileInput(inputId = "RC.ref.file",
                                       label = "Upload your reference (.csv)",
                                       accept = ".csv")
                           ),
                           fluidRow(
                             checkboxGroupInput(inputId = "RC.covars", label = "Covariables to Consider:",
                                                choices = c("Age", "Gender", "APOE e4"), inline = TRUE)
                           )
                    ),
                    column(width = 6,
                           fluidRow(
                             column(width = 4,
                                    radioButtons(inputId = "RC_marker_type", label = "Marker Type: ",
                                                 choices = c("Single" = "single", "Ratio" = "ratio"),
                                                 selected = "single", inline = TRUE)
                             ),
                             column(width = 4,
                                    selectizeInput(inputId = "RC.marker.sin", label="Marker",
                                                   choices= NULL, selected = NULL, multiple = FALSE)
                             ),
                             column(width = 4,
                                    conditionalPanel(condition = "input.RC_marker_type == 'ratio'",
                                                     selectizeInput(inputId = "RC.marker.rat", label = "Denominator",
                                                                    choices= NULL, selected = NULL, multiple = FALSE))
                             )
                           ), # Close first fluidrow
                           fluidRow(
                             column(width = 4,
                                    selectizeInput(inputId = "RC.ab.param", label="Aβ Parameter",
                                                   choices= NULL, selected = NULL, multiple = FALSE)
                             ),
                             column(width = 6,
                                    radioButtons(inputId = "RC.ab.dir", label = "Direction: ",
                                                 choices = c("Low is Aβ-" = "low_neg", "High is Aβ-" = "high_neg"),
                                                 selected = "low_neg", inline = TRUE)
                             )
                           )
                    ), # Close column 6
                    column(width = 2,
                           checkboxGroupInput(inputId = "RC.Diag.input", label = "Participant to use:",
                                              choices = c("CN", "MCI", "AD"),
                                              selected = "CN", inline = FALSE),
                           fluidRow(actionButton(inputId = "RC.create.model", label = "Run Model",
                                                 class = "rightAlign"))
                    )
                ), # RC Close Box Reference
                
                box(width = NULL, collapsible = TRUE,
                    title = tagList(
                      tags$span("2. Model Results"),
                      tags$span(icon("info-circle"), id = "icon_mod_res", style = "color:#428bca;")),
                    bsPopover("icon_mod_res", "Step 2", placement = 'right', trigger = 'click',
                              content = Model_results_content),
                    fluidRow(
                      column(width = 4,
                             numericInput(inputId = "RC.slider.ab.param", label="Aβ Param Threshold",
                                          min=0, max=100, value=0)
                      )
                    ), # Close FluidRow Centiloid, Sens and Spec options
                    fluidRow(
                      tags$br(),
                      htmlOutput("RC.model.results"),
                      tags$br()
                    ),
                    fluidRow(
                      column(width = 5, offset = 1,
                             column(width = 6,
                                    sliderInput(inputId = "RC.sens", label="Model Sens. Cutoff",
                                                min=0.01, max=0.99, step=0.01, value=0.9)
                             ),
                             column(width = 6,
                                    sliderInput(inputId = "RC.spec", label="Model Spec. Cutoff",
                                                min=0.01, max=0.99, step=0.01, value=0.9)
                             )
                      ),
                      column(width = 5,
                             column(width = 6,
                                    sliderInput(inputId = "RC.marker.sens", label="Marker Sens. Cutoff",
                                                min=0.01, max=0.99, step=0.01, value=0.9)
                             ),
                             column(width = 6,
                                    sliderInput(inputId = "RC.marker.spec", label="Marker Spec. Cutoff",
                                                min=0.01, max=0.99, step=0.01, value=0.9)
                             )
                      )
                    ), #Close Fluid Row Plots Figures
                    fluidRow(
                      column(width = 5, offset = 1,
                             plotOutput(outputId = "RC.density.plot", width="100%", height = "300px")
                      ),
                      column(width = 5,
                             fluidRow(plotOutput(outputId = "RC.marker.plot", width="100%", height = "300px"))
                      )
                    ), #Close Fluid Row Plots Figures
                    fluidRow(
                      column(width = 5, offset = 1,
                             htmlOutput("RC.results")
                      ),
                      column(width = 5,
                             htmlOutput("RC.results2")
                      )
                    ), #Close Fluid Row Plots Text
                    fluidRow(
                      box(width = NULL, collapsible = TRUE,
                          title = tagList(
                            tags$span("3. Individual Risk Parameters"),
                            tags$span(icon("info-circle"), id = "icon_ind_param", style = "color:#428bca;")),
                          bsPopover("icon_ind_param", "Step 3", placement = 'bottom', trigger = 'click',
                                    content = Individual_parameters_content),
                          column(width = 3,
                                 shinyjs::hidden(sliderInput(inputId = "RC.marker.val", label = "Marker Value",
                                                             min=0, max=10, step = 1, value = 5))
                          ),
                          column(width = 3,
                                 shinyjs::hidden(selectInput(inputId = "RC.gender.ind", label = "Gender",
                                                             choices = list("Female", "Male"), selected = 1))
                          ),
                          column(width = 3,
                                 shinyjs::hidden(numericInput(inputId = "RC.age.ind", label="Age", min=40, max=100, value=45))
                          ),
                          column(width = 3,
                                 shinyjs::hidden(selectInput(inputId = "RC.apoe.ind", label = "APOE e4",
                                                             choices = list("Non-Carrier", "Carrier"), selected = 1))
                          )
                      )# Close Box 3
                    )# Close Individual options
                )#Close box model results
            ), # Close Main Fluid row
      ) # Close tab Risk Comp

    ) #Close Tab items
  ) #close Body
) #close Page 


#####################
#Security part
###################


#ui <- secure_app(ui)
                 
