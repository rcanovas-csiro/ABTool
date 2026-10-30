################################################################################
### Create server function (server.R) for the app ###
################################################################################



server <- function(input, output, session) {
  ###########################################
  # Create credentials information data
  # credentials <- data.frame(
  #   user = "1", #"BioStatAIBL",
  #   password = "1", #"bioABplay#22",
  #   stringsAsFactors = F
  # )

  ##################### PASSWORD INPUT ##########################
  # result_auth <- secure_server(check_credentials = check_credentials(credentials))
  # 
  # output$res_auth <- renderPrint({
  #   reactiveValuesToList(result_auth)
  # })
  # 
  # observeEvent(result_auth, {
  #   if(!is.null(result_auth$user)){ #actually login
  #     ## Warning Message ###
  #     shinyalert(title = "Important Note",
  #                text = "Pop up message to advice users that this is real data, ethics approved, etc. App is on CSIRO server and stored in Australia",
  #                type = "info")
  #   }
  # })

  ####### TABS Codes ##########################################
  #max size of upload file 100 MB
  options(shiny.maxRequestSize=100*1024^2) 
  
  #global statics datasets
  dpa<- read.csv("data/prev_age.csv")
  dca<- read.csv("data/cent_age.csv")

  source("src/prev_age_cen.R", local=TRUE)
  source("src/graph_repr.R", local=TRUE)
  source("src/cohort_simulation.R", local=TRUE)
  source("src/compute_risk.R", local=TRUE)
  

  
	######################################################################
} #CLOSE SERVER
