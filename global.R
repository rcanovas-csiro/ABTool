##################################
# Global variables and libraries 
# Rodrigo Canovas August 2023

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(shinyalert)
library(shinyvalidate)
#library('shinyvalidate', lib.loc='R_libs/')
library(ggplot2)
library(cowplot)
library(flextable)
library(officer)
library(shinyBS)
library(shinyjs)
library(shinymanager)
library(scrypt)
library(extrafont)


source("src/tool_functions.R")


# HTML help texts

########################################
# Info TAB: Prevalence by Age         #
#######################################

PrevPredText <- paste0("This section provides an idea of the expected <b>Aβ+ prevalence</b> ",
                       "given the <b>Age</b> of samples studied. The information displayed was ",
                       "obtained from public availables studies which were obtained from ",
                       " the following papers:  <ul>",
                       "<li> Willemijnet al., Prevalence of Cerebral Amyloid Pathology in Persons Without Dementia, JAMA, 2015</li>",
                       "<li> Rik et al., Prevalence of Amyloid PET Positivity in Dementia Syndromes, JAMA, 2015 </li></ul> ",
                       "Here, the user is allowed to select which cognitive (CN, MCI and/or AD) ",
                       "information to be displayed, also providing scroll bars to be used as " ,
                       "reference lines on the plots.")

########################################
# Info TAB: Cohort Simulation        #
#######################################

Reference_content <- paste0("Run a univariable generalized logistic model using as outcome the ",
                            "Amyloid beta positivity (depending on the Aβ paramter threshold and ",
                            "direction), and the selected marker as predictor. In order to Run the ",
                            "Model please follow these steps: <ul>",
                            "<li> Upload a valid cohort (.csv file). </li>",
                            "<li> Choose the parameter to be used to define the Amyloid beta status ",
                            "(Aβ+ or Aβ-) and its direction. </li>",
                            "<li> Choose the marker to be used as Amyloid beta (Aβ+ or Aβ-) predictor. </li>",
                            "<li> Choose between using the Cognitive Normal (CN), Mild Cognitive Impairment (MCI) ",
                            "or Alzheimer (AD) participants (or any combinations of them). </li></ul>",
                            "Note that the uploaded cohort must be in .csv format and <b>must ",
                            "contain at least two numeric columns (to be used as Aβ parameter and maker) ",
                            " and the Diagnosis column (with values CN, MCI and AD)</b>. Additionally, ",
                            "note that the Aβ parameter selected must be able to divide the cohort into ",
                            " at least 10 cases of Aβ+ and  Aβ- each.")

Dynamic_content <- paste0("This section shows the <b>ROC curve</b> (of predicting the Aβ status using the ",
                          "selected marker), the <b>confusion matrix</b> for the reference (and simulated ",
                          "cohort) and a <b>table</b> containing all the <b>stats</b> of interest. ",
                          "Additionally, it is possible to dynamically explore how changing the ",
                          "<b>Aβ parameter</b> threshold (which defines the Aβ status), <b>sensitive</b> or ",
                          "<b>specificity</b> affects the displayed stats for the reference and the ",
                          "simulated cohort. <br><br>",
                          "Note that the table and plost are dynamically updated and that some of the ",
                          "values displayed are an approximation of the selected parameters, as the app ",
                          "choose the closer available computed stat. For more information, please go to ",
                          "the statistical design tab.")

Simulated_content <- paste0("This section allows you to simulate your own cohort based on the reference ",
                            "cohort results. First you need to select the desired <b>Aβ+ prevalence</b> in the simulated cohort and then choose between using ",
                            "the <b>sample size</b> or the desired <b>number of Aβ+ correctly predicted</b> within the simulated cohort. In both ",
                            "cases the tool will display the sample size and confusion matrix (TN, TP, FN and FP) obtanied by using parameters selected.",
                            "Also, the results are dynamically updated when the reference cohort parameters are changed.")


########################################
# Info TAB: Graphical Representation #
#######################################


Step2_GraphRep <- paste0("This section allows the user to dynamicaly change the <b>Aβ parameter threshold</b> used to define Aβ+ and Aβ-. ",
                         "The table and plots dinamically change depending on the parameter vale. Also, note that the plots are ",
                         "coloured by the Aβ status in the cohort.")



########################################
# Info TAB: Risk Comp #
#######################################

#                             
#                              Additionally, ",
#                             "note that the Aβ parameter selected must be able to divide the cohort into ",
#                             " at least 10 cases of Aβ+ and  Aβ- each."

Model_load_content <- paste0("Run an adjusted generalized logistic model using as outcome the ",
                             "Amyloid beta positivity (depending on the Aβ paramter threshold and ",
                             "direction), the selected marker as predictor and the covariables ",
                             "chosen (Age, Gender and/or APOE e4). In order to Run the Model, ", 
                             "please follow these steps: <ul>",
                             "<li> Upload a valid cohort (.csv file). </li>",
                             "<li> Choose the parameter to be used to define the Amyloid beta status ",
                             "(Aβ+ or Aβ-) and its direction. </li>",
                             "<li> Choose the marker to be used as Amyloid beta (Aβ+ or Aβ-) predictor. </li>",
                             "<li> Choose the parameters to be considered as covariable for the model. </li>",
                             "<li> Choose between using the Cognitive Normal (CN), Mild Cognitive Impairment (MCI) ",
                             "or Alzheimer (AD) participants (or any combinations of them). </li></ul>",
                             "Note that the uploaded cohort must be in .csv format and <b>must ",
                             "contain at least two numeric columns (to be used as Aβ parameter and maker) ",
                             " and the Diagnosis column (with values CN, MCI and AD)</b>. In case that covariables ",
                             "are selected then the selected covariable ccannot be part of the Aβ parameter nor marker ",
                             "sed. Also the columns (Age, Gender and apoe4) are expected to be part of the uploaded ",
                             "csv file, otherwise the covariable would be ignored.")


Model_results_content <- paste0("Once a model is created, in this section you can recompute it by changing the <b>Aβ ",
                                "parameter threshold</b> used. The tool displays the information of the cohort used ",
                                "to create the model (<b>sample size and Aβ+ prevalence</b>), the model performance ",
                                "(<b>AUC</b>) and the <b>density plot of the Amyloid probability Score (APS)</b> on ",
                                "the model. Additionally, at the bottom of the tool, the user can enter the individual ",
                                "information the display where in the model that individual would fall based and dependig ",
                                "on the population and Aβ+ prevalence of the data used to create the model. ",
                                "Finally, the tool allows the user to define sensitivity and specificity thresholds ",
                                "on the multivariable model and an univariable model (only using the user defined ",
                                "marker) such that it is possible to designate areas of risk of being Aβ+.")
                                


Individual_parameters_content <- paste0("Once a model has been created, here you can dynamically modify the individual parameters ",
                                        "that were selected to create the model. Depending on these parameters, on the  Model Results section, ",
                                        "you can find the <b>individual Aβ+ risk probability</b> and the risk area where the individual score belong.")

