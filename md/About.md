
#### <center> **CSIRO Aβ Toolbox**  </center> 

<br />

The Aβ Toolbox presents a set of apps that allows the user to explore how different user defined markers can be used to predict the Amyloid beta status (Aβ+ or Aβ-) in different scenarios. The tool offers the following apps:


* **Aβ Prevalence**: This section provides an idea of the expected Aβ+ prevalence given the Age of samples studied. Here, the user is allowed to select which cognitive (Cognitive Normal, Mild Cognitive Impediment and/or Alzheimer's Disease) information to be displayed, also providing scroll bars to be used as reference lines on the plots. The information displayed was obtained from public available studies which were obtained from the following papers (where the status and ages were computed from Amyloid-PET data assessed from PET images and quantitative assessments):  
  - [Willemijnet al., Prevalence of Cerebral Amyloid Pathology in Persons Without Dementia, JAMA, 2015)](https://jamanetwork.com/journals/jama/fullarticle/2293295)
  - [Rik et al., Prevalence of Amyloid PET Positivity in Dementia Syndromes, JAMA, 2015](https://jamanetwork.com/journals/jama/fullarticle/2293296)


<br />

* **Graphical Representation**: Given a user defined cohort (to be used as reference), this section allows the user to explore how a selected marker is related with the Aβ status (which is defined by the selected parameter). The app displays the cohort stats in a table, including the Diagnosis distribution, the parameter selected to define the Aβ status and its threshold, the resulting Aβ+ and Aβ- depending on this threshold and the direction choose for the parameter, and the AUC (CI 95%) obtained from running a univariable linear model between the Aβ status and the selected marker. Additionally, this section displays the density and boxplot plots of the marker when divided by the Aβ status.  


<br />


* **Cohort Simulation**: This section present the same options than the the Graphical Representation, but provides more information and options on how the univariable linear model behave. Furthermore, the user is allow to explore the changes on the predicting power of the model by changing the threshold used to define Aβ+ and the sensitivity and specificity of the resulting model. Finally, this section also allows the user to simulate a cohort assuming that it behaves as the reference cohort given provided by the user. That is, it calculates the number of corrected predicted Aβ status and minimal cohort sample size required given the desirable conditions (using the reference cohort, user defined prevalence, sensitivity and marker to be used as predictor).


<br />

* **Compute Risk**: Given the uploaded cohort (to be used as reference) and set of parameters to be 
used as covariables, this section computes a model to predict the risk that an individual is Aβ+. Also, this section allows you to predict the individual Amyloid probability Score (over the computed model), given the individual selected parameter values. Moreover, the tool allows the user to define sensitivity and specificity thresholds on the multivariable model and an univariable model (only using the user defined marker) such that it is possible to designate areas of risk of being Aβ+.

<br />

The only requirement of all the tools in this app is that the user provides a **csv** file having the column **Diagnosis (with values CN, MCI, and/or AD)** and ay least two numerical columns that could be used as parameter to define the Aβ status and marker to predict Aβ+.  In the case of the Compute Risk tool, is expected that the **csv** file also has the columns Age, Gender and apoe4, but it this is not a requirement (if they are not present and the user select the option to used, the tools will ignore the field selected).

