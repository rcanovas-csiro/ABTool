

#### **Graphical Representation**

<br />

In this section, an sample is classified as Aβ+ if:

* Its Aβ parameter is higher or equal than the selected threshold and the selected direction if Low is Aβ-.
* Its Aβ parameter is lower or equal than the selected threshold and the selected direction if High is Aβ-.

The AUC displayed on the table is compute using the **ROC curve** of the Aβ status and the fitted values obtaining from computing a binomial generalized linear model between Aβ status and the selected marker. Note that before computing the ROC curve, samples that do not have a recorded value for the selected marker are removed. In order to compute the ROC curve we used the pROC R library as follow:


* *pROC::roc(response = Aβ status, predictor = glm(Aβ status ~ marker, family="binomial")$fitted)*

Finally, the Density and Boxplot presented are stratified by the Aβ status defined by the Aβ parameter threshold.

<br />


#### **Cohort Simulation**

<br />

The cohort and Aβ status are computed as in the ***Graphical Representation*** tool.

<br />

##### ***Dynamic Exploration***

<br />

Given the loaded reference, the user-defined Aβ parameter threshold and the Sensitivity (or Specificity) values selected, the tool computes the following values:

* **N**: Cohort sample size.
* **TP/FP/FN/TN**: True positive, false positive, false negative and true negative
* **Aβ+ Prevalence**: The prevalence of Aβ positive on the cohort (Aβ+/N)
* **Aβ+/Aβ-**: Number of Aβ positive (TP+FN) and negative (FP+TN) depending on the Aβ parameter threshold
* **Pred(Aβ+/Aβ-)**: Number of predicted Aβ positive (TP+FP) and negative (FN+TN)
* **Sensitivity**: $$\frac{TP}{TP + FN}$$
* **Specificity**: $$\frac{TN}{TN + FP}$$
* **PPV**: Positive predictive value $$\frac{TP}{TP + FP}$$
* **NPV**: Negative predictive value $$\frac{TN}{TN + FN}$$


The ROC curve is computed as in the ***Graphical Representation*** tool.

<br />


##### ***Simulated Cohort***

<br />
            
In this section, using the results obtained from the reference cohort, the tool can answer the following two questions:

* Given a desirable prevalence (**prev**), sensitivity (**sens**) and cohort sample size (**N**), *how many Aβ+ should I expect to correctly predict if I collect and use the selected marker as predictor? (i.e. number of TP)* 
* Given a desirable prevalence (**prev**), sensitivity (**sens**) and expected number of correctly predicted Aβ+ using the selected marker as predictor, *how big would need to be the collected cohort sample?(i.e. value of N)* 

In order to answer both questions, and assuming that the sensitivity and specificity values have to behave as in the reference cohort, it is possible to get all the desirable values using the following equations:

  * $TP = sens * (prev*N)$ 
  * $prev*N = TP + FN$
  * $TN = senc * (N*(1-prev))$
  * $N*(1-prev) = FP + TN$
  
Note that, from the reference ROC curve it is possible to extract the sensitivity and specificity values at each desired point. 

<br />


#### **Compute Risk**

<br />

##### ***Model to Use***
  
<br />
  
Once the user has selected the cohort, marker, participants to be used and covariables to consider, the tool computes a Aβ+ predictor model using the selected information. Note that before computing the model, samples that do not have a recorded value for any of the selected parameters are removed. The model is computed using a binomial generalized linear model as follows:


* *model <- glm(Aβ status ~ marker + covar1 + covar2 + ..., family="binomial")*
 
 <br />

##### ***Model Results***

Given the computed model this tool allows the user to manually set an individual parameters and compute the Amyloid probability Score (APS) based on the population and Aβ+ prevalence of the data used to create the model. Note that for the displayed results the tool also compute the prediction error (95% CI) defined by the model standard error. Additionally, the tool allows the user to define sensitivity and specificity thresholds on the multivariable model and an univariable model (only using the user defined marker) such that it is possible to designate areas of risk of being Aβ+. The cutoff lines on the plots are obtained from the results obtained from ROC curves generated in each model.


