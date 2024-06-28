#---------------------------------------------------------------------------------------------------
# Appendix1_hyperparameters.R: leaning on BRT hyper-parameters to fit model
#---------------------------------------------------------------------------------------------------

# Fit a simple model to practice and consider all the information given on Elith et al. (2008)
# which is given below to fit your model.

#Load data
wd<- paste0(output_data, "/folds_dataset")
setwd(wd)
train <- read.csv("folds_dataset.csv", sep = ";")
names(train)
head(train)

#Set categorical predictors as categories:
train <- train %>% 
  mutate(fold = factor(train$fold),
         Cloud.cover = factor(train$Cloud.cover))

#Set continuous predictors as numerical:
train <- train %>%
  mutate(TL = as.numeric(gsub(",", ".", TL)),  # Convert comma decimal to dot decimal
         diff_at_sbt = as.numeric(gsub(",", ".", diff_at_sbt)),
         LN_MinsExposedtoAir = as.numeric(gsub(",", ".", LN_MinsExposedtoAir)),
         Average_speed = as.numeric(gsub(",", ".", Average_speed)),
         LN_TotalBiomassHaul = as.numeric(gsub(",", ".", LN_TotalBiomassHaul)),
         Trawl_duration = as.numeric(gsub(",", ".", Trawl_duration)),
         RN = as.numeric(gsub(",", ".", RN)),
         Alive_Dead = as.numeric(gsub(",", ".", Alive_Dead)),
         MinsExposedtoAir = as.numeric(gsub(",", ".", MinsExposedtoAir)),
         TotalBiomassHaul = as.numeric(gsub(",", ".", TotalBiomassHaul)))
head(train)

train <- train[, !names(train) %in% c("tripID", "organismID", "hour", "DW", "Sex", "Sea.state",
                                      "time", "time_hours", "at_celsius", "bathy", "sbt_merged")]

summary(train)

#List the name of the predictor variables
vars <- c("TL", "diff_at_sbt", "Cloud.cover", "LN_MinsExposedtoAir",
          "Average_speed", "Trawl_duration", "RN", "LN_TotalBiomassHaul") 

#Understand the hyper-parameters based on Elith et al. (2008): 
# Number of  trees (nt), learning rate (lr),and tree complexity (tc), bag fraction:

#####* 1. ‘bag fraction’: 
#* Controls stochasticity by specifying the proportion of data to be selected at each step. 
#* The  default  bag  fraction  is  0·5,  meaning  that,  at each iteration, 50% of  the data are drawn at random, 
#* without replacement, from the full training set. Optimal bag fractions can be established by comparing predictive 
#* performance and model-to-model variability under different bag fractions. 
#* In our  experience,  stochasticity  improves  model  performance,and 
#* IMPORTANT: fractions in the range 0·5–0·75 have given best results for presence–absence  responses.  

#####* 2. ‘learning rate and number of trees’: 
#* lr is used to shrink the contribution of  each tree as it is added  to  the  model.  

#* IMPORTANT: Decreasing  (slowing)  lr increases  the number  of   trees (nt)  required,  
#* in  general  a  smaller  lr (and larger nt) are preferable. Although this is conditioned by the number of  
#* observations  and  time  available  for  computation. It is recommended fitting models with at least 1000 trees.

#* TESTING: The  usual approach is to estimate optimal nt and lr with an independent test set or with CV, 
#* using deviance reduction as the measure of success. GOAL: Our aim here is to find the combination of parameters (lr, tc and nt) 
#* that achieves minimum predictive error (minimum error for predictions to independent samples). 
#* Slower lr values are generally preferable to faster ones, because they shrink the contribution of  each tree  more,  
#* and  help  the  final  model  to  reliably  estimate  the response. 

#####* 3. ‘tree complexity’: 
#* Tree complexity – the number of  nodes in a tree – also affects the optimal nt. 
#* For a given lr, fitting more complex trees leads to fewer trees being required for minimum error. 
#* So, as tc is increased, lr must be decreased if sufficient trees are to be fitted.  
#* Theoretically,  the  tc should  reflect  the true interaction order in the response being modelled, 
#* but as this is almost always unknown, tc is best set with independent data.
#* Sample  size  influences  optimal  settings  for  lr and tc.

#* IMPORTANT: As a general guide, lr needs to be decreased as tc increases to give approximately the same nt,
#* you will also have to consider the trade-off of computing time. 

#####* IDENTIFYING THE OPTIMAL PARAMETERS: CROSS-VALIDATION:
#* Techniques such as cross-validation (CV) are used for model development and/or evaluation.
#* Cross-validation provides a means for testing the model on withheld portions of  data, while still using all data at some stage to fit the model. 

#* METHOD:
#* 1. Randomly divide available data into n subsets (Elith used 10)

#* 2. Make n (=10) different training sets each comprising a unique combination
#* of 9 subsets. Therefore for each training set there is a unique omitted subset that is used for testing.

#* 3. Starting with a selected number fo trees (nt), say 50, develop 10 BRT models simultaneously
#* on each training set, and test predictive performance on their respective omitted data.
#* Both mean performance and standard errors are recorded. 

#* 4. Step forward and increase the nt in each model by a selected and constant amount, and repeat step 3.

#* 5. Repeat step 4 and after 10 steps start comparing the predictive performance of the 6th
#* and 10th previous iterations against that of the current 5th previous ones.
#* Once the average of the more recent set is higher than the average of the previous set, 
#* the minimum has been passed.

#* 6. Step and record the minimum, this is the optimal nt.

#---------------------------------------------------------------------------------------------------
#* IMPORTANT: Most usually optimal values range:
#---------------------------------------------------------------------------------------------------
#* tc - tree.complexity = 1 - 5 (usually try 1, 3 and 5); 

#* lr - learning.rate = 0.001 - 0.01 (usually try 0.001, 0.005, 0.05 and 0.01)

#* bag.fraction = 0·5 – 0·75 (usually try 0.5, 0.6 and 0.7)

#* You will make a combination of these usually potential optimal values and compare 
#* the results of all the potential combinations, selecting the best one.

# By now start just with a simple model: 

brt <- gbm.step(data=train, gbm.x = vars, gbm.y = "Alive_Dead",
                    family = "bernoulli", tree.complexity = 5,
                    learning.rate = 0.01, bag.fraction = 0.6,
                    fold.vector = train$fold,
                    n.folds = length(unique(train$fold)))

ggPerformance(brt)


#* Look at the graph and follow Elith et al. (2008) recommendations to check the performance of the model fitting. 
#* This particular model was built with the default 10-fold cross-validation. 
#* The solid black curve is the mean, and the dotted curves about 1 standard error, 
#* for the changes in predictive deviance (i.e., as measured on the excluded folds of the cross-validation). 
#* The red line shows the minimum of the mean, and the green line the number of trees at which that occurs. 
#* The final model that is returned in the model object and it is built on the train data set, 
#* using the number of trees identified as optimal.

#---------------------------------------------------------------------------------------------------
#########* IMPORTANT: Elith et al. (2008) recommendations are:
#---------------------------------------------------------------------------------------------------

#* 1. Convergence between red and green line over 1000 trees (number of trees where the minimum of the mean occurs).

#* 2. The lowest cv_deviance.

#* 2. Prioritise models with bigger lr (quicker) and larger nt and tc

#* 3. Visually check the graph and it is better if the cv_deviance curve has a progressive but steep descent with convergence over 1000 trees (never less) and 
#* then stabilization remaining straight (never going up again, which would mean over fitting, losing predicting capacity). 

#* 4. AUC: the higher the better (1 is the highest).

#IN THIS CASE, THE RESULT IS:
#* fitting final gbm model with a fixed number of 1150 trees for NA
#* mean total deviance = 1.255 
#* mean residual deviance = 0.361 
#* estimated cv deviance = 0.606 ; se = 0.02 
#* training data correlation = 0.884 
#* cv correlation =  0.755 ; se = 0.018 
#* training data AUC score = 0.985 
#* cv AUC score = 0.925 ; se = 0.009 
#* elapsed time -  0.26 minutes 


#### CHECK INFLUENCE OF VARIABLES:
# Check the relative influence of each predictor:
#* The measures are based on the number of  times a variable  is  selected  for  splitting, 
#* weighted  by  the  squared improvement to the model as a result of  each split, and aver-aged over all trees
#* The relative influence (or contribution) of  each variable is scaled so that the sum adds to 100, with higher 
#* numbers indicating stronger influence on the response.
summary(modtest)

#### PARTIAL DEPENDENCE PLOTS (variable response curves):
#Then you plot the results, once you have your best model:
#* While these graphs are not a perfect representation of  the effects of  each variable,  particularly  if   
#* there  are  strong  interactions #* in  the data or predictors are strongly correlated, they provide a 
#* useful basis for interpretation.

gbm.plot(modtest, n.plots=8, plot.layout=c(2, 4), write.title = FALSE)


#### IDENTIFYING IMPORTANT INTERACTIONS: IN OUR CASE WE WON'T USE THIS.
#* tc controls  the maximum level of  interaction that can be quantified, but no information  is  
#* provided  automatically  on  the  nature  and magnitude of  fitted interaction effects. 
#* To quantify these, we use a function that creates, for each possible pair of  predictors, a temporary grid of 
#* variables representing combinations ofvalues at fixed intervals along each of  their ranges. 
# Once identified, interactions can be visualized withjoint partial dependence plots.

### PREDICTIVE PERFORMANCE:
#* Prediction  to  any given site uses the final model, and consists of  the sum of predictions 
#* from  all  trees  multiplied  by  the  learning  rate.
#* Predicting based on the testing dataset gives you a measure of the accuracy of your model.

## Model evaluation
#We will use the testing data to evaluate the models
#Load data
wd<- paste0(output_data, "/training_testing")
setwd(wd)
test <- read.csv("test_dataset.csv", sep = ";")
names(test)
head(test)

#Set categorical predictors as categories: none
#Set continuous predictors as numerical:
test <- test %>%
  mutate(TL = as.numeric(gsub(",", ".", TL)),  # Convert comma decimal to dot decimal
         diff_at_sbt = as.numeric(gsub(",", ".", diff_at_sbt)),
         LN_MinsExposedtoAir = as.numeric(gsub(",", ".", LN_MinsExposedtoAir)),
         Average_speed = as.numeric(gsub(",", ".", Average_speed)),
         LN_TotalBiomassHaul = as.numeric(gsub(",", ".", LN_TotalBiomassHaul)),
         Trawl_duration = as.numeric(gsub(",", ".", Trawl_duration)),
         RN = as.numeric(gsub(",", ".", RN)),
         Alive_Dead = as.numeric(gsub(",", ".", Alive_Dead)),
         Cloud.cover = as.numeric(gsub(",", ".", Cloud.cover)))
head(test)

test  <- test [, !names(test ) %in% c("tripID", "organismID", "hour", "TotalBiomassHaul", "DW", "Sex", "Sea.state", "MinsExposedtoAir",
                                      "time", "time_hours", "at_celsius", "bathy", "sbt_merged")]
summary(test)

## predict on the testing dataset
test$brt_pred <- gbm::predict.gbm(newdata = test, object = modtest, n.trees=modtest$gbm.call$best.trees, type = "response")

# evaluate
e <- evaluate(p = as.numeric(test$brt_pred[test$Alive_Dead == 1]),
              a = as.numeric(test$brt_pred[test$Alive_Dead == 0]))
e

#RESULTS:
# class          : ModelEvaluation 
# n presences    : 335 
# n absences     : 61 
# AUC            : 0.8651333 
# cor            : 0.5420899 
# max TPR+TNR at : 0.8106794 

# plot evaluation results
par(mfrow=c(1, 3))
plot(e, 'ROC')
density(e)
boxplot(e, col=c('blue', 'red'))

#Grafica AUC, si sube rapido al 1 es que el AUC es muy alto, muy bueno 
# Mapa densidad: esto es como lo que veíamos antes
# boxplot: presentado ausencias y presencias, para el dataset tested, cpied from workshop: lo que vemos es que todos los valores por encima de 0.4, corresponde con posiciones de presecia. En ausencias tenemos puntos en los que el habiat suitability, casi todos los datos caen con unvalor de prebilidad de habita tmuy bajo, lo cual es muy bueno.

