#load training data in as MedData and prediction data as MedDataNew

#If the response is called dropout. Bivariate variable like 'dropout','active'. 
#Make sure the opposite of dropout is the reference level in the factor
#in order to atain predicted risk of dropping out
fit.null <- glm(factor(dropout)~1,family = binomial, data=MedData)
fitBIC <- step(fit.null,~MAT_Niveau+MATGrade+priop+kvotient+kvote+gender+ADGGRU+ageAtEnrolment,k=log(nrow(MedData)))
#the formula should contain all the relevant predictors. If these names are always the same
#it can just be reused each time. I also think it is fairly easy to make it from the data
#if we know in which columns the predictors are located.

#The predicted probabilities can then be added to the new data with
MedDataNew$risk <- predict(fitBIC,MedDataNew,type = 'response')
#The names of the predictors in the new data should be the same as those in the training data
#and for categorical variables no new categories should be introduced.

#Then it is just a question of adding some information about the model (training cohorsts and semester)
#and to pick the relevant colums. If it should be added to an existing dataframe use
rbind.fill(existingData,MedDataNew)
#missing colums will be filled with NA