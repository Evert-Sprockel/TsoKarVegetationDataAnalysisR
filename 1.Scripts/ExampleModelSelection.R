library(tidyverse)
fitnessData<-read.csv("data/fitnessEnvData.csv")
head(fitnessData)
#Plant performance data with many environmental variables collected using soil analysis.
#Number of inflorescences.plant, inflorescence.length..cm. emerg_per (number of seeds germinating) aver_w (seed weight) 


library(lme4) #for linear mixed models
library(car) #for vif() and Anova()
library(DHARMa) #for checking residuals
library(MuMIn) #for backwards selection, aicc, model averaging

fitnessData$InfL <- fitnessData$Inflorescence.length..cm. #renaming that annoying variable

m1 <- lmer(InfL~scale(Fe)+scale(popSize)+scale(organic.matter)+scale(NO3)+(1|pop), data=fitnessData, REML=F)
vif(m1)
plot(simulateResiduals(m1)) #heteroskedasticity!! can indicate unaccounted for variation in residuals, for example due to an unmodelled variable, space, or time. Can also indicate unequal variances across levels of predictor
#variables that are already in the model, which needs to be explicitly modelled (possible through nlme or glmmTMB packages). we will not cover this for now

#we have four different predictor variables. How do we know if we should keep them all?

#option 1 : evaluate the whole model, leave all variables in, because they are important to answering your research questions. They might also be important to leave in if they are a covariate that should be controlled for, 
#even if they themselves do not significantly affect the response variable. This choice should only be made if you have good argument to do so in the context of your research.
#This method is mainly for CONFIRMATORY research (driven by a main hypothesis e.g. treatment-effect) where you have main variables to answer your RQs and maybe some covariates that MUST be in the model.

#for mixed models, whether lmm or glmm: evaluate your model with chi-sq or F tests.
#see https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#tests-of-effects-i.e.-testing-that-several-parameters-are-simultaneously-zero
#From best to worst tests:
# - MCMC / bootstrapping (advanced, very computation-heavy, not shown here)
# - For LMMs ONLY! (not GLMM): F-test with df correction from the pbkrtest package.
# - For LMM or GLMM: likelihood ratio tests e.g. anova(m1, m2) or drop1(model,test="Chisq")
# - Wald chi-sq test: car::Anova(model,type="III"). Can be used but don't bother. this is similar to what is shown in glm output

#first, F-tests with pbkrtest
library(pbkrtest)
m2 <- update(m1, ~.-scale(Fe))
KRmodcomp(m1,m2)

# LLRTs (dropping one variable at a time):
LLRTtable <- drop1(m1, test="Chisq") #makes a table summarizing what would happen if you drop one single variable, for each variable in the model
#we can also do this manually
m2 <- update(m1, ~.-scale(Fe))
anova(m1,m2) #this anova tests the difference between these two models with an LLRT (equivalent to a chisq test). You can see that the results are the same as what we calculated in the table. You can do this one by one if you
#would like to record the significance of each variable one by one.

#Wald test
car::Anova(m1,type="III")

#option 2: backwards selection. dropping variables one by one until you have a model only with significant variables.
#I DO NOT RECOMMEND THIS OPTION except if you are just simplifying your model by removing interactions, or if your statistical skills are not advanced enough for model averaging

#we know from the LLRT table above that we should first drop Fe because it is the least significant. We already have this model represented by m2.
table2 <- drop1(m2, test="Chisq") #tells us we should next drop NO3
table2
m3 <- update(m2, ~.-scale(NO3))
table3 <- drop1(m3, test="Chisq") #now we should drop popSize
table3
m4 <- update(m3, ~.-scale(popSize))
table4 <- drop1(m4, test="Chisq") #actually now the p-value is no longer significant. this demonstrates the weirdness of backwards selection.
table4

AIC(m1, m2, m3, m4)
AICc(m1, m2, m3, m4) #more conservative estimate for small sample sizes

#option 3: model averaging
#Mainly for EXPLORATORY research i.e. you went out into the field and measured lots of things and you don't know what might effect what or which variables to keep in the model.
options(na.action=na.fail)
models <- dredge(m1) #all possible models. you can limit the dredge in many ways, e.g. to only a few variables or excluding combinations of variables. see help pages.
models
modelset <- get.models(models, subset = delta < 2) #subsetting models within 2 AICc of best model
modeltable <- model.sel(modelset) #table of the selected models
modeltable
avgmod <- model.avg(modelset) #averaging the selected models
avgmodsumm <- summary(avgmod) #storing the average model summary
avgmodsumm
coeftable <- as.data.frame(avgmodsumm$coefmat.full) #table of the "full" coefficients, which means that variables were assigned a coefficient of zero if they are not present in the model.


#visualizing a model averaged object
#lets do organic matter
newdataOM <- data.frame(organic.matter=with(fitnessData,seq(min(organic.matter),
                                                            max(organic.matter),
                                                            length.out=nrow(fitnessData))))
newdataOM$popSize <- mean(fitnessData$popSize)
newdataOM$NO3 <- mean(fitnessData$NO3)
newdataOM$Fe <- mean(fitnessData$Fe)
newdataOM$pop <- NA #setting random effects to NA

pred <- predict(avgmod,re.form=NA,full=T,se.fit=T,newdata=newdataOM)
pred <- as.data.frame(pred)
preddata <- cbind(newdataOM,pred)

ggplot(preddata,aes(x=organic.matter,y=fit))+
  geom_line()+
  geom_ribbon(aes(ymin=fit-(1.96*se.fit),ymax=fit+(1.96*se.fit)),alpha=0.2)

