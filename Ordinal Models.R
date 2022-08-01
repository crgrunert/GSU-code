######################################################################
########################### Ordinal Models ###########################
######################################################################

setwd("C:/Users/evanl/Desktop/Fall 2021 8830 TA/R Tutorials") ## Working directory
set.seed(112460) ## Seed for replicability

require(plyr) ## Utility tools
require(dplyr)## Utility tools
require(lmtest) ## Supplemental and postestimation tests
require(sandwich) ## Specific for the sandwich version of robust SE calculations
require(ggplot2) ## Graphical presentation
require(stargazer) ## Tables
require(car) # Companion to Applied Regression
require(carData) # Supplemental Data
require(gmodels) # Crosstabs
require(ggplot2) # Primary Graphs
require(MASS) # For polr
require(brant) # For brant test
require(margins) 
require(jtools) 
require(ggstance) 
require(broom.mixed) 
require(Rcpp) 
require(sjPlot)
require(sjmisc) 
require(ggeffects)
require(plm)
require(lmtest)
require(ordinal) # CLM
require(reshape2)
require(effects)

# Getting Data from the carData package

wvs<-carData::WVS
ces<-carData::CES11
wlf<-carData::Womenlf

# Factor issue illustration

e1 <- polr(importance ~ abortion + gender + education + urban, data=ces, Hess = TRUE)
e2 <- polr(importance ~ as.numeric(abortion) + as.numeric(gender) + as.numeric(education) + as.numeric(urban), data=ces, Hess = TRUE)

stargazer(e1, e2)

# WVS: World Values Survey with the ordered outcome of "Do you think that what the government is doing 
# for people in poverty in this country is about the right amount, too much, or too little?" as our outcome

summary(wvs)

# poverty: Too Little(1), About Right(2), Too Much(3) - factor
# religion: Member of a religion: no(1) or yes(2) - factor
# degree: Held a university degree: no(1) or yes(2) - factor
# country: Australia(1), Norway(2), Sweden(3), or USA(4) - factor
# age: in years - integer
# gender: female(1) or male(2) - factor

# Running the first polr model
wvs_fit<-polr(poverty ~ religion + degree + country + age + gender, data=wvs, Hess = TRUE)
summary(wvs_fit) # summarizing the results (to include SEs)
brant(wvs_fit) # PLA Test
poTest(wvs_fit) # PLA Test

# CES: Canadian Election Survey with the ordered outcome of 'importance of religion' as our outcome

summary(ces)

# Province: Canadian provinces - factor
# Population: raw number - integer
# Weight: Survey weighting - numeric
# gender: female(1) or male(2) - factor
# abortion: no(1) or yes(2) - factor
# importance [of religion]: not(1), notvery(2), somewhat(3), very(4) - factor
# education: bachelors(1), college(2), higher(3), HS(4), lessHS(5), somePS(6) - factor
# urban: rural(1), urban(2) - factor

# Running the second polr model

ces_fit<-polr(importance ~ abortion + gender + education + urban, data=ces, Hess = TRUE)
summary(ces_fit)
brant(ces_fit)
poTest(ces_fit)

# WLF: Survey of Canadian Women's Labour-Force Participation with `partic' as the ordered outcome

summary(wlf)

# partic: full time (1), not working (2), part time(3) - factor
# hincome: household income - integer
# children: absent(1), present(2) - factor
# region: CA Region - Atlantic(1), BC(2), Ontario(3), Prairie(4), Quebec(5) - factor

# Changing the partic measure to be ordered in a sensible manner

wlf$partic<-as.numeric(wlf$partic)
wlf$partic<-replace(wlf$partic, wlf$partic==1, 4)
wlf$partic<-replace(wlf$partic, wlf$partic==2, 1)
wlf$partic<-replace(wlf$partic, wlf$partic==3, 2)
wlf$partic<-replace(wlf$partic, wlf$partic==4, 3)
wlf$partic<-as.factor(wlf$partic)

CrossTable(wlf$partic)

# Running the third polr model

wlf_fit<-polr(partic ~ hincome + children + region, data=wlf, Hess = TRUE)
summary(wlf_fit)
brant(wlf_fit)
poTest(wlf_fit)

#####################################################################################

# Making all variables numeric for use with margins

wvs2<-data.frame(lapply(wvs,as.numeric))
ces2<-data.frame(lapply(ces,as.numeric))
wlf2<-data.frame(lapply(wlf,as.numeric))

# Re-running all of the models with numeric covariates for margins

wvs_fit2<-polr(as.factor(poverty) ~ religion + degree + country + age + gender, data=wvs2, Hess = TRUE)
ces_fit2<-polr(as.factor(importance) ~ abortion + gender + education + urban, data=ces2, Hess = TRUE)
wlf_fit2<-polr(as.factor(partic) ~ hincome + children + region, data=wlf2, Hess = TRUE)

#####################################################################################

# PLA Testing

brant(wvs_fit2)
poTest(wvs_fit2)

brant(ces_fit2)
poTest(ces_fit2)


brant(wlf_fit2)
poTest(wlf_fit2)

# Running the clm approach to relegate those covariates which violate the PLA to nominality

clm_fit<-clm(as.factor(poverty) ~ religion + age + gender, nominal = ~ degree + country, data=wvs2)
summary(clm_fit)

# Exluding those covariates with violate the PLA for use in margins and predicted probabilities 
# CLM is a better approach, but the clm isn't generally supported 

wvs_fit3<-polr(as.factor(poverty) ~ religion + age + gender, data=wvs2, Hess = TRUE)
wlf_fit3<-polr(as.factor(partic) ~ region, data=wlf2, Hess = TRUE)

###############################################################################################

# Marginal Effects

# Careful - these take a long time to run

# I've place the halt statement - a non command - to prevent mistakenly running this section of code and thus stalling the R session

halt 

wvs_m<-margins(wvs_fit2)
wvs_m_reli<-margins(wvs_fit2, at = list(religion=1:2))
wvs_m_degr<-margins(wvs_fit2, at = list(degree=1:2))
wvs_m_coun<-margins(wvs_fit2, at = list(country=1:4))
wvs_m_age <-margins(wvs_fit2, at = list(age=min(wvs$age):max(wvs$age)))
wvs_m_gend<-margins(wvs_fit2, at = list(gender=1:2))
wvs_m_full<-margins(wvs_fit2, at = list(religion=1:2, degree=1:2, country=1:4, age=mean(wvs$age), gender=1:2))

ces_m<-margins(ces_fit2)
ces_m_abor<-margins(ces_fit2, at = list(abortion=1:2))
ces_m_gend<-margins(ces_fit2, at = list(gender=1:2))
ces_m_educ<-margins(ces_fit2, at = list(education=1:6))
ces_m_urba<-margins(ces_fit2, at = list(urban=1:2))
ces_m_full<-margins(ces_fit2, at = list(abortion=1:2, gender=1:2, education=1:6, urban=1:2))

wlf_m<-margins(wlf_fit2)
wlf_m_inco<-margins(wlf_fit2, at = list(hincome=min(wlf$hincome):max(wlf$hincome)))
wlf_m_kids<-margins(wlf_fit2, at = list(children=1:2))
wlf_m_regi<-margins(wlf_fit2, at = list(region=1:5))
wlf_m_full<-margins(wlf_fit2, at = list(hincome=mean(wlf$hincome), children=1:2, region=1:5))



wvs_m
wvs_m_reli
wvs_m_degr
wvs_m_coun
wvs_m_age 
wvs_m_gend
wvs_m_full

ces_m
ces_m_abor
ces_m_gend
ces_m_educ
ces_m_urba
ces_m_full

wlf_m
wlf_m_abor
wlf_m_inco
wlf_m_kids
wlf_m_regi
wlf_m_full

#####################################################################################

# Predicted Probabilities: Basic format

summary(wlf_fit3) # Reminder of the model being used

wlf_pred <-predict(wlf_fit3, type = "probs") # These are the predicted probabilities based on the mean value of all variables in the model

pred_data_1 <- data.frame(region=c(1, 2, 3, 4, 5)) # Specifying region variable to values 1->5
pred_data_2<-pred_data_1 # Copying the data for use in the second prediction

## Fitting predicted probabilities with region at specified values
# All other variables held at mean
pred_data_1[, c("pred.prob")] <- predict(wlf_fit3, newdata=pred_data_1, type="probs") 

pred_data_1 # Read as the predicted probability of response at each category, based on the IV value

# Use "class" for the predicted category

pred_data_2[, c("pred.prob")] <- predict(wlf_fit3, newdata=pred_data_2, type="class") ## Predicted class, based on 
pred_data_2



# A bit more involved example: where religion is held at the median (given that it is a binary var)
# Age and gender are allowed to vary
# Note how each combination is included with 2 binary vars

summary(wvs_fit3)

pred_data_3 <- data.frame(religion=rep(median(as.numeric(wvs$religion)),4),
                     age=c(1,2,1,2),
                     gender=c(1,1, 2, 2))

pred_data_3[, c("pred.prob")] <- predict(wvs_fit3, newdata=pred_data_3, type="probs", se.fit=TRUE)

pred_data_3




# Scaling upwards in complexity
# Looking at the effect of increasing education while holding the other vars at 1
# data_4 and data_5 are identical, just specfied differently

summary(ces_fit2)

pred_data_4 <- data.frame(abortion=rep(1,6),
                     gender=rep(1,6),
                     urban=rep(1,6),
                     education=c(1,2, 3, 4, 5, 6))


pred_data_5 <- data.frame(abortion=c(1,1,1,1,1,1),
                     gender=c(1,1,1,1,1,1),
                     urban=c(1,1,1,1,1,1),
                     education=c(1,2, 3, 4, 5, 6))

# This is the brutal one, where all iterations are allowed. The only way I've found to do this is use the spacing to align the numbers
# Note that I deal with the binary vars first -- figuring out that series of combinations -- then repeat for each level of education

pred_data_6 <- data.frame(abortion=c(      1, 2, 1, 1, 2, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 2),
                          gender=c(        1, 1, 2, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 2, 1, 2) ,
                          urban=c(         1, 1, 1, 2, 1, 2, 2, 2, 1, 1, 1, 2, 1, 2, 2, 2, 1, 1, 1, 2, 1, 2, 2, 2, 1, 1, 1, 2, 1, 2, 2, 2, 1, 1, 1, 2, 1, 2, 2, 2, 1, 1, 1, 2, 1, 2, 2, 2) ,
                          education=c(     1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6)) 

pred_data_4[, c("pred.prob")] <- predict(ces_fit2, newdata=pred_data_4, type="probs")
pred_data_5[, c("pred.prob")] <- predict(ces_fit2, newdata=pred_data_5, type="probs")
pred_data_6[, c("pred.prob")] <- predict(ces_fit2, newdata=pred_data_6, type="probs")

pred_data_4
pred_data_5
pred_data_6


# Especially data_6, but all of the previous are likely not useful for your work, rather an illustration of how pred.probs works in r
# The following is more in line with what you want - to vary one var at a time to look at the effect
# Note the use of median over mean in these - they are binary so a value of 1.6 doesn't make logical sense

pred_data_7 <- data.frame(abortion=rep(median(ces2$abortion),6),
                     gender=rep(median(ces2$gender),6),
                     urban=rep(median(ces2$urban),6),
                     education=c(1, 2, 3, 4, 5, 6)) 


pred_data_7[, c("pred.prob")] <- predict(ces_fit2, newdata=pred_data_7, type="probs")

pred_data_7



# Four options for visualization

#1: Visualization from effects package
#                                 This is the 
#                                 var you want
#                                 to examine
#                                     ||
#                                     \/
plot(Effect(focal.predictors = c("education"), mod = ces_fit2), 
               rug = FALSE, style = "stacked", main = "Effect of Education on Probability of Religious Importance",
                ylab = "Probability of Importance Response")

#2: Visualization from effects package
#                                 These are the vars you                                            This is the variable by which
#                                     want to examine                                                 the plots are separated
#                                     ||         ||                                                       ||
#                                     \/         \/                                                       \/
plot(Effect(focal.predictors = c("education", "abortion"), mod = ces_fit2, latent = TRUE, xlevels = list(abortion = 1:2)), 
     rug = FALSE, main = "Effect of Education on Importance of Religion, by Abortion View", ylab = "Probability of Importance Response",
     xlab = "Education") # Note the absence of the style= option here; this is the default



#3: Predicted probabilities from predict() function

# Providing the values desired: here setting binaries to median, and varying education to visualize edu's effect
pred_data_8 <- data.frame(abortion=rep(median(ces2$abortion),6),
                          gender=rep(median(ces2$gender),6),
                          urban=rep(median(ces2$urban),6),
                          education=c(1, 2, 3, 4, 5, 6)) 


# combining the predicted probabilities to the values provided above; by column
ggdata<-cbind(pred_data_8, predict(ces_fit2, pred_data_8, type = "probs"))

# Checking 
head(ggdata)
head(pred_data_7)

# Note the difference in form between this and the previous usage of predict


ggdata2 <- melt(ggdata, id.vars = c("abortion", "gender", "urban", "education"),
               variable.name = "Level", value.name="Probability")

ggdata2 # molten effect (not class) for use in ggplot

ggplot(ggdata2, aes(x = education, y = Probability, colour = Level)) +
  geom_line(size=1.2) + ggtitle("Predicted Probability of Importance of Religion, by level of Education") + 
  ylab("Probability") + xlab("Education") + scale_x_continuous(breaks=c(1,2,3, 4, 5, 6),
                                                               labels=c("1","2","3", "4", "5", "6"))


#4: Using ggeffects package
ggdata3 <- ggpredict(ces_fit2, terms = c("education"))

plot(ggdata3)

ggplot(ggdata3, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)

###################################################################################################
#######################################     Diagnostics     #######################################
###################################################################################################

# To be updated within the week
