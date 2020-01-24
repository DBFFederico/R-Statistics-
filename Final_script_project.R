#Libraries
library(plyr)
library(wec)
library(rockchalk)
library(MLmetrics)
#INPUT FILES
dataDir="../data/"
fileName="wvs_data.rds"
wvs_data= readRDS(paste0(dataDir,fileName))

#COUNTRY CODING
wvs_data1=wvs_data
wvs_data1$V2d= wvs_data1$V2 #create a new variable for dummy coding countries
wvs_data1$V2d= factor(wvs_data1$V2)## DUMMY CODED VARIABLE (V2d)
wvs_data1$V2w= wvs_data1$V2d #Create a new variable for weighten coding countries, weighted coded (V2w)
contrasts(wvs_data1$V2w) <- contr.wec(wvs_data1$V2d, omitted = "156") #create weighted effect coding for coutnries
contrasts(wvs_data1$V2w)
#------------------------------------#
#QUESTIONS

#1.MULTIPLE LINEAR REGRESSION
#1.1 Which countries are represented in these data?
contrasts(wvs_data1$V2d)

#1.1 What are the sample sizes for each country represented in these data? 
count(wvs_data1, 'V2')

#1.3 Overall, is there a significant effect of country on feelings of happiness?
lm1.3= lm(V10~ V2, data= wvs_data1 )
summary(lm1.3)


#1.4 Which country has the highest level of feelings of happiness?
#Same analysis but with weighted coding method
lm1.4= lm(V10~V2d, data=wvs_data1)
summary(lm1.4)
c= lm1.4$coefficients
tot276= c[1] + c[2]
tot356= c[1] + c[3]
tot643= c[1] + c[4]
tot840= c[1] + c[5]
countries_list= c(tot276,tot356,tot643,tot840,c[1])
c_max=which.max(countries_list)
countries_list[c_max]
#1.5 Which country has the lowest level of feelings of happiness?
lm1.5= lm(V10~V2d, data=wvs_data1)
summary(lm1.5)
c_min=which.min(countries_list)
countries_list[c_min]
#1.6 How do the country-specific levels of feelings of happiness change after controlling for
#subjective state of health?
lm1.6=lm(V10 ~V2d , data = wvs_data1)
summary(lm1.6)
lm1.6.1= lm(V10 ~V11+V2d , data = wvs_data1)
summary(lm1.6.1)
a=lm1.6$coefficients
b=lm1.6.1$coefficients
l156=b[1] - a[1]
l276=b[3] - a[2]
l356=b[4] - a[3]
l643=b[5] - a[4]
l840=b[6] - a[5]
#2. CONTINUOS VARIABLE MODERATION

#2.1 After controlling for country, does the importance people afforded to democracy (DemImp)
#significantly predict the extent to which they think their country is being run
#democratically (DemRun)?
lm2.1= lm(V141~V2+V140, data=wvs_data1)
summary(lm2.1)

#2.2 After controlling for country, does the DemImp ! DemRun effect vary as a function of
#peoples’ satisfaction with their lives (SWL)?
lm2.2= lm(V141~V2d+V140*V23, data=wvs_data1)
summary(lm2.2)

#2.3 Within what range of SWL is the DemImp ! DemRun simple slope from Question 2
#statistically significant?
plotOut2.3 <- plotSlopes(lm2.2,plotx      = "V140",modx       = "V23",modxVals   = "std.dev")
testOut2.3 <- testSlopes(plotOut2.3)
testOut2.3$hypotests # Gives the simple slopes
plot(testOut2.3)

# 3. CATEGORICAL VARIABLE MODERATION

#3.1 After controlling for SWL, does the DemImp ! DemRun effect vary significantly by
#country?
#Used ANOVA test to compare the models, used weighted coding effect because India has 38% of observation in V2
lm3.1.1= lm(V141~V23+V140+V2w, data=wvs_data1)#Additive effect
summary(lm3.1.1)

lm3.1.2= lm(V141~V23+V140*V2w, data=wvs_data1)#moderated effect
summary(lm3.1.2)

anova(lm3.1.1, lm3.1.2)

#3.2 Visualize the results from Question 1 in a suitable way.
plotOut3.2 <- plotSlopes(model      = lm3.1.2,plotx      = "V140",modx       = "V2w",plotPoints = FALSE, main= "categorical moderator")

#3.3 For which country is the effect of DemImp on DemRun strongest, after controlling for
#SWL?
#It is possible to answer calculating the simple slopes
testOut3.3 <- testSlopes(plotOut3.2)

# 3.4For which country is the effect of DemImp on DemRun weakest, after controlling for
#SWL?
#It is possible to answer calculating the simple slopes
testOut3.4 <- testSlopes(plotOut3.2)

#3.5 Are the simple slopes referenced in Questions 3 and 4 statistically significant?
#It is possible to answer calculating the simple slopes
testOut3.5 <- testSlopes(plotOut3.2)

# 4. PREDICTIVE MODELING

#4.1 Select and list three (theoretically justified) sets of predictors (or functions thereof, e.g.,
#interactions or polynomials) to use in predicting FinSat
# 1 SET(V238,V239,V237), 2 SET(V55,V56,V170), 3 SET (V58+V57+V248)

#4.2 Briefly explain why you expect the three sets of predictors you chose in Question 1 to
#perform well. That is, explain your rationale for defining these three sets.

cor(wvs_data, wvs_data$V59)

#4.3 Use 10-fold cross-validation to compare the predictive performance of the three models
#define in Question 1
#Used the same function used in lab 3 
K      <- 10
k      <- 1
data   <- wvs_data
models<- c( "V59 ~ V238+V239+V237",
            "V59 ~ V55+V56+V170",
            "V59 ~ V58+V57+V248")
model  <- models[1]

## Specify a function to do K-Fold Cross-Validation with lm():
cv.lm <- function(data, models, K = 10) {
  ## Create a partition vector:
  part <- sample(rep(1 : K, ceiling(nrow(data) / K)))[1 : nrow(data)]
  
  ## Find the DV:
  dv <- trimws(strsplit(models[1], "~")[[1]][1])
  
  ## Apply over candidate models:
  sapply(X = models, FUN = function(model, data, dv, K, part) {
    ## Loop over K repititions:
    mse <- c()
    for(k in 1 : K) {
      ## Partition data:
      train <- data[part != k, ]
      valid <- data[part == k, ]
      
      ## Fit model, generate predictions, and save the MSE:
      fit    <- lm(model, data = train)
      pred   <- predict(fit, newdata = valid)
      mse[k] <- MSE(y_pred = pred, y_true = valid[ , dv])
    }
    ## Return the CVE:
    sum((table(part) / length(part)) * mse)
    
  },
  data = data,
  K    = K,
  dv   = dv,
  part = part)
}
cv.lm(data, models, K=10)

lm4.4=lm(V59 ~ V55+V56+V170, data=wvs_data)#Checking the R squared
summary(lm4.4)
#4.6 Based on the selection you made in Question 4, what can you say about the attributes that
#are are important for predicting financial satisfaction?
lm6.4=lm(V59 ~ V55+V56+V170, data=wvs_data)#Checking the R squared
summary(lm6.4)
