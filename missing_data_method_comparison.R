setwd("C:/Users/zacks/OneDrive/Documents/R Projects/6301_SEM")
dat <- read.csv("midTermDat.csv",stringsAsFactors = T)

head(dat)

### Ampute Data ###

install.packages("mice")
library(mice)

patMat <- matrix(c(1,1,1,1,0,0,0,1,1,1,1,1,1, # Agency variables missing due to other two factors
                   1,1,1,1,1,1,1,0,0,0,1,1,1, # Achievement variables missing due to other two factors
                   1,1,1,1,1,1,1,1,1,1,0,0,0, # Motivation variables missing due to other two factors
                   1,1,1,1,0,0,0,0,0,0,0,0,0),# Missingness on all construct due to class
                 ncol = 13,byrow = T)

wgtMat <- matrix(c(0,0,0,0,0,0,0,1,1,1,1,1,1, # Agency variables missing due to other two factors
                   0,1,1,1,1,1,1,0,0,0,1,1,1, # Achievement variables missing due to other two factors
                   0,1,1,1,1,1,1,1,1,1,0,0,0, # Motivation variables missing due to other two factors
                   0,0,0,3,0,0,0,0,0,0,0,0,0),# Missingness on all construct due to class
                 ncol = 13,byrow = T)

datMAR <- ampute(data = dat,
                 prop = .3,
                 mech = "MAR",
                 type = "RIGHT",
                 patterns = patMat,
                 weights = wgtMat)$amp

## Set factors back to factors
datMAR$class <- as.factor(datMAR$class)
levels(datMAR$class) <- levels(dat$class)
datMAR$gender <- as.factor(datMAR$gender)
levels(datMAR$gender) <- levels(dat$gender)

## Examine Data
summary(datMAR)

### MICE Imputation
Imp <- mice(data = datMAR,
            m = 100,     # Generates 100 Imputed data sets
            maxit = 100) # Each dataset undergoes 100 iterations

#### SEM Testing
### Model Comparison

### Fixed Factor ###
mod2 <- ' 
Agency =~ agen_1 + 
          agen_2 + 
          agen_3
          
Achiev =~ achiev_1 + 
          achiev_2 + 
          achiev_3 
          
Motiv  =~ motiv_1 + 
          motiv_2 + 
          motiv_3 
'
library(lavaan)
##### Full Data #####
fit<- cfa(model=SD.model, 
           data=Galveston_data, 
           missing="FIML", 
           estimator="ML", 
           std.lv=TRUE,
           meanstructure=TRUE)

summary(fit, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)


##### Missing Data FIML ####
fitFIML<- cfa(model=mod2, 
              data=datMAR, 
              missing="FIML", 
              estimator="ML", 
              std.lv=TRUE, 
              meanstructure=TRUE)

summary(fitFIML, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)


##### Missing Data, Multiple Imputation ####
##### Runs Model on all datasets #####

install.packages("semTools")
library(semTools)

fitMAR <- cfa.mi(model = mod2,
                 data = Imp,
                 estimator="ML", 
                 std.lv=TRUE,
                 meanstructure=TRUE,
                 miPackage = "mice")

summary(fitMAR,standardized = T, fit.measures=TRUE,fmi = T)


