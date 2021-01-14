install.packages("lavaan")
library(lavaan)

### Simulate Data ###
## The population model the data will be simulated from
modVec <- ("LV1 =~ .868*V11 + 
                   .821*V12 +
                   .656*V13 
                   
            LV2 =~ .699*V21 + 
                   .787*V22 +
                   .874*V23 +
                   .717*V24 
                   
                   ## factor correlation

                     LV1 ~~ .519*LV2")


dat  <- simulateData(modVec,
                     std.lv=T,
                     meanstructure=T, 
                     model.type="cfa",
                     sample.nobs=500,
                     seed=1234567)

### Set the data to be descrete values between 1 and 5
for (i in 1:length(names(dat))) {
        dat[,i] <- dat[,i]+abs(min(dat[,i]))
        dat[,i] <- dat[,i]/max(dat[,i])
        dat[,i] <- (dat[,i]*4)+1
        dat[,i] <- round(dat[,i])
}

dat$ID <- sample(1000:9999,nrow(dat),replace = F)

### Make a grouping variable 
group_sample <- sample(nrow(dat),nrow(dat)*.5)

dat$G  <- "A"
dat$G[group_sample]  <- "B"

dat$G <- as.factor(dat$G)

### Examine simulated data
head(dat)

### Ampute Data ###

install.packages("mice")
library(mice)

patMat <- matrix(c(0,0,0,0,1,0,0,1,0,
                   0,1,0,1,0,1,0,1,1,
                   1,1,0,1,0,0,1,1,1,
                   0,1,1,0,0,1,1,1,0,
                   0,0,0,1,1,1,1,1,1,
                   1,0,0,0,0,1,1,1,1,
                   1,0,1,0,1,0,0,1,0,
                   0,1,1,1,0,1,0,1,1),ncol = 9,byrow = T)

wgtMat <- matrix(c(0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0,
                   0,0,0,0,0,0,0,0,0),ncol = 9,byrow = T)

datMAR <- ampute(data = dat,
                           prop = .3,
                           mech = "MAR",
                           type = "RIGHT",
                           patterns = patMat,
                           weights = wgtMat)$amp
### MICE Imputation

datMAR$G <- as.factor(datMAR$G)


summary(datMAR)

Imp <- mice(data = datMAR,
            m = 100,     # Generates 10 Imputed data sets
            maxit = 100) # Each dataset undergoes 100 iterations

#### SEM Testing
### Model Comparison

### Fixed Factor ###
mod2 <- ' 
LV1 =~ V11 + V12 + V13
'

##### Full Data #####
fitF<- cfa(model=mod2, 
           data=dat, 
           missing="FIML", 
           estimator="ML", 
           std.lv=TRUE,
           meanstructure=TRUE)

summary(fitF, 
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

