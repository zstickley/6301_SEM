## Midterm Exam Starter Script ##

# Install and/or library necessary packages
install.packages("lavaan")
library(lavaan)

## Read in data ##
dat <- read.csv(file.choose())

## Check Data ##
head(dat)

### Effects Coding ###
### Configural ###

## Define the configural model
## Nothing is constrained between groups
conf <- ' 
# Loadings

Agency =~ c(L11a,L11b)*agen_1 + 
          c(L12a,L12b)*agen_2 + 
          c(L13a,L13b)*agen_3
          
Achiev =~ c(L21a,L21b)*achiev_1 + 
          c(L22a,L22b)*achiev_2 + 
          c(L23a,L23b)*achiev_3 
          
Motiv  =~ c(L31a,L31b)*motiv_1 + 
          c(L32a,L32b)*motiv_2 + 
          c(L33a,L33b)*motiv_3 

# Latent Variance

Agency~~c(NA,NA)*Agency
Achiev~~c(NA,NA)*Achiev
Motiv ~~c(NA,NA)*Motiv

# Latent Intercepts

Agency~c(NA,NA)*1
Achiev~c(NA,NA)*1
Motiv ~c(NA,NA)*1

# Item Intercepts

agen_1   ~ c(T11a,T11b)*1
agen_2   ~ c(T12a,T12b)*1
agen_3   ~ c(T13a,T13b)*1
achiev_1 ~ c(T21a,T21b)*1
achiev_2 ~ c(T22a,T22b)*1
achiev_3 ~ c(T23a,T23b)*1
motiv_1  ~ c(T31a,T31b)*1
motiv_2  ~ c(T32a,T32b)*1
motiv_3  ~ c(T33a,T33b)*1

# Model Constraints
L11a==3-L12a-L13a
L21a==3-L22a-L23a
L31a==3-L32a-L33a

L11b==3-L12b-L13b
L21b==3-L22b-L23b
L31b==3-L32b-L33b

T11a==0-T12a-T13a
T21a==0-T22a-T23a
T31a==0-T32a-T33a

T11b==0-T12b-T13b
T21b==0-T22b-T23b
T31b==0-T32b-T33b
'
confit<- lavaan(model=conf, 
                data=dat, 
                missing="FIML", 
                estimator="ML", 
                std.lv=F, auto.var=T, auto.fix.first=F, auto.cov.lv.x=T, int.ov.free=T, 
                group = "class",
                meanstructure=TRUE)

summary(confit, 
        fit.measures=T,
        standardized=T,
        rsquare=F)

## Get fit statistics for table
fitmeasures(confit)[c("chisq","df","pvalue","cfi","tli",
                      "srmr","rmsea","rmsea.ci.lower",
                      "rmsea.ci.upper")]

### Weak Invariance ###
## Constrain the item loadings to be equal between groups.
## HINT: Make the labels match within the parentheses
##       next to the loadings, then remove or "comment
##       out" the unneeded constraints at the bottom of
##       the model script.

weak <- ' 

'
weakfit<- lavaan(model=weak, 
                 data=dat, 
                 missing="FIML", 
                 estimator="ML", 
                 std.lv=F, auto.var=T, auto.fix.first=F, auto.cov.lv.x=T, int.ov.free=T, 
                 group = "class",
                 meanstructure=TRUE)

summary(weakfit, 
        fit.measures=T,
        standardized=T,
        rsquare=F)

## Get fit statistics for table
fitmeasures(weakfit)[c("chisq","df","pvalue","cfi","tli",
                       "srmr","rmsea","rmsea.ci.lower",
                       "rmsea.ci.upper")]

# Check if change in CFI is less than .01
fitmeasures(confit)["cfi"] - fitmeasures(weakfit)["cfi"]

(fitmeasures(confit)["cfi"] - fitmeasures(weakfit)["cfi"])<=.01

## Fill out Yes/No column on your table

### Strong ###
## Constrain the item intercepts to be equal between groups.
## HINT: Make the labels match within the parentheses
##       next to the intercepts, then remove or "comment
##       out" the unneeded constraints at the bottom of
##       the model script.
strong <- ' 

'
strongfit<- lavaan(model=strong, 
                   data=dat, 
                   missing="FIML", 
                   estimator="ML", 
                   std.lv=F, auto.var=T, auto.fix.first=F, auto.cov.lv.x=T, int.ov.free=T, 
                   group = "class",
                   meanstructure=TRUE)

summary(strongfit, 
        fit.measures=T,
        standardized=T,
        rsquare=F)

## Get fit statistics for table
fitmeasures(strongfit)[c("chisq","df","pvalue","cfi","tli",
                         "srmr","rmsea","rmsea.ci.lower",
                         "rmsea.ci.upper")]

# Check if change in CFI is less than .01
fitmeasures(weakfit)["cfi"] - fitmeasures(strongfit)["cfi"]

(fitmeasures(weakfit)["cfi"] - fitmeasures(strongfit)["cfi"])<=.01

## Fill out Yes/No column on your table

### Phantom Constructs ###
## See "Translating Strong to Phantom" script
## if you need help with this model.

phantom <- ' 

'
phantomfit<- cfa(model=phantom, 
                 data=dat, 
                 missing="FIML", 
                 estimator="ML", 
                 std.lv=TRUE, 
                 group = "class",
                 meanstructure=TRUE)

summary(phantomfit, 
        fit.measures=T,
        standardized=T,
        rsquare=F)

# Check to ensure model fit has not changed
fitmeasures(strongfit)[c("chisq","df","pvalue","cfi","tli",
                         "srmr","rmsea","rmsea.ci.lower",
                         "rmsea.ci.upper")]
fitmeasures(phantomfit)[c("chisq","df","pvalue","cfi","tli",
                          "srmr","rmsea","rmsea.ci.lower",
                          "rmsea.ci.upper")]
