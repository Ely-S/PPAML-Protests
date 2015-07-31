library(MASS)
library(fdrtool)
require(ggplot2)
library(fGarch)

PakData2 <- read.csv("/home/eli/Desktop/progs/ppaml/PakData2.csv")
summary(PakData2)
sapply(PakData2, sd)

shapiro.test(PakData2$GOVtALLhighhostilityct)
shapiro.test(PakData2$ALLtGOVeventstotals)
shapiro.test(PakData2$GOVtALLeventstotals)
shapiro.test(PakData2$Protests)
# We can conclude these are not normally distributed

# Let's make some plots
plot(PakData2$GOVtALLhighhostilityct)
plot(PakData2$ALLtGOVeventstotals)
plot(PakData2$GOVtALLeventstotals)
plot(PakData2$Protests)

# Let's check if they're exponential
# Reference Exponential
plot(ecdf(rexp(5000, rate = 100)))
hist(rexp(5000, rate = 100))
par(mfrow=c(2,2))
plot(ecdf(PakData2$GOVtALLhighhostilityct), main="GovtAllHighHostility")
hist(PakData2$GOVtALLhighhostilityct, main="GovtAllHighHostility")
plot(ecdf(PakData2$Protests), main="Protests")
hist(PakData2$Protests, main="Protests")

# Exponential rate param estimates for GOVtALLhighhostilityct and Protests
fitdistr(PakData2$GOVtALLhighhostilityct, "exponential")
fitdistr(PakData2$Protests, "exponential")


# These look exponential


# Skewed normal perhaps?
plot(ecdf(PakData2$GOVtALLeventstotals), main="ECDF of GovtAlleventstotals")
hist(PakData2$GOVtALLeventstotals, main="Histogram of GovtAlleventstotals")
# Fit most likely parameters for sn
sf<-snormFit(PakData2$GOVtALLeventstotals)
sim <- rsnorm(1000, sf$par['mean'], sf$par['sd'], sf$par['xi'])
plot(ecdf(sim), main="Best Fit Skewed Normal")
hist(sim, main="Best Fit Skewed Normal")
plot(sim, main="Best Fit Skewed Normal")
plot(PakData2$GOVtALLeventstotals, main="Best Fit Skewed Normal")
# Perform KS test
ks.test(sim, PakData2$GOVtALLeventstotals)
# Reject KS Test, but not by much
# Not Skewed Normal


sf<-snormFit(PakData2$ALLtGOVeventstotals)
sim <- rsnorm(1000, sf$par['mean'], sf$par['sd'], sf$par['xi'])
plot(sim)
plot(PakData2$ALLtGOVeventstotals)
# Perform KS test
ks.test(sim, PakData2$ALLtGOVeventstotals)
# Reject KS Test
# Not Skewed Normal

# Let's test for cauchy
fd<-fitdistr(PakData2$ALLtGOVeventstotals, "cauchy") 
ks.test(rcauchy(10000, fd$estimate['location'], fd$estimate['scale']), PakData2$ALLtGOVeventstotals)
fd<-fitdistr(PakData2$GOVtALLeventstotals, "cauchy") 
ks.test(rcauchy(10000, fd$estimate['location'], fd$estimate['scale']), PakData2$ALLtGOVeventstotals)

# Let's test for logistic
fd<-fitdistr(PakData2$ALLtGOVeventstotals, "logistic") 
ks.test(rlogis(10000, fd$estimate['location'], fd$estimate['scale']), PakData2$ALLtGOVeventstotals)
fd<-fitdistr(PakData2$GOVtALLeventstotals, "logistic") 
ks.test(rlogis(10000, fd$estimate['location'], fd$estimate['scale']), PakData2$ALLtGOVeventstotals)

# Let's test for Poisson
fd<-fitdistr(PakData2$ALLtGOVeventstotals, "poisson", start=list(x=10, shape1=5, shape2=5)) 
ks.test(rcauchy(10000, fd$estimate['lambda']), PakData2$ALLtGOVeventstotals)
fd<-fitdistr(PakData2$GOVtALLeventstotals, "poisson", start=list(shape1=10, shape2=10)) 
ks.test(ppois(1000, fd$estimate['lambda']), PakData2$ALLtGOVeventstotals)


