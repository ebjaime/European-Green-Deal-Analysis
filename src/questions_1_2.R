# @ebjaime - NPS - 2021

library(openxlsx)
library(MASS)
library(progress)

# Graphics packages
library(rgl)
library(hexbin)
library(ggplot2)

# GAM
library(mgcv)
library(conformalInference)


# Depth measures packages
#library(DepthProc)
#library(robustbase)

B <- 100000
seed <- 42

# 1000 Tons Of Oil Equivalent to Terajoules = 41.1868
tto_2_tj <- function(tto) {
    return(tto * 41.868)
}


# Load datasets
# In energy balances we dont have the value Final Consumption (FC), but it can be estimated as sum of several
# https://www.eea.europa.eu/data-and-maps/indicators/final-energy-consumption-by-sector-13
energy.balances.1  <- read.xlsx("data/complete_energy_balances.xlsx",sheet=1) # Gross available energy, first sheet should be the best, even if in the presentation we chose the fourth

consump.fossils  <- read.xlsx("data/consumption_solid_fossil_fuels.xlsx",sheet=1) # Inland consumption
consump.gas      <- read.xlsx("data/consumption_gas.xlsx",sheet=1) # Inland consumption
consump.oil.petr <- read.xlsx("data/consumption_oil_petroleum.xlsx",sheet=1) # Inland consumption

consump.renew.1  <- read.xlsx("data/consumption_renewables.xlsx",sheet=1) # Geothermal, inland consump
consump.renew.2  <- read.xlsx("data/consumption_renewables.xlsx",sheet=2) # Solar thermal, inland consump.
consump.renew.3  <- read.xlsx("data/consumption_renewables.xlsx",sheet=3) # Biofuels, inland consump.
consump.renew.4  <- read.xlsx("data/consumption_renewables.xlsx",sheet=4) # Biogases, inland consump.
consump.renew.5  <- read.xlsx("data/consumption_renewables.xlsx",sheet=5) # Renewable waste, inland consump.

percent.renew <- read.xlsx("data/percentage_renewables.xlsx") 

prod.by.fuel <- read.xlsx("data/production_capacities_by_fuel.xlsx")


# Function cleans df removing NAN rows, setting values as numeric...
preprocess <- function(df, start_nrows=6, end_nrows=2, tj=T) { 
    
    # Remove first rows not containing significant data
    df <- df[-c(1:start_nrows,(dim(df)[1]-(end_nrows-1)):dim(df)[1]),]
    
    rownames(df) <- df[,1]
    df <- df[,-1]
    
    rownames(df)[1] <- "year"
    
    # Remove rows with NA values 
    df <- df[!df[,1]==':',]
    
    
    # 3 decimals
    df[] <- lapply(df, as.numeric)  
    
    if(!tj) # All data should be converted to Terajoules
        df[-1,] <- lapply(df[-1,], tto_2_tj)
    
    return(df)
}



energy.balances.p <- preprocess(energy.balances.1, tj=F)

consump.fossils.p <- preprocess(consump.fossils[,-32], tj=T) # wo 2020

consump.fossils.p[-1,] <- consump.fossils.p[-1,] * 25 #  Calorific power of FF
                                 
consump.oil.petr.p <- preprocess(consump.oil.petr[,-32], tj=F) # wo 2020
consump.gas.p      <- preprocess(consump.gas[,-32], tj=T) # wo 2020


consump.renew.1.p <- preprocess(consump.renew.1[,-32])
consump.renew.2.p <- preprocess(consump.renew.2[,-32])
consump.renew.3.p <- preprocess(consump.renew.3[,-32])
consump.renew.4.p <- preprocess(consump.renew.4[,-32])
consump.renew.5.p <- preprocess(consump.renew.5[,-32])

consump.renew.p <- consump.renew.1.p # Sum of all renewable energies
consump.renew.p[-1,] <- consump.renew.p[-1,] + consump.renew.2.p[-1,] + consump.renew.3.p[-1,] +
    consump.renew.4.p[-1,] + consump.renew.5.p[-1,]



# Check sum of all consumptions -------------------------------------------

energy.balances.p[2,1]

# derived.heat <- 2446635 
# electricity  <- 1987708.001 * 18/5 # Gigawatt-hour to TJ
consump.total.p <- 
                    consump.renew.p[2,1] +
                    consump.oil.petr.p[2,1] +
                    consump.fossils.p[2,1] +
                    consump.gas.p[2,1]
                    # derived.heat +
                    # electricity 
consump.total.p

#######################

# Country plot examples ---------------------------------------------------

# Example of energy balances
p_italy <- ggplot(data=as.data.frame(t(energy.balances.p)), aes(x=year, y=Italy)) +
            labs(title="Energy Balances Progression - Italy", 
                 subtitle="Gross Available Energy") +
            xlab("Year") + ylab("Terajoules") +
            geom_point() +
            geom_smooth(method="loess", formula=y~x, fill="blue", colour="darkblue", size=1)
p_italy

# Example of consumption of solid fossil fuels
p_sweden <- ggplot(data=as.data.frame(t(consump.fossils.p)), aes(x=year, y=Sweden)) + #plot of energy balance or consumption of solid fossil fuels?
            labs(title="Consumption Solid Fossil Fuels - Sweden", 
                 subtitle="Inland consumption") +
            xlab("Year") + ylab("Terajoules") +
            geom_point() +
            geom_smooth(method="loess", formula=y~x, fill="red", colour="darkred", size=1)
p_sweden

#######################

#######################
# 1.
#######################
# How are the renewable energies levels of consumptions growing? 
# And the non renewables ones?

year.min <- min(energy.balances.p[1,])
year.max <- max(energy.balances.p[1,])
countries.list <- rownames(consump.fossils.p[-c(1,2,3,4),]) 

energy.balances.countries <- energy.balances.p[-c(1,2,3,4),]
consump.fossils.countries <-consump.fossils.p[-c(1,2,3,4),]
consump.oil.petr.countries <- consump.oil.petr.p[-c(1,2,3,4),]
consump.gas.countries <- consump.gas.p[-c(1,2,3,4),]
consump.renew.countries <- consump.renew.p[-c(1,2,3,4),]

# Consumption plots by country -------------------------------------------------------------------

matplot(seq(year.min, year.max), t(energy.balances.countries), type="l", lty=1,
        main="Energy Balances EU 1990 - 2019", xlab="Year", ylab="Terajoules")

matplot(seq(year.min, year.max), t(consump.fossils.countries), type="l", lty=1,
        main="Consumption of solid fossil fuels EU 1990 - 2019", xlab="Year",
        ylab="Terajoules")

matplot(seq(year.min, year.max), t(consump.oil.petr.countries), type="l", lty=1,
        main="Consumption of Oil and Petroleum EU 1990 - 2019", xlab="Year",
        ylab="Terajoules")

matplot(seq(year.min, year.max), t(consump.gas.countries), type="l", lty=1,
        main="Consumption of Natural Gas EU 1990 - 2019", xlab="Year",
        ylab="Terajoules")

matplot(seq(year.min, year.max), t(consump.renew.countries), type="l", lty=1,
        main="Consumption of RE EU 1990 - 2019", xlab="Year",
        ylab="Terajoules")

boxplot(energy.balances.countries, main="Boxplot Energy Balances", xlab="Countries", 
        ylab="Terajoules")

boxplot(consump.fossils.countries, main="Boxplot of Consumption of Solid Fossil Fuels", xlab="Countries", 
        ylab="Terajoule")

boxplot(consump.oil.petr.countries, main="Boxplot of Consumption of Oil and Petroleum", xlab="Countries", 
        ylab="Terajoule")

boxplot(consump.gas.countries, main="Boxplot of Consumption of Natural Gas", xlab="Countries", 
        ylab="Terajoule")

boxplot(consump.renew.countries, main="Consumption of Renewables", xlab="Countries", 
        ylab="Terajoule")

#######################

# NP tests to compare year by year renewable vs non  renewable energy usage 

# First, lets check for Normality 
# Shapiro Wilk test on Consumption ----------------------------------------

index.italy <- which(rownames(consump.renew.countries) == c("Italy"))
hist(t(consump.fossils.countries)[,index.italy])
hist(t(consump.oil.petr.countries)[,index.italy])
hist(t(consump.gas.countries)[,index.italy])
hist(t(consump.renew.countries)[,index.italy])

p.values.ff <- numeric(length(countries.list)) # Fossil fuels
p.values.op <- numeric(length(countries.list)) # Oil&Petr.
p.values.ng <- numeric(length(countries.list)) # Natural gas 
p.values.re <- numeric(length(countries.list)) # RE
for(c in 1:length(countries.list)){
    p.values.ff[c] <- shapiro.test(t(consump.fossils.countries)[,c])$p.value
    p.values.op[c] <- shapiro.test(t(consump.oil.petr.countries)[,c])$p.value
    p.values.ng[c] <- shapiro.test(t(consump.gas.countries)[,c])$p.value # Ignore errors
    p.values.re[c] <- shapiro.test(t(consump.renew.countries)[,c])$p.value


    
}
hist(p.values.ff) # Shapiro-Wilk test might fail with small n values
hist(p.values.op) # Shapiro-Wilk test might fail with small n values
hist(p.values.ng) # Shapiro-Wilk test might fail with small n values
hist(p.values.re) # Shapiro-Wilk test might fail with small n values

##################################

# Permutational Two population MV test - Comparing Consumption distributions 
# We want to compare the distributions of NR vs R fuels for each country 
consump.fossils.mean <- colMeans(consump.fossils.countries, na.rm=T)
consump.oil.petr.mean <- colMeans(consump.oil.petr.countries, na.rm=T)
consump.gas.mean <- colMeans(consump.gas.countries, na.rm=T)
consump.renew.mean <- colMeans(consump.renew.countries, na.rm=T)

matplot(seq(year.min, year.max), t(rbind(consump.fossils.mean,
                                       consump.oil.petr.mean,
                                       consump.gas.mean,
                                       consump.renew.mean)),
        type="l",main="Consumption means for each type of fuel",xlab="Years",ylab="Terajoules",
        ylim=c(0,range(consump.fossils.mean)[2]+300000))

legend("topright", legend =c("Fossil Fuels","Oil and Petroleum","Natural Gas","Renewable Energies"), col=1:4, pch=2)


# Renewable consumption and Fossil Fuels are very different from oil & petr. and natural gas consumption
# But do these last two follow the same distribution?

# H0: Distributions are equal
# H1: Distributions are not equal


# NG vs OP ----------------------------------------------------------------

n1 <- dim(as.matrix(consump.gas.mean))[1]
n2 <- dim(as.matrix(consump.oil.petr.mean))[1]
n  <- n1 + n2

# Test statistic
T10 <- as.numeric((consump.gas.mean-consump.oil.petr.mean) %*% (consump.gas.mean-consump.oil.petr.mean))
T10

# Permutational distribution
T1 <- numeric(B)
set.seed(seed)
pb <- progress_bar$new(
    format = "  processing [:bar] :percent eta: :eta",
    total = B, clear = FALSE)

for(perm in 1:B){
    t_pooled = rbind(consump.gas.countries, consump.oil.petr.countries)
    permutation = sample(n)
    t_perm = t_pooled[permutation,]
    t1_perm = t_perm[1:n1,]
    t2_perm = t_perm[(n1+1):n,]
    
    # Evaluation of the test statistic on permuted data
    t1.mean_perm = colMeans(t1_perm, na.rm = T)
    t2.mean_perm = colMeans(t2_perm, na.rm = T)
    T1[perm]  = (t1.mean_perm-t2.mean_perm) %*% (t1.mean_perm-t2.mean_perm) 
    
    pb$tick()
    
}

# Graphics for the permutational distribution
hist(T1,xlim=range(c(T1,T10)))
abline(v=T10,col=3,lwd=4)

plot(ecdf(T1))
abline(v=T10,col=3,lwd=4)

# Calculate p-value
p_val <- sum(T1>=T10)/B
p_val

#############################


# FF vs OP ----------------------------------------------------------------

n1 <- dim(as.matrix(consump.fossils.mean))[1]
n2 <- dim(as.matrix(consump.oil.petr.mean))[1]
n  <- n1 + n2

# Test statistic
T20 <- as.numeric((consump.fossils.mean-consump.oil.petr.mean) %*% (consump.fossils.mean-consump.oil.petr.mean))
T20

# Permutational distribution
T2 <- numeric(B)
set.seed(seed)
pb <- progress_bar$new(
    format = "  processing [:bar] :percent eta: :eta",
    total = B, clear = FALSE)

for(perm in 1:B){
    t_pooled = rbind(consump.fossils.countries, consump.oil.petr.countries)
    permutation = sample(n)
    t_perm = t_pooled[permutation,]
    t1_perm = t_perm[1:n1,]
    t2_perm = t_perm[(n1+1):n,]
    
    # Evaluation of the test statistic on permuted data
    t1.mean_perm = colMeans(t1_perm, na.rm = T)
    t2.mean_perm = colMeans(t2_perm, na.rm = T)
    T2[perm]  = (t1.mean_perm-t2.mean_perm) %*% (t1.mean_perm-t2.mean_perm) 
    
    pb$tick()
    
}

# Graphics for the permutational distribution
hist(T2,xlim=range(c(T2,T20)))
abline(v=T20,col=3,lwd=4)

plot(ecdf(T2))
abline(v=T20,col=3,lwd=4)

# Calculate p-value
p_val <- sum(T2>=T20)/B
p_val

#############################


# RE vs OP ----------------------------------------------------------------
# Use year by year change of each energy (RE vs OP (or NG or FF))

# Does the renew. consumption follow the same distribution as fossils or oil & petr. consumption?

mean.consump.nonrenew = ( consump.fossils.countries[,]+consump.gas.countries[,]+consump.oil.petr.countries[,] )/3
mean.consump.nonrenew.mean=colMeans(mean.consump.nonrenew)

matplot(seq(year.min, year.max), t(rbind(mean.consump.nonrenew.mean,
                                         consump.renew.mean)),
        type="l",main="Consumption means renewables vs non-renewables",xlab="Years",ylab="Terajoules",
        ylim=c(0,range(mean.consump.nonrenew.mean)[2]+300000),col=c(1,3),lty=c(1,1))
legend("topright",legend=c("Non-renewable sources","Renewable sources"),col=c(1,3),pch=2)


mean.consump.nonrenew.diff <- mean.consump.nonrenew[,-length(mean.consump.nonrenew)] - mean.consump.nonrenew[,-1] #"inverse" deltas to obtain growing curve differences
consump.renew.diff <- consump.renew.countries[,-1] - consump.renew.countries[,-length(consump.renew.countries)]

mean.consump.nonrenew.diff.mean <- colMeans(mean.consump.nonrenew.diff, na.rm=T)
consump.renew.diff.mean <- colMeans(consump.renew.diff, na.rm=T)

matplot(seq(year.min+1,year.max), consump.renew.diff.mean,type="l",ylab="Terajoules",xlab="Years",
        main="Renewable consumptions year to year average deltas",col="green")
matlines(seq(year.min+1,year.max), mean.consump.nonrenew.diff.mean,type="l",ylab="Terajoules",xlab="Years",
        main="Mean non-renewable consumptions year to year inverse average deltas")


n1 <- dim(as.matrix(consump.renew.diff))[1]
n2 <- dim(as.matrix(mean.consump.nonrenew.diff))[1]
n  <- n1 + n2

# Test statistic
T30 <- as.numeric((consump.renew.diff.mean-mean.consump.nonrenew.diff.mean) %*% (consump.renew.diff.mean-mean.consump.nonrenew.diff.mean))
T30

# Permutational distribution
T3 <- numeric(B)
set.seed(seed)
pb <- progress_bar$new(
    format = "  processing [:bar] :percent eta: :eta",
    total = B, clear = FALSE)

for(perm in 1:B){
    t_pooled <- cbind(consump.renew.diff,mean.consump.nonrenew.diff)
    permutation <- sample(n)
    t_perm <- t_pooled[permutation,]
    t1_perm <- t_perm[1:n1,]
    t2_perm <- t_perm[(n1+1):n,]
    
    # Evaluation of the test statistic on permuted data
    t1.mean_perm <- colMeans(t1_perm, na.rm = T)
    t2.mean_perm <- colMeans(t2_perm, na.rm = T)
    T3[perm]  <- (t1.mean_perm-t2.mean_perm) %*% (t1.mean_perm-t2.mean_perm) 
    
    pb$tick()
    
}

# Graphics for the permutational distribution
hist(T3,xlim=range(c(T3,T30)),main="Squared L2 norm of means difference - permutational distribution")
abline(v=T30,col=3,lwd=4)

plot(ecdf(T3),main="Squared L2 norm of means difference - permutational ecdf")
abline(v=T30,col=3,lwd=4)

# Calculate p-value
p_val <- sum(T3>=T30)/B
p_val

#################################


# Two way permutational ANOVA - Oil & Petr. consumption + Renewables to represent Energy Balances 

# For each country we obtain a different ANOVA.
aov_Belgium <- aov(energy.balances.countries$Belgium ~ consump.oil.petr.countries$Belgium +       #are these factors? Maybe a linear model should make more sense
                                        consump.renew.countries$Belgium +
                                        consump.renew.countries$Belgium:consump.oil.petr.countries$Belgium)

summary(aov_Belgium)

# We can calculate for the total of the 27 members of the EU:
aov_EU <- aov(energy.balances.p$`European Union - 27 countries (from 2020)` ~
                  consump.oil.petr.p$`European Union - 27 countries (from 2020)`[-1] + 
                  consump.renew.p$`European Union - 27 countries (from 2020)`[-1] +
                  consump.renew.p$`European Union - 27 countries (from 2020)`[-1]:
                  consump.oil.petr.p$`European Union - 27 countries (from 2020)`[-1])
summary(aov_EU)

# Test if relation bw renew and oil+fuels can be ignored in a LM model
# H0: gamma = 0
# H1: gamma != 0
T0_oil_renew <- summary.aov(aov_EU)[[1]][3,4] # F-value
T0_oil_renew

# Compute the permutational distribution to calculate p-value
aov_EU.H0 <- aov(energy.balances.p$`European Union - 27 countries (from 2020)` ~
                  consump.oil.petr.p$`European Union - 27 countries (from 2020)`[-1] + 
                  consump.renew.p$`European Union - 27 countries (from 2020)`[-1])
aov_EU.H0

residuals_EU.H0 <- aov_EU.H0$residuals
n <- length(energy.balances.p$`European Union - 27 countries (from 2020)`) 


T_oil_renew <- numeric(B)
pb <- progress_bar$new(
    format = "  processing [:bar] :percent eta: :eta",
    total = B, clear = FALSE)

for(perm in 1:B){
    permutation <- sample(n)
    residuals_EU.H0 <- residuals_EU.H0[permutation]
    energy.balances.perm.H0 <- aov_EU.H0$fitted + residuals_EU.H0
    T_oil_renew[perm] <- summary.aov(aov(energy.balances.perm.H0 ~ 
                                     consump.oil.petr.p$`European Union - 27 countries (from 2020)`[-1] + 
                                     consump.renew.p$`European Union - 27 countries (from 2020)`[-1] +
                                     consump.renew.p$`European Union - 27 countries (from 2020)`[-1]:
                                     consump.oil.petr.p$`European Union - 27 countries (from 2020)`[-1]))[[1]][3,4]
    pb$tick()
}

p_val <- sum(T_oil_renew >= T0_oil_renew)/B
p_val # Significant p-value


# NP tests to analyze Geographical differences from prev data via
# Two sample paired MV test - Comparing Countries for diff. consumptions

# We can compare Italy's consumption of fossil fuels + renewables + petroleum
# vs Spain's
country1 <- "Spain"
country2 <- "Italy"
c12 <- c(country1, country2)
# Plot their respective boxplot's
boxplot(energy.balances.countries[c12],
        main="Boxplot Energy Balances", xlab="Countries", 
        ylab="Thousand tonnes of oil equivalent")

boxplot(consump.fossils.countries[c12],
        main="Boxplot of Consumption of Solid Fossil Fuels", xlab="Countries", 
        ylab="Thousand tonnes (solid fossil fuels)")

boxplot(consump.oil.petr.countries[c12],
        main="Boxplot of Consumption of Oil and Petroleum", xlab="Countries", 
        ylab="Thousand tonnes (oil and petroleum products)")

boxplot(consump.renew.countries[c12], 
        main="Consumption of Renewables", xlab="Countries", 
        ylab="Terajoule")

# Create matrices for each country
t1 <- cbind(energy.balances.countries[,country1], 
            consump.fossils.countries[,country1],  #quit -1
            consump.oil.petr.countries[,country1], #quit -1
            consump.renew.countries[,country1])    #quit -1

t2 <- cbind(energy.balances.countries[country2], 
            consump.fossils.countries[,country2],
            consump.oil.petr.countries[,country2],
            consump.renew.countries[,country2])
# Calculate means per consumption
t1.mean <- colMeans(t1)
t2.mean <- colMeans(t2)

p  <- dim(t1)[2] # Number of cols
n1 <- dim(t1)[1] # Number of rows
n2 <- dim(t2)[1] # Number of rows
n <- n1+n2

t1.cov  <-  cov(t1)
t2.cov  <-  cov(t2)
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)
Spinv   <- solve(Sp)

delta.0 <- rep(0, p) # Assumed value for means 

diff <- t1-t2
diff.mean <- colMeans(diff)
diff.cov <- cov(diff)
diff.invcov <- solve(diff.cov)

# Computing the statistic
T30 <- as.numeric((diff.mean-delta.0)  %*% (diff.mean-delta.0))

# Permutational test
T3 <- numeric(B)
set.seed(seed)
pb <- progress_bar$new(
    format = "  processing [:bar] :percent eta: :eta",
    total = B, clear = FALSE)

for(perm in 1:B){
    # Random permutation
    signs.perm <- rbinom(n1, 1, 0.5)*2 - 1
    
    diff_perm <- diff * matrix(signs.perm,nrow=n1,ncol=p,byrow=FALSE)
    diff.mean_perm <- colMeans(diff_perm)
    diff.cov_perm <- cov(diff_perm)
    diff.invcov_perm <- solve(diff.cov_perm)
    
    T3[perm] <- as.numeric((diff.mean_perm-delta.0) %*% (diff.mean_perm-delta.0))
    
    pb$tick()
}

# plotting the permutational distribution under H0
hist(T3,xlim=range(c(T2,T20)),breaks=100)
abline(v=T30,col=3,lwd=4)

plot(ecdf(T3))
abline(v=T30,col=3,lwd=4)

# p-value
p_val <- sum(T3>=T30)/B
p_val


# Regression to view the trend of energy consumption 

# Lets first look at the trends of the three types of energies 

# Means of the three types of consumptions
matplot(seq(year.min, year.max.2), consump.fossils.mean, type="l")
matplot(seq(year.min, year.max.2), consump.oil.petr.mean, type="l")
matplot(seq(year.min, year.max.2), consump.renew.mean, type="l")

# Or similarly the total for all EU countries
matplot(seq(year.min, year.max.2), consump.fossils.p$`European Union - 27 countries (from 2020)`, type="l")
matplot(seq(year.min, year.max.2), consump.oil.petr.p$`European Union - 27 countries (from 2020)`, type="l")
matplot(seq(year.min, year.max.2), consump.renew.p$`European Union - 27 countries (from 2020)`, type="l")

# Lets try to fit a linear model for the Fossil energy consumption and the Renew. Energiy consumption

year.grid <- seq(year.min, year.max)

# First with fossil fuels
consump.fossils.lm=lm(consump.fossils.p$`European Union - 27 countries (from 2020)` ~ consump.fossils.p$year)
summary(consump.fossils.lm)

preds.lm.fossils <- predict(consump.fossils.lm, list(year=year.grid),se=T)
se.bands <- cbind(preds.lm.fossils$fit + 2 * preds.lm.fossils$se.fit,
                  preds.lm.fossils$fit - 2 * preds.lm.fossils$se.fit)

plot(consump.fossils.p$year, consump.fossils.p$`European Union - 27 countries (from 2020)`,
     xlim=range(year.grid) ,cex =.5, col =" darkgrey ",main='Linear Fit')
lines(year.grid, preds.lm.fossils$fit, lwd =2, col =" blue")
matlines(year.grid, se.bands, lwd =1, col =" blue", lty =3)
# Does not get very good results


# This time for RE consumption levels
consump.renew.lm <- lm(consump.renew.p$`European Union - 27 countries (from 2020)` ~ consump.renew.p$year)
summary(consump.renew.lm)

preds.lm.renew <- predict(consump.renew.lm, list(year=year.grid),se=T)
se.bands <- cbind(preds.lm.renew$fit + 2 * preds.lm.renew$se.fit,
                  preds.lm.renew$fit - 2 * preds.lm.renew$se.fit)

plot(consump.renew.p$year, consump.renew.p$`European Union - 27 countries (from 2020)`,
     xlim=range(year.grid) ,cex =.5, col =" darkgrey ",main='Linear Fit')
lines(year.grid, preds.lm.renew$fit, lwd =2, col =" blue")
matlines(year.grid, se.bands, lwd =1, col =" blue", lty =3)
# Seems to get much better results than for fossil fuels



# REVIEW: Try to make a conformal prediction with renewables consumption as response variable and oil, 
# fossil fuels, gas and years as covariates.


vec_oil <- cbind(consump.oil.petr.countries[,1],rep(1990,35))
vec_ff <- cbind(consump.fossils.countries[,1],rep(1990,35))
vec_ng <- cbind(consump.gas.countries[,1],rep(1990,35))
for (j in 2:30){
    v <- cbind(consump.oil.petr.countries[,j], rep(1990+j-1, 35))
    vec_oil <- rbind(vec_oil,v)
    v <- cbind(consump.fossils.countries[,j], rep(1990+j-1, 35))
    vec_ff <- rbind(vec_ff,v)
    v <- cbind(consump.gas.countries[,j], rep(1990+j-1, 35))
    vec_ng <- rbind(vec_ng,v)
}
vec_oil <- data.frame(vec_oil)
colnames(vec_oil) <- c("value_oil","year")
vec_ff <- data.frame(vec_ff)
colnames(vec_ff) <- c("value_ff","year")
vec_ng <- data.frame(vec_ng)
colnames(vec_ng) <- c("value_ng","year")

# Predict levels of oil+petr, ff, gas 2020-2024 with ind. variable year

# With GAM:
# a. Oil+Petr
consump.oil.petr.gam <- gam(value_oil ~ s(year, bs="tp"),
                           data = vec_oil) 
summary(consump.oil.petr.gam)

hist(consump.oil.petr.gam$residuals)
qqnorm(consump.oil.petr.gam$residuals)
shapiro.test(consump.oil.petr.gam$residuals)

# b. Fossil fuels
consump.fossils.gam <- gam(value_ff ~ s(year, bs="tp"),
                           data = vec_ff) 
summary(consump.fossils.gam)

hist(consump.fossils.gam$residuals)
qqnorm(consump.fossils.gam$residuals)
shapiro.test(consump.fossils.gam$residuals)

# c. Natural Gas
consump.gas.gam <- gam(value_ng ~ s(year, bs="tp"),
                           data = vec_ng) 
summary(consump.gas.gam)

hist(consump.gas.gam$residuals)
qqnorm(consump.gas.gam$residuals)
shapiro.test(consump.gas.gam$residuals)

# Prediction with GAM model
year.grid <- seq(range(vec_oil$year)[1], 2024, by=0.5)

consump.oil.petr.gam.preds <- predict(consump.oil.petr.gam, newdata=list(year=year.grid), se=T)

consump.fossils.gam.preds <- predict(consump.fossils.gam, newdata=list(year=year.grid), se=T)

consump.gas.gam.preds <- predict(consump.gas.gam, newdata=list(year=year.grid), se=T)

# Combine predictions into a single DF

consump.nonr.preds <- cbind(year.grid, consump.oil.petr.gam.preds$fit,
                            consump.fossils.gam.preds$fit, consump.gas.gam.preds$fit)
consump.nonr.preds <- data.frame(consump.nonr.preds)
colnames(consump.nonr.preds) <- c("year", "value_oil", "value_ff", "value_ng")

# See graphs
with(vec_oil, plot(year ,value_oil ,xlim=range(year.grid),cex =.5,xlab="Year",ylab="Terajoule" ,
                   col =" darkgrey ",main='GAM Prediction - Oil & Petr.'))
lines(year.grid, consump.oil.petr.gam.preds$fit ,lwd =2, col =" red")

with(vec_ff, plot(year ,value_ff ,xlim=range(year.grid),cex =.5,xlab="Year",ylab="Terajoule" ,
                   col =" darkgrey ",main='GAM Prediction - Fossil Fuels'))
lines(year.grid, consump.fossils.gam.preds$fit ,lwd =2, col =" red")

with(vec_ng, plot(year ,value_ng ,xlim=range(year.grid),cex =.5,xlab="Year",ylab="Terajoule" ,
                   col =" darkgrey ",main='GAM Prediction - Natural Gas'))
lines(year.grid, consump.gas.gam.preds$fit ,lwd =2, col =" red")


# Conformal prediction (with GAM)

# Join all energies for common DF
vec_re <- cbind(consump.renew.countries[,1],rep(1990,35))
for (j in 2:30){
    v <- cbind(consump.renew.countries[,j], rep(1990+j-1, 35))
    vec_re <- rbind(vec_re,v)
}
vec_ <- cbind(vec_re, vec_oil[,1], vec_ff[,1], vec_ng[,1])
vec_ <- data.frame(vec_)
colnames(vec_) <- c("value_re","year", "value_oil", "value_ff", "value_ng")
# Also create DF for Renewable energies
vec_re=data.frame(vec_re)
colnames(vec_re)=c("value_re","year")


model_gam <- gam(value_re ~ s(year,bs='cr') + s(value_oil,bs='cr') + 
                  s(value_ff,bs='cr') + s(value_ng,bs='cr'), data=vec_)

year.grid <- consump.nonr.preds$year
oil.grid  <- consump.nonr.preds$value_oil
ff.grid   <- consump.nonr.preds$value_ff
ng.grid   <- consump.nonr.preds$value_ng

grid <- expand.grid(year.grid,oil.grid,ff.grid, ng.grid)
names(grid) <- c("year","value_oil","value_ff", "value_ng")
# pred <- predict(model_gam, newdata=grid)


train_gam=function(x,y,out=NULL){
    colnames(x)=c("var1", "var2", "var3", "var4")
    train_data=data.frame(y,x)
    model_gam=gam(y ~ s(var1,bs='cr') + s(var2,bs='cr') + s(var3,bs='cr') + s(var4,bs='cr'),
                  data=train_data)
}


predict_gam=function(obj, new_x){
    new_x=data.frame(new_x)
    colnames(new_x)=c("var1", "var2", "var3", "var4")
    predict.gam(obj,new_x)
}


c_preds <- conformal.pred.split(cbind(vec_$year, vec_$value_oil, vec_$value_ff, vec_$value_ng), 
                                vec_$value_re,
                                cbind(consump.nonr.preds$year, consump.nonr.preds$value_oil, consump.nonr.preds$value_ff, consump.nonr.preds$value_ng),
                                alpha=0.05, verbose=T, train.fun = train_gam,
                                predict.fun = predict_gam)

with(vec_re, plot(year, value_re,xlim=range(year.grid),cex =.5,xlab="Year",ylab="Terajoule",
                  col =" darkgrey ",main='GAM conformal prediction - Renewable Energies'))

lines(consump.nonr.preds$year,c_preds$pred ,lwd =2, col ="green",lty=3)
lines(year.grid, numeric(length(year.grid)),lwd=1,col="blue",lty=3)
matlines(consump.nonr.preds$year ,c_preds$up ,lwd =1, col =" blue",lty =3)



#######################
# 2. 
#######################
# Which countries are more projected to switch to renewable energy?

north_europe=c("Denmark","Estonia","Latvia","Lithuania","Finland","Sweden","Norway","Iceland","United Kingdom","Ireland")

east_europe=c("Bulgaria","Czechia","Hungary","Poland","Romania","Slovakia","Turkey","Ukraine")

south_europe=c("Greece","Spain","Italy","Cyprus","Malta","Portugal","Croatia","Slovenia","North Macedonia","Albania","Serbia")

central_europe=c("France","Belgium","Germany (until 1990 former territory of the FRG)","Luxembourg","Netherlands","Austria")


# TODO: Check independence between countries or bw energy sources

library(roahd)

year.grid <- seq(year.min, year.max)

# Bw countries
countries.mfdata <- vector("list", length(countries.list)) 
for(i in 1:length(countries.mfdata)){
    c.fdata <- rbind(consump.renew.countries[i,],
                           consump.fossils.countries[i,],
                           consump.oil.petr.countries[i,],
                           consump.gas.countries[i,])
    
    c.fdata <- fData(year.grid, c.fdata)
    countries.mfdata[[i]] <- c.fdata
}

countries.mfdata <- as.mfData(countries.mfdata)

# Calculate Spearman Matrix and Plot
countries.SM <- cor_spearman(countries.mfdata, ordering='MHI')

# Plot
library(corrplot)
corrplot(countries.SM,
         title = "Spearman Matrix")


# Bw energies 
energies.mfdata <- as.mfData(list(fData(year.grid,consump.renew.countries),
                                  fData(year.grid,consump.oil.petr.countries),
                                  fData(year.grid,consump.gas.countries),
                                  fData(year.grid,consump.fossils.countries)))

# Calculate Spearman Matrix and Plot
energies.SM <- cor_spearman(energies.mfdata, ordering='MHI')

# Plot
corrplot(energies.SM,
         title = "Spearman Matrix")


# Differences bw Countries

year.grid <- seq(year.min, year.max)

# Bw countries
countries.diff.mfdata <- vector("list", length(countries.list)) 
for(i in 1:length(countries.diff.mfdata)){
    c.fdata <- rbind(consump.renew.countries[i,-1] - consump.renew.countries[i,-length(year.grid)],
                     consump.fossils.countries[i,-1] - consump.fossils.countries[i,-length(year.grid)],
                     consump.oil.petr.countries[i,-1] - consump.oil.petr.countries[i,-length(year.grid)],
                     consump.gas.countries[i,-1] - consump.gas.countries[i,-length(year.grid)])
    
    c.fdata <- fData(year.grid[-1], c.fdata)
    countries.diff.mfdata[[i]] <- c.fdata
}

countries.diff.mfdata <- as.mfData(countries.diff.mfdata)

# Calculate Spearman Matrix and Plot
countries.diff.SM <- cor_spearman(countries.diff.mfdata, ordering='MHI')

# Plot
corrplot(countries.diff.SM,
         title = "Spearman Matrix")



#######################
# SSP year to year predictions (from intervals of 5 years) 
#######################

# SSP data cleaning
library(openxlsx)

ssp  <- read.xlsx("data/ssp_iiasa.xlsx",sheet=1) # Gross available energy, first sheet should be the best, even if in the presentation we chose the fourth

ssp_clean_cols <- ssp[,c("SCENARIO","REGION","VARIABLE","UNIT","2010","2015",
                         "2020","2025","2030","2035","2040","2045","2050","2055",
                         "2060","2065","2070","2075","2080","2085","2090","2095",
                         "2100" )]   

country_codes <- data.frame(c(north_europe,east_europe,south_europe,central_europe),
                            c("DNK","EST","LVA","LTU","FIN","SWE","NOR","ISL","GBR","IRL", #North
                              "BGR","CZE","HUN","POL","ROU","SVK","TUR","UKR",
                              "GRC","ESP","ITA","CYP","MLT","PRT","HRV","SVN","MKD","ALB","SRB",
                              "FRA","BEL","DEU","LUX","NLD","AUT"))
colnames(country_codes) <- c("Name","Code")

country_name2code <- function(code="",name=""){
    # Returns name and code for a given name or code
    if(nchar(name)>1)
        return(country_codes[country_codes["Name"] == name][2]) #return code
    else if(nchar(code)>1)
        return(country_codes[country_codes["Code"] == code][1]) #return name
} 

ssp_clean_cols[,-1:-4] <- lapply(ssp_clean_cols[,-1:-4], as.numeric) # transform data into numeric
ssp_clean_cols[,"REGION"] <- sapply(ssp_clean_cols[,"REGION"], country_name2code) # Use country names

ssp_eu <- ssp_clean_cols[!is.na(ssp_clean_cols$REGION),]

# ssp_eu <- rbind(colnames(ssp_eu), ssp_eu) # if wanting to remove colnames and use as rows
# colnames(ssp_eu) <- NULL 

length(unique(ssp_eu$REGION)) == length(countries.list) # check if data for all countries is present


# Divide GDP and Population datasets
ssp_gdp <- ssp_eu[ssp_eu$VARIABLE=="GDP|PPP",] 
ssp_pop <- ssp_eu[ssp_eu$VARIABLE=="Population",]



# Obtain Year to year values from functional data
library(fda)

# countries.mfdata <- vector("list", length(ssp_gdp_ssp1$REGION)) 
ssp.year.grid <- seq(2010,2100,5)

ssp.year.grid.str <-c("2010","2015","2020","2025","2030","2035","2040","2045","2050","2055",
                      "2060","2065","2070","2075","2080","2085","2090","2095","2100")

# Create empty df to populate
new.ssp_gdp <- data.frame(matrix(ncol = 4+length(2010:2100), nrow = 0))
new.ssp_pop <- data.frame(matrix(ncol = 4+length(2010:2100), nrow = 0))

for(ssp.var.idx in 1:2){
    if(ssp.var.idx == 1)
        ssp_ <- ssp_gdp
    else
        ssp_ <- ssp_pop
    
    for(sspX in unique(ssp_$SCENARIO)){  # For each scenario
        ssp_sspX <- ssp_[ssp_$SCENARIO==sspX,]
        for(c in 1:length(ssp_sspX$REGION)){ # For each country
            
            ssp_sspX.data <- as.numeric(ssp_sspX[c,ssp.year.grid.str]) # For first country + ssp1
            
            # With roahd
            # f_data <- fData(ssp.year.grid, ssp_sspX[,ssp.year.grid.str])
            # plot(f_data)
            
            times_basis <-ssp.year.grid 
            knots    <- c(ssp.year.grid) #Location of knots
            n_knots   <- length(knots) #Number of knots
            n_order   <- 2 # order of basis functions: cubic bspline: order = 3 + 1
            n_basis   <- length(knots) + n_order - 2;
            
            basis <- create.bspline.basis(c(min(times_basis),max(times_basis)),n_basis,n_order,knots)
            # plot(basis)
            
            ys <- smooth.basis(argvals=times_basis, y=ssp_sspX.data, fdParobj = basis)
            
            xfd <- ys$fd
            # plotfit.fd(ssp_sspX.data, times_basis, xfd) #  Plot the curve along with the data
            
            ssp_sspX.yby <- eval.fd(seq(2010,2100,1), xfd) # Obtain data year by year
            
            # plot(seq(2010,2100,1), ssp_ssp1.yby)
            if(ssp.var.idx == 1)
                new.ssp_gdp <- rbind(new.ssp_gdp, c(sspX, ssp_sspX$REGION[c],
                                                 unique(ssp_$VARIABLE)[1],
                                                 unique(ssp_$UNIT)[1],
                                                 ssp_sspX.yby))
            else
                new.ssp_pop <- rbind(new.ssp_pop, c(sspX, ssp_sspX$REGION[c],
                                                 unique(ssp_$VARIABLE)[1],
                                                 unique(ssp_$UNIT)[1],
                                                 ssp_sspX.yby))
        }
    }
}
# rename columns
colnames(new.ssp_gdp) <- c(colnames(ssp_gdp[,1:4]), 2010:2100)    
colnames(new.ssp_pop) <- c(colnames(ssp_pop[,1:4]), 2010:2100) 

# Write to xlsx
write.table(new.ssp_gdp, "data/ssp_iiasa_YtY_gdp.xlsx")
write.table(new.ssp_pop, "data/ssp_iiasa_YtY_pop.xlsx")


