# @ebjaime - NPS - 2021

library(openxlsx)
library(MASS)
library(progress)

# Graphics packages
library(rgl)
library(hexbin)
library(ggplot2)


# Depth measures packages
#library(DepthProc)
#library(robustbase)

B <- 100000
seed <- 42


# Load datasets
# REVIEW: Qué hoja de cada xlsx??
energy.balances <- read.xlsx("data/complete_energy_balances.xlsx") #first sheet should be the best,
                                                            # even if in the presentation we chose the fourth
consump.fossils <- read.xlsx("data/consumption_solid_fossil_fuels.xlsx") #first sheet
consump.oil.petr <- read.xlsx("data/consumption_oil_petroleum.xlsx") #first sheet
consump.renew <- read.xlsx("data/consumption_renewables.xlsx") #sum all sheets
percent.renew <- read.xlsx("data/percentage_renewables.xlsx")
prod.by.fuel <- read.xlsx("data/production_capacities_by_fuel.xlsx")


# Function cleans df removing NAN rows, setting values as numeric...
preprocess <- function(df, rm_nrows=6) {
    
    # Remove first rows not containing significant data
    df <- df[-c(1:rm_nrows),]
    
    # colnames(energy.balances) <- energy.balances[1,]
    # energy.balances <- energy.balances[-1,]
    rownames(df) <- df[,1]
    df <- df[,-1]
    
    rownames(df)[1] <- "year"
    
    # Remove rows with NA values 
    df <- na.omit(df)
    
    # REVIEW: Change cells to numeric. How many decimals??
    df[] <- lapply(df, as.numeric)
    
    # Transpose to get years as columns
    df.t <- as.data.frame(t(df))       #why transpose?
    return(df.t)
}



energy.balances.p <- preprocess(energy.balances)
consump.fossils.p <- preprocess(consump.fossils)
consump.oil.petr.p <- preprocess(consump.oil.petr)
consump.renew.p   <- preprocess(consump.renew)

# Visualize data examples
# Example of energy balances
p_italy <- ggplot(data=energy.balances.p, aes(x=year, y=Italy)) +
            labs(title="Energy Balances Progression - Italy", 
                 subtitle="Gross Available Energy") +
            xlab("Year") + ylab("Thousand tonnes of oil equivalent") +
            geom_point() +
            geom_smooth(method="loess", formula=y~x, fill="blue", colour="darkblue", size=1)
p_italy

# Example of consumption of solid fossil fuels
p_sweden <- ggplot(data=energy.balances.p, aes(x=year, y=Sweden)) + #plot of energy balance or consumption of solid fossil fuels?
            labs(title="Consumption Solid Fossil Fuels - Sweden", 
                 subtitle="Fuel consumption - energy use") +
            xlab("Year") + ylab("Thousand tonnes of oil equivalent") +
            geom_point() +
            geom_smooth(method="loess", formula=y~x, fill="red", colour="darkred", size=1)
p_sweden


#######################
# 1.
#######################
# How are the renewable energies levels of consumptions growing? 
# And the non renewables ones?


year.min <- min(energy.balances.p$year)
year.max.1 <- max(energy.balances.p$year)
year.max.2 <- max(consump.fossils.p$year)
countries.list <- colnames(consump.fossils.p[,-c(1,2,3,4)]) 

energy.balances.countries <- energy.balances.p[,-c(1,2,3,4)]
consump.fossils.countries <-consump.fossils.p[,-c(1,2,3,4)]
consump.oil.petr.countries <- consump.oil.petr.p[,-c(1,2,3,4)]
consump.renew.countries <- consump.renew.p[,-c(1,2,3,4)]

# Graphics

matplot(seq(year.min, year.max.1), energy.balances.countries, type="l", lty=1,
        main="Energy Balances EU 1990 - 2020", xlab="Year", ylab="Thousand tonnes of oil equivalent")

matplot(seq(year.min, year.max.2), consump.fossils.countries, type="l", lty=1,
        main="Consumption of solid fossil fuels EU 1990 - 2020", xlab="Year",
        ylab="Thousand tonnes (solid fossil fuels)")

matplot(seq(year.min, year.max.2), consump.oil.petr.countries, type="l", lty=1,
        main="Consumption of Oil and Petroleum EU 1990 - 2020", xlab="Year",
        ylab="Thousand tonnes (oil and petroleum products)")

matplot(seq(year.min, year.max.2), consump.oil.petr.countries, type="l", lty=1,
        main="Consumption of Oil and Petroleum EU 1990 - 2020", xlab="Year",
        ylab="Terajoules")

boxplot(energy.balances.countries, main="Boxplot Energy Balances", xlab="Countries", 
        ylab="Thousand tonnes of oil equivalent")

boxplot(consump.fossils.countries, main="Boxplot of Consumption of Solid Fossil Fuels", xlab="Countries", 
        ylab="Thousand tonnes (solid fossil fuels)")

boxplot(consump.oil.petr.countries, main="Boxplot of Consumption of Oil and Petroleum", xlab="Countries", 
        ylab="Thousand tonnes (oil and petroleum products)")

boxplot(consump.renew.countries, main="Consumption of Renewables", xlab="Countries", 
        ylab="Terajoule")

# NP tests to compare year by year renewable vs non  renewable energy usage 

# First, lets check for Normality 
hist(consump.fossils.countries$Italy)
hist(consump.oil.petr.countries$Italy)
hist(consump.renew.countries$Italy)

# REVIEW: Shapiro-Wilk test might fail to reject with small n
p.values <- numeric(length(countries.list))
for(c in 1:length(countries.list)){
    p.values[c] <- shapiro.test(na.omit(consump.fossils.countries[,c]))$p.value
}
hist(p.values)

# 1a. Permutational Two population MV test - Comparing Consumption distributions 
# We want to compare the distributions of NR vs R fuels for each country 
consump.fossils.mean <- rowMeans(consump.fossils.countries, na.rm=T)
consump.oil.petr.mean <- rowMeans(consump.oil.petr.countries, na.rm=T)
consump.renew.mean <- rowMeans(consump.renew.countries, na.rm=T)

matplot(seq(year.min, year.max.2), consump.fossils.mean, type="l")
matplot(seq(year.min, year.max.2), consump.oil.petr.mean, type="l")
matplot(seq(year.min, year.max.2), consump.renew.mean, type="l")

# Renewable consumption is very different from oil & petr. and fossil fuels consumption
# But do these last two follow the same distribution?

# H0: Distributions are equal
# H1: Distributions are not equal
n1 <- dim(as.matrix(consump.fossils.mean))[1]
n2 <- dim(as.matrix(consump.oil.petr.mean))[1]
n  <- n1 + n2

# Test statistic
T10 <- as.numeric((consump.fossils.mean-consump.oil.petr.mean) %*% (consump.fossils.mean-consump.oil.petr.mean))
T10

# Permutational distribution
T1 <- numeric(B)
set.seed(seed)
pb <- progress_bar$new(
    format = "  processing [:bar] :percent eta: :eta",
    total = B, clear = FALSE)

for(perm in 1:B){
    t_pooled = cbind(consump.fossils.countries,consump.oil.petr.countries)
    permutation = sample(n)
    t_perm = t_pooled[,permutation]
    t1_perm = t_perm[,1:n1]
    t2_perm = t_perm[,(n1+1):n]
    
    # Evaluation of the test statistic on permuted data
    t1.mean_perm = rowMeans(t1_perm, na.rm = T)
    t2.mean_perm = rowMeans(t2_perm, na.rm = T)
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

# Does the inverse renew. consumption follow the same distribution as fossils or oil & petr. consumption?
# (supposing distribution of consump. of oil & petr = distribution of consump. fossils)
matplot(seq(year.min, year.max.2), -consump.renew.mean)

consump.renew.countries.neg <- -consump.renew.countries
consump.renew.mean.neg <- -consump.renew.mean


# H0: Distributions are equal
# H1: Distributions are not equal
n1 <- dim(as.matrix(consump.renew.mean.neg))[1]
n2 <- dim(as.matrix(consump.oil.petr.mean))[1]
n  <- n1 + n2

# Test statistic
T20 <- as.numeric((consump.renew.mean.neg-consump.oil.petr.mean) %*% (consump.renew.mean.neg-consump.oil.petr.mean))
T20

# Permutational distribution
T2 <- numeric(B)
set.seed(seed)
pb <- progress_bar$new(
    format = "  processing [:bar] :percent eta: :eta",
    total = B, clear = FALSE)

for(perm in 1:B){
    t_pooled <- cbind(consump.renew.countries.neg,consump.oil.petr.countries)
    permutation <- sample(n)
    t_perm <- t_pooled[,permutation]
    t1_perm <- t_perm[,1:n1]
    t2_perm <- t_perm[,(n1+1):n]
    
    # Evaluation of the test statistic on permuted data
    t1.mean_perm <- rowMeans(t1_perm, na.rm = T)
    t2.mean_perm <- rowMeans(t2_perm, na.rm = T)
    T2[perm]  <- (t1.mean_perm-t2.mean_perm) %*% (t1.mean_perm-t2.mean_perm) 
    
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


# 1b. Two way permutational ANOVA - Oil & Petr. consumption + Renewables to represent Energy Balances 

# For each country we obtain a different ANOVA. 
aov_Belgium <- aov(energy.balances.countries$Belgium ~ consump.oil.petr.countries$Belgium[-1] + 
                                        consump.renew.countries$Belgium[-1] +
                                        consump.renew.countries$Belgium[-1]:consump.oil.petr.countries$Belgium[-1])
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


# 1c. NP tests to analyze Geographical differences from prev data via
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
t1 <- cbind(energy.balances.countries[country1], 
            consump.fossils.countries[-1,country1],
            consump.oil.petr.countries[-1,country1],
            consump.renew.countries[-1,country1])

t2 <- cbind(energy.balances.countries[country2], 
            consump.fossils.countries[-1,country2],
            consump.oil.petr.countries[-1,country2],
            consump.renew.countries[-1,country2])
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

# REVIEW: Could retry with Mahalannobis distance like lab 4


# 1d. Regression to view the trend of energy consumption 

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

year.grid <- seq(year.min, year.max.2)

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

# REVIEW: Try with step regression / local regression ?


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

# REVIEW: Bootstrap - make inference about parameters ?


# Nonparametric regression on fossils and oil+petroleum consumption

# REVIEW: Try Step regression / local regression before GAM? 

# REVIEW: Try to make a gam model with renewables consumption as response variable and oil, fossil fuels and years
# as covariates.

# GAM
consump.nonr <- cbind(consump.fossils.p$year,
                      consump.fossils.p$`European Union - 27 countries (from 2020)`,
                      consump.oil.petr.p$`European Union - 27 countries (from 2020)`)
consump.nonr <- as.data.frame(consump.nonr)
colnames(consump.nonr) <- c("year", "eu_fossils", "eu_oil_petr")


consump.nonr.gam <- gam(year ~ s(eu_fossils, bs="tp") + s(eu_oil_petr, bs="tp"),
                           data = consump.nonr) 
summary(consump.nonr.gam)

hist(consump.nonr.gam$residuals)
qqnorm(consump.nonr.gam$residuals)

shapiro.test(consump.nonr.gam$residuals)
# Residuals normally distributed

# With natural cubic splines smoothers
consump.nonr.gam.ns <- lm(year ~ ns(eu_fossils, df=3) + ns(eu_oil_petr, df=3),
                   data = consump.nonr)

plot(consump.nonr.gam.ns$residuals, consump.nonr.gam$residuals)
cor(consump.nonr.gam.ns$residuals, consump.nonr.gam$residuals) # Almost 1
# We obtain the same model (almost) using natural cubic splines smoothers

# Prediction with GAM model

consump.fossils.grid <- seq(range(consump.nonr$eu_fossils)[1],
                            range(consump.nonr$eu_fossils)[2] ,
                            length.out = 100)

consump.oil.petr.grid <- seq(range(consump.nonr$eu_oil_petr)[1],
                            range(consump.nonr$eu_oil_petr)[2] ,
                            length.out = 100)


consump.nonr.grid <- expand.grid(consump.fossils.grid,
                                 consump.oil.petr.grid)
names(consump.nonr.grid) <- c('eu_fossils','eu_oil_petr')

consump.nonr.gam.pred <- predict(consump.nonr.gam,
                                 newdata=consump.nonr.grid)

# 3D representation of predictions
persp3d(consump.fossils.grid, consump.oil.petr.grid, consump.nonr.gam.pred, col='grey30')
with(consump.nonr,points3d(eu_fossils, eu_oil_petr, year,col='black',size=5))



#######################
# 2.
#######################
# Which countries are more projected to switch to renewable energy?

# TODO: Nonparametric regression  on the solid fossil fuels and petroleum products 
# imports datasets to investigate the renewable transition  of each country.



# TODO: Analyze, for each country,  the percentage of energy produced
# by renewable sources over the total energy  produced

# dividir en regiones y luego tomar la region mejor y comparar los paises de esa region
north_europe=c("Denmark","Estonia","Latvia","Lithuania","Finland","Sweden","Norway","Iceland","United Kingdom","Ireland")

east_europe=c("Bulgaria","Czechia","Hungary","Poland","Romania","Slovakia","Turkey","Ukraine")

south_europe=c("Greece","Spain","Italy","Cyprus","Malta","Portugal","Croatia","Slovenia","North Macedonia","Albania","Serbia")

central_europe=c("France","Belgium","Germany (until 1990 former territory of the FRG)","Luxembourg","Netherlands","Austria")

