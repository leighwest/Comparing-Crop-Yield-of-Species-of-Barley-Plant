# Import libraries and source functions
library(ggplot2)
library(effects)
source("Rfunctions.r")

# Read data from data file (Barley.txt) and store in R in a data frame (barley.df)
barley.df <- read.table("Barley.txt", header=T)

# Declare factors
barley.df$BSPACE <- factor(barley.df$BSPACE)
barley.df$BVARIETY <- factor(barley.df$BVARIETY)
barley.df$BBLOCK <- factor(barley.df$BBLOCK)
attach(barley.df)

# Construct an interaction plot
interaction.plot(BSPACE, BVARIETY, BYIELD, fixed=TRUE, leg.bty = "o", lwd=2, main="Interaction Plot of Mean Yield of Barley Against Spacing by Variety")

# Construct an effects plot including the blocking effect and the interaction between variety and space
barley.eff <- allEffects(lm(BYIELD~BBLOCK+BVARIETY*BSPACE))
plot(barley.eff, col="black", which=1:2)  


# Stepwise regression

# Minimal model
formL <- formula(~1)
# Maximum model (includes blocking effect and interaction term)
formU <- formula(BYIELD ~ BBLOCK + BVARIETY*BSPACE, data=barley.df)
start.model <- lm(BYIELD ~ BBLOCK + BVARIETY*BSPACE, data=barley.df)
step.model <- step(start.model, direction = "backward", scope = list(lower = formL, upper = formU))

# Output anova of final model
anova(step.model)

# Output summary of final model
summary(step.model)

# Output table of means
print(barley.eff)

# Output 95% CIs for final model
betaCI(step.model)


# Diagnostics

# Check model assumptions
par(mfrow=c(2,2))
plot(step.model,which=1:4)

# Output a Shapiro-Wilks test
shapiro.test(step.model$residuals)

# Calculate the percentile of observation 35 on our F-distribution
pf(0.15, 12, 24)

