
## 1. Read in the csv file data ------------------------
returns <- read.csv(file.choose()) #  returns_data.csv

head(returns)
summary(returns)
dd<-tidy(returns)

## 2. Save as separate objects ---------------------------
food <- ts(returns$food, start = c(2000,1), end = c(2014,12), 
           frequency = 12)
durables <- ts(returns$durables, start = c(2000,1), end = c(2014,12),
               frequency = 12)

## 3. Expected returns -----------------------------------
# Simple average: mean monthly return
r.food <- mean(food)
r.durables <- mean(durables)
# you can try to calculate the geometric mean as an exercise


# 4. Annualized returns, we can use the EAR ---------------
# EAR = (1+r_monthly)^12 - 1

library(scales) # change numeric data to percentage

EAR.food <- (1+r.food/100)^12-1

round(EAR.food, digits = 3) # rounding of numbers

label_percent(accuracy = 0.01)(EAR.food)

EAR.dur <- (1+r.durables/100)^12-1

label_percent(accuracy = 0.01)(EAR.dur)

# round function: specify the number of decimal places

# 5. Standard deviation -----------------------------------
sd.food <- sd(food)
sd.food

sd.durables <- sd(durables)
sd.durables

## 6. Moving average standard deviation -------------------

install.packages("TTR")
library(TTR)

# moving average sd
ma.sd.food <- runSD(food, n = 12)
head(ma.sd.food, n = 30)

# plot the ma.sd
plot(ma.sd.food, type = "l", lwd = 2)
abline(h = sd(food), col="red", lwd=3, lty=2) 

plot(food, type = "l", lwd = 1)
abline(h = 0, col="red", lwd=3, lty=2) 


# 7. moving average mean ----------------------------------
ma.mean.food <- runMean(food, n = 12)
head(ma.mean.food, n = 30)

# plot the ma.mean
plot(ma.mean.food, type = "l", lwd = 2)
abline(h = mean(food), col="red", lwd=3, lty=2) 

# 8. Multiple plots together -------------------------------
par(mfrow = c(2, 1))

par(mar=c(4,2,2,2)) #c(bltr)

plot(ma.sd.food, type = "l", lwd = 2, main = "Volatility")
abline(h = sd(food), col="red", lwd=3, lty=2) 

plot(ma.mean.food, type = "l", lwd = 2, main = "Mean Return")
abline(h = mean(food), col="red", lwd=3, lty=2) 

par(mfrow = c(1,1))

# overall correlation between returns and risks     
cor.food <- cor(ma.sd.food, ma.mean.food, use = "complete.obs")
cor.food

# try first 2-year data, 2002/01-2003/12
r <- ma.mean.food[13:36]
sd <- ma.sd.food[13:36]

cor(r, sd)

# 9. Put variables together as one object -------------------
ma <- data.frame(returns$date, food, ma.sd.food, ma.mean.food)

# 10. Export data as CSV -----------------------------------
write.csv(ma, file = "moving average.csv")




