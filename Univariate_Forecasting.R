library(readxl)
setwd("D:/APU/Sem 2/CP1")
cities = read_excel("City.xlsx")
 
View(cities)
all_food = ts(cities$AllSpend, start = 2000)

plot(all_food)

library(forecast)
library(tseries)

nsdiffs(all_food) # no seasonality detected
ndiffs(all_food) # difference lag-1

library(Kendall)
MannKendall(all_food) # strong upward trend

library(ggplot2)
ggplot(data = cities, aes(x = Year, y = AllSpend)) +  # Use all_food instead of IDR
  geom_line() +
  geom_point() +
  ggtitle("Average Monthly Food Consumption Spending per Capita") +
  xlab("Year") +
  ylab("IDR") +  # Keep the y-axis label as IDR for clarity
  scale_y_continuous(labels = scales::comma)

library(forecast)
library(tseries)

acf(all_food, ylim = c(-1,1))
pacf(all_food, ylim = c(-1,1))
adf.test(all_food)
#p-value 0.4154, means H0 is not rejected, data is not stationary.
tsdisplay(all_food)

ndiffs(all_food) #find out differencing value
acf(diff(all_food,1)) #the acf chart shows no spike only at lag 0
tsdisplay(diff(all_food,1))
adf.test(diff(all_food,1))
# p value 0.468 > 0.05, H0 is not rejected, data still not stationary

ndiffs(diff(all_food,1))
nsdiffs(diff(all_food,1))

kpss.test(diff(all_food,1))

library(TTR)
#WITH TRAIN DATA
  
#Train Data : Year 2000 - 2020
all_food_train <- ts(all_food[1:21], start = 2000)
all_food_test <- all_food[22:23]
# MOVING AVERAGE with SES 
all_food_3MA <- SMA(all_food_train,3)

cities323 = cities[3:23,]

#Forecast SES
all_food_ses <- ses(all_food_train,initial = "simple", h=2)
All_Food_ses <- c(all_food_train,670304,670304)
City <- data.frame(cities323, All_Food_ses)

# Create the plot with both lines
ggplot(data = City, aes(x = Year)) +
  geom_line(aes(y = AllSpend), color = "black") +  # Original data line
  geom_line(aes(y = All_Food_ses), color = "darkblue", lwd = 1) +  # ses line
  geom_point(aes(y = AllSpend)) +  # Add points for the original data
  ggtitle("Average Monthly Food Consumption Spending per Capita") +
  xlab("Year") +
  ylab("IDR") +
  scale_y_continuous(labels = scales::comma)


plot(all_food_ses, main = "SES Forecast", xlab = "Year", ylab = "Spending")
lines(all_food_ses$fitted, col=2, lwd=2)
legend("bottomright", c("Actual", "SES", "Forecast"), col=c(1:2, "darkgreen"), lwd=2, cex = 0.5)

#check performance
accuracy(all_food_ses,all_food_test)

# HOLT
all_food_holt <- holt(all_food_train, initial= "simple", h=2)
holt2122 <- forecast(all_food_holt, h = 2)
all_food_holtextend <- c(all_food_train, 708058.7, 744114.1)
City <- data.frame(cities323, All_Food_ses, all_food_holtextend)

ggplot(data = City, aes(x = Year)) +
  geom_line(aes(y = AllSpend), color = "black") +  # Original data line
  geom_line(aes(y = All_Food_ses), color = "darkblue", lwd = 1) +  # 3MA line
  geom_line(aes(y = all_food_holtextend), color = "darkred", lwd = 1) +  # Holt Winters
  geom_point(aes(y = AllSpend)) +  # Add points for the original data
  ggtitle("Average Monthly Food Consumption Spending per Capita") +
  xlab("Year") +
  ylab("IDR") +
  scale_y_continuous(labels = scales::comma)

plot(all_food_holt, main = "Holt Forecast", xlab = "Year", ylab = "Spending")
lines(all_food_holt$fitted, col=2, lwd=2)
legend("bottomright", c("Actual", "Holt", "Forecast"), col=c(1:2, "blue"), lwd=2, cex = 0.5)

#check performance  
accuracy(all_food_holt,all_food_test)

# ARIMA
#check best model based on auto.arima
auto.arima(all_food_train, trace = TRUE, allowdrift = FALSE)

arima_210 <- Arima(all_food_train, order = c(2,1,0))
all_food_arima <- forecast(arima_210, h=2)

#check performance
accuracy(all_food_arima, all_food_test)

#check arima with drift allowed
auto.arima(all_food_train, trace = TRUE)

arima_010drift <- Arima(all_food_train, order = c(0,1,0), include.drift = TRUE)
all_food_arima010 <- forecast(arima_010drift, h=2)

#check performance
accuracy(all_food_arima010, all_food_test)

#arima with drift accuracy is actually lower - not used

#graph
plot(all_food_arima, main = "ARIMA (2,1,0) Forecast", 
     xlab = "Year", ylab = "Spending")
lines(arima_210$fitted, col=2, lwd=2)
legend("bottomright", c("Actual", "ARIMA(2,1,0)"), col = c(1:2, "red"), lwd=2, cex = 0.7)

summary(arima_210)

library(lmtest)
checkresiduals(arima_210)
coeftest(arima_210)

#check performance
accuracy(all_food_arima, all_food_test)

# BEST MODEL for UNIVARIATE: ARIMA
#Apply ARIMA 210 on Whole dataset

all_food = ts(all_food[1:23], start = 2000)
arima_210_whole <- Arima(all_food, order = c(2,1,0))
arima_2327_forecast <- forecast(arima_210_whole, h=5)

#graph
plot(arima_2327_forecast, main = "Indonesia Average Monthly Food per Capita Spending Forecast", 
     xlab = "Year", ylab = "Spending")
lines(arima_210_whole$fitted, col=2, lwd=2)
legend("bottomright", c("Actual", "ARIMA(2,1,0)"), lwd=2, cex = 0.7)

summary(arima_210_whole)
checkresiduals(arima_210_whole)
coeftest(arima_210_whole)
