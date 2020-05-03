######## Preparation #######

### Source
# https://www.kaggle.com/manualrg/spanish-electricity-market-demand-gen-price/download/spanish-electricity-market-demand-gen-price.zip

### Load data from Git Repository
git <- "https://raw.githubusercontent.com/stchkir/Energy_Spain/master/data/spain_energy_market.csv"
temp <- tempfile()
download.file(git,temp)
energy_data <- read.csv(temp,sep=",")
unlink(temp)

### Load Packages
if (!require("lubridate")) install.packages("lubridate")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("caret")) install.packages("caret")
if (!require("corrplot")) install.packages("corrplot")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("forecast")) install.packages("forecast")
if (!require("zoo")) install.packages("forecast")
if (!require("knitr")) install.packages("knitr")
if (!require("kableExtra")) install.packages("kableExtra")

library("lubridate")
library("tidyverse")
library("caret")
library("corrplot")
library("RColorBrewer")
library("forecast")
library("zoo")
library("knitr")
library("kableExtra")

####### Data Cleaning #######

head(energy_data)
str(energy_data)

### Change time to datetime format
energy_data$datetime <- as_datetime(energy_data$datetime)
energy_data <- energy_data %>% mutate(year=year(datetime), month=month(datetime),
                                      week=week(datetime), wday=wday(datetime),
                                      hour=hour(datetime))

table(energy_data$year,energy_data$geoname)

### Clean up dublicates of spot prices
energy_data %>% filter(id==600) %>% arrange(datetime) %>% head(12)

energy_data <- energy_data %>% filter(name!="") # remove rows without name variable
table(energy_data$year,energy_data$geoname)

### Change variable names to English
variables <- energy_data %>% group_by(name) %>% 
  summarise(id = paste(unique(id), collapse = ', ')) 
print(variables,n=20)

variables_new <- c("total_demand_plan",
                   "total_demand_actual",
                   "trade_ESP",
                   "trade_FRA", 
                   "plan_coal",
                   "plan_combicycle",
                   "plan_wind",
                   "plan_gas",
                   "plan_nuclear",
                   "plan_pv",
                   "total_generation_plan",
                   "plan_hydro",
                   "plan_reverse_hydro",
                   "spot_price_ESP",
                   "spot_price_FRA",
                   "spot_price_POR",
                   "export_FRA",
                   "import_FRA",
                   "export_POR",
                   "import_POR")

variables <- variables %>% mutate(key=variables_new)
print(variables,n=21)

### Add English names and remove variables without additional information
energy_data <- energy_data %>% left_join(select(variables,-id),by="name") %>%
  select(-id,-name,-geoid,-geoname)

### Spread table (to link spot prices to other variables)
energy_data_spread <- energy_data %>% spread(key=key,value=value)

######## Train and Test Set #######

### Use year 2018 as test model
model_data <- energy_data_spread %>% filter(year!=2018)
test_data <- energy_data_spread %>% filter(year==2018)

####### Data Exploration #######

### Calculate mean spot price by country
mean_price <- model_data %>% 
  summarise(mean_price_ESP=mean(spot_price_ESP),
            mean_price_FRA=mean(spot_price_FRA),
            mean_price_POR=mean(spot_price_POR))

print(mean_price)

### Calculate error in train set using naive method
RMSE_model <- data.frame(method="naive",
                              accuracy=RMSE(mean_price$mean_price_ESP,
                                            model_data$spot_price_ESP))

### Show development of spot prices over time
model_data %>% 
  gather(key=market,value=value,spot_price_ESP,spot_price_FRA,spot_price_POR) %>%
  ggplot(aes(datetime, value)) +
  geom_point(aes(color=market))

### Show boxplot of spot prices by month and country (monthly bias)
model_data %>%
  gather(key=market,value=value,spot_price_ESP,spot_price_FRA,spot_price_POR) %>%
  mutate(month=as.factor(month)) %>% 
  group_by(month,market) %>% 
  ggplot(aes(month, value)) +
  geom_boxplot() +
  ylim(0,100) +
  facet_wrap(~market)

### Show boxplot of spot prices by weekday and country (weekday bias)
model_data %>%
  gather(key=market,value=value,spot_price_ESP,spot_price_FRA,spot_price_POR) %>%
  mutate(wday=as.factor(wday)) %>% 
  group_by(wday,market) %>% 
  ggplot(aes(wday, value)) +
  geom_boxplot() +
  ylim(0,100) +
  facet_wrap(~market)

### Show correlation between energy generation and spot market prices

# Coal energy
model_data %>% ggplot(aes(plan_coal,spot_price_ESP)) +
  geom_point(aes(color=as.factor(year))) +
  geom_smooth() +
  ylim(0,100)

# Nuclear energy
model_data %>% ggplot(aes(plan_nuclear,spot_price_ESP)) +
  geom_point(aes(color=as.factor(year))) +
  geom_smooth() +
  ylim(0,100)

# Wind energy
model_data %>% ggplot(aes(plan_wind,spot_price_ESP)) +
  geom_point(aes(color=as.factor(year))) +
  geom_smooth() +
  ylim(0,100)

# Gas energy
model_data %>% ggplot(aes(plan_gas,spot_price_ESP)) +
  geom_point(aes(color=as.factor(year))) +
  geom_smooth() +
  ylim(0,100)

### Calculate average generation by source
annual_generation <- model_data %>%
  gather(key=energy_source,value=value,
         plan_coal,plan_combicycle,plan_gas,plan_hydro,plan_nuclear,plan_pv,
         plan_reverse_hydro,plan_wind) %>%
  group_by(energy_source,year) %>% summarise(mean_generation=mean(value,na.rm=TRUE)) 

### Create table with average generation by source
annual_generation %>%
  spread(key=year,value=mean_generation) %>% arrange(desc(`2017`))

### Show average generation by source
annual_generation %>% ggplot(aes(year,mean_generation,col=energy_source)) +
  geom_line()

### Calculate correlation betwwen energy sources and to spot prices
model_data <- model_data %>% mutate(plan_renewables_total=plan_hydro+plan_pv+plan_wind)

impact_generation <- model_data %>% select(spot_price_ESP,
                                           plan_coal,plan_combicycle,
                                           plan_gas,plan_hydro,
                                           plan_nuclear,plan_pv,
                                           plan_reverse_hydro,plan_wind,
                                           plan_renewables_total) %>%
  cor(use="complete.obs")

corrplot(impact_generation, type="upper", order="original",
         col=brewer.pal(n=8, name="RdYlBu"))

####### Prediction model #######

### Calculate regression line through spot prices
regline <- model_data %>% lm(spot_price_ESP~datetime, data=.)

model_data <- model_data %>% mutate(price_regline=predict(regline,newdata=.))

### Show regression line in scatter plot of spot prices
model_data %>% 
  ggplot(aes(datetime, spot_price_ESP)) +
  geom_point() +
  geom_line(aes(datetime,price_regline), col="blue")

### Calculate error in train set using regression line
RMSE_model <- rbind(RMSE_model,
                         data.frame(method="regline",
                                    accuracy=RMSE(model_data$price_regline,
                                                  model_data$spot_price_ESP)))

### Calculate monthly bias (delta to regression line)
bias_month <- model_data %>% group_by(month) %>% 
  summarise(bias_m=mean(spot_price_ESP-price_regline,na.rm=TRUE))

model_data <- model_data %>% left_join(bias_month, by="month") %>%
  mutate(price_month=price_regline+bias_m)

### Calculate error in train set using regression line plus monthly bias
RMSE_model <- rbind(RMSE_model,
                    data.frame(method="regline+monthly bias",
                               accuracy=RMSE(model_data$spot_price_ESP,
                                             model_data$price_month)))

### Calculate bias by calendar week
bias_week <- model_data %>% group_by(week) %>% 
  summarise(bias_w=mean(spot_price_ESP-price_regline,na.rm=TRUE))

model_data <- model_data %>% left_join(bias_week, by="week") %>%
  mutate(price_week=price_regline+bias_w)

### Calculate error in train set using regression line plus weekly bias
RMSE_model <- rbind(RMSE_model,
                    data.frame(method="regline+bias by calendar week",
                               accuracy=RMSE(model_data$spot_price_ESP,
                                             model_data$price_week)))

### Calculate bias by weekday
bias_wday <- model_data %>% group_by(wday) %>% 
  summarise(bias_d=mean(spot_price_ESP-price_month,na.rm=TRUE))

model_data <- model_data %>% left_join(bias_wday, by="wday") %>%
  mutate(price_wday=price_regline+bias_m+bias_d)

### Calculate error in train set using regression line plus monthly amd daily bias
RMSE_model <- rbind(RMSE_model,
                    data.frame(method="regline+monthly and daily bias",
                               accuracy=RMSE(model_data$spot_price_ESP,
                                             model_data$price_wday)))

### Show predicted prices in scatter plot of spot prices
model_data %>% 
  ggplot(aes(datetime, spot_price_ESP)) +
  geom_point() +
  geom_line(aes(datetime,price_wday),col="blue")

### Show differences between spot prices and predicted prices
model_data %>% mutate(price_error=price_wday-spot_price_ESP) %>% 
  ggplot(aes(datetime, price_error)) +
  geom_point() +
  geom_smooth()

### Forecast prices with Holt Winters smoothing (by month)
# Calculate average spot price by month
spot_price_ESP <- model_data %>% group_by(year,month) %>% 
  summarize(spot_price_ESP=mean(spot_price_ESP))

# Convert spot prices into time series
ts_price_ESP <- ts(spot_price_ESP[,3], frequency=12, start=c(2014,1))

# Calculate Holt Winters model, including forecast for 2018
ts_price_hw <- hw(ts_price_ESP, h=12, seasonal="additive")

autoplot(ts_price_ESP) +
  autolayer(ts_price_hw)

# Convert prices from Holt Winters into data frame for train period and forecast (test)
price_hw_model <- data.frame(date=as.Date(yearmon(time(ts_price_hw$x))),
                       price_hw=matrix(ts_price_hw$x)) %>%
  mutate(year=year(date),month=month(date)) %>% 
  select(-date)

price_hw_test <- data.frame(date=as.Date(yearmon(time(ts_price_hw$mean))),
                             price_hw=matrix(ts_price_hw$mean)) %>%
  mutate(year=year(date),month=month(date)) %>%
  select(-date)

model_data <- model_data %>% left_join(price_hw_model,by=c("year","month"))

### Calculate error in train set using Holt Winters method
RMSE_model <- rbind(RMSE_model,
                    data.frame(method="hw by month",
                               accuracy=RMSE(model_data$price_hw,
                                             model_data$spot_price_ESP)))

### Add bias by weekday on top of Holt Winters
model_data <- model_data %>% mutate(price_hw_wday=price_hw+bias_d)

### Calculate error in train set using Holt Winters method plus daily bias
RMSE_model <- rbind(RMSE_model,
                    data.frame(method="hw by month+daily bias",
                               accuracy=RMSE(model_data$spot_price_ESP,
                                             model_data$price_hw_wday)))

### Show difference between regrssion line plus monthly bias and Holt Winters method
model_data %>% 
  ggplot(aes(datetime, spot_price_ESP)) +
  geom_point() +
  geom_line(aes(datetime,price_wday),col="blue") +
  geom_line(aes(datetime,price_hw_wday),col="red")

####### Results #######

### Calculate error in test set using naive method
RMSE(test_data$spot_price_ESP,mean_price$mean_price_ESP)

RMSE_test <- data.frame(method="naive",
                            accuracy=RMSE(test_data$spot_price_ESP,
                                          mean_price$mean_price_ESP))

### Calculate error in test set using regression line
test_data <- test_data %>% mutate(price_regline=predict(regline,newdata=.))

RMSE_test <- rbind(RMSE_test,
                data.frame(method="regline",
                           accuracy=RMSE(test_data$price_regline,
                                         test_data$spot_price_ESP)))

### Calculate error in test set using regression line plus monthly bias
test_data <- test_data %>% left_join(bias_month, by="month") %>%
  mutate(price_month=price_regline+bias_m)

RMSE_test <- rbind(RMSE_test,
                       data.frame(method="regline+monthly bias",
                                  accuracy=RMSE(test_data$price_month,
                                                test_data$spot_price_ESP)))

### Calculate error in test set using regression line plus weekly bias
test_data <- test_data %>% left_join(bias_week, by="week") %>%
  mutate(price_week=price_regline+bias_w)

RMSE_test <- rbind(RMSE_test,
                         data.frame(method="regline+bias by calendar week",
                                    accuracy=RMSE(test_data$price_week,
                                                  test_data$spot_price_ESP)))

### Calculate error in test set using regression line plus monthly amd daily bias
test_data <- test_data %>% left_join(bias_wday, by="wday") %>%
  mutate(price_wday=price_regline+bias_m+bias_d)

RMSE_test <- rbind(RMSE_test,
                       data.frame(method="regline+monthly and daily bias",
                                  accuracy=RMSE(test_data$price_wday,
                                                test_data$spot_price_ESP)))

### Calculate error in test set using Holt Winters method
test_data <- test_data %>% left_join(price_hw_test,by=c("year","month"))

RMSE_test <- rbind(RMSE_test,
                    data.frame(method="hw by month",
                               accuracy=RMSE(test_data$price_hw,
                                             test_data$spot_price_ESP)))

### Calculate error in test set using Holt Winters method plus monthly bias
test_data <- test_data %>% mutate(price_hw_wday=price_hw+bias_d)

RMSE_test <- rbind(RMSE_test,
                    data.frame(method="hw by month+daily bias",
                               accuracy=RMSE(test_data$spot_price_ESP,
                                             test_data$price_hw_wday)))

### Show difference between regression line plus monthly bias and Holt Winters method
test_data %>% 
  ggplot(aes(datetime, spot_price_ESP)) +
  geom_point() +
  geom_line(aes(datetime,price_wday),col="blue") +
  geom_line(aes(datetime,price_hw_wday),col="red")

