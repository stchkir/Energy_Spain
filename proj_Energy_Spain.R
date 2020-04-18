# https://www.kaggle.com/manualrg/spanish-electricity-market-demand-gen-price/download/spanish-electricity-market-demand-gen-price.zip

git <- "https://raw.githubusercontent.com/stchkir/Energy_Spain/master/spain_energy_market.csv"
temp <- tempfile()
download.file(git,temp)
energy_data <- read.csv(temp,sep=",")
unlink(temp)

# Load Packages
if (!require("lubridate")) install.packages("lubridate")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("caret")) install.packages("caret")
if (!require("corrplot")) install.packages("corrplot")
if (!require("RColorBrewer")) install.packages("RColorBrewer")

library("lubridate")
library("tidyverse")
library("caret")
library("corrplot")
library("RColorBrewer")

### Data Cleaning -----
head(energy_data)
str(energy_data)

# Clean up times
energy_data$datetime <- as_datetime(energy_data$datetime)
energy_data <- energy_data %>% mutate(year=year(datetime), month=month(datetime),
                                      week=week(datetime), wday=wday(datetime),
                                      hour=hour(datetime))

table(energy_data$year,energy_data$geoname)

# Clean up dublicates of spot prices
energy_data %>% filter(id==600) %>% arrange(datetime) %>% head(15)

energy_data <- energy_data %>% filter(name!="")
table(energy_data$year,energy_data$geoname)

# Change variable names to English
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

energy_data <- energy_data %>% left_join(select(variables,-id),by="name") %>%
  select(-id,-name,-geoid,-geoname)

# Spread table to link spot prices to other variables
energy_data_spread <- energy_data %>% spread(key=key,value=value)

# OPEN: sort columns

# Use year 2018 as validation model
model_data <- energy_data_spread %>% filter(year!=2018)
validation_data <- energy_data_spread %>% filter(year==2018)

### Data Exploration -----

# Calculate mean price by country
mean_price <- model_data %>% 
  summarise(mean_price_ESP=mean(spot_price_ESP),
            mean_price_FRA=mean(spot_price_FRA),
            mean_price_POR=mean(spot_price_POR))

print(mean_price)

RMSE(mean_price$mean_price_ESP,model_data$spot_price_ESP)

# Analyze time-based correlations
model_data %>% 
  gather(key=market,value=value,spot_price_ESP,spot_price_FRA,spot_price_POR) %>%
  ggplot(aes(datetime, value)) +
  geom_point(aes(color=market))

# Analyze price by month
model_data %>%
  gather(key=market,value=value,spot_price_ESP,spot_price_FRA,spot_price_POR) %>%
  mutate(month=as.factor(month)) %>% 
  group_by(month,market) %>% 
  ggplot(aes(month, value)) +
  geom_boxplot() +
  ylim(0,100) +
  facet_wrap(~market)

# Analyze price by weekday
model_data %>%
  gather(key=market,value=value,spot_price_ESP,spot_price_FRA,spot_price_POR) %>%
  mutate(wday=as.factor(wday)) %>% 
  group_by(wday,market) %>% 
  ggplot(aes(wday, value)) +
  geom_boxplot() +
  ylim(0,100) +
  facet_wrap(~market)

# Analyze generation related correlations

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

# Average generation by source
annual_generation <- model_data %>%
  gather(key=energy_source,value=value,
         plan_coal,plan_combicycle,plan_gas,plan_hydro,plan_nuclear,plan_pv,
         plan_reverse_hydro,plan_wind) %>%
  group_by(energy_source,year) %>% summarise(mean_generation=mean(value,na.rm=TRUE)) 

# Table
annual_generation %>%
  spread(key=year,value=mean_generation) %>% arrange(desc(`2017`))

# Graph
annual_generation %>% ggplot(aes(year,mean_generation,col=energy_source)) +
  geom_line()

# Calculate correlation to energy sources

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

### Prediction model -----

# Calculate regression line by datetime
regline <- model_data %>% lm(spot_price_ESP~datetime, data=.)

model_data <- model_data %>% mutate(price_regline=predict(regline,newdata=.))

model_data %>% 
  ggplot(aes(datetime, spot_price_ESP)) +
  geom_point() +
  geom_line(aes(datetime,price_regline), col="blue")

RMSE(model_data$price_regline,model_data$spot_price_ESP)

# Calculate monthly bias
bias_month <- model_data %>% group_by(month) %>% 
  summarise(bias_m=mean(spot_price_ESP-price_regline,na.rm=TRUE))

model_data <- model_data %>% left_join(bias_month, by="month") %>%
  mutate(price_month=price_regline+bias_m)
  
RMSE(model_data$spot_price_ESP,model_data$price_month)

# Calculate bias by weekday
bias_day <- model_data %>% group_by(wday) %>% 
  summarise(bias_d=mean(spot_price_ESP-price_month,na.rm=TRUE))

model_data <- model_data %>% left_join(bias_day, by="wday") %>%
  mutate(price_day=price_regline+bias_m+bias_d)

RMSE(model_data$spot_price_ESP,model_data$price_day)

model_data %>% 
  ggplot(aes(datetime, spot_price_ESP)) +
  geom_point() +
  geom_line(aes(datetime,price_day),col="blue")

### Results -----

# Use mean price for prediction
RMSE(validation_data$spot_price_ESP,mean_price$mean_price_ESP)

price_predict <- data.frame(method="mean",
                            accuracy=RMSE(validation_data$spot_price_ESP,
                                          mean_price$mean_price_ESP))

# Use regression line for prediction
validation_data <- validation_data %>% mutate(price_regline=predict(regline,newdata=.))

price_predict <- rbind(price_predict,
                data.frame(method="regline",
                           accuracy=RMSE(validation_data$price_regline,
                                         validation_data$spot_price_ESP)))

# Include monthly bias in prediction
validation_data <- validation_data %>% left_join(bias_month, by="month") %>%
  mutate(price_month=price_regline+bias_m)

price_predict <- rbind(price_predict,
                       data.frame(method="regline+monthly bias",
                                  accuracy=RMSE(validation_data$price_month,
                                                validation_data$spot_price_ESP)))

# Include daily bias in prediction
validation_data <- validation_data %>% left_join(bias_day, by="wday") %>%
  mutate(price_day=price_regline+bias_m+bias_d)

price_predict <- rbind(price_predict,
                       data.frame(method="regline+monthly and daily bias",
                                  accuracy=RMSE(validation_data$price_day,
                                                validation_data$spot_price_ESP)))