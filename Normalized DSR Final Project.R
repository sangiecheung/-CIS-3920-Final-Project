#Final Project EDA

getwd()
setwd("C:/Users/Honors/Desktop")
data=read.csv("cars.csv")

#Examining Data----------------------------------------------------------------- 
summary(data)
dim(data) # Number of observations and columns
head(data) # Take a quick preview of the data
names(data) # Variable/column names 

#Converting variables to different types: 
data$manufacturer_name = as.factor(data$manufacturer_name)
data$transmission = as.factor(data$transmission)
data$color = as.factor(data$color)
data$state = as.factor(data$state)
data$year_produced = as.integer(data$year_produced)
data$body_type = as.factor(data$body_type)
data$engine_type = as.factor(data$engine_type)
data$has_warranty = as.logical(data$has_warranty)
data$drivetrain = as.factor(data$drivetrain)
data$engine_has_gas = as.logical(data$engine_has_gas)

str(data) # Get information of variables with data type changes

#Data Cleaning------------------------------------------------------------------
#Variable: State
summary(data$state)
data$state[data$state=="new"] = NA # Not including any new cars 
data$state[data$state=="emergency"] = NA # Not including any damaged cars 
summary(data$state)

#Variable: Odometer_value
summary(data$odometer_value)
boxplot(data$odometer_value)
data$odometer_value[data$odometer_value > 600000] = NA # Odometer over 600000 miles are considered outliers
data$odometer_value[data$odometer_value < 100] = NA # Odometer under 100 miles are considered outliers
summary(data$odometer_value)

#Variable: price_usd
summary(data$price_usd)
hist(data$price_usd,breaks = 100)
data$price_usd[data$price_usd < 100] = NA #Not including cars under $100
summary(data$price_usd)

#Handling Missing Values:
missing = is.na(data)
sum(missing)

for (i in names(data)) {
  print(paste(i,":",sum(is.na(data[i])),sep=" "))
} #To see which variables have missing values 

data = na.omit(data) # Delete missing values 
sum(is.na(data)) # Count missing again

#Removing levels of manufacturer_names
summary(data$manufacturer_name)
levels(data$manufacturer_name)
manufacturers = c("Acura","Alfa Romeo","Audi","BMW","Buick","Cadillac","Chery",
                  "Chevrolet","Chrysler","Citroen","Dacia","Daewoo","Dodge",
                  "Fiat","Ford","Geely","Great Wall","Honda","Hyundai","Infiniti",
                  "Iveco","Jaguar","Jeep","Kia","LADA","Lancia","Land Rover",
                  "Lexus","Lifan","Lincoln","Mazda","Mercedes-Benz","Mini",
                  "Mitsubishi","Nissan","Opel","Peugeot","Pontiac","Porsche",
                  "Renault","Rover","Saab","Seat","Skoda","SsangYong","Subaru",
                  "Suzuki","Toyota","Volkswagen","Volvo")         
data <- droplevels(data[data$manufacturer_name %in% manufacturers,])
levels(data$manufacturer_name)

#Data with selected variables---------------------------------------------------
features = c("manufacturer_name","transmission","color","odometer_value",
             "year_produced", "engine_type", "engine_capacity",
             "body_type","has_warranty","drivetrain","price_usd") 
# Variables we want to focus on

cars_data = data[,features] # Creates data-set with chosen variables 
dim(cars_data)
str(cars_data) # Get information about variables
#Dummy Variables--turn categorical to numeric-----------------------------------
str(cars_data)
#Car Brands--------------------------------
summary(cars_data$manufacturer_name)
cars_data$GM = 0
cars_data$GM [cars_data$manufacturer_name %in% c("Chevrolet","Buick","Cadillac","Chery","Daewoo","Pontiac")] = 1
cars_data$GM = as.factor(cars_data$GM)
summary(cars_data$GM)

##Stellantis
cars_data$Stellantis = 0
cars_data$Stellantis [cars_data$manufacturer_name %in% c("Citroen","Alfa Romeo","Chrysler","Dodge","Fiat","Jeep","Lancia","Opel","Peugeot")] = 1
cars_data$Stellantis = as.factor(cars_data$Stellantis)
summary(cars_data$Stellantis)

##Volkswagen
cars_data$Volkswagen = 0
cars_data$Volkswagen [cars_data$manufacturer_name %in% c("Audi","Porsche","Seat","Skoda","Volkswagen")] = 1
cars_data$Volkswagen = as.factor(cars_data$Volkswagen)
summary(cars_data$Volkswagen)

#Color--------------------------------------
##black
cars_data$black = 0
cars_data$black [cars_data$color == "black"] = 1
cars_data$black = as.factor(cars_data$black)
summary(cars_data$black)
##blue
cars_data$blue = 0
cars_data$blue [cars_data$color == "blue"] = 1
cars_data$blue = as.factor(cars_data$blue)
summary(cars_data$blue)
##brown
cars_data$brown = 0
cars_data$brown [cars_data$color == "brown"] = 1
cars_data$brown = as.factor(cars_data$brown)
summary(cars_data$brown)
##silver
cars_data$silver = 0
cars_data$silver [cars_data$color == "silver"] = 1
cars_data$silver = as.factor(cars_data$silver)
summary(cars_data$silver)
##white
cars_data$white = 0
cars_data$white [cars_data$color == "white"] = 1
cars_data$white = as.factor(cars_data$white)
summary(cars_data$white)

#Drivetrain--------------------------
##Front wheel drive
cars_data$fwd = 0
cars_data$fwd [cars_data$drivetrain == "front"] = 1
cars_data$fwd = as.factor(cars_data$fwd)
summary(cars_data$fwd)
##Rear wheel drive
cars_data$rwd = 0
cars_data$rwd [cars_data$drivetrain == "rear"] = 1
cars_data$rwd = as.factor(cars_data$rwd)
summary(cars_data$rwd)

#Transmission------------------------
##Automatic
cars_data$mechanical = 0
cars_data$mechanical [cars_data$transmission == "mechanical"] = 1
cars_data$mechanical = as.factor(cars_data$mechanical)
summary(cars_data$mechanical)

#Engine Type--------------------------
##Gasoline
cars_data$gasoline = 0
cars_data$gasoline [cars_data$engine_type == "gasoline"] = 1
cars_data$gasoline = as.factor(cars_data$gasoline)
summary(cars_data$gasoline)

#Body Type----------------------------
##Hatchback
cars_data$hatchback = 0
cars_data$hatchback [cars_data$body_type == "hatchback" ] = 1
cars_data$hatchback = as.factor(cars_data$hatchback)
summary(cars_data$hatchback)
##Minivan
cars_data$minivan = 0
cars_data$minivan [cars_data$body_type == "minivan" ] = 1
cars_data$minivan = as.factor(cars_data$minivan)
summary(cars_data$minivan)
##Sedan
cars_data$sedan = 0
cars_data$sedan [cars_data$body_type == "sedan" ] = 1
cars_data$sedan = as.factor(cars_data$sedan)
summary(cars_data$sedan)
##Universal
cars_data$universal = 0
cars_data$universal [cars_data$body_type == "universal" ] = 1
cars_data$universal = as.factor(cars_data$universal)
summary(cars_data$universal)

#DATASET--with dummy variables--------------------------------------------------
selection = c("GM","Stellantis","Volkswagen","black","blue","brown","silver",
              "white","mechanical","odometer_value",
              "year_produced", "gasoline", "engine_capacity",
              "hatchback","minivan","sedan","universal","has_warranty",
              "fwd","rwd","price_usd")
dataset=cars_data[,selection] # Creates data-set with chosen variables 

str(dataset)

normalize = function(x) {
  return((x-min(x))/(max(x)-min(x)))
}
norm.odometer = normalize(dataset$odometer_value)
norm.year = normalize(dataset$year_produced)
norm.capacity = normalize(dataset$engine_capacity)

norm.dataset = cbind(dataset[,1:9],norm.odometer,norm.year,dataset[12],norm.capacity,dataset[,14:21])
summary(norm.dataset)

#Training/Test Set--------------------------------------------------------------
set.seed(1) # for reproducibility purposes
train.index = sample(1:nrow(norm.dataset),nrow(norm.dataset)*0.80)

train = norm.dataset[train.index,]
test = norm.dataset[-train.index,]

summary(train)
summary(test)

#Linear Regression--------------------------------------------------------------
##Model Building
model = lm(price_usd~.,data=train)
summary(model)

library(leaps)

# Apply forward selection to model with all variables (the ~. represents all predictors)
model_fwd = regsubsets(price_usd~., data=train,
                       nvmax=NULL, method="forward")
summary(model_fwd)# Take a look at the process
plot(model_fwd, scale="adjr2", main="Forward Selection: AdjR2")
model_fwd_summary = summary(model_fwd) # Store summary output
which.max(model_fwd_summary$adjr2) # Display best subset by adjr2
summary(model_fwd)$which[15,]

best_model = lm(price_usd~GM+Stellantis+Volkswagen+silver+mechanical+
                  norm.odometer+norm.year+gasoline+norm.capacity+
                  hatchback+minivan+sedan+universal+fwd+rwd,data = train)
summary(best_model)

##Making Predictions
train.pred.y = predict(best_model,train) # make predictions on training set
train.rmse = sqrt(mean((train$price_usd-train.pred.y)^2))
train.rmse # What is our training RMSE?

##Applying Model
pred.y = predict(best_model,test) # apply the model on our test set now
test.rmse = sqrt(mean((test$price_usd-pred.y)^2))
test.rmse # this is our test RMSE
