#Decision Tree

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

# Splitting the dataset into the Training set and Test set
# # install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling                      #no need feature scaling as decision trees reg based on conditions and 
                                      #independent variables and not on euclidean distances
# training_set = scale(training_set)  #usually machine learning models uses euclidean dist(all indep var on same scale)-so feature scaling needed
# test_set = scale(test_set)

# Fitting the DecisionTree Regression to the dataset
#install.packages('rpart')
#library(rpart)               #prob related to no of splits -so add control parameter in regressor
regressor = rpart(formula = Salary ~ .,
                  data = dataset,
                  control = rpart.control(minsplit = 1))


# Predicting a new resultrpart.control()
y_pred = predict(regressor, data.frame(Level = 6.5))

# Visualising the DecisionTree Regression results  
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (DecisionTree Regression)') +
  xlab('Level') +
  ylab('Salary')

# Visualising the  DecisionTreeRegression results (for higher resolution and smoother curve)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (DecisionTree Regression)') +
  xlab('Level') +
  ylab('Salary')