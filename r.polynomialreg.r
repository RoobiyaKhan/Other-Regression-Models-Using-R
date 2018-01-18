#Polynomial Regression

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

#Splitting the dataset into the Training set and Test set
#install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$DependentVariable, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

#fitting linear regression to the dataset
 lin_reg = lm(formula = Salary ~ .,
              data = dataset)
 summary(lin_reg)
#fitting polynomial regression to the dataset
 dataset$Level2 = dataset$Level^2
 dataset$Level3 = dataset$Level^3
 dataset$Level4 = dataset$Level^4
 poly_reg = lm(formula = Salary ~ .,
              data = dataset)
 summary(poly_reg)
 
#visualizing the linear regression results
 #install.packages('ggplot2')
 #library(ggplot2)
 ggplot() +
   geom_point(aes(x = dataset$Level, y = dataset$Salary),
                       color = 'red') + 
   geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
             color = 'blue') +
   ggtitle('truth or bluff(linear regression)') + 
  xlab('Level') +
  ylab('Salary')
#visualizing the polynomial regression results(for higher resolution and smoother curve)
 x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1) #x_grid is a vector
 ggplot() +
   geom_point(aes(x = dataset$Level, y = dataset$Salary),
              color = 'red') + 
   geom_line(aes(x = x_grid, y = predict(poly_reg, newdata = data.frame(Level = x_grid,
                                                                        Level2 = x_grid^2,
                                                                        Level3 = x_grid^3,
                                                                        Level4 = x_grid^4))),#making x_grid into dataframe
             color = 'blue') +
   ggtitle('truth or bluff(polynomial regression)') + 
   xlab('Level') +
   ylab('Salary')
 
#predicting a new result with linear regression
y_pred = predict(lin_reg, data.frame(Level = 6.5)) # creating a new data frame with only one value 6.5
y_pred   #here y_pred is not the vector,it is the single prediction value here
#predicting a new result with polynomial regression
y_pred = predict(poly_reg, data.frame(Level = 6.5,
                                      Level2 = 6.5^2,
                                      Level3 = 6.5^3,
                                      Level4 = 6.5^4))