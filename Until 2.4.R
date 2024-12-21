#Importing Data Set 
library(readr)
STW7089CEM_Data <- read_csv("STW7089CEM-Data.csv")

#Library Files
library(ggplot2)
library(reshape2)
library(ggpubr)
suppressMessages(library(GGally))
library(GGally)
library(DT)
library(knitr)

#Renaming the Data Set
Data<- STW7089CEM_Data
x1 <- Data$x1
x2 <- Data$x2
x3 <- Data$x3
x4 <- Data$x4
x5 <- Data$x5


#Viewing data if they are render properly or not
head(Data)
kable(head(Data))

#Checking for missing values
missingvalues <- is.na(Data)
missvalues <- sum(missingvalues)
print(sprintf("Number of missing value in the dataset is %d", missvalues))


#Task 1.1: Time Series Plot 
#Creating a new variable time in interval of 0.06
Data$Time <- seq(from = 0.06, by = 0.06, length.out = 200)
head(Data)

#Plotting Time Series Plot between input X1 and Time
plot(Data$Time, x1, type = "l", col = "black",
     xlab = "Time", ylab = "Input X1", 
     main = "Time Series of Input X1 and Time")

#Plotting Time Series Plot between output X2 and Time
plot(Data$Time, x2, type = "l", col = "black",
     xlab = "Time", ylab = "Output X2", 
     main = "Time Series of Output X2 and Time")

#Plotting Time Series Plot between input X3 and Time
plot(Data$Time, x3, type = "l", col = "black",
     xlab = "Time", ylab = "Input X3", 
     main = "Time Series of Input X3 and Time")

#Plotting Time Series Plot between input X4 and Time
plot(Data$Time, x4, type = "l", col = "black",
     xlab = "Time", ylab = "Input X4", 
     main = "Time Series of Input X4 and Time")



#Task 1.2: Distribution for each EEG signals
##Histogram for Input 
#Histogram for x1
ggplot(data = Data, aes(x = x1)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", color = "black") +
  stat_density(geom = "line", color = "black", size = 3) +
  geom_rug() +
  labs(title = "Distribution of the Input x1",
       xlab = "Input Signal x1",
       ylab = "Density") +
        theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        plot.title = element_text(color = "black"))

#Task 1.2: Distribution for each EEG signals
#Histogram for x3
ggplot(data = Data, aes(x = x3)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", color = "black") +
  stat_density(geom = "line", color = "black", size = 3) +
  geom_rug() +
  labs(title = "Distribution of the Input x3",
       xlab = "Input Signal x3",
       ylab = "Density") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        plot.title = element_text(color = "black"))


#Task 1.2: Distribution for each EEG signals
#Histogram for x4
ggplot(data = Data, aes(x = x4)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", color = "black") +
  stat_density(geom = "line", color = "black", size = 3) +
  geom_rug() +
  labs(title = "Distribution of the Input x4",
       xlab = "Input Signal x4",
       ylab = "Density") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        plot.title = element_text(color = "black"))


#Task 1.2: Distribution for each EEG signals
#Histogram for x5
ggplot(data = Data, aes(x = x5)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", color = "black") +
  stat_density(geom = "line", color = "black", size = 3) +
  geom_rug() +
  labs(title = "Distribution of the Input x5",
       xlab = "Input Signal x5",
       ylab = "Density") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        plot.title = element_text(color = "black"))


#Task 1.2: Distribution for each EEG signals
##Histogram for Output 
#Histogram for x2 
ggplot(data = Data, aes(x = x2)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightgreen", color = "black") +
  stat_density(geom = "line", color = "black", size = 3) +
  geom_rug() +
  labs(title = "Distribution of the Output x2",
       xlab = "Output Signal x2",
       ylab = "Density") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        plot.title = element_text(color = "black"))



#Task 1.3  Correlation and scatter plots (between different combination of input and output signals) to examine their dependencies

print(correlation_test)

#Scatter Plot for X1 input and X2 output
ggscatter(
  data = Data,
  x = "x1", 
  y = "x2",
  xlab = "Input Signal X1",
  ylab = "Output Signal X2",
  add = "reg.line",
  add.params = list(color = "black"),  
  color = "black",                  
  size = 2,                       
  shape = 10,
  title = "Scatter Plot of X1 Input vs X2 Output",
cor.coef = TRUE, cor.method = "pearson"
)


#Scatter Plot for X3 input and X2 output
ggscatter(
  data = Data,
  x = "x3", 
  y = "x2",
  xlab = "Input Signal X3",
  ylab = "Output Signal X2",
  add = "reg.line",
  add.params = list(color = "black"),  
  color = "black",                  
  size = 2,                       
  shape = 10,
  title = "Scatter Plot of X3 Input vs X2 Output",
  cor.coef = TRUE, cor.method = "pearson"
)


#Scatter Plot for X4 input and X2 output
ggscatter(
  data = Data,
  x = "x4", 
  y = "x2",
  xlab = "Input Signal X4",
  ylab = "Output Signal X2",
  add = "reg.line",
  add.params = list(color = "black"),  
  color = "black",                  
  size = 2,                       
  shape = 10,
  title = "Scatter Plot of X4 Input vs X2 Output",
  cor.coef = TRUE, cor.method = "pearson"
)

#Scatter Plot for X5 input and X2 output
ggscatter(
  data = Data,
  x = "x5", 
  y = "x2",
  xlab = "Input Signal X5",
  ylab = "Output Signal X2",
  add = "reg.line",
  add.params = list(color = "black"),  
  color = "black",                  
  size = 2,                       
  shape = 10,
  title = "Scatter Plot of X5 Input vs X2 Output",
  cor.coef = TRUE, cor.method = "pearson"
)


#Correlation between variables
ggpairs (
  data = Data,
  columns = 1:5,
)




#Task 2.1 Estimate Model Parameters 
#Model 1
constant = matrix(1 , length(Data)/4,1)
model_1<-cbind(constant,(Data[,"x4"]),(Data[,"x3"])^2)
model_1 <- as.matrix(model_1)
theta_hat_1 <- solve(t((model_1)) %*% (model_1)) %*% t((model_1)) %*% (Data$x2)
print(theta_hat_1) 

#Model 2
constant = matrix(1 , length(Data)/4,1)
model_2<-cbind(constant,(Data[,"x4"]),(Data[,"x3"])^2,(Data[,"x5"]))
model_2 <- as.matrix(model_2)
theta_hat_2 <- solve(t((model_2)) %*% (model_2)) %*% t((model_2)) %*% (Data$x2)
print(theta_hat_2)


#Model 3
constant = matrix(1 , length(Data)/4,1)
model_3 <- cbind(constant,(Data[,"x3"]),(Data[,"x4"]),(Data[,"x5"]^3))
model_3 <- as.matrix(model_3)
theta_hat_3 <- solve(t((model_3)) %*% (model_3)) %*% t((model_3)) %*% (Data$x2)
print(theta_hat_3)

#Model 4 
constant = matrix(1 , length(Data)/4,1)
model_4 <- cbind(constant,(Data[,"x4"]),(Data[,"x3"]^2),(Data[,"x5"]^3))
model_4 <-as.matrix(model_4)
theta_hat_4 <- solve(t((model_4)) %*% (model_4)) %*% t((model_4)) %*% (Data$x2)
print(theta_hat_4)

#Model 5
constant = matrix(1 , length(Data)/4,1)
model_5 <- cbind(constant,(Data[,"x4"]),(Data[,"x1"]^2),(Data[,"x3"]^2))
model_5 <- as.matrix(model_5)
theta_hat_5 <- solve(t((model_5)) %*% (model_5)) %*% t((model_5)) %*% (Data$x2)
print(theta_hat_5)


#Task 2.2 Model Residual Error and Sum of Square Error 
#Calculating RSS for Model 1
theta_hat_1 <- as.matrix(theta_hat_1)
Y_hat_model_1 <- model_1 %*% theta_hat_1
RSS_model_1 <- sum((Data$x2 - Y_hat_model_1)^2)
print(RSS_model_1)

#Calculating RSS for Model 2
theta_hat_2 <- as.matrix(theta_hat_2)
Y_hat_model_2 <- model_2 %*% theta_hat_2
RSS_model_2 <- sum((Data$x2 - Y_hat_model_2)^2)
print(RSS_model_2)

#Calculating RSS for Model 3
theta_hat_3 <- as.matrix(theta_hat_3)
Y_hat_model_3 <- model_3 %*% theta_hat_3
RSS_model_3 <- sum((Data$x2 - Y_hat_model_3)^2)
print(RSS_model_3)

#Calculating RSS for Model 4
theta_hat_4 <- as.matrix(theta_hat_4)
Y_hat_model_4 <- model_4 %*% theta_hat_4
RSS_model_4 <- sum((Data$x2 - Y_hat_model_4)^2)
print(RSS_model_4)

#Calculating RSS for Model 5
theta_hat_5 <- as.matrix(theta_hat_5)
Y_hat_model_5 <- model_5 %*% theta_hat_5
RSS_model_5 <- sum((Data$x2 - Y_hat_model_5)^2)
print(RSS_model_5)


#Task 2.3 Log Likelihood Function 
N <- nrow(Data)
#Calculating Log Likelihood Function for Model 1
var_model_1 = RSS_model_1/(N-1)
print(var_model_1)
likelihood_model_1 <- -(N/2)*(log(2*pi))-(N/2)*(log(var_model_1))-(1/(2*var_model_1))*RSS_model_1
print(likelihood_model_1)

#Calculating Log Likelihood Function for Model 2
var_model_2 = RSS_model_2/(N-1)
print(var_model_2)
likelihood_model_2 <- -(N/2)*(log(2*pi))-(N/2)*(log(var_model_2))-(1/(2*var_model_2))*RSS_model_2
print(likelihood_model_2)

#Calculating Log Likelihood Function for Model 3
var_model_3 = RSS_model_3/(N-1)
print(var_model_3)
likelihood_model_3 <- -(N/2)*(log(2*pi))-(N/2)*(log(var_model_3))-(1/(2*var_model_3))*RSS_model_3
print(likelihood_model_3)

#Calculating Log Likelihood Function for Model 4
var_model_4 = RSS_model_4/(N-1)
print(var_model_4)
likelihood_model_4 <- -(N/2)*(log(2*pi))-(N/2)*(log(var_model_4))-(1/(2*var_model_4))*RSS_model_4
print(likelihood_model_4)

#Calculating Log Likelihood Function for Model 5
var_model_5 = RSS_model_5/(N-1)
print(var_model_5)
likelihood_model_5 <- -(N/2)*(log(2*pi))-(N/2)*(log(var_model_5))-(1/(2*var_model_5))*RSS_model_5
print(likelihood_model_5)

#Task 2.4 Calculating AIC and BIC
#Calculating AIC and BIC in Model 1
AIC_1 <- 2* length(model_1[1,]) - 2 * likelihood_model_1
print(AIC_1)
BIC_1 <- length(model_1[1,]) * log(N) - 2 * likelihood_model_1
print(BIC_1)

#Calculating AIC and BIC in Model 2
AIC_2 <- 2* length(model_2[1,]) - 2 * likelihood_model_2
print(AIC_2)
BIC_2 <- length(model_2[1,]) * log(N) - 2 * likelihood_model_2
print(BIC_2)

#Calculating AIC and BIC in Model 3
AIC_3 <- 2* length(model_3[1,]) - 2 * likelihood_model_3
print(AIC_3)
BIC_3 <- length(model_3[1,]) * log(N) - 2 * likelihood_model_3
print(BIC_3)

#Calculating AIC and BIC in Model 4
AIC_4 <- 2* length(model_4[1,]) - 2 * likelihood_model_4
print(AIC_4)
BIC_4 <- length(model_4[1,]) * log(N) - 2 * likelihood_model_4
print(BIC_4)

#Calculating AIC and BIC in Model 5
AIC_5 <- 2* length(model_5[1,]) - 2 * likelihood_model_5
print(AIC_5)
BIC_5 <- length(model_5[1,]) * log(N) - 2 * likelihood_model_5
print(BIC_5)

#Task 2.5 
#Error Distribution 
model_1_error <- Data$x2 - Y_hat_model_1
model_2_error <- Data$x2 - Y_hat_model_2
model_3_error <- Data$x2 - Y_hat_model_3
model_4_error <- Data$x2 - Y_hat_model_4
model_5_error <- Data$x2 - Y_hat_model_5


#QQ Plot
qqnorm(t(model_1_error),col = "skyblue", main = "Q-Q Plot for Model 1" )
qqline(model_1_error, col = "black", lwd = 1,lty = 2)

qqnorm(t(model_2_error),col = "blue", main = "Q-Q Plot for Model 2" )
qqline(model_2_error, col = "black", lwd = 1,lty = 2)

qqnorm(t(model_3_error),col = "yellow", main = "Q-Q Plot for Model 3" )
qqline(model_3_error, col = "black", lwd = 1,lty = 2)


qqnorm(t(model_4_error),col = "red", main = "Q-Q Plot for Model 4" )
qqline(model_4_error, col = "black", lwd = 1,lty = 2)

qqnorm(t(model_5_error),col = "lightgreen", main = "Q-Q Plot for Model 5" )
qqline(model_5_error, col = "black", lwd = 1,lty = 2)


#Task 2.7 #best model 2
#Splitting input signals x
set.seed(123)  
sample_index <- sample(1:nrow(Data), size = 0.7 * nrow(Data))

# Create training and testing sets
training_data <- Data[sample_index, ]
testing_data <- Data[-sample_index, ]

# Separate input and output signals
X_train <- training_data[, c("x1", "x3", "x4", "x5")]
Y_train <- training_data[, c("x2")]
X_test <- testing_data[, c("x1", "x3", "x4", "x5")]
Y_test <- testing_data[, c("x2")]

#Verifying split
cat("Training set size: ", nrow(training_data), "\n")
cat("Testing set size: ", nrow(testing_data), "\n")
cat("Testing set size: ", nrow(Y_train), "\n")

training_constant <- matrix(1, length(X_test$x1),1) 
training_model_x <- cbind(training_constant,(X_test[,"x4"]),(X_test[,"x3"])^2,(X_test[,"x5"]))
training_model_X <- as.matrix(training_model_x)

#making matrix 
Y_test <- as.matrix(Y_test)
X_test <- as.matrix(X_test)
X_train <- as.matrix(X_train)

training_thetahat <-solve(t(training_model_X) %*% training_model_X) %*% t(training_model_X) %*% Y_test
training_thetahat <- as.matrix(training_thetahat)


y_testing_hat <- X_test %*% training_thetahat
y_training_hat <- X_train %*% training_thetahat

RSS_testing <- sum((Y_test-y_testing_hat)^2) 
print(RSS_testing)


#2.7.3
t.test(y_training_hat, y_testing_hat, mu=200, alternative="two.sided", conf.level=0.95)

C_I1 <- -0.05758957

C_I2 <- 0.78702051
  
Y_train <- data.frame(Y_train = rnorm(140))  
Y_test <- data.frame(Y_test = rnorm(140))  
  
#Quantiles
C_I1 <- quantile(Y_train$Y_train, 0.05758957)
C_I2 <- quantile(Y_train$Y_train, 0.78702051)
  
# Plot
ggplot(data = Y_train, aes(x = Y_train)) +
  geom_density(col = "black", fill = "grey", linewidth = 1) +
  geom_vline(xintercept = C_I1, col = "black") +
  geom_vline(xintercept = C_I2, col = "black") +
  labs(
    title = "Distribution of Training Y Data with 95% Confidence Intervals",
    x = "Y Training Data",
    y = "Density"
    ) +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()  
  )

# Plot
ggplot(data = Y_test, aes(x = Y_test)) +
  geom_density(col = "black", fill = "grey", linewidth = 1) +
  geom_vline(xintercept = C_I1, col = "black") +
  geom_vline(xintercept = C_I2, col = "black") +
  labs(
    title = "Distribution of Testing Y Data with 95% Confidence Intervals",
    x = "Y Testing Data",
    y = "Density"
  ) +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()   
  )


#Task 3: Approximate Bayesian Computation (ABC)
numbers <- c(theta_hat_2)
sorted_numbers <- sort(abs(numbers), decreasing=TRUE)
largest_two_values <- sorted_numbers[1:2]
print(largest_two_values)

theta_hat_2

#Largest parameter
theta_one <- largest_two_values[1] 
theta_three <- largest_two_values[2]

#Constant Parameter 
theta_two <- 0.03927117
theta_bias <- -0.05929464

theta_one
print(theta_two)
print(Data)

# Initial values

arr_1 = 0
arr_2=0
f_value=0
s_value=0

# Calculating epsilon 

epsilon <- RSS_model_2 * 2
num <- 100 
print(num)
counter <- 0

for (i in 1:num) {
  p1 <- runif(1,-abs(theta_one),abs(theta_one)) 
  p2 <- runif(1,-abs(theta_three),abs(theta_three))
  ABC_thetahat <- matrix(c(p1,p2,theta_two,theta_bias)) 
  ABC_y_Hat <- as.matrix(model_2) %*% ABC_thetahat 
  ABC_RSS <- sum((x2 - ABC_y_Hat)^2) 
  
  if (ABC_RSS > epsilon){
    arr_1[i] <- p1 
    arr_2[i] <- p2 
    counter = counter+1
    f_value <- matrix(arr_1)
    s_value <- matrix(arr_2)}
}
    
 print(f_value)
 
# Plotting the joint and marginal posterior distribution
    
# Create a data frame with the values of f_value and s_value and a column for "group"
ABC_result <- data.frame(f_value, s_value, legend=rep(c("f_value","s_value"), each=length(f_value)/2))
    
# Plot the scatter plot using and hiding legends

ggplot(ABC_result, aes(x = f_value, y = s_value, color = legend)) +
  geom_point() +
  theme(
    legend.position = "bottom",       
    legend.title = element_blank(),  
    panel.background = element_rect(fill = "grey"),  
    )
  ggtitle("Joint and Marginal Posterior Distribution") +
  scale_color_manual(values = c("lightblue", "yellow"))  



