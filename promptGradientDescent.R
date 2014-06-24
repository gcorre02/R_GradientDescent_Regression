prompt = readline("Please input the name of the company you want to evaluate data for  \n")
inputIndicator = (Quandl.search(query = prompt, silent = F))[[as.integer(readline("which result ?"))]]$code
# The quantmod package is ready for use:
library(Quandl)
library(quantmod)
# Load the Facebook data with the help of Quandl
inpt = Quandl(inputIndicator,type="xts", start_date = readline("Start date as yyyy-mm-dd"), end_date = readline("End date as yyyy-mm-dd"))
print(head(inpt))
# Plot the chart with the help of candleChart()

candleChart(inpt)
# ------------------check


# ------------------other stuff

#prices = inpt[,"Close"] // depends on the position of the prices:, close is more interesting if it exists

if("Close"%in%colnames(inpt)){
	prices = inpt[,"Close"]
} else {
	prices = inpt[,1]	
}

print(head(prices))
y = c(matrix(prices)[,1])
print(head(y))
print(class(y))

# these should be lenghts
x = 1.0:length(y)

print( head(x))
print(class(x))
length(y)

df = data.frame(x,y)
head(df)

res <- lm( y ~ x )

print(res)


plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')
abline(res, col='blue')

# ------------- imported and adapted from http://www.r-bloggers.com/linear-regression-by-gradient-descent/ ----------------

# squared error cost function
cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}

# prompt for optimization : learning rate and iteration limit
print(length(x))
print(length(y))

alpha = as.numeric(readline( "0.0000001 alpha \n" ))
str(alpha)
print(alpha)

num_iters = as.integer(readline( "300 iterations \n"))
str(num_iters)
print(num_iters)

# keep history
cost_history <- double(num_iters)
theta_history <- list(num_iters)

# initialize coefficients
theta <- matrix(c(0,0), nrow=2)

# add a column of 1's for the intercept coefficient
X <- cbind(1, matrix(x))


# gradient descent
for (i in 1:num_iters) {
  error <- (X %*% theta - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X, y, theta)
  theta_history[[i]] <- theta
 
 #debug
 if(F){
 	print(i)
  	print("error")
  	print(head(error))
  	print("delta")
  	print(head(delta))
  	print("theta")
  	print(head(theta))
  	print("cost history")
  	print(head(cost_history))
  	print("theta history")
  	print(head(theta_history))
  	print("end of")
  }	
 #\debug 
}
print(theta)


# plot data and converging fit
plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')
for (i in c(1,3,6,10,14,seq(20,num_iters,by=10))) {
  abline(coef=theta_history[[i]], col=rgb(0.8,0,0,0.3))
}
abline(coef=theta, col='blue')


plot(cost_history, type='line', col='blue', lwd=2, main='Cost function', ylab='cost', xlab='Iterations')

