source("source_data.r")
source('computeCost.r')
library(plotly)

# read in data from ex1data1.txt
dat1 <- read.delim(paste(src_dat,'ex1data1.txt',sep='/'),header=F,sep=',',
                   col.names=c('population','profit'))
plot(dat1$population,dat1$profit)

# convert data from to matrix
dat1.matrix <- as.matrix(dat1)
dat2.matrix<- cbind(rep(1,nrow(dat1)),dat1.matrix[,1:ncol(dat1.matrix)-1])

# learning rate
alpha <- 0.01

# theta - initial value
#theta <- rep(0,ncol(dat1)+1)
theta <- matrix(c(0,0),nrow=2)
y <- dat1.matrix[,ncol(dat1.matrix)]

#############################################
# gradient descent function
#############################################
output <- double()
grad_des <- function(init_theta, maxinter,convthre) {
  theta <- init_theta
  thetam <- theta
  i <- 1
  while (i <= maxinter) {
    H <- dat2.matrix %*% theta
    result <- cost(H, y, theta)
    print(result)
    output <- c(output,result)
    if (i >=2) {
       if (output[[i-1]] - output[[i]] <= convthre) {
          break
       } else {
         theta <- theta - t(alpha * crossprod(H-y,dat2.matrix)/length(y))
         i <- i + 1
       }
    } else if (i == 1) {
      theta <- theta - t(alpha * crossprod(H-y,dat2.matrix)/length(y))
      i <- i + 1
    }
    thetam <- cbind(thetam,theta)
   if (i > maxinter) {
      print('Max number of interation reached: not converge')
      break
    } 
  }
  return(list(thetam,output))
}

# calculate the theta where the cost function achieve the minimum
costval <- grad_des(init_theta = theta,maxinter = 500,convthre=0.01)
costval

# surface plot
library(plotly)

axis1 <- seq(-3,3,by=0.1)
axis2 <- seq(-3,3,by=0.1)

sur_cost <- double()
for (theta0 in axis1) {
  for (theta1 in axis2) {
       theta <- matrix(c(theta0,theta1),nrow=2)
       H <- dat2.matrix %*% theta
       result <- cost(H, y, theta)
       sur_cost <- c(sur_cost,result)
  }
}


# df <- data.frame(x = rep(axis1,each=51),
#                 y = rep(axis2,201),
#                 z = sur_cost)
# 
# plot_ly (data=df, x = df$x, y = df$y, z = df$z) %>% add_trace(type="mesh3d" )


z <- matrix(sur_cost,nrow=61)
plot_ly() %>% add_surface(x = ~axis1, y = ~axis2, z = ~z) %>%
layout(
  title = "Cost in a 3d surface plot",
  scene = list(
    xaxis = list(title = "Theta0"),
    yaxis = list(title = "Theta1"),
    zaxis = list(title = "Cost")
  ))
