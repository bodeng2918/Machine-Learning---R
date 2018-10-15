# read in data from ex1data1.txt
dat1 <- read.delim("D:/Work/Roche/machine learning/machine-learning-ex1/ex1/ex1data1.txt",header=F,sep=',',
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


# cost function
output <- double()
cost <- function(init_theta, maxinter,convthre) {
  theta <- init_theta
  thetam <- theta
  i <- 1
  while (i <= maxinter) {
    H <- dat2.matrix %*% theta
    result <- crossprod(H - y)/(2*nrow(dat1.matrix))
    print(result)
    output <- c(output,result)
    if (i >=2) {
       if (output[[i-1]] - output[[i]] <= convthre) {
          break
       } else {
         theta <- theta - t(alpha * crossprod(H-y,dat2.matrix)/nrow(dat1.matrix))
         i <- i + 1
       }
    } else if (i == 1) {
      theta <- theta - t(alpha * crossprod(H-y,dat2.matrix)/nrow(dat1.matrix))
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

cost <- cost(init_theta = theta,maxinter = 500,convthre=0.01)



