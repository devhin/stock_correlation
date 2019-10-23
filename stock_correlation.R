setwd("~/Projets_R/10_stock/code")
library(readxl)
library(MLmetrics)
library(data.table)

# read data
# data_source <- as.data.frame(read_excel("data_test.xlsx", sheet = 1))
data_source <- cbind(1:nrow(data_in), data_in)

# set the parameters
# resolution (days)
resolution <- 5
# number of clusters (#)
nb_clusters <- 10
# maximum shift to be explored (# of resolution)
max_shift <- floor(100/resolution)

# define smoothed data to work on
n <- floor(nrow(data_source)/resolution)
p <- ncol(data_source) - 1
stock_smooth <- data.frame(matrix(0, n, p), stringsAsFactors=F)
for (i in 1:n) {
  stock_smooth[i,] <- colSums(data_source[((i-1)*resolution+1):(i*resolution),2:(p+1)])/resolution
}
head(stock_smooth)

variation <- data.frame(matrix(0, n-1, p), stringsAsFactors=F)
for (i in 1:(n-1)) {
  variation[i,] <- ((stock_smooth[i+1,]-stock_smooth[i,])/(resolution*stock_smooth[i,]))
}

# # plot the variations
# plot(1:(n-1),variation[,1],type="l",col="red")
# lines(1:(n-1),variation[,2],col="green")
# lines(1:(n-1),variation[,3],col="blue")
# lines(1:(n-1),variation[,4],col="yellow")
# lines(1:(n-1),variation[,5],col="black")
# lines(1:(n-1),variation[,6],col="purple")

# launch clustering
cl <- kmeans(t(variation), nb_clusters)
# display clusters
cl$cluster

### TO-DO 20191016 ###
# moyenne par cluster
# +- : y1, y2, mse, Topt

data_out <- data.frame(matrix(0, p, 7), stringsAsFactors=F)
colnames(data_out) <- c("value"
                        ,"err_neg"
                        ,"idx_neg"
                        ,"tau_neg"
                        ,"err_pos"
                        ,"idx_pos"
                        ,"tau_pos")

for (i in 1:p) {
  err_pos <- 10000
  idx_pos <- 0
  tau_pos <- n
  err_neg <- 10000
  idx_neg <- 0
  tau_neg <- n
  
  for (tau in -max_shift:max_shift) {
    a <- variation[,i][(1+max(0,tau)):((n-1)+min(0,tau))]
    
    for (j in 1:p) {
      if (tau<0) {b <- shift(variation[,j],tau)[1:((n-1)+tau)]}
      if (tau>=0) {b <- shift(variation[,j],tau)[(1+tau):(n-1)]}
      
      if(err_neg>MSE(a,-b) & i!=j) {
        err_neg <- MSE(a,-b)
        idx_neg <- j
        tau_neg <- tau
        }
      if(err_pos>MSE(a,b) & i!=j) {
        err_pos <- MSE(a,b)
        idx_pos <- j
        tau_pos <- tau
      }
    }
  }
  
  data_out$value[i] <- i
  data_out$err_neg[i] <- err_neg
  data_out$idx_neg[i] <- idx_neg
  data_out$tau_neg[i] <- tau_neg
  data_out$err_pos[i] <- err_pos
  data_out$idx_pos[i] <- idx_pos
  data_out$tau_pos[i] <- tau_pos
}

data_out
subset(data_out, err_neg == min(data_out$err_neg))
# # ensure nb_clusters is an acceptable value
# ratio_ss <- data.frame(cluster = seq(from = 1, to = 5, by = 1)) 
# for (k in 1:5) {
#   km_model <- kmeans(t(variation), k, nstart = 20)
#   ratio_ss$ratio[k] <- km_model$tot.withinss / km_model$totss
# }
# ggplot(ratio_ss, aes(cluster, ratio)) + 
#   geom_line() +
#   geom_point()

i=42
j=81
tau=6
a <- variation[,i][(1+max(0,tau)):((n-1)+min(0,tau))]
if (tau<0) {b <- shift(variation[,j],tau)[1:((n-1)+tau)]}
if (tau>=0) {b <- shift(variation[,j],tau)[(1+tau):(n-1)]}

plot(1:(n-1-abs(tau)),a,type="l",col="red")
lines(1:(n-1-abs(tau)),b,col="green")

min(data_out$err_neg)
