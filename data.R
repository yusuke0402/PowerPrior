means=read.csv("domain_means.csv")
covariances=read.csv("domain_covariances.csv")
numbers=read.csv("domain_numbers.csv")
library("mvtnorm")

truefunction=function(X,t,epsilon,wi){
  X[,1]+X[,2]+X[,3]+1.0*t+epsilon
}
createdata=function(self.pattern_id){
  
  #currentdatacreate
  current_number=with(numbers,current[pattern_id==self.pattern_id])
  current_vcvmatrix=diag(3)*with(covariances,current[pattern_id==self.pattern_id])
  current_mean=c(with(means,dim1[pattern_id==self.pattern_id&domain=="current"]),with(means,dim2[pattern_id==self.pattern_id&domain=="current"]),with(means,dim3[pattern_id==self.pattern_id&domain=="current"]))
  currentx=rmvnorm(n=current_number,mean =current_mean,current_vcvmatrix)
  epsilon=rnorm(n=current_number,mean = 0,sd=1)
  current_y=truefunction(currentx,rep(1,current_number),epsilon,wi)
  current_x=cbind(1,currentx[,1],currentx[,2],currentx[,3])
  
  #historical1datacreate
  historical_1_number=with(numbers,historical_1[pattern_id==self.pattern_id])
  historical_1_vcvmatrix=diag(3)*with(covariances,historical[pattern_id==self.pattern_id])
  historical_1_mean=c(with(means,dim1[pattern_id==self.pattern_id&domain=="historical"]),with(means,dim2[pattern_id==self.pattern_id&domain=="historical"]),with(means,dim3[pattern_id==self.pattern_id&domain=="historical"]))
  historical_1_x=rmvnorm(n=historical_1_number,mean = historical_1_mean,historical_1_vcvmatrix)
  epsilon=rnorm(n=historical_1_number,mean = 0,sd=1)
  historical_1_y=truefunction(historical_1_x,rep(0,historical_1_number),epsilon,wi)
  #historical_1_x=cbind(0,historical1x[,1],historical1x[,2],historical1x[,3])
  
  list(current_x=current_x,current_y=current_y,historical_1_x=historical_1_x,historical_1_y=historical_1_y)
}