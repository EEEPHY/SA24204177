## -----------------------------------------------------------------------------
x<-seq(0,2*pi,length=300)
plot(x,sin(x),type = 'l',col="green",
     main = "Sine function and cosine function image",
     ylab = "value of function",axes = FALSE)#正弦
lines(x,cos(x),col="yellow")#余弦
legend(0,-0.5,col =c("green","yellow"),
       lty = c(1,1),lwd=c(2,2),legend = c("sin","cos"))#图例
axis(2)#纵坐标
axis(1,at=(0:4)/2*pi,labels =
       c(0,expression(pi/2),expression(pi),
         expression(3*pi/2),expression(2*pi)))#横坐标

## -----------------------------------------------------------------------------
set.seed(1234)
d1<-floor(runif(10,70,100))
d2<-floor(runif(10,70,100))
d3<-floor(runif(10,70,100))
score<-data.frame(1:10,d1,d2,d3,d1+d2+d3)
colnames(score)<-c("学号","语文","数学","英语","总分")
score

## -----------------------------------------------------------------------------
library(ggplot2)
ggplot(data=mpg,aes(x=displ,y=hwy,color=class))+geom_point()

## -----------------------------------------------------------------------------
RL<-function(sigma){#Rayleigh分布的产生函数
  n<-1000
  u<-runif(n)
  x<-sqrt(-2*sigma^2*log(1-u))
  return(x)
}

## -----------------------------------------------------------------------------
sig<-1
hist(RL(sig),prob=TRUE,xlab = "x",main = "Rayleigh(1)",ylim=c(0,0.7))
y<-seq(0,10,0.01)
lines(y,(y/sig^2)*exp(-(y^2)/(2*sig^2)))

## -----------------------------------------------------------------------------
sig<-2
hist(RL(sig),prob=TRUE,xlab = "x",main = "Rayleigh(2)")
y<-seq(0,10,0.01)
lines(y,(y/sig^2)*exp(-(y^2)/(2*sig^2)))

## -----------------------------------------------------------------------------
sig<-4
hist(RL(sig),prob=TRUE,xlab = "x",main = "Rayleigh(4)",ylim = c(0,0.18))
y<-seq(0,15,0.01)
lines(y,(y/sig^2)*exp(-(y^2)/(2*sig^2)))

## -----------------------------------------------------------------------------
set.seed(1234)
N<-1000
p1<-0.75
x1<-rnorm(N,0,1)
x2<-rnorm(N,3,1)
M<-matrix(0,N,2)
for (i in 1:N) {
  M[i,sample(x=c(1,2),size = 1,prob = c(p1,1-p1))]<-1
}
x0<-x1*M[,1]+x2*M[,2]#产生混合正态分布随机数
hist(x0,prob=TRUE,ylim =c(0,0.3),main = "p1=0.75")
y<-seq(-4,6,0.01)
lines(y,p1/sqrt(2*pi)*exp(-y^2/2)+(1-p1)/sqrt(1*pi)*exp(-(y-3)^2/2))

## -----------------------------------------------------------------------------
set.seed(1234)
N<-1000
p1<-0.5
x1<-rnorm(N,0,1)
x2<-rnorm(N,3,1)
M<-matrix(0,N,2)
for (i in 1:N) {
  M[i,sample(x=c(1,2),size = 1,prob = c(p1,1-p1))]<-1
}
x0<-x1*M[,1]+x2*M[,2]#产生混合正态分布随机数
hist(x0,prob=TRUE,ylim =c(0,0.3),main = "p1=0.5")
y<-seq(-4,6,0.01)
lines(y,p1/sqrt(2*pi)*exp(-y^2/2)+(1-p1)/sqrt(1*pi)*exp(-(y-3)^2/2))

## -----------------------------------------------------------------------------
set.seed(1234)
N<-1000
p1<-0.25
x1<-rnorm(N,0,1)
x2<-rnorm(N,3,1)
M<-matrix(0,N,2)
for (i in 1:N) {
  M[i,sample(x=c(1,2),size = 1,prob = c(p1,1-p1))]<-1
}
x0<-x1*M[,1]+x2*M[,2]#产生混合正态分布随机数
hist(x0,prob=TRUE,ylim =c(0,0.5),main = "p1=0.25")
y<-seq(-4,6,0.01)
lines(y,p1/sqrt(2*pi)*exp(-y^2/2)+(1-p1)/sqrt(1*pi)*exp(-(y-3)^2/2))

## -----------------------------------------------------------------------------
set.seed(1234)
t<-10
lambda<-10
alpha<-0.5
beta<-2
m<-1000#取1000次重复实验
X_t<-rep(0,m)
Y_1<-rep(0,m)
for(i in 1:m){
  N<-rpois(t,lambda*t)
Y<-rgamma(N[t],shape = 0.5,rate = 2)
X_t[i]<-sum(Y)
Y_1[i]<-Y[1]
}
mean(X_t)#E[X(t)]
lambda*t*mean(Y_1)
var(X_t)#Var(X(t))
lambda*t*mean(Y_1^2)

## -----------------------------------------------------------------------------
set.seed(1234)
t<-10
lambda<-12
alpha<-2
beta<-0.3
m<-1000#取1000次重复实验
X_t<-rep(0,m)
Y_1<-rep(0,m)
for(i in 1:m){
  N<-rpois(t,lambda*t)
Y<-rgamma(N[t],shape = 0.5,rate = 2)
X_t[i]<-sum(Y)
Y_1[i]<-Y[1]
}
mean(X_t)#E[X(t)]
lambda*t*mean(Y_1)
var(X_t)#Var(X(t))
lambda*t*mean(Y_1^2)

## -----------------------------------------------------------------------------
set.seed(1234)
t<-10
lambda<-5
alpha<-0.8
beta<-5
m<-1000#取1000次重复实验
X_t<-rep(0,m)
Y_1<-rep(0,m)
for(i in 1:m){
  N<-rpois(t,lambda*t)
Y<-rgamma(N[t],shape = 0.5,rate = 2)
X_t[i]<-sum(Y)
Y_1[i]<-Y[1]
}
mean(X_t)#E[X(t)]
lambda*t*mean(Y_1)
var(X_t)#Var(X(t))
lambda*t*mean(Y_1^2)

## -----------------------------------------------------------------------------
set.seed(1234)
t<-10
lambda<-15
alpha<-5
beta<-8
m<-1000#取1000次重复实验
X_t<-rep(0,m)
Y_1<-rep(0,m)
for(i in 1:m){
  N<-rpois(t,lambda*t)
Y<-rgamma(N[t],shape = 0.5,rate = 2)
X_t[i]<-sum(Y)
Y_1[i]<-Y[1]
}
mean(X_t)#E[X(t)]
lambda*t*mean(Y_1)
var(X_t)#Var(X(t))
lambda*t*mean(Y_1^2)

## -----------------------------------------------------------------------------
x<-seq(0.1,0.9,0.1)
m<-10000
u<-runif(m)#Y~U(0,1)
cdf<-numeric(length(x))
for (i in 1:length(x)) {
  g<-30*x[i]^3*u^2*(1-x[i]*u)^2
  cdf[i]<-mean(g)
}
Phi<-pbeta(x,3,3)
print(round(rbind(x,cdf,Phi),3))

## -----------------------------------------------------------------------------
RL_sam<-function(sigma,R=1000,antithetic=TRUE){#服从Rayleigh分布的样本产生函数
  u<-runif(R/2)
  if(!antithetic){
    v<-runif(R/2)
    }
  else{
    v<-1-u
  }
  u<-c(u,v)
  x<-sqrt(-2*sigma^2*log(1-u))
  return(mean(x))
}
RL_sam(2,R=1000)
RL_sam(5)
RL_sam(0.2)

## -----------------------------------------------------------------------------
set.seed(1234)
m<-1000
MC1<-MC2<-numeric(m)
sig<-2
for (i in 1:m) {
  MC1[i]<-RL_sam(sigma = sig,R=1000,antithetic = FALSE)#普通独立抽样
  MC2[i]<-RL_sam(sigma = sig,R=1000)#对偶
}
print(sd(MC1))
print(sd(MC2))
round((var(MC1)-var(MC2))/var(MC1),4)


## -----------------------------------------------------------------------------
set.seed(1234)
m<-1000
MC1<-MC2<-numeric(m)
sig<-0.5
for (i in 1:m) {
  MC1[i]<-RL_sam(sigma = sig,R=1000,antithetic = FALSE)#普通独立抽样
  MC2[i]<-RL_sam(sigma = sig,R=1000)#对偶
}

print(sd(MC1))
print(sd(MC2))
round((var(MC1)-var(MC2))/var(MC1),4)

## -----------------------------------------------------------------------------
set.seed(1234)
m<-10000
theta.hat<-se<-numeric(2)
g<-function(x){
  x^2/sqrt(2*pi)*exp(-x^2/2)*(x>1)
}
x<-rnorm(m)#令f1为重要函数
fg<-g(x)/dnorm(x)
theta.hat[1]<-mean(fg)
se[1]<-sd(fg)

u<-runif(m)#令f2为重要函数，并使用逆变换法
x<-sqrt(-2*log(1-u))
fg<-g(x)/(x*exp(-x^2/2))
theta.hat[2]<-mean(fg)
se[2]<-sd(fg)

rbind(theta.hat,se)

## -----------------------------------------------------------------------------

quick_sort<-function(x){
  num<-length(x)
  if(num==0||num==1){return(x)
  }else{
    a<-x[1]
    y<-x[-1]
    lower<-y[y<a]
    upper<-y[y>=a]
    return(c(quick_sort(lower),a,quick_sort(upper)))}
}

time_com<-function(N){
  a<-numeric(100)
for (i in 1:100) {
  test<-sample(1:N)
a[i]<-system.time(quick_sort(test))[1]
}
return(mean(a))
}

t_fun<-function(n){
  return(n*log(n))
}

## -----------------------------------------------------------------------------
set.seed(1234)
an<-c(time_com(1e4),time_com(2e4),time_com(4e4),time_com(6e4),time_com(8e4))
tn<-c(t_fun(1e4),t_fun(2e4),t_fun(4e4),t_fun(6e4),t_fun(8e4))

## -----------------------------------------------------------------------------
plot(tn,an)
abline(lm(an~tn)$coef[1],lm(an~tn)$coef[2])

## -----------------------------------------------------------------------------
set.seed(1234)
m<-1000
sk<-numeric(m)
for (i in 1:m) {
  x<-rnorm(1000)
  sk[i]<-mean(((x-mean(x))/sd(x))^3)
}

p<-c(0.025,0.05,0.95,0.975)
q_mc<-quantile(sk,p)
sd_sk<-numeric(4)
for(i in 1:4){
  sd_sk[i]<-sqrt(p[i]*(1-p[i])/(m*dnorm(q_mc[i],0,sqrt(6/m))^2))
}
q_mc#蒙特卡洛估计分位数
qnorm(p,0,sqrt(6/m))#大样本近似分位数
sd_sk#标准差

## -----------------------------------------------------------------------------
library(MASS)
set.seed(123)
n<-100
mu<-c(0,0) 
sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
data <- mvrnorm(n, mu, sigma)#产生(X,Y)

pearson_test <- cor.test(data[,1],data[,2],method = "pearson")#Pearson相关系数检验
pearson_test$p.value

spearman_test <- cor.test(data[,1],data[,2],method = "spearman")#Spearman秩相关系数检验
spearman_test$p.value

kendall_test <- cor.test(data[,1],data[,2],method = "kendall")#Kendall系数检验
kendall_test$p.value

## -----------------------------------------------------------------------------
set.seed(123)
n<-100
X0<-rnorm(n)
Y0<-2*X0^2+rnorm(n)

pearson_test <- cor.test(X0,Y0,method = "pearson")#Pearson相关系数检验
pearson_test$p.value

spearman_test <- cor.test(X0,Y0,method = "spearman")#Spearman秩相关系数检验
spearman_test$p.value

kendall_test <- cor.test(X0,Y0,method = "kendall")#Kendall系数检验
kendall_test$p.value

## -----------------------------------------------------------------------------
1-pnorm(3.09)

## -----------------------------------------------------------------------------
set.seed(1234)
 d1 <- c(-2.961, 0.478, -0.391, -0.869, -0.460, 
            -0.937, 0.779, -1.409, 0.027, -1.569)
d2  <- c(1.608, 1.009,  0.878,  1.600, -0.263,  
             0.680, 2.280,  2.390, 1.793,  8.091, 1.468)
mean_d1 <- mean(d1)  
mean_d2 <- mean(d2)  
  
# 计算均值差异  
mean_diff <- mean_d2 - mean_d1  

# 计算样本标准误差  
n1 <- length(d1)  
n2 <- length(d2)  
se_diff <- sqrt((sd(d1)^2 / n1) + (sd(d2)^2 / n2))  
 
R <- 10000  
  
bootstrap_diffs <- numeric(R)  
  
# 生成自助样本并计算均值差异  
for (i in 1:R) {  
  d1_bootstrap <- sample(d1, replace = TRUE)  
  d2_bootstrap <- sample(d2, replace = TRUE)  
  mean_d1_bootstrap <- mean(d1_bootstrap)  
  mean_d2_bootstrap <- mean(d2_bootstrap)  
  bootstrap_diffs[i] <- mean_d2_bootstrap - mean_d1_bootstrap  
}  
  
# 计算自助标准误差  
bootstrap_se_diff <- sd(bootstrap_diffs)  
  
round(c(bias=mean(bootstrap_diffs)-mean_diff,se.boot=bootstrap_se_diff,se.samp=se_diff),3)

## -----------------------------------------------------------------------------
set.seed(1234)
N <- 1000  
n1 <- 950  #null 
n2 <- 50   #alter
alpha <- 0.1  
m <- 10000   
  
fwer_bf <- numeric(m)  
fdr_bf <- numeric(m)  
tpr_bf <- numeric(m)  
  
fwer_bh <- numeric(m)  
fdr_bh <- numeric(m)  
tpr_bh <- numeric(m)  
  
for (i in 1:m) {  
  #生成p值  
  p_values <- c(runif(n1), rbeta(n2, 0.1, 1))  
    
  #调整p值  
  bf_adjusted_p_values <- rbind(p.adjust(p_values, method = "bonferroni"))  
  bh_adjusted_p_values <- rbind(p.adjust(p_values, method = "BH"))  
    
  #拒绝  
  bf_rejected <- bf_adjusted_p_values < alpha  
  bh_rejected <- bh_adjusted_p_values < alpha  
    
  # 计算FWER, FDR, TPR  
  fwer_bf[i] <- any(bf_rejected[1:n1])  
  fdr_bf[i] <- sum(bf_rejected[1:n1]) / sum(bf_rejected)  
  tpr_bf[i] <- sum(bf_rejected[(n1+1):N]) / n2  
  fwer_bh[i] <- any(bh_rejected[1:n1])  
  fdr_bh[i] <- sum(bh_rejected[1:n1]) / sum(bh_rejected)  
  tpr_bh[i] <- sum(bh_rejected[(n1+1):N]) / n2  
}  
  
fwer_bf_mean <- mean(fwer_bf)  
fdr_bf_mean <- mean(fdr_bf)  
tpr_bf_mean <- mean(tpr_bf)  
fwer_bh_mean <- mean(fwer_bh)  
fdr_bh_mean <- mean(fdr_bh)  
tpr_bh_mean <- mean(tpr_bh)  
 
results <- rbind(  
  c(fwer_bf_mean, fwer_bh_mean),  
  c(fdr_bf_mean, fdr_bh_mean),  
  c(tpr_bf_mean, tpr_bh_mean)  
)  
colnames(results) <- c("Bonferroni", "BH")  
rownames(results) <- c("FWER", "FDR", "TPR")  
  
print(results)  
  

## -----------------------------------------------------------------------------
library(boot)
set.seed(1234)
data<-as.vector(aircondit$hours)
#极大似然函数 
mle_lambda <- function(data, indices) { 
  n <- length(data[indices])  
  return(n / sum(data[indices]))  
}  
obj<-boot(data,statistic = mle_lambda,R=1e4)
obj

## -----------------------------------------------------------------------------
set.seed(1234)
m<-1e3
ci.norm<-ci.basic<-ci.perc<-ci.bca<-matrix(NA,m,2)
for (i in 1:m) {
  de<-boot(data,statistic = mle_lambda,R=999)
  ci<-boot.ci(de,type = c("norm","basic","perc","bca"))
  ci.norm[i,]<-ci$norm[2:3]
  ci.basic[i,]<-ci$basic[4:5]
  ci.perc[i,]<-ci$percent[4:5]
  ci.bca[i,]<-ci$bca[4:5]
}
cat("normal:", mean(ci.norm[,1]),mean(ci.norm[,2]), "\n")  
cat("basic:", mean(ci.basic[,1]),mean(ci.basic[,2]), "\n")  
cat("percentile:", mean(ci.perc[,1]),mean(ci.perc[,2]), "\n")  
cat("bca:", mean(ci.bca[,1]),mean(ci.bca[,2]), "\n")

## -----------------------------------------------------------------------------
library(bootstrap)
df<-scor
sort_eigen<- sort(eigen(cov(df))$values, decreasing = TRUE)  
theta_hat <- sort_eigen[1] / sum(sort_eigen)
n <- nrow(df)  
jackknife_estimates <- numeric(n)  
for (i in 1:n) {  
  leave_one_out <- df[-i, ] 
  sample_cov_matrix <- cov(leave_one_out)  
  eigenvalues <- eigen(sample_cov_matrix)$values  
  sorted_eigenvalues <- sort(eigenvalues, decreasing = TRUE)  
  jackknife_estimates[i] <- sorted_eigenvalues[1] / sum(sorted_eigenvalues)  
}  
bias.jack<-(n-1)*(mean(jackknife_estimates)-theta_hat)
se.jack<-sqrt((n-1)*mean((jackknife_estimates-theta_hat)^2))
round(c(original=theta_hat,bias.jack=bias.jack,se.jack=se.jack),3)

## -----------------------------------------------------------------------------
set.seed(1234)
library(DAAG)
attach(ironslag)
n <- length(magnetic) #in DAAG ironslag
e1 <- e2 <- e3 <- e4 <- numeric(n)

for (k in 1:n) {
y <- magnetic[-k]
x <- chemical[-k]

J1 <- lm(y ~ x)
yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
e1[k] <- magnetic[k] - yhat1

J2 <- lm(y ~ x + I(x^2))
yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] +J2$coef[3] * chemical[k]^2
e2[k] <- magnetic[k] - yhat2

J3 <- lm(log(y) ~ x)
logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
yhat3 <- exp(logyhat3)
e3[k] <- magnetic[k] - yhat3

J4 <- lm(y ~ x + I(x^2) + I(x^3))  
yhat4 <- J4$coef[1] + J4$coef[2] * chemical[k] + J4$coef[3] * chemical[k]^2 + J4$coef[4] * chemical[k]^3  
e4[k] <- magnetic[k] - yhat4  
}

c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))

## -----------------------------------------------------------------------------
L1 <- lm(magnetic ~ chemical)
L2 <- lm(magnetic ~ chemical + I(chemical^2))
L3 <- lm(log(magnetic) ~ chemical)
L4 <- lm(magnetic ~ chemical + I(chemical^2)+ I(chemical^3))
c(summary(L1)$adj.r.squared,summary(L2)$adj.r.squared,summary(L3)$adj.r.squared,summary(L4)$adj.r.squared)

## -----------------------------------------------------------------------------
set.seed(1234)
attach(chickwts)
x <- sort(as.vector(weight[feed == "soybean"]))
y <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)

cvm_statistic <- function(x, y) {  
  n <- length(x)  
  m <- length(y)  
  Fn <- ecdf(x)  
  Gm <- ecdf(y)  
  cvm <- m*n/((m+n)^2)*(sum((Fn(x)-Gm(x))^2)+sum((Fn(y)-Gm(y))^2)) 
  return(cvm)  
}

R <- 999 #number of replicates
z <- c(x, y) #pooled sample
K <- 1:26
reps <- numeric(R) #storage for replicates
cvm0 <- cvm_statistic(x, y)
for (i in 1:R) {
#generate indices k for the first sample
k <- sample(K, size = 14, replace = FALSE)
x1 <- z[k]
y1 <- z[-k] #complement of x1
reps[i] <- cvm_statistic(x1, y1)
}
p<-mean(c(cvm0,reps)>=cvm0)
p

## -----------------------------------------------------------------------------
set.seed(123) 
n<-50
x <- rnorm(n)
y <- 0.5 * x + rnorm(n)# x服从标准正态分布，y与x有一定的线性关系
R <- 999
z <- c(x, y) #pooled sample
K <- 1:(2*n)
reps <- numeric(R) #storage for replicates
rho0 <- cor(x, y, method = "spearman")
for (i in 1:R) {
#generate indices k for the first sample
k <- sample(K, size = n, replace = FALSE)
x1 <- z[k]
y1 <- z[-k] #complement of x1
reps[i] <- cor(x1,y1, method = "spearman") 
}
p<-mean(abs(c(rho0,reps))>=abs(rho0))
cat("原始Spearman秩相关系数:", rho0, "\n")  
cat("置换检验得到的p值:", p, "\n")  
cat("cor.test函数得到的p值:",cor.test(x, y, method = "spearman")$p.value, "\n")

## -----------------------------------------------------------------------------
# 设置参数
set.seed(1234)  
theta <- 1  
eta <- 0     # Cauchy
n <- 5000    # 生成样本的总数量
burn_in <- 1000   # 丢弃的前1000个样本
samples <- numeric(n)
samples[1] <- rnorm(1)  # 初始化链的第一个样本

for (i in 2:n) {
  candidate <- samples[i - 1] + rnorm(1, mean = 0, sd = 1)
  alpha <- min(1, (dcauchy(candidate, location = eta, scale = theta)))
  if (runif(1) < alpha) {
    samples[i] <- candidate
  } else {
    samples[i] <- samples[i - 1]
  }# 接受或拒绝候选样本
}
samples <- samples[(burn_in + 1):n]#丢弃前1000个样本
gene_qt <- quantile(samples, probs = seq(0.1, 0.9, by = 0.1))#产生的样本分位数
theo_qt <- qcauchy(seq(0.1, 0.9, by = 0.1), location = eta, scale = theta)#理论分位数
result <- data.frame(Decile = seq(0.1, 0.9, by = 0.1),
                     Generated = gene_qt,
                     Theoretical = theo_qt)
print(result)
plot(seq(0.1, 0.9, by = 0.1), gene_qt, type = "b", pch = 19, col = "blue",
     xlab = "Decile", ylab = "Quantiles", main = "Comparison of Deciles")
points(seq(0.1, 0.9, by = 0.1), theo_qt, type = "b", pch = 19, col = "red")
legend("topright", legend = c("Generated", "Theoretical"), col = c("blue", "red"), pch = 19)

## -----------------------------------------------------------------------------
set.seed(1234)
a <- 2 
b <- 3 
n <- 10 
iterations <- 5000  # 迭代次数
burn_in <- 1000
x_samples <- numeric(iterations)
y_samples <- numeric(iterations)
y_samples[1] <- 0.5  # 初始化 y 值
for (i in 2:iterations) {
  x_samples[i] <- rbinom(1, size = n, prob = y_samples[i - 1])#根据当前y值获得x
  y_samples[i] <- rbeta(1, shape1 = x_samples[i] + a, shape2 = n - x_samples[i] + b)#根据当前x值获得y
}
x_samples <- x_samples[(burn_in + 1):iterations]
y_samples <- y_samples[(burn_in + 1):iterations]#丢弃

plot(x_samples, type = "l", col = "blue", lwd = 1.5,
     main = "Gibbs", xlab = "Index", ylab = "Random numbers",
     ylim = range(c(x_samples, y_samples))) 
lines(y_samples, type = "l", col = "red", lwd = 1.5) 
legend("topright", legend = c("x values", "y values"), col = c("blue", "red"), lwd = 1.5)

## -----------------------------------------------------------------------------
library(coda)
set.seed(1)
iterations <- 5000  
burn_in <- 1000  
num_chains <- 3    
target_dist <- function(x) dcauchy(x)  #标准柯西分布
MH_sampler <- function(init, n_iter, proposal_sd = 1) {
  x <- numeric(n_iter)
  x[1] <- init
  for (i in 2:n_iter) {
    proposal <- rnorm(1, mean = x[i - 1], sd = proposal_sd)
    accept_prob <- min(1, target_dist(proposal) / target_dist(x[i - 1]))
    x[i] <- ifelse(runif(1) < accept_prob, proposal, x[i - 1])
  }
  return(x)
}
chains <- matrix(NA, nrow = iterations - burn_in, ncol = num_chains)
for (chain in 1:num_chains) {
  init_value <- rnorm(1) 
  full_chain <- MH_sampler(init_value, iterations)
  chains[, chain] <- full_chain[(burn_in + 1):iterations]  # 丢弃
}
mcmc_chains <- mcmc.list(lapply(1:num_chains, function(i) mcmc(chains[, i])))
gelman_results <- gelman.diag(mcmc_chains)
print(gelman_results)



## -----------------------------------------------------------------------------
library(coda)
set.seed(1)
a <- 2  
b <- 3  
n <- 10  
iterations <- 5000  
burn_in <- 1000   
num_chains <- 3   
x_chains <- matrix(0, nrow = iterations - burn_in, ncol = num_chains)
y_chains <- matrix(0, nrow = iterations - burn_in, ncol = num_chains)
for (chain in 1:num_chains) {
  x_samples <- numeric(iterations)
  y_samples <- numeric(iterations)
  y_samples[1] <- 0.5 
  for (i in 2:iterations) {
    x_samples[i] <- rbinom(1, size = n, prob = y_samples[i - 1])#根据当前y值获得x
    y_samples[i] <- rbeta(1, shape1 = x_samples[i] + a, shape2 = n - x_samples[i] + b)#根据当前x值获得y
  }
  x_chains[, chain] <- x_samples[(burn_in + 1):iterations]
  y_chains[, chain] <- y_samples[(burn_in + 1):iterations]
}
x_mcmc <- mcmc.list(lapply(1:num_chains, function(i) mcmc(x_chains[, i])))
y_mcmc <- mcmc.list(lapply(1:num_chains, function(i) mcmc(y_chains[, i])))
gelman_x <- gelman.diag(x_mcmc)
gelman_y <- gelman.diag(y_mcmc)
print(gelman_x)
print(gelman_y)

## -----------------------------------------------------------------------------
library(pracma)
library(base)
kth_term <- function(k, a, d) {#计算第k个式子
  norm_a<-sqrt(sum(a^2))#欧式范数
  numerator<-(-1)^k/(factorial(k)*2^k)*(norm_a)^(2*k+2)/((2*k+1)*(2*k+2))#前两项乘积
  gamma_factor<-gamma((d+1)/2)*gamma(k+3/2)/gamma(k+d/2+1)#第三项
  term <- numerator* gamma_factor
  return(term)
}

sum_series <- function(a,d,tol=1e-10) {#求和
  sum<-0
  k<-0
  repeat {
    term<-kth_term(k, a, d)
    sum<-sum+term
    if(abs(term)<tol) break
    k<-k+1
  }
  return(sum)
}

a<-c(1, 2)
d<-length(a)
sum_series(a, d)#给定a=(1,2)^T时求和结果


## -----------------------------------------------------------------------------
library(stats)
c_k<-function(a,k){
  return(sqrt(((a^2)*k)/(k+1-a^2)))
}
LHS<-function(a,k){#等号左边
  ck_1<-c_k(a,k-1)
  if (is.nan(ck_1) || ck_1 <= 0) {
    return(NA)  # 返回 NA 以避免无效积分
  }
  integrand<-function(u){#积分项
    (1+(u^2)/(k-1))^(-k/2)
  }
  integral_result<-integrate(integrand,0,ck_1)$value
  (2*gamma(k/2))/(sqrt(pi*(k-1))*gamma((k-1)/2))*integral_result
}
RHS<-function(a,k){#等号右边
  ck<-c_k(a, k)
  if (is.nan(ck) || ck <= 0) {
    return(NA)  # 返回 NA 以避免无效积分
  }
  integrand<-function(u){#积分项
    (1+(u^2)/k)^(-(k+1)/2)
  }
  integral_result<-integrate(integrand,0,ck)$value
  (2*gamma((k+1)/2))/(sqrt(pi*k)*gamma(k/2))*integral_result
}
objective_function<-function(a,k){#差值函数，越接近0说明LHS与RHS越接近
  LHS(a,k)-RHS(a,k)
}
find_intersection<-function(k){
suppressWarnings({
result<-optimize(function(a) {#寻找一个使得差值函数绝对值最接近0的值
  obj_val <- objective_function(a,k)
  if (is.na(obj_val)) return(Inf)  #如果值为NA,返回无穷大，表示不适合的解
  abs(obj_val)
}, c(0.01,sqrt(k)))
})
return(result$minimum)
}
k_values <- c(4, 25, 100, 500, 1000)
results <- sapply(k_values, find_intersection)
rbind(k_values,results)

## -----------------------------------------------------------------------------
library(stats)
S_k_1 <- function(a, k) {
  threshold<-sqrt(a^2*(k-1)/(k-a^2))
  pt(threshold, df=k-1,lower.tail=FALSE)
}
S_k <- function(a, k) {
  threshold<-sqrt(a^2*k/(k+1-a^2))
  pt(threshold, df = k, lower.tail = FALSE)
}
objective_function <- function(a, k) {#差值函数
  S_k_1(a, k)-S_k(a, k)
}

find_intersection <- function(k) {#求根
  result <- tryCatch({
    uniroot(function(a) objective_function(a, k), c(0.01, sqrt(k) - 0.01))$root
  }, error = function(e) NA)  
  return(result)
}

k_values <- c(4, 25, 100, 500, 1000)
results <- sapply(k_values, find_intersection)
rbind(k_values,results)


## -----------------------------------------------------------------------------
data<-c(0.54,0.48,0.33,0.43,1.00,1.00,0.91,1.00,0.21,0.85)
tau<-1 
lambda<-1  #给定的初始λ值
n<-length(data)
max_iter<-100
tol<-1e-6  
for (iter in 1:max_iter) {#EM算法
  expected_censored<-sum((data==tau)*(tau+1/lambda))#E步,计算T_i>tau时T_i的期望值
  lambda_new<-n/(sum(data[data<tau])+expected_censored)#M步
  if(abs(lambda_new-lambda)<tol){
    lambda<-lambda_new
    break
  }
  lambda<-lambda_new
}
lambda_mle<-1/mean(data)#MLE
lambda
lambda_mle


## -----------------------------------------------------------------------------
library(boot) #for simplex function
A1 <- rbind(c(2, 1, 1), c(1, -1, 3))
b1 <- c(2, 3)
a <- c(4, 2, 9)
simplex(a = a, A1 = A1, b1 = b1, maxi = FALSE)

## -----------------------------------------------------------------------------
formulas <- list(
mpg ~ disp,
mpg ~ I(1 / disp),
mpg ~ disp + wt,
mpg ~ I(1 / disp) + wt
)
for_loops<-list()
for (i in 1:length(formulas)) {#使用for循环
  for_loops[[i]]<-lm(formulas[[i]],data = mtcars)
}
for_loops
lapply(formulas, function(formula) lm(formula,data = mtcars))#使用lappy函数

## -----------------------------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {
rows <- sample(1:nrow(mtcars), rep = TRUE)
mtcars[rows, ]
})
for_loops<-list()
for (i in 1:length(bootstraps)) {#使用for循环
  for_loops[[i]]<-lm(mpg~disp,data = bootstraps[[i]])
}
for_loops
lapply(bootstraps, function(boo) lm(mpg~disp,data = boo))#使用lappy函数

## -----------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared
formulas <- list(
mpg ~ disp,
mpg ~ I(1 / disp),
mpg ~ disp + wt,
mpg ~ I(1 / disp) + wt
)
for_loops<-list()
r_for<-numeric(length(formulas))
for (i in 1:length(formulas)) {#使用for循环
  for_loops[[i]]<-lm(formulas[[i]],data = mtcars)
  r_for[i]<-rsq(for_loops[[i]])
}
r_for
la<-function(formula){
  mod<-lm(formula,data = mtcars)
  rsq(mod)
}
lapply(formulas,la)#使用lappy函数

## -----------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared
bootstraps <- lapply(1:10, function(i) {
rows <- sample(1:nrow(mtcars), rep = TRUE)
mtcars[rows, ]
})
for_loops<-list()
r_for<-numeric(length(bootstraps))
for (i in 1:length(bootstraps)) {#使用for循环
  for_loops[[i]]<-lm(mpg~disp,data = bootstraps[[i]])
  r_for[i]<-rsq(for_loops[[i]])
}
r_for
la<-function(boo){
  mod<-lm(mpg~disp,data = boo)
  rsq(mod)
}
lapply(bootstraps, la)#使用lappy函数

## -----------------------------------------------------------------------------
set.seed(1234)
trials <- replicate(
100,
t.test(rpois(10, 10), rpois(7, 10)),
simplify = FALSE
)
sapply(trials,function(x) x$p.value)

## -----------------------------------------------------------------------------
set.seed(1234)
trials <- replicate(
100,
t.test(rpois(10, 10), rpois(7, 10)),
simplify = FALSE
)
sapply(trials, '[[',"p.value")

## -----------------------------------------------------------------------------
lapply1<-function(fun,...,fun.value){#lapply函数的variant
  inputs<-list(...)
  res<-Map(fun,...)
  vapply(res,identity,FUN.VALUE = fun.value)
}

#举例，输入三个列表，计算它们对应元素的和，并以向量的形式返回
x<-list(1,2,3,4)
y<-list(5,6,7,8)
z<-list(2,4,6,8)
add<-function(a,b,c){
  a+b+c
}
lapply1(add,x,y,z,fun.value = numeric(1))

## -----------------------------------------------------------------------------
chisq_quick<-function(x,y){
  x_lev<-unique(x)
  y_lev<-unique(y)
  obs<-matrix(0,nrow = length(x_lev),ncol = length(y_lev))
  for (i in seq_along(x)) {
    x_index<-which(x_lev==x[i])
    y_index<-which(y_lev==y[i])
    obs[x_index,y_index]<-obs[x_index,y_index]+1
  }
  exp<-outer(rowSums(obs),colSums(obs))/sum(obs)
  chi<-sum((obs-exp)^2/exp)
  return(chi)
}

## -----------------------------------------------------------------------------
set.seed(1234)
chisq_quick1<-function(x,y){
  obs<-table(x,y)
  exp<-outer(rowSums(obs),colSums(obs))/sum(obs)
  chi<-sum((obs-exp)^2/exp)
  return(chi)
}
#举例比较
a<-sample(1:10,1e6,replace = T)
b<-sample(1:10,1e6,replace = T)
system.time(chisq_quick(a,b))
system.time(chisq_quick1(a,b))

## -----------------------------------------------------------------------------
library(RcppArmadillo)
library(SA24204177)
n<-10
a<-3
b<-5
N<-1000
out<-200
res_rcpp<-Gibbs(n,a,b,N,out)
plot(res_rcpp[,1],res_rcpp[,2],pch=20,col=rgb(0,0,1,0.5),xlab = "x",ylab = "y",main = "Rcpp")


## -----------------------------------------------------------------------------
set.seed(1234)
gibbs_r<-function(n,a,b,N,out){
  res_rx<- numeric(N)
  res_ry<- numeric(N)
  res_ry[1] <- 0.5
  for (i in 2:N) {
    res_rx[i] <- rbinom(1,n,res_ry[i-1])
    res_ry[i] <- rbeta(1,res_rx[i]+a,n-res_rx[i]+b)
  }
  cbind(res_rx,res_ry)
}
res_r<-gibbs_r(n,a,b,N,out)
#比较x
qqplot(res_rcpp[, 1],res_r[,1], 
       main = "x",
       xlab = "Rcpp Samples", ylab = "R Samples", pch = 20, col = "blue")
abline(0, 1, col = "red", lwd = 2)
#比较y
qqplot(res_rcpp[, 2],res_r[,2], 
       main = "y",
       xlab = "Rcpp Samples", ylab = "R Samples", pch = 20, col = "green")
abline(0, 1, col = "red", lwd = 2)

## -----------------------------------------------------------------------------
library(microbenchmark)
ts<-microbenchmark(Rcpp=Gibbs(n,a,b,N,out),R=gibbs_r(n,a,b,N,out))
summary(ts)[,c(1,3,5,6)]

