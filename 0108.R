#0108

rm(list=ls())


# Uniform
a= 3
b= 5
dunif(4.25,min=a,max=b) #함수값 출력
punif(3.5, a,b,lower.tail=F) # 1-F(x) 계산, F(x): CDF(누적확률분포) 누적확률 계산 default는 lower.tail=T
qunif(0.75,min=a,max=b,lower.tail=T) # CDF이용 분위수를 계산(x값이라고 보면 됨)
x = runif(1000000,a,b)
x11()
hist(x,breaks=15,F)

#2차원 균등분포에서 난수 생성
#x+y<=r^2(좌표평면의 원)안에서 균일분포에서 난수 생성
r = runif(10000,0,3)
theta = runif(10000,0,2*pi)
x = r*cos(theta)
y = r*sin(theta)
x11()
plot(x,y,type='p',pch=16, ,cex = 0.1)

# Hypergeometry(초기하분포)
m = 3; n = 2; k = 3 #m: 타겟 공, n: 나머지 공, k: 시행횟수 
dhyper(2, m, n, k, log = F) # HG(m+n, m, k)
phyper(3, m, n, k, lower.tail = TRUE,log.p=T)
phyper(3, m, n, k, lower.tail = TRUE,log.p=F)
phyper(2, m, n, k, lower.tail = TRUE)
qhyper(0.7, m, n, k, lower.tail = T)
x = rhyper(100, m, n, k)
tb_x = table(x)
tb_x
x11()
plot(tb_x)

# Binomial
n = 10 #시행횟수
pr = 0.5 #성공확률
dbinom(2, n, pr,log = F) # B(n, pr) 성공횟수 2번
pbinom(5, n, pr,log = F)
qbinom(0.5, n, pr,log = F) 
x = rbinom(10000,n,pr)
tb_x = table(x)
tb_x
x11()
plot(tb_x)

# Normal
x = seq(-2,2,by=0.01)
y = dnorm(x,mean=0,sd=1)
x11()
plot(x,y,type='l')
pnorm(0.5,mean=0,sd=5)
qnorm(0.99)
qnorm(c(0.95,0.975,0.995))
z = rnorm(1000,mean=10,sd=5) 
x11()
hist(z,breaks=15)

# Chisq
x = seq(0.001,20,by=0.1)
y = dchisq(x,df=5)
x11()
plot(x,y,type='l')
pchisq(0.5,df=5)
qchisq(c(0.95,0.975),df=5)
z = rchisq(1000,df=10)
x11()
hist(z,breaks=15)

# t-dist
x = seq(-5,5,by=0.01)
y = dt(x,df=4)
plot(x,y,type='l')
pt(0.5,df=4)
qt(c(0.95,0.975,0.995),df=5)
z = rt(1000,df=5) 
x11()
hist(z,breaks=15)

# F-dist
x = seq(0.01,20,by=0.01)
y = df(x,df1=4,df2=5)
plot(x,y,type='l')
pf(0.5,df1=4,df2=5)
qf(c(0.95,0.975,0.995),df1=4,df2=5)
1/qf(c(0.05,0.025,0.005),df1=5,df2=4)
z = rf(1000,4,5) 
x11()
hist(z,breaks=15)

# CLT(Central Limit Theorem)
df = 4
niter = 1000
xm <- rep(0,niter)
for(i in 1:niter)
{
  X <- rchisq(100,df=df)
  xm[i] = (mean(X)-df)/(sqrt(2*df)/sqrt(100))
}
x11()
hist(X,breaks=20,main=expression(chi^2~(4)),
     col='lightblue')
x11()
hist(xm,breaks=20,
     main=expression(over(bar(X)-mu,sigma/sqrt(n))),
     col='gray', xlab='normalized sample mean')

x11()
demo(plotmath)

setwd('d:/example')
load(file='expr_dat.Rdata')
n = nrow(expr_dat)
p = ncol(expr_dat)
gr_ind = gl(2,221)
uq_names = colnames(expr_dat)

# t.test (one sample)
t1 = t.test(expr_dat[,1],mu=7,conf.level=0.95, #신뢰수준
            alternative="two.sided") #alternative는 대립가설 말하는거
#H0: mu = 7, H1: mu != 7 이게 alternative
#p-value = 0.6962 > alpha = 0.05 H0를 기각할 수 없다. 따라서 mu는 7이라 판단 가능

t2 = t.test(expr_dat[,1],mu=6.5,conf.level=0.95,
            alternative="less")

t3 = t.test(expr_dat[,1],mu=6.5,conf.level=0.95,
            alternative="greater")
t1; t2; t3
names(t1) #원래 리스트인데 출력형식을 t.test함수에서 저렇게 해준거임
is.list(t1)
summary(t1)

k = ncol(expr_dat)
pval = rep(0,k)
for(i in 1:k)
{
  temp =t.test(expr_dat[,i],mu=6.5,conf.level=0.95,
               alternative="two.sided")
  pval[i] = temp$p.value
}
which(pval<0.05)



# t.test (two sample, indep)
x = expr_dat[gr_ind==1,1]
y = expr_dat[gr_ind==2,1]
t1 = t.test(x,y,mu=0,conf.level=0.95,paired=F,
            alternative="two.sided")
t2 = t.test(x,y,mu=0,conf.level=0.95,
            alternative="less")
t3 = t.test(x,y,mu=0,conf.level=0.95,
            alternative="greater")
t1; t2; t3


# Paired t-test
n = 25
x = rnorm(n,mean=1,sd=1)
y = x + rnorm(n,mean=0.5,sd=1)
t1 <-t.test(x,y,alternative='two.sided',
            paired=T,var.equal=F)
t2 <-t.test(x,y,alternative="less",
            paired=T,var.equal=F)
t3 <-t.test(x,y,alternative="greater",
            paired=T,var.equal=F)
t1;t2;t3

#---------------------------------------------------------------------------------------------------

# Regression
indy = 8
indx = 200
x = expr_dat[,indx]
y = expr_dat[,indy]
out = lm(y~x)
summary(out)
plot(x,y,pch=16)
abline(out,col=2,lwd=1.5)

names(out)

out$coefficients # out[[1]] 
out$outted.values # out[[5]]

coef(out)
resid(out)
fitted(out)
result = summary(out); names(result)
result[4] # result[[4]]

confint(out)

pred1 = predict(out,newdata=data.frame(x=2.3))
# or out$coefficients ( coef(out) ) 이용 계산
est= coef(out); x1 = 2.3
y1 =  est[1] + est[2]*x1

pred2 = predict(out,
                newdata=data.frame(x=c(1,2.2,6.7)))
x2 = c(1,2.2,6.7)
y = est[1] + est[2]*x2

# variable selection
indy = 8
indx = c(10,30,200)
indxr = c(10,200)
xf = expr_dat[,indx]
xr = expr_dat[,indxr]
y = expr_dat[,indy]

fit1 = lm(y ~ xf)
fit2 = lm(y ~ xr)
anova(fit2,fit1)


ftxt = paste0(uq_names[indy],'~',
              paste0(uq_names[indx],collapse="+"))
ftxtr = paste0(uq_names[indy],'~',
               paste0(uq_names[indxr],collapse="+"))
ftxt
ftxtr

colnames(expr_dat) = gsub("[ .]","",uq_names)
lm_dat = data.frame(expr_dat)
fit1 = lm(as.formula(ftxt),data=lm_dat)
fit2 = lm(as.formula(ftxtr),data=lm_dat)
anova(fit2,fit1)

# model diagnostics
rsd = resid(fit2)
ft = fitted(fit2)
plot(ft,rsd,type='p',pch=16)
hist(rsd,breaks=20)

x11()
qqnorm(rsd)
qqline(rsd,col=2,lwd=2)



# ANOVA
?PlantGrowth
PlantGrowth
PlantGrowth$group
x11()
boxplot(weight ~ group, data = PlantGrowth,
        main = "PlantGrowth data",
        ylab = "Dried weight of plants",
        col = "lightgray")

dat = list(ctrl=PlantGrowth$weight[PlantGrowth[,2]=='ctrl'],
           trt1=PlantGrowth$weight[PlantGrowth[,2]=='trt1'],
           trt2=PlantGrowth$weight[PlantGrowth[,2]=='trt2'])
boxplot(dat,
        main = "PlantGrowth data",
        ylab = "Dried weight of plants",
        col = "lightgray")


library(dplyr)
group_by(PlantGrowth, group) %>%
  summarize(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

fit = lm(weight ~ group, data = PlantGrowth)
summary(fit)
model.matrix(fit)
cbind(PlantGrowth$group,model.matrix(fit))
anova(fit)
rsd = resid(fit)
x11()
hist(rsd,breaks='Sturges')
shapiro.test(rsd)


fit_aov = aov(weight~group, data=PlantGrowth)
summary(fit_aov)
fit_aov

TukeyHSD(fit_aov)

t.test(PlantGrowth[PlantGrowth$group=='trt1',1],
       PlantGrowth[PlantGrowth$group=='trt2',1],
       var.equal = F,mu=0,alternative = 'two.sided')

