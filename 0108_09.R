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

#0109
setwd('C:\\Users\\Taekyeong\\Documents\\R')
load(file='expr_dat.Rdata')
n = nrow(expr_dat)
p = ncol(expr_dat)
gr_ind = gl(2,221)
uq_names = colnames(expr_dat)
#요거 입력해주고 스타뜨

# Regression
indy = 8
indx = 200
x = expr_dat[,indx]
y = expr_dat[,indy]
out = lm(y~x) #yi=b0(베타) + b1xi + ei(입실론) 273p보셈 얘도 리스트임
is.list(out)
names(out)
coef(out) #이러면 계수만 보여줌
out$residuals #잔차(ri) ri = yi-b0(hat)-b1(hat)xi 일종의 ei 추정값인듯
out$fitted.values #xi 회기분석직선식에 넣어서 나온 yi 값들이래
#yi = 0.2675 + 0.8724x + ei
summary(out)

plot(x,y,pch=16)
y9 = y[y>9] 
x9 = x[y>9] #y>9인 행의 x값 가져오는거
points(x9, y9, col=2, pch=16)
abline(h=9, col=4, lwd=2, lty=3) #h는 horizontal line, lty는 line type
abline(v=8, col=3, lwd=2) #v는 vertical line
text(7,8,"abc",col = 2) #(7,8)에 "abc" 쓰는거
text(7.2,8,"abcd",col = 4) 

abline(out,col=4,lwd=1.5) #abline(a,b)면 y=a+bx 그려줌 lwd line width
abline(coef(out)[1], coef(out)[2], lwd=2, col=2) #abline(lmfit)이거랑 똑같음
#이건 플랏한번하고 덧칠만 할 수 있음
for(i in 5:1)
{
  abline(out, lwd=2*i, col=i)
}
#걍 에너지파 만들어봄

names(out)

out$coefficients # out[[1]] 
out$outted.values # out[[5]]

coef(out)
resid(out)
fitted(out)
result = summary(out); names(result) #얘도 리스트타입임
result[4] # result[[4]])
confint(out)

pred1 = predict(out,newdata=data.frame(x=2.3)) #뭔지 모르겠다 277p보셈
# or out$coefficients ( coef(out) ) 이용 계산
est= coef(out); x1 = 2.3
y1 =  est[1] + est[2]*x1

pred2 = predict(out,
                newdata=data.frame(x=c(1,2.2,6.7)))
x2 = c(1,2.2,6.7)
y = est[1] + est[2]*x2

# variable selection 279p
indy = 8
indx = c(10,30,200)
indxr = c(10,200)
xf = expr_dat[,indx]
xr = expr_dat[,indxr]
y = expr_dat[,indy]

fit1 = lm(y ~ xf)
fit2 = lm(y ~ xr)
anova(fit2,fit1) #첨에 indxr = c(200) 해보고 200까지만 추리는 건 너무 많았고 그래서 10살려서 다시 함
#p-value보고 기각할건지 말건지
summary(fit1)

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
qqnorm(rsd) #정규분포랑 비교해서 
qqline(rsd,col=2,lwd=2)
#샘플의 평균이랑 분산을 가지고 정규분포를 따른다고 가정했을 때 기준 정규분포랑 비교해서 그린 직선
rsd_sc = scale(rsd, center = T, scale = T) #정규화 시키는거
qqnorm(rsd_sc) 
qqline(rsd_sc,col=2,lwd=2) #이러면 y=x 그래프
#qqplot()는 샘플끼리 비교할 때 쓸 수 있는데 그런 경우가 많이 없어서 잘 안 쓰나봐

# ANOVA(Analysis of Variance) 세개 이상의 집단에 대한 평균 비교 가능
?PlantGrowth
head(PlantGrowth)
PlantGrowth
PlantGrowth$group
x11()
boxplot(weight ~ group, data = PlantGrowth, #이거도 포뮬라 쓸 수 있음
        main = "PlantGrowth data",
        ylab = "Dried weight of plants",
        col = "lightgray")
#ctrl랑 trt2가 차이가 큼 가설검정을 해봐야함
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
    count = n(), #갯수 세주는 거
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
shapiro.test(rsd) #정규성 테스트 여기서 H0: 정규분포를 따른다. 기각하면 정규분포 아닌거


fit_aov = aov(weight~group, data=PlantGrowth) #Analysis of Variance 분산분석
summary(fit_aov)
fit_aov

#mu1 mu2 mu3가 다른데 뭐가 다른지 모름 하나씩 비교해보면 유의수준이 좀 깨짐
#그거 조절하는 방법 중 하나가 밑에꺼
TukeyHSD(fit_aov)

t.test(PlantGrowth[PlantGrowth$group=='trt1',1],
       PlantGrowth[PlantGrowth$group=='trt2',1],
       var.equal = F,mu=0,alternative = 'two.sided')
#p-value가 다른데 이게 TukeyHSD가 세 개 유의수준 다 고려한거라 보정값이 좀 들어감
