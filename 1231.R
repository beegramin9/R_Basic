#12.31
install.packages("dplyr")
library(dplyr)
ls(2)

?mtcars
filter(mtcars, cyl==4, am==1)
filter(mtcars, wt>2, cyl==8)
names(mtcars)
select(mtcars, mpg, cyl, wt)
select(mtcars, -hp, -gear) #-원래 숫자 앞에만 붙는데 select함수에서 사용가능
mtf = select(mtcars, mpg,am, cyl,hp, wt)
mutate(mtf, hw_rat=hp/wt)
mutate(mtf, hw_rat=hp/wt, cw_rat = cyl/wt)

arrange(mtf, mpg, desc(cyl))
arrange(mtf, am, cyl, wt)
summarize(mtcars, m_mpg=mean(mpg), v_mpg=var(mpg))
summarize(mtcars, md_mpg=median(mpg), md_wt=median(wt))
summarize(mtcars, trows=range(mpg)) #하나의 출력값나오는 함수만 쓸 수 있음

mt_gr <- group_by(mtcars, cyl)
mt_gr
summarize(mt_gr, m_mpg=mean(mpg), m_wt = mean(wt))

dat <- group_by(mtcars, cyl)
dat <- select(dat, mpg, wt, cyl)
dat <- summarise(dat, m_mpg = mean(mpg))
dat <- filter(dat, m_mpg > 16)
dat

dat2 <- mtcars %>%
  group_by(cyl) %>%
  select(mpg, wt, cyl) %>%
  summarize(m_mpg = mean(mpg)) %>%
  filter(m_mpg>16)
dat2

dat3 <- mtcars %>% group_by(cyl) %>%
  select(mpg, wt, cyl) %>% summarise_all(list(m=mean, v=var))
dat3

dat4 <- mtcars %>% group_by(cyl) %>%
  select(mpg, wt, cyl) %>% summarise_all(mean) #함수이름을 하나만 주면 걍나옴
dat4

#Practice4 #결측값대체를 평균값으로 하기
x <- matrix(c(NA, 1,3,2,1,NA,2,NA),4,2)
cm <- apply(x, 2, mean, na.rm = T)
idx2 <- which(is.na(x), T) #(1,1), (2,2), (4,2) 자리에 NA가 있다는거
x[idx2] <- cm[idx2[,2]]
x

x<- 1:5
y<- -2:2
if(any(x<0)) print(x) #any는 하나라도 T라도 T
if(any(y<0)) print(abs(y)) #abs 절댓값
if(y<0) print(y) #조건문은 하나만 들어가야해서 벡터의 첫번째값으로 판단하고 warning 메시지줌
if(any(y<0)) 
{
  print(abs(y))
  cat("\n y contains negative values")
}

x<- 3
if(x>0)
{
  print(x)
} else {
  y = -x
}

x <- c(10, 3,6,9)
y <- c(1,5,4,12)
ifelse(x>y, x,y)
score<- c(80, 75, 40, 98)
grade <- ifelse(score >= 50, "pass", "fail")
data.frame(score, grade)
y<- -2:2
ifelse(y >=0, y, -y)
abs(y)

x<-c(1,3,2,5,2)
i <- 3
switch(i, mean(x), median(x), sd(x), var(x))
type <- "mean"
switch(type, mean=mean(x), sd=sd(x), var=var(x))
i <- 1
switch(i, {mean(x);var(x)}, median(x), sd(x), var(x)) #두개를 실행하진 않네

#Loop statement
s<-0
for(i in 1:100)
{
  s = s+i
  if(i %% 20==0) #%%나눴을 때 나머지
    cat('\n current sum =',s)
}

sum(1:100)
for (i in 1:10)
{
  cat(rep("*",i),"\n")
}

sum = 0; x=runif(100,0,1) #random number uniform(균등분포(확률이 다 같은 분포)에서 임의의 수 뽑음)
for(i in 1:99)
  for(j in (i+1):100)
    sum = sum + x[i]*x[j]

(sum(x%o%x) - sum(x*x))/2 #for을 피하기 위해(for이 계산이 느리니깐)

setwd("d:/example")
dir.create('./datafile')
f_pre = './datafile/file_' 
f_suf = '.txt'
dat = mtcars; n = nrow(dat); cut =5
nfile = ceiling(n/cut)
for(i in 1:(nfile-1))
{
  ind = (cut*(i-1)+1):(cut*i)
  write.table(dat[ind,],
              file = paste0(f_pre, i, f_suf), sep = '\t')
}
ind = (cut*i+1):n
write.table(dat[ind,],
            file=paste0(f_pre, i+1, f_suf), sep = '\t')

setwd("d:/example/datafile")
dat = read.table(file = 'file_1.txt', header = T)
for(i in 2:7)
{
  temp = read.table(file = paste0('file_',i,'.txt'), header = T)
  dat = rbind(dat, temp)
}
dat

#Sum from 1 to 100
s = 0
i = 1
while(s<=5000) #i<=100
{
  s = s+i
  i = i+1
  cat('\n', s)
}
s; sum(1:(i-1))
c(i-1, s)

#parsing
ch <- c("A/B/C/D/E", "A/A/A/AA", "BB/B", "Quit", "D/CC/C")
xp <- list()
i = 1
while(ch[i]!="Quit" & i <= length(ch))
{
  xp[[i]] = unlist(strsplit(ch[i], '/')) 
  print(xp[[i]])
  i = i+1
}
table(unlist(xp))