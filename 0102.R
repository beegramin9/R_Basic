#system.time
system.time({
  s=0
  for(i in 1.1:1e8)
  {
    s= s+i
  }
})
# User명령을 돌리는데 필요한 시간
#System 시스템에서 명령을 부르는데 필요한 시간(R자체에서 핸들링하는 시간) 
#elapsed 전체 시간(보통 이게 걸린시간)
# 1e6 = 1*10^6, 
y <- system.time(sum(1.1:1e8))
y[1]; y[2]; y[3]

#직접계산하는 법
tic = Sys.time() #현재시각
s=0
for(i in 1.1:1e8)
{
  s=s+i
}
toc = Sys.time()
as.numeric(toc-tic, units = 'secs')

tic = Sys.time()
s2 = sum(1.1:1e8)
toc = Sys.time()
as.numeric(toc-tic, units = 'secs')

#사용자 정의 함수
#function의 변수를 처음에 정의 안되있어도 쓰이면 사용가능
wd_count = function(x, sep = " ") { #sep=" " 이거는 default값을 쓸 수 있다.
  temp = gsub("[(),.?!/ ]", sep, x)
  temp = unlist(strsplit(temp, sep))
  xrm = c("", "a", "an", "the")
  temp = temp[!(temp %in% xrm)] # A %in% B A가 B에 포함되면 T
  table(tolower(temp)) #return 생략가능 그냥 table~~써도 됨
} #return의 object는 하나만 해야함 return(c(x,y)) 이런식으로 하면
#타입이 다 하나도 통일됨 그래서 return(list(x,y))이런식으로 하면됨 타입 유지됨
tx_data = c("Hell, I like statistics",
            "I want to go out",
            "You need to take a break",
            "You are so nice")
res = wd_count(tx_data, sep = " ")
sort(res, decreasing = T)

xlst <- list(a = rnorm(10), b = 1:10)
plot_list = function(x, c=1)
{
  n = length(x)
  for(i in 1:n)
  {
    x11()
    plot(x[[i]], type = 'p', pch=c, main=paste0("Num-", i))
  }
  n #return
}
y = plot_list(xlst,16)
y

#폴더 100개 만들어볼까?
dir.create("d:/test_folder/sd")
setwd("d:/")
cr_flds = function(n,name="fd")
{
  for(i in 1:n)
  {
    dir.create(paste0('./', name, '_',i))
  }
}
cr_flds(100, name = 'test')
unlink(dir(), recursive = T)
dir('./')

#Fibonacci
fibo <- function(n)
{
  if(n==1) return(1) #return만나면 연산 종료
  if(n==2) return(rep(1,2))
  x <- rep(1,n)
  for(i in 3:n)
    x[i] <- x[i-1] + x[i-2]
  return(x)
}  
fibo(1)
fibo(2)
fibo(30)

#recurrent form 이거가 함수를 설정하고 자신을 다시 부르는 거라서 좀 느림
fibr = function(n)
{
  if(n==0) return(0)
  if(n==1) return(1)
  return(fibr(n-1)+fibr(n-2))
}
fibr(1)
fibr(2)
fibr(30)

a <- c(1,3,5,6)
mean.k <- function(x,k)
  return(mean(x^k))
mean.k(a,2)
mean.k(a) #default값을 설정안했는데 변수를 생략해서 에러

mean.k2 <- function(x, k=3)
  return(mean(x^k))
mean.k2(k=2, x=a)
mean.k2(k=2, a)
mean.k2(a)

std.ftn2 <- function(x)
  return(list(mean=mean(x), var=var(x),
              std=(x-mean(x))/sd(x)))
std.ftn2(a)

mat = matrix(1:12,4,3)
f= function(x)
  return(prod(x)-10)
apply(mat, 1, f)

apply(mat, 1, function(x) return(prod(x)-10))

#전역변수 GlobalEnv에 정의 된 애들 x<-3 이런거
#지역변수
#function(x)
#{
#  a<-3
#}
#여기서만 참조되고 함수 만드는건 끝나면 저장안됌. 즉, GlobalEnv에 저장 안됌.
# a <<- 3 이렇게하면 함수 안에서도 전역변수가 됨

a <- c(1,3,5)
noact <- function(x)
{
  loc <- 3
  return(loc)
}
noact(a)
loc

a <- c(1,3,5)
noact <- function(x)
{
  a[1] <- 3 #a[1] <<- 3 이러면 전역할당
  return(a)
}
noact(10)
a

#Practice 6
#3
x <- c()
for(i in 1:200)
{
  if(sum(i%%(1:(i+1))==0)==2) x<-c(x,i)
}
x
length(x)

x <- c()
for(i in 1:200)
{
  if(sum(i%%(1:(i+1))==0)==2) x[i]<-i
}
which(!is.na(x))
length(which(!is.na(x)))

#시험유형 한시간 반
#코드보고 결과 작성
#결과보고 코드 작성
#서술보고 코드 작성
#코드+빈칸 있을 때 빈칸채우기
#TF문제 맞으면+ 틀리면-