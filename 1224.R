#12.24
#round-off error 이진법 변환할 때 생기는 오차
real <- 3.5
as.integer(real) #정수로
intg <- 3
is.integer(intg) #정수냐?
intg2 <- as.integer(3)
is.integer(intg2)
is.integer(c(1,2))

1.02*10^3
1.02e3
1.5e-3
real <-3
is.double(real)
x <- rnorm(10) #표준정규분포에서 x개의 난수 생성
y <- scale(x)
mean(y) #10^-16보다 작으니까 0이라고 봐도 무방

ch <- c("red", "blue", "yellow")
ch[1];ch[3]
toupper(ch) #대문자로
ch2<- c("a", "b","c")
ch3 <- c("It's sunny", '"abc"', 'ab\"c', 'ab\'c')
ch2뜻
ch3 #escape 문자 "\" 여기서 닫힌게 아니다!라는 뜻    문자 그대로 따옴표 \"이게 그냥 "임
cat(ch3) #출력함수 문자를 출력해줌
tolower(ch3) #소문자화

test <- c(TRUE, FALSE, TRUE)
is.logical(test)
test2 <- c(T,F,T)
as.numeric(test2)
as.character(test)
as.character(test2)
as.logical(c(1,0,2,3))
as.logical(c("T","F"))
as.logical(c("a","b"))
x = c(1,2,4);
x[test2]
#index 1 2 3
#   x  1 2 4
#   x [T F T] T만 키는거임
a = "TRUE" ; as.numeric(a); as.integer(a); as.logical(a)
b = "1.5" ; as.numeric(b); as.integer(b); as.logical(b)
as.numeric(as.logical(a))

#다 숫자인데 하나가 문자인 데이터를 받아서 어떤 인덱스가 문자인지 찾고 싶은거
x = c(rep(5,200), "a", 30:50) #as.character(c(rep(5,200), "a", 30:50)) rep(값, 반복수) repeat임
x
x[1:5]
y = as.numeric(x)
is.na(NA)
is.na(y)
y[100] = NA #y의 100번째 NA를 넣는거
x[201]
which(is.na(y)) #x중 TRUE에 해당하는 Index
sum(is.na(y))

vec <- c(1,3,4,2,5)
vec
vec[1]
vec[7]
vec[10] = 5
vec
if <- c(1,2,3) #if, else, for에는 값을 할당 못함
vec2 <- c()
vec2
vec2[1] = 3
vec2[3] = 1
vec3 = vector() #벡터를 논리(가장 낮은거)로 정의
vec3
vec4 = character() #logical() numeric()
vec4[1]
vec4

rm(list=ls())
assign("x", 1:10)
x
name = paste0("x", 1:200) #paste0("a", "b") ->"ab" 붙여주는거
name
for(i in 1:5) #for(변수 in 객체) 반복대상{}
 assign(name[i], 1:10)  
ls()
rm(list=name)
ls()

paste0(c("X", "Y"), 1:5) #수치연산이 아니라서 경고메세지가 안뜸
x <- c(1,2,3); y<- c(1,2,3,4)
v <- 2*x + y+1 #x=c(1,2,3,4) 1=(1,1,1,1)
v
z=c(4,5)
w=2*z+y
w

a=3;b=2;c=3+3
a%%b;c%%b
A <- T; B <- F; C <- c(T,T); D <- c(F,T)
A&B
A&&B
C&D
C&&D
A|B
A||B
C|D
C||D

a<-c(1,2);b<-c(2,2);d<-c(3,4)
a<b ; a<=b
a<d;a<=d
a<b
a>=b
A<-c(T,T); B<-c(F,T); C<-c(T,T); D<-(T,F)
A==B; C==D
all(A==B); all(A==C) #모두 참일때 TRUE
any(A==B); any(A==C) #적어도 하나만 참 일때 TRUE
a = "true"
b = "Atat"
a<b

a=1
a=a+2
a
set.seed(123) #엄밀하게말하면 컴퓨터에서 발생한 난수는 진짜 난수가 아님: 의사난수
#초기값을 정해주면 난수가 똑같음(컴퓨터는 로직에 의해 돌아가니까)
rnorm(5)
sample(1:45,6)
sample(1:5, 10, replace = T)
log(10) #log(10, base = x)
log10(10); log(4, base = 2)
log(exp(1))
exp(1); exp(2)
sin(30); sin(pi/2); asin(1); pi/2
x<- c(1,3,2,5,10)
max(x);min(x)
x<-c(-5,10,2,5,10); range(x)
var(x); sd(x); sqrt(var(x))
prod(x); mean(x)

x<-c(1,5,3,2,4)
sort(x)
sort(x, decreasing = T)
order(x) #54p
y <- c("D","F","G","B","A")
y[order(x)]
