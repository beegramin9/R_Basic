#12.26 55p
(x=2) #x에다가 2를 집어넣고 x도 실행 
x=3; y=0
if(x=y) print(x)
if((x=y))
{
  print(x)
} else {
  print(y)
}
x

complex(real = -17, imaginary = 0)
complex(3,1) #length=3, real=1, imaginary=0
complex(3,10,-2) #length=3, real=10, imaginary=-2
sqrt(-17) #[1] NaN - Warning message (Not a Number)
sqrt(-17+0i) #sqrt(-17+0*i)이러면 에러뜸
z <- 1+2i
Re(z)
Im(z)
Mod(z)
Conj(z)
z*Conj(z)
Arg(z); asin(2/sqrt(5)); acos(1/sqrt(5))
as.complex(paste0(3,"+",2, "i"))

rep(c(1,2), times=3)
rep(c(1,2), each=3)
seq(from=1, to=10)
seq(length=10, from = -5, by = 3)
seq(length=6, to = 1, by=2)
x=seq(-3, 3, length.out = 15)
y=dnorm(x) #density 밀도함수 
x11()
plot(x,y, type = 'p', pch = 7)
z = seq_len(9);z #1부터 5까지 만들어줌
t = seq_along(x); t #seq_len(length(x)) 이거랑 똑같음 안외우고 기본함수로 만들어도됨

vec2 = letters[1:3] #abcde 이런식으로 넣어주는거
names(vec2) #인덱스에 이름 지어주는 함수
names(vec2) <- c("first", "second", "third")
vec2[1] #쳐보면 first에 ""가 없음 -> 문자형이 아니다 뭔가 다른 용도
vec2
vec2["first"]
names(vec2)[2] <- "2nd" #인덱스 이름 바꾸는거
vec2[names(vec2)>"g"]
is.null(NULL)
is.null(1)

#Matrix
x1 <- matrix(1:10, nrow = 5, ncol = 2, byrow = T); x1
x2 <- matrix(1:10, 5, 2, byrow=F); x2 #byrow는 F가 default임
cbind(x1, x2)
x2[1,2]
x2[2:4,2]
x2[1,] 
x2[,2] #행이나 열 하나만 보는 경우에 벡터타입으로 보여줌
x2[-1,]
x2[,-1]
cbind(x1,x2[-1,])
cbind(x1, x2[1,]) #이건 붙음 왜냐 벡터이니까(벡터는 연산할 때 부족하면 반복해주는거 기억하지?)
rbind(x1, x2)
as.numeric(x1)
A <- matrix(1:12,4,3)
rownames(A) <- c("n1", "n2", "n3", "n4")
colnames(A) <- c("x1", "x2", "x3")
paste0("x", 1:3)
rname <- paste0("n", 1:4)
cname <- paste0("x", 1:3)
B <- matrix(1:12, 4, 3, dimnames = list(rname, cname))
B; matrix(1:2, 3,2)

A <- matrix(1:12, 4,3); B<- matrix(1,4,3)
A; B
A[7]; A[11]
A + B; A-B
t(A) #트랜스포즈
A <- matrix(1:6, 2 ,3); B <- matrix(1,3,2)
A+B
A-B
B%*%A
A%*%t(B)
A<- matrix(c(1,1,1,4),2,2);A
solve(A)
x = 1:3
A + x
A + matrix(x,1,3)
A[2]
A[3]

nrow(B);ncol(B) #행갯수 열갯수
B[1,2] = 'F'; B
x = c(1,3,5); y=c(2,4,5)
x
as.matrix(x)
rbind(x,y)
rbind(as.matrix(x), as.matrix(y))
A <- matrix(1:12, 4,3)
A
3:5-1 #이러면 c(3,4,5)-1 = c(3,4,5)-c(1,1,1) = c(2,3,4) 빼기보다 콜론 연산이 더 빠름
3:(5-1)
A[,-3]
ind_mat = matrix(c(1,2,3,3,3,2), ncol = 2, byrow = T)
ind_mat
A[ind_mat]
A[4,2] = NA; A[1,3] = NA ;A
is.na(A)
A[is.na(A)]
A[is.na(A)] = 0
which(is.na(A)) #바로 위에꺼만 빼고 실행
which(is.na(A), arr.ind = T)

A <- matrix(c(1,1,1,4), 2,2);A
sum(diag(A))
det(A)
diag(A)
diag(3)
diag(c(1,3,5))
x <- c(1,2,3)
y <- c(1,2,3)
sum(x*y) #x%o%y <=> x y^T
x %o% y
diag(A) = c(1,2)
A
diag(A) = 1

a = c(1,3)
b = letters[1:10]
c = matrix("A", 3,4)
x = list(a,b,c)
x
x[[1]] #안에꺼를 가져옴
x[1] #리스트 구조를 유지
x[1:2]

x[[3]][2.3]

x = list(y1 = a, y2=b, y3=c )
x
x$y1
x$y2
names(x)
x[[1]]

#List
lst1 = list(a=1:10, b=matrix(1:4,2,2)); lst1
lst2 <- list() #비어있는 리스트 만듬
lst2[[1]] <- matrix(1:10, 5,2)
lst2[[2]] <- lst1; lst2
lst1[[2]][1,2] = 10; lst1
unlist(lst1) #구조 깨서 다 벡터로 만듬
lst1$a; lst1$b
lst1['a']
lst1[['a']]
lst1[1]
lst1[[1]]; lst2[[1]]
lst2[[1]][3]
lst2[[2]][[1]]

#Practice 2
x <- c("0", "21", "12", "16")
x <- sort(x)
x <- as.logical(x)
y <- seq(from = 0, to=30, by = 10)
x<y&x<=y
z <- rep(c(TRUE,FALSE), 5)
