#12.27(Fri)
#Factor
grade <- c("A", "A", "B", "C", "B", "B")
f.grade <- factor(grade)
f.grade
f2.grade <- factor(grade, order = T)
f2.grade
lev <- c("C", "B", "A")
f3.grade <- factor(grade, levels = lev, ordered = T)
f3.grade
levels(f2.grade)
levels(f3.grade)
as.numeric(f2.grade)
as.numeric(f3.grade)
as.character(f3.grade)

#data frame
x1 = 1:4; x2 = c("Kim", "Lee", "Jung", "Park")
dat = data.frame(x1, x2); dat
dat2 = data.frame(num=x1, name=x2); dat2
rownames(dat)
colnames(dat)
dat2$num
dat2$name
dat3 = data.frame(x1, x2, stringsAsFactors = F)
dat3[[2]] #이건 되긴하는데 좀 어색함
is.list(dat3); is.matrix(dat3)

#행렬의 인덱스 형태의 사용
dat[,1]; dat[2,]
dat[2,1]; dat[3,2]

#리스트의 인덱스 형태의 사용
dat[1]; dat[2]
dat[[1]]; dat[[2]]
dat[[1]][2]; dat[[2]][1]
unlist(dat)
dat[,1:2]
dat[,-1]
dat[-1,-2]

#array
x1 <- array(1:24, dim=c(4,3,2))
x2 <- array(1:32, dim = c(2,2,4,2))
x1
x1[,,1]
x1[,3,]
x2[,,3,1]
x2[,2,,1]

#Sum all values greater than 12 in a vector x
x <- 1:100
sum1 <- sum(x[13:100])
x<- c(1:5, 10:20, 30:45, 2:5, 11:30)
x>12
sum2 <- sum(x[x>12])
#Sum all values between 10 and 20
sum3 <- sum(x[x>10 & x<20])

y <- rnorm(1e8)
s1 = 0
for(i in 1:1e8)
  if(y[i]>0.2)
    s1 = s1+y[i] 
s1
sum(y[y>0.2]) #이게 훨씬 빠름

#apply
x_mat <- matrix(rnorm(100), 20, 5)
apply(x_mat, 2, mean)
apply(x_mat, 2, var)
apply(x_mat, 1, var)
x_mat[1,2] = NA
apply(x_mat, 1,mean)
apply(x_mat, 2, mean)
mean(c(1,2,NA,3))
mean(c(1,2,NA,3), na.rm = T) #remove NA
apply(x_mat, 2, mean, na.rm=T)
mean(x_mat[,2], na.rm=T)

#Practice3
x <- seq(from = -10, to = 10, length = 30)
y <- rep(c("S", "T", "A", "T", "s"), 6)
z <- rep(c(0,7), each =15)
zm <- as.logical(z)
xmat <- matrix(x, 6, 5, byrow = T)
xdat <- data.frame(x,y,zm)
xlst <- list(x,y,zm,xdat)
as.matrix(xdat[,1:2])
as.matrix(xdat[,-2])
sum(x[-2<x & x<4])
apply(xmat, 1, sum); apply(xmat, 2, mean); apply(xmat, 2, var)

#paste 이거 paste0랑 비슷 paste는 default가 sep=" " 기말고사에서 파일 100개 불러오기
file_id = 1533 # file name
paste("Dataset_", file_id, ".txt,", sep="")

paste("A", "B", collapse = '/') #collapse는 벡터를 붕괴시킴
paste(c("A", "B"), collapse = '/')
paste(c("A", "B"),1:2, collapse = '/')
paste(c("A", "B"),1:2,sep = '?', collapse = '/')
paste(c("A", "B"),1:2,sep = , collapse = '/')
paste0(c("A", "B"),1:2, collapse = '/')
paste0(LETTERS[1:5], letters[1:5])

test <- c("abcdefg", "AFFY1245820")
nchar(test)
f_name <- c("AFFY123", "AFFY1245820")
substr(f_name, 5, nchar(f_name))
strtrim("ABCDEF", 3)
strtrim(rep("abcdef",3), c(1,4,10))

#102p 확장 정규 표현식
x <- c(as= "asfef", qu = "qwerty", "yuiop[", "b", "stuff.blah.yech")
strsplit(x, "e")
strsplit(x, "[a-e]")
strsplit(x, "[aleu][ab]")
unlist(strsplit("a.b.c", "."))
unlist(strsplit("a.b.c", "[.]"))
unlist(strsplit("a.b.c", ".", fixed = TRUE))

y = c("ab/cd/ef~z!yk?c", "a/x!b,y,z.t/s")
strsplit(y, "[/~!?]")
strsplit(y, "[[:punct:]]") #103p에 [:punct:]설명 있음
