#12/30
getwd()
setwd('d:/')
dir.create('example4')
setwd("example4")
getwd()
dir()
dir.create('./test') #dir.create('test') 이래도 똑같음
dir()

unlink("test", recursive = T) #디렉토리 지우는거
dir()
dir("c:/windows")

test = dir("c:/windows")
t1 = grep("^[A-Ca-c][a-z]", test)
t2 = grep(".exe$", test, value = T)
t3 = grep("^[BbCc].+exe$", test, value = T)
length(colors())
colors()[1:5]
grep("red", colors(), value = T)
x = c("010-3123-2134", "032-860-7642", "02-111-1234", "010-123-5656")
grep("010-[0-9]{3,4}-[0-9]{4}",x)
col_vec = grep("red", colors(), value = T)
x11()
plot(1:length(col_vec), rep(4, length(col_vec)), col=col_vec, type='p', pch=8, cex=3)
#p는 포인트, pch 포인트오브 캐릭터 점모양임, cex character extension 점크기 default 1임
x=c("red", "red2", "blue"); gsub("e", "a", x)

dat = data.frame()
edit(dat)
dat = edit
fix(dat)
dat

a = readline("Input any integer: ")
a
b = readline("Input two integers with comma (ex: 1,2)")
as.numeric(unlist(strsplit(b, "[,]")))
b

path = "d:/example/"
setwd(path)
x1 = scan(file = "input_noh.txt", what = numeric())
x2 = scan(file = "input_noh.txt", what = character())
x3 = scan(file = "input_noh.txt")
x4 = scan(file = "input_h.txt") #, what = character()) #scan이 하나의 유형만 불러올 수 있어서 헤더가 있는 거에 애러 
x = matrix(as.numeric(x4[-(1:2)], 200, 2, byrow = T))
           ; x[1:3]
x4 = scan(file=paste0(path, "input_h.txt"))
?read.table

dat = read.table(file="input_noh.txt")
dat2 = read.table(file="input_noh.txt", header = T)
dat3 = read.table(file="input_h.txt", header = F)
dat4 = read.table(file="input_h.txt", header = T)
dat5 = read.table(file="input_h.txt", header=F, stringsAsFactors = F)
dat3[1:6,1]
as.numeric(dat3[1:6,1]) #이게 팩터변환되서 숫자가 이상하게 바뀌는거임
dat5[1:6,1]
head(dat) #위에서부터 6번째 줄 보여줌
head(dat2) #데이터 한 줄이 변수명이 되서 잃어버림
head(dat3,4)
head(dat4)
tail(dat)

i=0
while(1) #while은 괄호안에가 참이면 밑에께 돌아감
{
  i=i+1
  cat('\n', i)
}
x <- 1:10
cat(x, file = "x.txt",append=F, sep="\t") #append = T 면 기존에 있던 파일에 이어서 씀
cat('\t', file,"x.txt", sep = "", append = T)
cat(x, sep="\t")
cat("\n", 1, "st element of x = ",x[1])

x1 <- 1:20
x2 <- rep(c("A", "B", "B", "A"), 5)
x3 <- rep(c(T,F), each = 10)
dat <- cbind(x1, x2, x3)
dat <- data.frame(x1, x2, x3)

write.table(dat, file="test1.txt", row.names=T,
             col.names = T, quote = T, sep = "\t")
write.table(dat, file="test2.txt", row.names=F,
            col.names = F, quote = F, sep = "\n")
write.table(dat, file="test3.csv", sep = ",", row.names = T)
write.table(dat, file="test4.txt", row.names=T,
            col.names = T, quote = T, sep = "\t")

system("Java -version")
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_201')
install.packages("xlsx")
library(xlsx)
dat = read.xlsx(file = 'food.xlsx', sheetIndex = 1, as.data.frame = T, header = T, startRow = 1)
head(dat)
x1 = data.frame(a = rnorm(10))
x1[2,1] = NA
write.xlsx(x1, file= 'test.xlsx', sheetName = as.character(1),
           col.names = T, row.names = F, append = F, showNA = T )
write.xlsx(x1, file= 'test.xlsx', sheetName = as.character(2),
           col.names = T, row.names = F, append = T, showNA = F )
for(i in 3:10)
  write.xlsx(x1, file = 'test.xlsx', sheetName = as.character(i),
             col.names = T, row.names = F, append = T, showNA = F)

save(list = c("dat"), file = "ex_dat1.Rdata")
save.image(file="ex_dat2.Rdata")
rm(list = ls())
load(file = "ex_dat1.Rdata")
ls()
load(file = "ex_dat2.Rdata")
ls()

v <- c("a,b,c,d,e", "a,b", "d,e,f")
lapply(v, nchar)
sp = strsplit(v,','); sp
lapply(sp, length)
lapply(sp, paste0, 3)
v <- c("a,b,c,d,e", "a,b", "d,e,f")
sapply(v, nchar)
sp
lapply(sp, "[[", 2) #함수자리에 "[[" 해주면 두 번째 꺼 출력해줌
sapply(sp, "[[", 2)
sapply(sp, length)
x = list(a=1:10, b=rnorm(20))
sapply(x, quantile, c(0.25, 0.5, 0.75))

n<-10
fac <- factor(rep_len(1:3, n), levels = 1:5) #rep_len 길이가 n이 될 때 까지 반복
length(fac)
x <- 1:n
table(fac)
cbind(x,fac)
tapply(1:n, fac, sum) #그룹별로 연산할 때
sum(x[fac==1])
