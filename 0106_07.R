#0106

.Platform
setwd("C:\\Users\\Taekyeong\\Documents\\R")
raw_dat = read.csv(file="Ex_data.csv",
                   header=T,stringsAsFactors=F)
#여러개 있는 데이터는 평균내서 하나로 만드는게 이 데이터 목적
#1. 수치(1열)/문자(나머지 열) 분리
#2. 수치 전치(트랜스포즈) 통계 작업할 때 변수를 열에 놓고 샘플을 행에 놓는게 일반적이라서
#3. 변수명에 유전자 이름을 넣음
#4. 같은 유전자 이름을 갖는 변수를 평균내서 하나로 만듬

gr_ind = gl(2, 221) #group label 그룹의 수 gl(두개의 레벨, 그룹하나당 221개)

dat_mat <- t(as.matrix(raw_dat[,-1]))
dim(dat_mat)
rownames(dat_mat) <- paste0("S",1:nrow(dat_mat))
colnames(dat_mat) <- raw_dat[,1]
head(dat_mat[,1:10])
#3까지 한거 
#4아이디어: 데이터 정리할 때 컬럼이름 유일하면 그냥 집어넣고 두 개 이상이면 평균내서

indx <- which(is.na(dat_mat),T)
indx
nrow(indx)
col_ind = indx[,2]
col_m = apply(dat_mat[,col_ind],2,mean,na.rm=T)
col_m
dat_mat[indx] = col_m
sum(is.na(dat_mat))
dim(dat_mat)




uq_names<- unique(colnames(dat_mat)) #유일한 거 찾는거
p <- length(uq_names)
n <- dim(dat_mat)[1]
expr_dat <- matrix(0,n,p)
for(i in 1:p)
{
  expr_dat[,i] = apply(as.matrix(dat_mat[,colnames(dat_mat)==uq_names[i]]),
                       1,mean)
  cat("\n",i,"-th step")
}

colnames(expr_dat) <- uq_names
rownames(expr_dat) <- rownames(dat_mat)
head(expr_dat[,1:20])
dim(expr_dat); sum(is.na(expr_dat))



#Set working directory
# setwd('./')
# Read a dataset
dat = read.table("ex211.txt",header=T,sep='\t')
head(dat)
dat$Job
attach(dat) #데이타 프레임의 변수명을 Global Env에 등록해줘서 dat$Job이렇게 안치고 Job만 쳐도 됨
Job
x11()#windows()
plot(Job, main="직업의 막대그림", xlab='직업',
     ylab="인원수(명)", ylim=c(-5,20))
#main은 타이틀, x label x축 이름, y limit y축 범위
box() #그림에 테두리 만들어 줌

freq = table(as.character(Job)) #dat[,6])
x11()
#png(file='d:/bar.png')
#pdf(file = "d:/bar.pdf") #이거 dev.off랑 세트트 닫는거처럼 
barplot(freq,main="직업의 막대그림", xlab='직업',
        ylab="인원수(명)", ylim=c(-5,20),xlim=c(-2,7))
box()
#남여남여남남 이런거 그릴려면 barplot(c(4,2)) 이런식으로 세서 넣어야함
#그냥 plot으로 할려면 x<-factor(c(남,여,남,여,남,남)) 이걸 팩터처리 해줘야함 plot(X)
points(0.5, 1, pch = 16) #이거 점 찍는거 points(x,y) (x,y) 찍고 특성들
points(1, 1, pch = 16, col = 2)
points(0.6, 10, pch = 50, col = 2) 
text(0.75, freq[1]/2, as.character(freq[1]), col = 'white') #이거도 (x,y)찍고 어떤거 쓸지
text(1.95, freq[2]+0.5, as.character(freq[2]), cex = 1.5) #cex 글자크기
#dev.off() #여기까지만 저장. 안닫아주면 계속 기달려서 계속 기록됨.

detach(dat) #이러면 attatch 해제도됨


install.packages('gplots')

data(VADeaths)
?VADeaths
VADeaths
gray(seq(0,1,length=11)) #RGB빛의 삼원색을 0~255가 있는데 16진수로 표현
#A=10, B=11, ..., F=15 회색은 RGB를 동일한 비율로 섞으면 나옴
#09A0F0 이면 RED 16*0+9=9 GREEN 10*16+0=160 BLUE 15*16+0=240
barplot(rep(1,11), col = gray(seq(0,1,length.out = 11))) #0~255를 0~1로 본거
library(gplots)
x11()
barplot2((VADeaths), beside = TRUE, 
         col = gray(seq(0.4,0.9,length=4)),
         legend = rownames(VADeaths),
         ylim = c(0, 100))
#beside T 면 누적, plot.ci confidence interval(신뢰구간) 그래프 사이 간격, legend는 범주인듯
title(main = "Death Rates in Virginia",font.main = 4) #plot안에 main이랑 같은거

x11()
barplot2(t(VADeaths), beside = TRUE, 
         col = gray(seq(0.4,0.9,length=5)),
         legend = rownames(t(VADeaths)),
         ylim = c(0, 100))
title(main = "Death Rates in Virginia",font.main = 4)
#남성은 rural이 사망률이 높은데 여성은 불규칙



hh <- t(VADeaths)[,5:1]
mybarcol <- "gray20"
ci.l <- hh * 0.85 #confidence interval lower bound
ci.u <- hh * 1.15 #confidence interval upper bound
x11()
mp <- barplot2(hh, beside = TRUE, 
               col = gray(seq(0.4,0.9,length=5)), 
               legend = colnames(VADeaths), 
               ylim = c(0, 100), 
               main = "Death Rates in Virginia", 
               font.main = 4,
               sub = "Faked 95 percent error bars", 
               cex.names = 1.5, 
               plot.ci = TRUE, 
               ci.l = ci.l, ci.u = ci.u, plot.grid = TRUE) #plot.grid 이거 참조선(저 점선)
box()



rainbow(24) #RGB에 자리가 하나 더 생김 Alpha channel 투명도임
barplot(rep(1,6), col = paste0("#FF0000",c("00", "11", "33", "95", "99", "FF")))
x11()
pie(freq, main="직업의 원그림")
pie(rep(1, 24), col = rainbow(24), radius = 0.9) #radius 반지름 길이
pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
lbl =  c("Blueberry", "Cherry",
         "Apple", "Boston Cream", "Other", "Vanilla Cream")
names(pie.sales) = paste0(lbl," (",pie.sales*100,"%)")
pie(pie.sales, col=rainbow(length(pie.sales)))
graphics.off() #그래픽 디바이스 다 닫아줌

# histogram
x <- expr_dat[,10]
x11()
hist(x,breaks= 20,col="gray",main=uq_names[10]) #breaks 구간 자르는 건데 20개 정도로 가장 적합하게 잘라줌
hist(x,breaks= 40,freq=F,col="lightblue",main=uq_names[10]) #freq가 네모 넓이 합 1로 만들어줌
hist(x,breaks= 40,plot=F) #plot을 끄면 그림을 안 그리고 설명
#이거 이용하면 히스토그램에서 내가 원하는영역에만 색칠가능

set.seed(7)
hist.data <- rnorm(100, 3, 2)
hist_info = hist(hist.data, breaks = "Sturges", plot=F)
hist_info
nbins = length(hist_info$mids) #bin이 상자임 상자갯수
col_vec = rep('white',nbins)
col_vec[hist_info$density>0.15] = 'lightblue'
x11()
hist(hist.data, breaks = "Sturges", col=rainbow(nbins)) #col_vec)

#boxplot
mat = expr_dat[,c(3,4,7,8)]
x11() 
boxplot(mat, col=rainbow(4, alpha = 0.4))
res = boxplot(mat,plot=F)
res
res$out[res$group==1]
x11()
c_name = colnames(expr_dat)
plot(expr_dat[,1],expr_dat[,2],type='l',xlab=c_name[1],ylab=c_name[2])
windows()
plot(sort(expr_dat[,3]),expr_dat[order(expr_dat[,3]),4],type='l',xlab=c_name[1],ylab=c_name[2])



# plot 
pop_dat = read.csv(file='table_2_2.csv')
x11()
plot(pop_dat[,1],pop_dat[,2],type='l',xlab='연도',ylab='인구수')
windows() #x11이랑 똑같음
plot(pop_dat[,1],pop_dat[,2],type='b',xlab='연도',ylab='인구수') #type b는 both 점선둘ㄷ
windows()
plot(pop_dat[,1],pop_dat[,2],type='o',xlab='연도',ylab='인구수') #type o는 오버랩
#line 그래프는 점 찍힌 순서에 따라 그림 (점 그래프는 똑같아 보여도 선으로 그리면 순서 때매 다를 수 있음)
#-----------------------------------------------------------------------------------------------------------

#0107
setwd('C:\\Users\\Taekyeong\\Documents\\R')
load(file='expr_dat.Rdata')
n=nrow(expr_dat)
p=ncol(expr_dat)
gr_ind = gl(2,221)
uq_names = colnames(expr_dat)

# scatter plot
x11()
ind1 = 8; ind2=12
plot(expr_dat[,ind1],expr_dat[,ind2],type='p',pch=16,
     xlab=uq_names[ind1],ylab=uq_names[ind2])
cor_mat = cor(expr_dat)
{x<-c(2.1,3.1,1.1,-1.5,3.1)
which(x==max(x)) #x의 최댓값의 인덱스 다 찾아줌
which.max(x)} #최댓값 가장 빠른 인덱스 찾아줌 which.min도있음
which.max(cor_mat[ind1,-ind1]) #자기자신 코릴레이션 가장 큰 1 이니까 뺌
#근데 8밑에 있는 애들은 인덱스가 그대로인데 8보다 크면 하나 씩 땡겨짐
#그래서 여기서 값이 199나와도원래 200 밑에서 확인
ind1 = 8; ind2=200
plot(expr_dat[,ind1],expr_dat[,ind2],type='p',pch=16,
     xlab=uq_names[ind1],ylab=uq_names[ind2])
cor(expr_dat[,ind1], expr_dat[,ind2]) #cor(vec, vec)->값 
cor(expr_dat[,c(ind1, ind2)]) #cor(matrix)->상관계수 행렬


x11()
#pairs example 산점도 행렬
ind = c(2,8,12,200)
pairs(expr_dat[,ind])
pairs(expr_dat[,ind], "Expression Data",
      pch = 21, bg = c("red", "blue")[gr_ind]) #bg background는 색깔표현
head(iris)
pairs(iris[1:4], pch = 21,
        bg = c("red", "green3", "blue")[as.numeric(iris$Species)]) #종마다 색깔구분
iris$Species #여기에 레벨이 있어서 뉴메릭시키고 벡터로 색깔 부여해준거
#저거 플랏보면 밑에있는 Petal.Length와 Petal.Width가 좋은 변수로 보임

# Stat
mean(expr_dat[,10])
median(expr_dat[,10])

x = c(1,2,3,1,2,5,5,3,3,3,2)
tb_x = table(x); tb_x
as.numeric(names(tb_x)[which.max(tb_x)])
Mode = function(vec) {
  tb = table(vec)
  return(as.numeric(names(tb)[which.max(tb)])) }
Mode(x)

quantile(expr_dat[,1],0.25)
quantile(expr_dat[,1],c(0.25,0.5,0.75))

min(expr_dat[,1])
max(expr_dat[,1])
range(expr_dat[,1])

x <- rnorm(100)
summary(x) #수치형자료는 기초통계량 계산 
y <- c('red','blue','red','white')
summary(y) #문자 서머리는 별거 뭐 없음 
f.y <- factor(y)
summary(f.y) #factor타입인 경우는 테이블함수랑 똑같음 

var(expr_dat[,1])
sum((expr_dat[,1]-mean(expr_dat[,1]))^2)/(n-1)
sd(expr_dat[,1])

# CV
height=c(72, 74, 68, 76, 74, 69, 72, 79, 70, 69, 77, 73)
sd(height)/mean(height)*100


install.packages("moments")
library(moments)
skewness(expr_dat[,1]) #왜도 치우친 정도
kurtosis(expr_dat[,1]) #첨도 뾰족한 정도
hist(expr_dat[,1],freq=F)
x = seq(5.5,8.5,length=100)
lines(x,dnorm(x,mean=mean(expr_dat[,1]),sd(expr_dat[,1])))

# contingency table

table(mtcars$cyl)
table(mtcars$am)

table(mtcars$cyl,mtcars$am)

table(mtcars$cyl,mtcars$am,mtcars$gear)

# cov , cor
cov(expr_dat[,1],expr_dat[,5])
cov(expr_dat[,c(1,5,8)])
var(expr_dat[,1])

cor(expr_dat[,1],expr_dat[,5])
cor(expr_dat[,c(1,5,8)])
cor(expr_dat[,1],expr_dat[,5]/(sd(expr_dat[,1])*sd(expr_dat[,5])))

# Reference web page
# https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/
# selectorgadjet 설치해야함
install.packages("rvest")
library(rvest)

# read_html (web page url)
# html_nodes(web_page, "css selector location")
# html_text ( html to text)

# gsub

# selectorGadget chrome

# IMDB Example
#Specifying the url for desired website to be scrapped
url <- 'https://www.imdb.com/search/title?count=100&release_date=2019,2019&title_type=feature'

#Reading the HTML code from the website
webpage <- read_html(url)
webpage

temp <- html_nodes(webpage,'.lister-item-header a')
temp #우리가 원하는건 이런게 아니야 영화제목이지 그래서 밑에 함수 씀

html_text(temp)

temp2 <- html_nodes(webpage,'.ratings-imdb-rating strong')
temp2

temptest <- html_nodes(webpage, '.runtime')
temptest <- html_text(temptest)
temptest <- gsub('min','',temptest)
temptest <- as.numeric(temptest)

temptest2 <- html_nodes(webpage, ".unbold") %>% html_text
#직접해보기
vote <- html_nodes(webpage, '.sort-num_votes-visible span:nth-child(2)') %>% html_text
vote <- as.numeric(gsub(',','',vote))

str_date = "2019-01-01"
end_date = "2019-12-31"
cnt = 100
str_num = 1
paste0("https://www.imdb.com/search/title?count=100&release_date=",
str_date, ",",end_date,'&count=', cnt,'&start=',str_num,"&red_=adv_nxt")
page = read_html(url)
temp = html_node(page,'.desc span:nth-child(1)')
num_tt = as.numeric(gsub(",","",strsplit(html_text(temp), "[ ]")[[1]][3]))
num_page = ceiling(num_tt/cnt)
mv_list = list()
for(i in 1:5)
{
  url = paste0("https://www.imdb.com/search/title?count=100&release_date=",
               str_date, ",",end_date,'&count=', cut,'&start=',str_num,"&red_=adv_nxt")
  page = read_html(url)
  
  temp <- html_nodes(page,'.lister-item-header a')
  temp = html_text(temp)
  temp2 <- html_nodes(page,'.ratings-imdb-rating strong')
  temp2 = html_text(temp2)
  mv_list[[i]] = list(temp, temp2) #data.frame(title=temp, rate = temp2)
  Sys.sleep(runif(1,0.5,2)) #서버 공격하는줄알고 차단박힐까봐 랜덤하게 좀 쉬는거
  str_num = str_num + cnt
  cat('\n Page: ', i, '/ Total: ', num_page)
}
mv_list #별점이 빠져서 다 밀려서 좆됨

#---------------------------------------------------------------------------------------------------------

dat = data.frame(html_text(temp),html_text(temp2))
head(dat)

temp3 <- html_nodes(webpage,'.sort-num_votes-visible')
t3_text = html_text(temp3)
t3_text[1:3]
spl = strsplit(t3_text,'[\n]')
vote = sapply(spl,"[[",3)
vote = as.numeric(gsub("[ ,]","",vote))
vote

#Using CSS selectors to scrap the rankings section
rank_data_html <- html_nodes(webpage,'.text-primary')
rank_data_html

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

#Let's have a look at the rankings
head(rank_data)

rank = gsub("[.]","",rank_data)
rank
as.numeric(rank_data)

#Using CSS selectors to scrap the title section
title_data_html <- html_nodes(webpage,'.lister-item-header a')

#Converting the title data to text
title_data <- html_text(title_data_html)

#Let's have a look at the title
head(title_data)

data.frame(rank=as.numeric(rank_data),title=title_data)

# https://movie.naver.com/movie/bi/mi/basic.nhn?code=136900
# iframe src="/movie/bi/mi/pointWriteFormList.nhn?code=136900&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false"
url = "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=132623&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false"
i = 1
url_temp = paste0(url,"&page=",i)
url_temp

mv_webpage = read_html(url_temp)
tt_cnt_temp = html_nodes(mv_webpage,'.score_reple p')
html_text(tt_cnt_temp)


mv_webpage = read_html(url_temp)
tt_cnt_temp = html_nodes(mv_webpage,'.total em')
v_count = html_text(tt_cnt_temp)[2]
v_count
v_count = gsub("[,]","",v_count)
tt_cnt = as.numeric(v_count)
tt_cnt

page_indx = 1:ceiling(tt_cnt/10)
length(page_indx)

rating = numeric()
u_name = character()
for(i in 1:10)
{
  w_temp = read_html(paste0(url,"&page=",i))
  r_temp = html_nodes(w_temp, ".score_result .star_score em")
  rating = c(rating, as.numeric(html_text(r_temp)))
  u_temp = html_nodes(w_temp,".score_reple a span")
  u_txt = html_text(u_temp)
  u_txt = u_txt[u_txt!="BEST"]
  u_name = c(u_name,u_txt)
  Sys.sleep(runif(1,1,3))
}

rating
data.frame(rating,u_name)

temp = gsub( ")","",u_name)
temp_list = strsplit(temp,"[(]")
res = sapply(temp_list, function(x) x[length(x)])
head(res)

x11()
plot(table(rating),type='h')



