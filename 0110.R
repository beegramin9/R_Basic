
library(lattice)
?barley
barley
x11()
trellis.par.set(theme = col.whitebg())
dotplot(variety ~ yield | site, data = barley, groups = year,
        key = simpleKey(levels(barley$year), space = "bottom", columns = 2),
        xlab = "Barley Yield (bushels/acre) ", ylab = "Variety",
        aspect=0.5, layout = c(2, 3),
        scales = list(alternating = FALSE,
                      y = list(abbreviate = TRUE, minlength = 5),
                      font = 4, col = "blue"),
        main = "dotplot(variety ~ yield | site, data = barley, groups = year)",
        sub = list("arguments = key, aspect, scales, etc", col = "gray")
)




attach(anscombe)
anscombe
cal_corr = c(cor(x1,y1),cor(x2,y2),cor(x3,y3),cor(x4,y4))
names(cal_corr) = paste(paste("x",1:4,sep=''),paste("y",1:4,sep=''),sep=' vs. ')
cal_corr

res = list()
res[[1]] = lm(y1~x1)
res[[2]] = lm(y2~x2)
res[[3]] = lm(y3~x3)
res[[4]] = lm(y4~x4)
summ = matrix(0,4,4)
cname = paste(paste("x",1:4,sep=''),paste("y",1:4,sep=''),sep=' vs. ')
colnames(summ) = cname
rownames(summ) = c("estimated beta0", "estmated beta1","estiamted error variance","R-squre")
for(i in 1:4)
{
  temp = summary(res[[i]])
  summ[1:2,i] = temp[[4]][1:2,1]
  summ[3,i] = temp[[6]]
  summ[4,i] = temp[[8]]
}
summ


x11()
par(mfrow=c(2,2)) #parameter조정 or 참 multi figure 가로로 채움(그래프 여러개 그림)
# mfcol는 원래대로 by col
for(i in 1:4)
{
  plot(anscombe[,i],anscombe[,i+4],pch=16,col=2,main=cname[i],
       xlab=paste("x",i,sep=''), #paste0 써도됨
       ylab=paste("y",i,sep=''), 
       xlim=c(4,20),ylim=c(3,13))
  abline(summ[1:2,i],col=4,lwd=2)
}

x11()
plot(1:5, rep(1,5), type = 'n', main='A', xlim = c(0,5), ylim = c(0,1))

x11()
plot(1,1, type = 'n', xlim = c(0,5), ylim = c(0,1), main = 'B')



col.table <- function(cols, main=NULL, fg=NULL) {
  n <- length(cols)
  plot(seq(n), rep(1, n), xlim = c(0, n), ylim = c(0, 1),
       type = "n", xlab = "", ylab = "", axes = F) #type='n' none임 아무것도 안 그리는거
  if(is.null(main)) 
  {  
    main=paste("Color Table by", deparse(substitute(cols)))
  }
  title(main=main)
  
  if(is.null(fg))
  {
    fg = unlist(lapply(cols,
                       function(x)
                         ifelse(mean(col2rgb(x)) > 127 | toupper(x) %in% #col2rgb 색깔 숫자로 나타내줌
                                  c("WHITE", "#FFFFFF"), "black", "white")))
  } else
    fg = rep(fg, n)
  
  for(i in 1:n)
  {
    polygon(c(i - 1, i - 1, i, i), c(0.05, 1, 1, 0.05), col = cols[i]) #polygon 좌표 순서대로 선이어서 다각형 그려줌
    text(mean(c(i - 1, i)), 0.52, labels = cols[i], srt=90, adj=0.5, col=fg[i], cex=1.2)
  } #srt string rotation 돌리는거 adj=0.5 가운데 정렬
}
par(mfrow=c(2,1))
col.table(1:16)
col.table(5:20)


col_name = colors()[1:5]
col_name
col_rgb = col2rgb(col_name)
colnames(col_rgb) = col_name
col_rgb
grep("red",colors(),value=T)
col.table(col_name)


cols <- colors()
length(cols)
cols[1:5]
grep("sky", cols, value=TRUE)
col2rgb(grep("sky", cols, value=TRUE))
par(mfrow=c(1, 1), mar=c(1, 1, 3, 1)) #mar margine 여백 백터 순서는 6,9,12,3시
col.table(grep("orange", cols, value=TRUE))
col.table(grep("red", cols, value=TRUE))


col.map <- function(cols=colors())
{
  n <- length(cols)
  cnt <- floor(sqrt(n)) #내림함수
  plot.new()
  plot.window(xlim=c(0, cnt), ylim=c(0, cnt)) #plot(type='n')이랑 똑같음
  
  for (i in 1:cnt) for (j in 1:cnt)
    rect(i-1, j-1, i, j, col=cols[(i-1)*cnt +j], border=NA)
  #rectangular(a,b,c,d) (a,b) (c,d) 직사각형 그림
}
col.map(colors())


seqs <- seq(0, 255, length = 15)
seqs
hexs <- toupper(as.character.hexmode(seqs)) #16진수문자로 바꿔줌
hexs
rev(hexs) #reverse 
red <- paste("#", hexs, "0000", sep = "")
green <- paste("#00", hexs, "00", sep = "")
blue <- paste("#0000", hexs, sep = "")
mix1 <- paste("#", hexs, hexs, hexs, sep = "")
mix2 <- paste("#", rev(hexs), hexs, rev(hexs), sep = "")
par(mfrow=c(1, 1), mar=c(1, 1, 3, 1))
col.table(red)
col.table(green)
col.table(blue)
col.table(mix1)
col.table(mix2)

x = c("#332211","#130015","#AA4455")
col.table(x)

r = seq(50,200,length=10)
g = 255
b = c(10,20)
hr = as.character.hexmode(r)
hr
toupper(hr)
x = paste0("#",toupper(hr),toupper(as.character.hexmode(g)),
           toupper(as.character.hexmode(b))[1],"1B")
x
col.table(x)



rainbow(20)
par(mfrow = c(6, 1), mar = c(0, 0, 2, 0))
col.table(rainbow(20))
col.table(heat.colors(20))
col.table(terrain.colors(20))
col.table(topo.colors(20))
col.table(cm.colors(20))
col.table(gray(seq(0,255,length=20)/255))


par(mfrow = c(2, 2), mar = c(0, 0, 2, 0))
col.map(rainbow(400, start = 0, end = 0.8))
col.map(heat.colors(400))
col.map(cm.colors(400))
col.map(topo.colors(400))


seqs <- seq(0, 255, length = 20)
alpha <- toupper(as.character.hexmode(seqs))
par(mfrow = c(5, 1), mar = c(0, 0, 2, 0))
col.table(paste("#FF0000", alpha, sep = ""), fg = 1)
col.table(paste("#00FF00", alpha, sep = ""), fg = 1)
col.table(paste("#0000FF", alpha, sep = ""), fg = 1)
col.table(rainbow(20), main = "Alpha Channel 사용 안함")
col.table(rainbow(20, alpha = seq(0, 1, length = 20)),
          main = "Alpha Channel 사용", fg=1)


x <- c(1, 1.3, 1.6)
y <- c(1, 2, 1)
par(mar = c(4, 2, 3, 1), mfrow = c(1, 2))
plot(x, y, pch = 16, cex = 20, col = c("red", "green", "blue"),
     xlim = c(0,3), ylim = c(-2, 5))
title(main = "col=c('red','green','blue')")
plot(x, y, pch = 16, cex = 20, col = c("#FF000022", "#00FF0022", "#0000FF22"),
     xlim = c(0, 3), ylim = c(-2, 5))
title(main = "alpha channle by \"77\"")


par(mfrow=c(2,2))
par(mar=c(0.1,0.1,1,0.1))
hsv(0.5, 0.5, 0.5) 
hsv1 <- c(hsv(0.5, 0.5, 0.5), hsv(0.6, 0.5, 0.5),
          hsv(0.7, 0.5, 0.5), hsv(0.8, 0.5, 0.5))
hsv2 <- c(hsv(0.5, 0.5, 0.5), hsv(0.5, 0.6, 0.5),
          hsv(0.5, 0.7, 0.5), hsv(0.5, 0.8, 0.5))
hsv3 <- c(hsv(0.5, 0.5, 0.5), hsv(0.5, 0.5, 0.6),
          hsv(0.5, 0.5, 0.7), hsv(0.5, 0.5, 0.8))
hsv4 <- c(hsv(0.5, 0.5, 0.5), hsv(0.6, 0.6, 0.6),
          hsv(0.7, 0.7, 0.7), hsv(0.8, 0.8, 0.8))
col.map(hsv1)
title("hsv1")
col.map(hsv2)
title("hsv2")
col.map(hsv3)
title("hsv3")
col.map(hsv4)
title("hsv4")
hsv1
hsv2
hsv3
hsv4


reds1 <- rgb((0:15)/15, g = 0, b = 0)
reds2 <- rgb((0:15)/15, g = 0, b = 0, alpha = 0.5)
gray1 <- gray(0:8/8)
gray2 <- gray(0:8/8, alpha = 0.5)
reds1
reds2
gray1
gray2

par(mfrow = c(2, 2), mar = c(1, 3, 1, 1))
col.map(reds1)
title("rgb((0:15)/15, g=0, b=0)")
col.map(reds2)
title("rgb((0:15)/15, g=0, b=0, alpha=0.5)")
col.map(gray1)
title("gray(0:8/8)")
col.map(gray2)
title("gray(0:8/8, alpha=0.5)")


rm(list=ls())
setwd("C:\\Users\\Taekyeong\\Desktop\\나\\소프트웨어 실습")
ls()

# png device

png(filename='./test.png')
a = 1:10
b = 2*a + rnorm(10)
plot(a,b)
dev.off() #디바이스를 닫아야 저장됨

# jpeg
x11()
dev.copy(jpeg,filename='test2.png')
dev.off()
getwd() #여기 저장됨

# dev
x11()
plot(1:3)
x11()
plot(6:4)
dev.cur()
dev.prev()
dev.set(4) #활성화 시키는거 보면active되있음
dev.set(3)
dev.off(2)
dev.off(3) #출력값은 디바이스 끄면서 나머지 디바이스 중에 활성화된 디바이스



