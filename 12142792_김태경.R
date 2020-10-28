#Practice 4
setwd("D:/")
raw_dat <- read.csv("Ex_data.csv", header = T, stringsAsFactors = F) #Practice 4-1
head(raw_dat) #Practice 4-2
dim(raw_dat) #Practice 4-3
write.table(raw_dat[,1], file = "./gene_name.txt",
            row.names = F, col.names = F, quote = F) #Practice 4-4
write.table(raw_dat[,c(3,9,10)], file = "./sub_data.txt", quote = F, sep = "\t") #Practice 4-5
raw_dat2 <- raw_dat[seq(32,100,2),seq(22,40,2)] #Practice 4-6
write.table(raw_dat2, file = "./sub_data2.csv", sep = ',') #Practice 4-6
sum(is.na(raw_dat)) #Practice 4-7
which(is.na(raw_dat)) #Practice 4-8
which(is.na(raw_dat), arr.ind = T) #Practice 4-9
#Practice 4-10
index <- which(is.na(raw_dat), arr.ind = T)
cm <- apply(raw_dat[,-1], 2, mean, na.rm = T)
length(cm)
for(i in 442:1)
{
  cm[i+1] <- cm[i]
}
length(cm)  
raw_dat[index] <- cm[index[,2]]

#Practice 6
#Practice 6-2
s <- 0
for(i in 100:1)
{
  s <- i+s
  if(s>3000)
  {
    print(i)
    print(s)
    break
  }
} 

#Practice 6-4
s <- 0
for(i in 30:50)
{
  if(sum(i%%(1:i)==0)!=2) next
  s <- s+i
}
s

#Practice 6-5
s <- 0
x = runif(1e7, 0, 100)
tic <- Sys.time()
for(i in 1:1e7)
{
  if(x[i]>5 & x[i]<20) s <- s+x[i]
}
toc <- Sys.time()
as.numeric(toc - tic, units = 'secs')

#Practice 7
#Practice 7-1
vectorman = function(v)
{
  return(list(length=length(v),sum=sum(v),
              prod=prod(v), min=min(v), max=max(x),
              mean=mean(v), var=var(v)))
}
x<-1:10
vectorman(x)

#Practice 7-2
vec_switch = function(v, a)
{
  switch(a, vectorman(v)[1], vectorman(v)[2], vectorman(v)[3],
         vectorman(v)[4], vectorman(v)[5], vectorman(v)[6], vectorman(v)[7])
}  
vec_switch(x, 1)
vec_switch(x, 2)
vec_switch(x, 3)
vec_switch(x, 4)
vec_switch(x, 5)
vec_switch(x, 6)
vec_switch(x, 7)

#Practice 7-3
vec_switch = function(v, a=3)
{
  switch(a, vectorman(v)[1], vectorman(v)[2], vectorman(v)[3],
         vectorman(v)[4], vectorman(v)[5], vectorman(v)[6], vectorman(v)[7])
}
vec_switch(x)

#Practice 7-4
root <- function(a,b,c)
{
  if(a==0) 
  {
    return(cat('x=', -c/b))
  } else {
  D <- b^2-4*a*c
  if(D<0) d <- 1
  if(D==0) d <- 2
  if(D>0) d <- 3
  switch(d, cat('imaginary roots','x=',(-b+sqrt(D+0i))/(2*a),
                'or', 'x=', (b+sqrt(D+0i))/(2*a)),
                cat('x=', -b/(2*a)),
                cat('x=',(-b+sqrt(D))/(2*a),
                    'or', 'x=', (b+sqrt(D)/(2*a))))
  }
}