#0113

# ggplot2
library(ggplot2)

head(mtcars)

p <- ggplot(mtcars, aes(wt, mpg, color=cyl));p #아무것도 안뜨지만 x축y축만 정의 되어있음. 매핑만 되있음
p <- p + geom_point()
print(p2)
p # print(p)

attributes(p)

p$data
p$layers
p$scales
p$mapping
p$theme
p$coordinate
p$facet
p$plot_env
p$labels
p$labels$x <- 'Weight'
p$labels$y <- 'MPG'
p$labels$colour <- 'Cylinder'

summary(p)

p <- ggplot(mtcars, aes(factor(cyl), fill=factor(cyl))) #fill 실린더에 따라서 색을 다르게 주겠다.
x11()
p

p <- p + geom_bar(width=.5)
x11()
p

p <- p + facet_grid(. ~ gear) #기어마다 다르게 그리드 형태로 저장
x11()
png(file = 'd:/gg_test.png')
p
dev.off() #이래야 저장 되는건 알지?
graphics.off()
unlink('d:/gg_test.png', recursive = T) #지우기


# aes
#Global mapping
p <- ggplot(data=iris,aes(x=Sepal.Length, y=Sepal.Width))
p <- p + geom_point(aes(color=Species))
p + geom_point(aes(color=Species))
x11()
p
#Local mapping
p <- ggplot(data=iris)
p <- p + xlab("Length") + ylab("Width")
p <- p + geom_point(mapping=
                      aes(x=Sepal.Length, y=Sepal.Width), colour="blue", pch=19)
p <- p + geom_point(mapping=
                      aes(x=Petal.Length, y=Petal.Width), colour="red", pch=17)
x11()
p




summary(p)

p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p = p + geom_point(colour="orange", size=3)
p <- p + xlab("Weight") + ylab("MPG(miles/gallon") #+ xlim(c(3,5)) + ylim(c(10,15))
p

p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p = p + geom_point(aes(colour=cyl, size=gear))
p


#geom_line
x11()
p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p + geom_line()
x11()
p2 <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p2 + geom_line(linetype=2,size=2,color=3)


# geom_abline
mw_coef = coef(lm(mpg~wt,mtcars))
p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p <- p + geom_point()
p
p <- p + xlim(1, 5) + ylim(10, 35) + geom_point()

p + geom_abline(intercept = mw_coef[1], slope = mw_coef[2],color=4)
p + geom_vline(xintercept = 3,color=2) +
  geom_hline(yintercept = 20,color=2)

# geom_smooth
p <- ggplot(data=mtcars, aes(x=wt, y=mpg, color = factor(cyl)))
p = p + geom_point()
p + geom_smooth() #회색은 신뢰구간
p + geom_smooth(aes(linetype=factor(cyl))) #신뢰구간 넓은 거는 데이터가 별로 없어서


# geom_bar
p <- ggplot(data=mtcars, aes(factor(cyl)))
p + geom_bar(fill="steelblue",color='black')
p + geom_bar(aes(fill=factor(gear)),color='black')

# geom_area

huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
p = ggplot(data=huron, aes(x=year,y=level))
p + geom_area()
range(huron$level)
p + geom_area(fill='steelblue') +
  coord_cartesian(ylim=c(min(huron$level)-2,max(huron$level)+2))

p = ggplot(data=huron, 
           aes(x=year,y=level,
               ymin=huron$level-2,
               ymax=huron$level+2))
p + geom_ribbon(fill='steelblue') #이건 레인지범위 색칠


# Exercise

setwd('C:\\Users\\student\\Downloads')

kospi = read.table(file='kospi_data.txt',header=T,sep='\t')
head(kospi)
kospi[,1] = as.Date(kospi[,1]) #날짜형식으로
p <- ggplot(data=kospi, aes(x=Date, y= CurrentIndex, ymin = Lowest, ymax=Highest))
p <- p + geom_ribbon(fill = "lightblue")
p + geom_point() + geom_line()

# geom_boxplot

p <- ggplot(mtcars, aes(factor(cyl), mpg))
p + geom_boxplot()
p + geom_boxplot(aes(fill=factor(carb)))

# geom_histogram
movies = read.csv(file='movies.csv',header=T)
head(movies)
dim(movies)
p <- ggplot(data=movies, aes(x=rating))
p + geom_histogram()
p + geom_histogram(binwidth=1, aes(y=..density..)) #..이거 density로 그리는거. 그리는 방법

p + geom_histogram(binwidth=1,
                   aes(y=..density.., fill=..count..)) +
  geom_density(color='red') +
  scale_fill_gradient(low='white',high='#496ff5')

# geom_density
p <- ggplot(movies, aes(x = rating))
p + geom_density()
p + geom_density(aes(fill=factor(mpaa)), alpha=0.25)

# geom_text

p <- ggplot(mtcars, 
            aes(x=wt, y=mpg, 
                label=rownames(mtcars)))
p <- p + geom_point()
p + geom_text(aes(x=wt+0.05, 
                  color=factor(cyl)),size=5)

library(reshape2)
library(mapproj)
library(maps)
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
head(crimes)

states_map <- map_data("state")
head(states_map)

p <- ggplot(crimes, aes(map_id=state))
p <- p + geom_map(aes(fill=Murder), map=states_map)
p <- p + expand_limits(x=states_map$long, y=states_map$lat)
p + coord_map()

#ECDF 
df <- data.frame(x = c(rnorm(100, 0, 2),rnorm(100, 1, 4)), g = gl(2, 100))
p <- ggplot(df, aes(x, color = g))
p + stat_ecdf()
p + stat_ecdf(geom="line", size=1)

# stat_function

dat <- data.frame(x = rnorm(100))
p <- ggplot(dat, aes(x = x))
p <- p + geom_density(fill = "green",
                      alpha = 0.15)
p + stat_function(fun = dnorm, 
                  color = "red", fill="red", 
                  alpha=0.15, geom="area")

# coord_cartesian

p <- ggplot(data=mtcars, aes(x=disp, y=wt))
p <- p + geom_smooth()
p
p + coord_cartesian(xlim=c(325, 500), ylim=c(3,6))

#coord_flip xy축 뒤바꿔서 그려주는거
p <- ggplot(mtcars, 
            aes(factor(cyl), mpg))
p + geom_boxplot() +
  coord_flip()

p <- ggplot(mtcars, 
            aes(factor(cyl)))
p + geom_bar(fill="steelblue",
             color='black') +
  coord_flip()

# coord_fixed()

x <- c(500, 350, 700, 600, 400)
y <- c(10, 20, 30, 30, 20)
dat <- data.frame(x, y)
p <- ggplot(data=dat, aes(x=x, y=y))
p <- p + geom_point(size=5)
p
p + coord_fixed(ratio=1)

# coord_map

library(maps)
world <- map_data("world")
head(world)
korea <- world[grep("Korea$", world$region),]
head(korea)
p <- ggplot(korea, aes(x=long, y=lat, group=group))
p <- p + geom_polygon(fill="white", colour="black")
p
p + coord_fixed(ratio=1)

#coord_trans
p <- ggplot(data=diamonds, 
            aes(x=carat, y=price, colour=factor(cut)))
p <- p + geom_point()
p
p + coord_trans(x = "log10", y = "log10")

# gg map
install.packages('ggmap')
library(ggmap)

register_google(key = "AIzaSyCYVybXv9ATgtHmpJe_LsnyBo3iJ3L7MPo")

has_google_key()
google_key()

lon = 126.653
lat = 37.45
map = get_googlemap("Incheon",zoom=12,maptype="roadmap",
                    markers=data.frame(126.6,lat))
ggmap(map)

# get_map
setwd('C:\\Users\\student\\Downloads')
food = read.csv(file='food.csv',header=T)
head(food)
loc = geocode("Incheon")
loc = as.numeric(loc)
loc = c(lon,lat)
map = get_map(loc,zoom=13,maptype='satellite')
p = ggmap(map, extent='device') + 
  geom_point(
    aes(x=Lon,y=Lat,size=Rating,fill=Rating),
    alpha=0.6,pch=21,data=food) +
  scale_size(range=c(0,10)) +
  geom_text(aes(x=Lon+0.001,y=Lat+0.001,
                label=Name),color="white",size=5,data=food)
p


# Wordcloud in R

install.packages(c('tm', 'SnowballC', 'wordcloud'))
library(tm)
library(SnowballC)
library(wordcloud)

jeopQ <- read.csv('JEOPARDY_CSV.csv', stringsAsFactors = FALSE) 

### Data Cleaning
jeopCorpus <- Corpus(VectorSource(jeopQ$Question))
jeopCorpus <- tm_map(jeopCorpus, removePunctuation) ## . , ? ! ()
jeopCorpus <- tm_map(jeopCorpus, removeWords, 
                     c("the", "this", "The", "This", stopwords('english')))
## stopwords("en")
jeopCorpus <- tm_map(jeopCorpus, stemDocument)  ## went -> go

x11()
wordcloud(jeopCorpus, max.words = 100, random.order = FALSE) 
