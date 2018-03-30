### r tutorial ######
library(dplyr)
getwd()
setwd('/Users/catherine.t/Documents/scw_2018/intro_R/')

#### data wangling with dplyr ####
gapminder<- read.csv('gapminder-FiveYearData.csv.txt')

gapminder2<- gapminder %>%
  select(continent, country,lifeExp,gdpPercap,year) %>%
  filter(continent=='Europe',year>1976 & year<1981)

## get total by group
 gapminder %>% 
  group_by(country)
   tally() %>%      
  
     ## get mean by group and sort 
 gapminder %>% 
     group_by(country) %>% 
     summarise(avg=mean(lifeExp),std=sd(lifeExp),total=n(),min=min(lifeExp),max=max(lifeExp)) %>% 
     arrange(avg) %>% # sort by average ascending order(default) arrange(desc(avg)) for descending order
     filter(avg==min(avg) | avg==max(avg))    

   head(desc)
   tail(desc)

####main command of dplyr : select, filter, group_by, summarise, mutate, arrange
   #### mutate in dplyr
   gapminder_mod<- gapminder %>% 
     mutate(gdp=pop*gdpPercap) 
   head(gapminder_mod)

### plotting
   plot(x=gapminder_mod$gdpPercap,y=gapminder_mod$lifeExp)

### ggplot
library(ggplot2)
   
   ### basic ggplot
   ggplot(gapminder,aes(x=log10(gdpPercap),y=lifeExp))+
     geom_point()
   
   ### modify ggplot
   ggplot(gapminder,aes(x=log10(gdpPercap),y=lifeExp,color=continent))+
     geom_point(alpha=1/3,size=3) ## alpha for transparency 
   
p<- ggplot(gapminder,aes(x=log10(gdpPercap),y=lifeExp,color=continent))+
  geom_point() + facet_wrap(~continent) ##  split by continent
p + geom_smooth(method = 'loess',color='orange')
   
## ggplot and dplyr
gapminder_mod %>% mutate(loggdp= log10(gdp)) %>% 
  filter(continent=='Africa') %>%
  ggplot(aes(loggdp,lifeExp,color=country)) + geom_point() + facet_wrap(~country)+
  geom_smooth(method='leoss') 


### samething with hist
ggplot(gapminder,aes(lifeExp,fill=continent))+
  geom_histogram(binwidth = 1)

gapminder_mod %>% mutate(loggdp= log10(gdp)) %>% 
  filter(continent=='Africa') %>%
  ggplot(aes(lifeExp,fill=country))+
  geom_histogram(binwidth = 1) + facet_wrap(~country)

### save the file
ggsave(p,file='plot_gy_country.png')

##### ggplot point 
p<-ggplot(gapminder_mod,aes(x=year,y=lifeExp,color=continent))+geom_point()
p<-p+facet_wrap(~continent)
p2<-p+geom_smooth(method='loess',color="orange")
p2

###using gritExtra
library(gridExtra)
gridExtra::grid.arrange(
  p3<-ggplot(gapminder_mod,aes(lifeExp,year)) +geom_point()+ facet_wrap()
  
  p4<-ggplot(gapminder_mod,aes(gdpPercap,lifeExp)) +geom_point(size=0.25)+ geom_density_2d()+
    scale_x_log10()
)
###################
### creating loop###
####################

contin<-unique(gapminder_mod$continent) ## to get the unique observation

#### loop with dplyr
for(i in contin) {
  res<-gapminder_mod %>% filter(continent==i) %>%
    summarise(avg=mean(lifeExp))
  print(paste0('average life exp of',i,' is:',res))
} ### can get the same things with dplyr

#### writing a function in r ######
mean(2,3)
## basic function syntax
adder<-function(x,y){
  return(x+y)
}
adder(2,3) ### the function adder take 2 and 3 and execute as specify(x+Y> 2+3)

adder<-function(x,y){
  print(paste0('the sum of ',x,'and ',y,' is :',x+y))
  return(x+y)
}
new_data<-adder(gapminder_mod$year,gapminder_mod$lifeExp)


#everything in hagtag will be ignored
cats<- 10 #creazy cat lady
cats-9 #answers

## R function to be avoid in creating variable : c, C, F, t, T, S

### data type in R : 6 mains (character, interger, numeric, complex numbers, logical, factor)
class(cats)# function to ask the type of objects
typeof(cats) ## object data type (double as class of object mean numeric or integer)
i<- 11L
j<- 2

class(j)
typeof(j)

### complex number 

k<- i+4i

## data structures
logical_vector <- c(TRUE,TRUE,FALSE,TRUE)
class(logical_vector)

anyNA(variable) ## to see if there is anymissing value in my variable/vector

mixed<- c('True',TRUE)
class(mixed)

anotherv<- c('Stanford',TRUE,2,2L,3.4,2+5i)
mylist<-list(char='cat',num=c(2.2,4.05,5),int=c(1:5),log=T,lists=list(me=1:2,quoi=rep(3+4i,4)))
mylist
str(mylist)
data<-list(day=1:5, magn=c(2,10,5,2,5),obs=c('Gr', 'Death','no change','Gr', 'Death'))

ma<- matrix(nrow=2,ncol=3)
class(ma)
ma<-matrix(data=1:6,nrow=2,ncol=3) #data=1:6 create value from 1 to 6 and fill into the matrix by col (default) 
ma
ma<-matrix(data=1:6,nrow=2,ncol=3,byrow = T)
ma

data<-data.frame(day=1:5, magn=c(2,10,5,2,5),obs=c('Gr', 'Death','no change','Gr', 'Death'))
str(data)

r<-factor(c('high','medium','low','low'))

###import data frame
gapminder<- read.csv('gapminder-FiveYearData.csv.txt')
str(gapminder)
