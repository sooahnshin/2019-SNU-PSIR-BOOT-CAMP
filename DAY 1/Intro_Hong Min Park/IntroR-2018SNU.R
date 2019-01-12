########################################
## R code for 2018SNU-PSIR            ##
## Topic: Introduction to R           ##
## By Hong Min Park (hmpark1@uwm.edu) ##
## Last updated on 12/20/2018         ##
########################################


## help?

?summary
help(summary)


## working directory

getwd()
setwd("/Users/hmpark1/Dropbox/TALK/2018SNUmethods/R")  ## choose your own working directory!
dir()
getwd()


## calculator

3 + 2
3^2
(2-4)*6
2-4*6

2 ^^ 2

2 *
3

sqrt(4)
exp(3)
log(10)

x <- 5   ## a command, giving x the value 5
x = 5    ## a command, giving x the value 5
x == 5   ## a question, with answers TRUE and FALSE


## c() to enter data

height <- c(5.2, 5.5, 6.2, 6.0, 5.7, 5.8, 6.4, 5.6)
height
heightX <- c(5.2, 5.5, 6.2, 6.0)
heightY <- c(5.8, 5.2, 6.0, 5.0)
c(heightX, heightY)

park.kim <- c("Hong Min", "Yu Ha", "Chloe", "Steven")
park.kim

mix <- c("Park", 1)
mix

names(park.kim) <- c("dad", "mom", "daughter", "son")
park.kim

sum(height)
length(height)
sort(height)
min(height)
max(height)


## column and row vectors

CXY <- cbind(heightX, heightY)
CXY

RXY <- rbind(heightX, heightY)
RXY

summary(CXY)
dim(CXY) # rows x columns
ncol(CXY)
nrow(CXY)

a <- c(1,2,3)
b <- cbind(1,2,3)
c <- rbind(1,2,3)
a
b
c
dim(a)
dim(b)
dim(c)

cbind(a,a)   ## a <- c(1,2,3)
cbind(b,b)   ## b <- cbind(1,2,3)
cbind(c,c)   ## c <- rbind(1,2,3)

rbind(a,a)   ## a <- c(1,2,3)
rbind(b,b)   ## b <- cbind(1,2,3)
rbind(c,c)   ## c <- rbind(1,2,3)


## matrix

C <- matrix(c(1,2,3,4), nrow=2)
C

D <- matrix(c(5,6,7,8), nrow=2)
C*D    ## inner product
C %*% D    ## multiplication


## structured data

1:10
rev(1:10)
10:1

seq(1, 9, by=2)    ## odd numbers
seq(1, 9, length=5)

rep(1, 10)
rep(1:3, 3)


## use indices

ebay <- c(88.8, 88.3, 90.2, 93.5, 95.2, 94.7, 99.2, 99.4, 101.6)
length(ebay)

ebay[1]    ## first value
ebay[length(ebay)]    ## last value

ebay[1:4]
ebay[c(1,5,9)]

ebay[-1]    ## all but the first
ebay[-c(1:4)]    ## all but the first - fourth
ebay[-(c(1:4,6))]    ## all but the first - fourth, and sixth

x <- c(1,2,9)
names(x) <- c("one", "two", "three")
x["two"]

ebay[1] <- 76.0
ebay

ebay[c(10:13)] <- c(97.0, 99.3, 102.0, 101.8)
ebay

ebayT <- matrix(ebay[-1], nrow=3)
ebayT

ebayT[1,2]
ebayT[1,]
ebayT[,3]


## logical values

ebay > 100

ebay[ebay>100] # values greater than 100
which(ebay>100) # which indices
ebay[c(9,12,13)]

sum(ebay > 100)

x > 3 & x < 7
x > 3 | x < 7
x == 7
x != 7
x %in% c(1:3, 7) # range of values use %in%


## missing values

shuttle <- c(0,1,0,NA,0,0)
shuttle
is.na(shuttle)
sum(shuttle)
sum(shuttle, na.rm=TRUE)

test <- c(1,1,2,1,1,8,1,2,1,9,1,8,2,1,9,1,2,9,9,1)
test.new <- replace(test, test==8 | test==9, NA)
test.new <- replace(test.new, test.new==1, 0)
test.new <- replace(test.new, test.new==2, 1)
test.new


## table

test
t <- table(test)
t

table(test.new)
test.new
table(test.new, useNA = "always")

grade <- c("A", "B", "A", "A", "C")
attend <- c("good", "good", "good", "bad", "bad")
table(grade, attend)


## loop

for (i in 1:5) {
  print(i)
}


## control statement

last.name <- "park"
if (last.name == "park") {
  print("yay!!!")
  } else {
  print("hmm..")
  }


## function

newfn <- function(a) {
  b <- 2*a + 5
  return(b)
}
newfn(2)


## packages

install.packages("UsingR")
require(UsingR)


## data.frame

park.family <- data.frame(first = c("Hong Min", "Yu Ha", "Chloe", "Steven"),
                          age = c(41, 36, 6, 4),
                          gender = c("M", "F", "F", "M"),
                          spicy = c("hate", "like", "hate", "hate"))
park.family

park.family[1,2]
names(park.family)
park.family$age

class(park.family$age)
class(park.family$first)

park.family$food <- c("noodle", "soup", "pasta", "salad")
park.family

more <- data.frame(first = c("Addy", "Bunny"),
                   age = c(6, 4),
                   gender = c("F", "M"),
                   spicy = c(NA, NA),
                   food = c(NA, NA))
park.family <- rbind(park.family, more)
park.family


## Selection and sorting data frame

adults <- park.family[park.family$age >= 18,]
adults

a.m.cond <- park.family$age >= 18 & park.family$gender == "M"
a.m <- park.family[a.m.cond,]
a.m

order(park.family$age)
park.family[order(park.family$age),]
park.family[rev(order(park.family$age)),]


## Two-way table

table(park.family$gender, park.family$spicy)

park.family$adult <- rep(0, 6)
park.family$adult <- replace(park.family$adult, park.family$age >= 18, 1)
table(park.family$spicy, park.family$adult)



## Read data

h.pol <- read.table("ftp://k7moa.com/wf1/house_polarization_46_114.txt")
s.pol <- read.table("ftp://k7moa.com/wf1/senate_polarization_46_114.txt")
head(h.pol)

h.pol.sub <- h.pol[,c(1,2,3,7)]
names(h.pol.sub) <- c("cong", "year", "partydiff", "overlap")

head(h.pol.sub)
h.pol.sub$partydiff


## csv data

coffee <- read.csv("excelData.csv", header=TRUE)
coffee


## Different data files

require(foreign)
gpa <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/gpa1.dta")
head(gpa)


## Write data

write.csv(park.family, "park.csv", row.names=FALSE)
write.dta(park.family, "park.dta")


## Merge data

fruit <- data.frame(day=c("Monday", "Tuesday", "Wednesday",
                          "Thursday", "Friday", "Saturday"),
                    fruit=c(6, 5, 6, 7, 7, 7))
shop <- merge(coffee, fruit, by.x="day", by.y="day", all=TRUE)


## The plot function

plot(h.pol.sub$partydiff)

plot(h.pol.sub$partydiff, h.pol.sub$overlap)


## Figure construction

length(h.pol.sub$partydiff)
length(h.pol.sub$overlap)

par(mfrow=c(1,2))
plot(h.pol.sub$partydiff)
plot(h.pol.sub$partydiff, h.pol.sub$overlap)
dev.off()

plot(h.pol.sub$year, h.pol.sub$partydiff, 
     type="l", lty=1, lwd=2, col=2,
     main="Party Polarization in the House",
     xlim=c(1900, 2015),
     xlab="Year", ylab="Difference in Party's Mean Ideology")


## End