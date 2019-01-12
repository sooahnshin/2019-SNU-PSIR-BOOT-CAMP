########################################
## R code for 2018SNU-PSIR            ##
## Topic: Regression in R             ##
## By Hong Min Park (hmpark1@uwm.edu) ##
## Last updated on 12/26/2018         ##
########################################


## Uploading data

require(UsingR)
attach(MLBattend)  ## attach data so tha we can use variable names
head(MLBattend)  ## look at first several entries of data

stl.win <- wins[franchise == "STL"]
stl.off <- runs.scored[franchise == "STL"]
stl.def <- runs.allowed[franchise == "STL"]


## Mean and median

MLB.win <- cbind(mean(wins), mean(stl.win),
                 median(wins), median(stl.win))
MLB.off <- cbind(mean(runs.scored), mean(stl.off),
                 median(runs.scored), median(stl.off))
MLB.def <- cbind(mean(runs.allowed), mean(stl.def),
                 median(runs.allowed), median(stl.def))
MLB.record <- rbind(MLB.win, MLB.off, MLB.def)
rownames(MLB.record) <- c("win", "offense", "defense")
colnames(MLB.record) <- c("MLB-mean", "STL-mean",
                          "MLB-median", "STL-median")

print(MLB.record)


## Variance and standard deviation

MLB.win.spread <- cbind(var(wins), var(stl.win),
                        sd(wins), sd(stl.win))
MLB.off.spread <- cbind(var(runs.scored), var(stl.off),
                        sd(runs.scored), sd(stl.off))
MLB.def.spread <- cbind(var(runs.allowed), var(stl.def),
                        sd(runs.allowed), sd(stl.def))
MLB.spread <- rbind(MLB.win.spread, MLB.off.spread, MLB.def.spread)
rownames(MLB.spread) <- rownames(MLB.record)
colnames(MLB.spread) <- c("MLB-var", "STL-var", "MLB-sd", "STL-sd")

print(MLB.spread)


## Covarianze and Correlation

head(kid.weights)   # another data: kid's weight and height

cov(kid.weights$weight, kid.weights$height)
cor(kid.weights$weight, kid.weights$height)


## Linear Regression via OLS

require(foreign)
vote1 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/vote1.dta")
head(vote1)

m1 <- lm(voteA ~ shareA, data=vote1)
print(m1)

plot(vote1$voteA ~ vote1$shareA,
     xlim=c(0,100), ylim=c(0,100),
     main="Money in Election",
     xlab="Campaign Expenditure Share by Candidate A",
     ylab="Vote Share by Candidate A")
abline(m1, col=4, lwd=3)

summary(m1)

mod2 <- lm(voteA ~ shareA + prtystrA, data=vote1)
coef(mod2)

summary(mod2)

cbind(coef(mod2), c(coef(m1), NA))


## Quadratics

wage1 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/wage1.dta")
wage.mod3 <- lm(wage ~ exper + I(exper^2), data=wage1)
summary(wage.mod3)

peak.mod3 <- - coef(wage.mod3)[2]/(2*coef(wage.mod3)[3])
print(peak.mod3)

plot(wage ~ exper, data=wage1)
abline(lm(wage ~ exper, data=wage1), col=2)
x <- seq(0, 55, 0.001)
y <- predict(wage.mod3, newdata=data.frame(exper=x))
lines(y ~ x, col=4)
legend(37, 24, c("linear", "quadratic"), col=c(2, 4), lty=1)


## Dummy

wage.mod5 <- lm(log(wage) ~ educ + exper + female, data=wage1)
summary(wage.mod5)


## Logit and probit

library(car)
head(Mroz)    ## We are using Mroz (1987) data
(n <- nrow(Mroz))
?Mroz

mroz.probit <- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
                   family = binomial(link=probit), data=Mroz)
mroz.logit <- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
                  family = binomial(link=logit), data=Mroz)
summary(mroz.probit)
summary(mroz.logit)


## Poisson

head(Ornstein)  ## from car package
nrow(Ornstein)
?Ornstein

tab <- xtabs(~ interlocks, data=Ornstein)
tab2 <- table(Ornstein$interlocks)    ## alternative way
print(tab)

plot(tab, type="h", main="Ornstein's Interlocks",
     xlab="Number of Interlocks", ylab="Frequency")

mod.ornstein <- glm(interlocks ~ log2(assets) + nation + sector,
                    family=poisson, data=Ornstein)
summary(mod.ornstein)


## Other count models

nb.ornstein <- glm.nb(interlocks ~ log2(assets) + nation + sector,
                      data=Ornstein)
summary(nb.ornstein)

library(pscl)
z.mod.ornstein <- zeroinfl(interlocks ~ log2(assets) + nation + sector,
                           data=Ornstein)
z.nb.ornstein <- zeroinfl(interlocks ~ log2(assets) + nation + sector,
                          dist = "negbin", data=Ornstein)
summary(z.mod.ornstein)
summary(z.nb.ornstein)


## End