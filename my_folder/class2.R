library(tidyverse)
genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps
  tibble(x1=x1, x2=x2, y=y)
}
str(genreg)

dat<- genreg(1000)
str(dat)
#2 
dat <- mutate(dat, yhat = 5, yhat1 = 5-x1, yhat2 = 5+2*x2, yhat3=5-x1 + 2*x2)
dat

mse <- mean((dat$yhat - dat$y)^2)
mse1 <- mean((dat$yhat1 - dat$y)^2)
mse2 <- mean((dat$yhat2 - dat$y)^2)
mse12 <- mean((dat$yhat3 - dat$y)^2)

mse
mse1
mse2
mse12

#Classification

ggplot(tibble(x=c(-7, 7)), aes(x)) +
  stat_function(fun=function(x) 0.8/(1+exp(-x))) +
  ylim(c(0,1)) +
  geom_hline(yintercept=c(0,0.8), linetype="dashed", alpha=0.5) +
  theme_bw() +
  labs(y="P(Y=B|X=x)")

gencla <- function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(x) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, x, 1-x)))
  tibble(x=x, y=y)
}

y<-gencla(1000)
y
p1 <- 0.8/(1+exp(-1))
p1
p2 <- 0.8/(1+exp(2))
p2
dat2 <- mutate(dat, yhat = 5, yhat1 = 5-x1, yhat2 = 5+2*x2, yhat3=5-x1 + 2*x2)
dat2
