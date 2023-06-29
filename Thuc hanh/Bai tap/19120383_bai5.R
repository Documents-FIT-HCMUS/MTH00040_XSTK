#BAI 5.2
#ÐINH LY 1
size = 10
prob = 0.3
mauX <- function(n){
	x <- rbinom(n, size, prob)
	sqrt(n)*(mean(x) - 3)/sqrt(2.1)
}
Y <- function(m)
	replicate(m,mauX(n))
n = 100 m = 1000
hist(Y(m), freq = 0, breaks = 40)
curve(dnorm(x), col = "blue", lty = 1, lwd = 2, add = TRUE)

n = 10000 hist(Y(m), freq = 0, breaks = 40)
curve(dnorm(x), col = "blue", lty = 1, lwd = 2, add = TRUE)

n = 100000 hist(Y(m), freq = 0, breaks = 40)
curve(dnorm(x), col = "blue", lty = 1, lwd = 2, add = TRUE)

#DINH LY 2
prob = 0.3 
Y <- function(n) {
	x <- rbinom(n, 1, prob) 
	(mean(x) - prob)*sqrt(n)/sqrt(prob*(1 - prob))
}
vecY <- function(m)
	replicate(m, Y(n))
n = 100
m = 1000
hist(vecY(m), freq = 0, breaks = 40)
curve(dnorm(x), col = "blue", lty=1, lwd = 2, add = TRUE)

Z<-function(n) {
	x<-rbinom(n,1,prob)
	(mean(x)-prob)*sqrt(n)/sqrt(mean(x)*(1 - mean(x)))
	#mean(x) - prob tuc ta lay ti le trung binh cua mau
	#tru cho ti le prob cua phan phoi bernoulli
	#dua theo dinh ly 2
}
vecZ <- function(m)
	replicate(m, Z(n))
hist(vecZ(m), freq=0, breaks=40)
curve(dnorm(x), col="blue", lty=1, lwd=2, add=TRUE)












