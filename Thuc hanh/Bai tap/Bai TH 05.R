#VI DU 5.1
#5.1 - TRUNG BINH
muy = 2
sigma = 2
Y <- function()
	rnorm(1, muy, sigma)
vecY <- function(n)
	replicate(n, Y())
n = 5
vecY(n)

MeanY <- function() 
	mean(vecY(n))
SampleMeanY <- function(m)
	replicate(m, MeanY())
m = 10000

hist(SampleMeanY(m), freq = 0, breaks = 40)
dnorm(x, muy, sigma)
curve(dnorm(x, muy, sigma/sqrt(n)), col = "blue", lty = 1, lwd = 2, add = TRUE)

#5.1 - PHUONG SAI MAU
muy = 2
sigma = 2
n = 10
m = 10000
Z <- function() {
	x = rnorm(n, muy, sigma)
	(n - 1)*var(x)/sigma^2
}
vecZ<-function(m) 
	replicate(m, Z())

hist(vecZ(m),freq = 0, breaks = 40)
curve(dchisq(x, n - 1), col = "blue", lty=1, lwd=2, add = TRUE)

#5.2



#BAI TAP 1
Y <- function(){
	x1 = rnorm(1, 0, 1)
	x2 = rnorm(1, 0, 1)
	x1 ** 2 + x2 ** 2
}
VecY <- function(n)
	replicate(n, Y())
n <- c(100, 1000, 10000)

layout(matrix(c(1, 3)
hist(VecY(n[3]), freq = 0, breaks = 40)
curve(dchisq(x, 2), from = 0, to = 8, col = "red", add = TRUE)







