y = function(x){
	x*x*x
}
x = seq(-10, 10, by = 0.5)
curve(y(x), from = -5, to = 5)
plot(stepfun(x, y(x)))
