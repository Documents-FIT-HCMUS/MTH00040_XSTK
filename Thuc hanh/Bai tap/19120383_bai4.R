# Bai 04

# Ham ci.mean
ci.mean <- function(x, alpha) {
	muy = mean(x)
	s = sd(x)
	n = length(x)
	epsilon = qt(1 - alpha / 2, n - 1) * s / sqrt(n) # Dung sai
	x1 = muy - epsilon
	x2 = muy + epsilon
	cat('Khoang tin cay cho trung binh mau voi do tin cay', (1 - alpha) * 100,
	'% la [', x1, ', ', x2,']\n')
}

# Ham ci.prop
ci.prop <- function(f, n, alpha) {
	p.hat = f / n # Ti le mau
	epsilon = qnorm(1 - alpha / 2) * sqrt(p.hat * (1 - p.hat) / n) # Dung sai mau
	x1 = p.hat - epsilon
	x2 = p.hat + epsilon
	cat('Khoang tin cay cho ti le mau voi do tin cay', (1 - alpha) * 100,
	'% la [', x1, ', ', x2,']\n')
}

# Cau a
x <- rep(1.3, 6)
x <- c(x, rep(1.5, 34))
x <- c(x, rep(1.7, 31))
x <- c(x, rep(1.9, 42))
x <- c(x, rep(2.1, 12))
ci.mean(x, 1 - 0.95)

# Cau b
f = length(x[x >= 1.7])
n = length(x)
ci.prop(f, n, 1- 0.95)