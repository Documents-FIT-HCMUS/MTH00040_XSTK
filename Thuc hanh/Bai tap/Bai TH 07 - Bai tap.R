#Bai 01
n = 35
muy = 10
sigma = 5
x = rnorm(n, muy, sigma) # Tao bien ngau nhien 
s = sd(x)	
alpha = 0.05
epsilon = qnorm(1 - alpha / 2) * s / sqrt(n) # Tinh dung sai mau
x1 = mean(x) - epsilon
x2 = mean(x) + epsilon
cat('Khoang tin cay cho trung binh mau voi do tin cay',
 (1 - alpha) * 100, '% la [', x1, ', ', x2,']\n')

#Bai 02
data = read.csv("data31.csv")
ci.mean = function(x, alpha) {
	muy = mean(x)
	s = sd(x)
	n = length(x)
	epsilon = qt(1 - alpha / 2, n - 1) * s / sqrt(n);
	cat(muy - epsilon, muy + epsilon, '\n')
}

p = data$profit
ci.mean(p, alpha)

#Bai 03
data = read.csv("data32.csv", header = T);
time = data[,1]
ci.mean(time, 1 - 0.95)

ci.prop <- function(f, n, alpha) {
	p.hat = f / n;
	epsilon = qnorm(1 - alpha / 2) * sqrt(p.hat * (1 - p.hat) / length(x));
	cat(p.hat - epsilon, p.hat + epsilon);
}

cnt = length(time[time > 5])
ci.prop(cnt, length(time), 1 - 0.9);
