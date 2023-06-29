# VI DU
prop.test(448, 800, p = 0.5, alternative = "greater", conf.level = 0.99)

# Bai tap 4
p0 = 0.6
alpha = 0.05
load("data04.rda")
mu = mean(survey)
n = length(survey)
y = length(survey[survey == 1])

result = prop.test(y, n, p = p0, alternative = "greater", conf.level = 0.95)
if (result$p.value < alpha) cat('BAC BO H0\n') else cat('KHONG DU CO SO BAC BO H0\n')

# Bai tap 5
p0 = 0.15
y = 20
n = 100
alpha = 0.05
result = binom.test(y, n, p = p0, alternative = "two.sided", conf.level = 1 - alpha)
if (result$p.value < alpha) cat('BAC BO H0\n') else cat('KHONG DU CO SO BAC BO H0\n')
