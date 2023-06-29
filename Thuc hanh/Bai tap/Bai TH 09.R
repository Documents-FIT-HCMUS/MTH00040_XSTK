# VI DU 1
alpha = 0.05
load("scores.rda")
equal = TRUE
varTest = var.test(scores$midterm, scores$final) # Phuong sai khac nhau
if (varTest$p.value < alpha) equal = FALSE
t.test(scores$midterm, scores$final, var.equal = equal)
boxplot(scores)

# VI DU 2
data = read.table("cholesterol.txt", header = TRUE)
data
attach(data)
v.equal = ifelse(var.test(Before, After)$p.value <= alpha, FALSE, TRUE)
t.test(Before, After, paired = TRUE, var.equal = v.equal, alternative = "greater")

# VI DU 3
y = c(94, 76)
n = c(360, 320)
prop.test(y, n, alternative = "greater")

# BAI TAP
# Cau a
data = read.csv("volume.csv", header = TRUE)
attach(data)
alpha = 0.05
result = t.test(machine1, machine2, alternative = "two.sided", conf.level = 1 - alpha)
if (result$p.value < alpha) cat('BAC BO H0\n') else cat('KHONG DU CO SO BAC BO H0\n')
# Cau b
result$p.value
#Cau c
KTC = result$conf.int
KTC