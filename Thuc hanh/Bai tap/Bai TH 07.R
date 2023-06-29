m = 50; n=30; p = .5; alpha = 0.05 # Tung 30 dong xu can doi 50 lan
p.hat = rbinom(m,n,p)/n # Tinh ti le mau
epsilon = qnorm(1-alpha/2)*sqrt(p.hat*(1-p.hat)/n) # Tinh dung sai

matplot(rbind(p.hat - epsilon, p.hat + epsilon), rbind(1:m,1:m),type="l",lty=1,
 xlab = "Vi tri cac khoang tin cay", ylab = "50 khoang tin cay")
# Ve 50 khoang tin cay
abline(v=p) # Ve duong thang p = 0.5