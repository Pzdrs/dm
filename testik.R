marketing <- read.csv("data/GOODS1n.csv")

revenue_increase <- (marketing$After - marketing$Before) / marketing$Before * 100

marketing$RevenueIncrease <- revenue_increase

plot(marketing$Promotion, marketing$RevenueIncrease,
     main = "Vztah mezi náklady na marketing a vzrůst tržeb",
     xlab = "Marketing costs",
     ylab = "Increase in revenue"
     )

ggplot(marketing)+
  geom_point(aes(
    x = Promotion, 
    y = RevenueIncrease, 
    color = Class
  ))
