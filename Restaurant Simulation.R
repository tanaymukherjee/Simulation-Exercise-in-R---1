#----------------Restaurant---------------------------
# Restaurant

Weekdays_m <- 2000
Weekdays_sd <- 500
Weekend_m <- 3000
Weekend_sd <- 700

Prob_weekdays <- 5/7
Prob_weekend <- 1 - Prob_weekdays

Rent <- 2500
Labor <- 4500
Food <- 4500
Others <- 2500
Total_weekly_expenses <- Rent + Labor + Food + Others

?rnorm

#1
nsim = 1e6
prob <- 0
for (i in 1:nsim ) {
  Weekdays <- rnorm(5, mean = Weekdays_m , sd = Weekdays_sd)
  Weekend <- rnorm(2, mean = Weekend_m, sd = Weekend_sd)
  Week_result <- sum(c(Weekdays, Weekend)) - Total_weekly_expenses 
  if(Week_result > 0){
    prob <- prob + 1
  }
}
prob/nsim


#2
# 22 weekdays in dec 2019
# 9 weekends in dec 2019
# 1 holiday in 25th dec which is a weekday but must be treated as a weekend
# therefore, new weekdays count is 21 and weekend count is 10

nsim = 1e6
prob <- 0

Avg_Total_weekly_expenses <- Total_weekly_expenses / 7
# for 30 days
Avg_Total_weekly_expenses <- Avg_Total_weekly_expenses * 31

for (i in 1:nsim ) {
  Weekdays <- rnorm(21, mean = Weekdays_m , sd = Weekdays_sd)
  Weekend <- rnorm(10, mean = Weekend_m, sd = Weekend_sd)
  Week_result <- sum(c(Weekdays, Weekend)) - Avg_Total_weekly_expenses 
  if(Week_result < 0){
    prob <- prob + 1
  }
}
prob/nsim


#3
nsim = 1e6
Profit <- 0
a <- 0
b <- 0
for (i in 1:nsim ) {
  
  Weekdays <- rnorm(5, mean = Weekdays_m , sd = Weekdays_sd)
  Weekend <- rnorm(2, mean = Weekend_m, sd = Weekend_sd)
  Week_result <- sum(c(Weekdays, Weekend)) - Total_weekly_expenses 
  
  if(Week_result > 0){
    a <- a + Week_result     
  }else if (Week_result < 0) {
    b <- b + Week_result
  } 
  Profit = a + b
}

Profit <- Profit*52/nsim
Profit



#4

Marketing <- 10000/52 #Cost of Marketing campaign per week
Marketing

nsim = 1e6
Profit_s <- 0
Profit_f <- 0
Profit_net <- 0
a <- 0
b <- 0

# When the campaign is successful
for (i in 1:nsim ) {
  Weekdays <- rnorm(5, mean = Weekdays_m * 1.15 , sd = Weekdays_sd)
  Weekend <- rnorm(2, mean = Weekend_m * 1.15, sd = Weekend_sd)
  Week_result <- sum(c(Weekdays, Weekend)) - Total_weekly_expenses - Marketing
  
  if(Week_result > 0){
    a <- a + Week_result     
  }else if (Week_result < 0) {
    b <- b + Week_result
  } 
  Profit_s = a + b
}

Profit_s <- Profit_s*52/nsim

# When the campaign is not successful
a <- 0
b <- 0

for (i in 1:nsim ) {
  Weekdays <- rnorm(5, mean = Weekdays_m, sd = Weekdays_sd)
  Weekend <- rnorm(2, mean = Weekend_m, sd = Weekend_sd)
  Week_result <- sum(c(Weekdays, Weekend)) - Total_weekly_expenses - Marketing
  
  if(Week_result > 0){
    a <- a + Week_result     
  }else if (Week_result < 0) {
    b <- b + Week_result
  } 
  Profit_f = a + b
}

Profit_f <- Profit_f*52/nsim

Profit_net <- 0.8*Profit_s + 0.2*Profit_f
Profit_net

if (Profit_net > Profit) {
  (paste("Marketing campaign is driving more business to the restaurant"))
}else paste("Marketing campaign is not a good RoI for the restaurant")
