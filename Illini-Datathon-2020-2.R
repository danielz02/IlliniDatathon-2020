part1 = read.csv("/Users/yangyunye/Desktop/Illinois-Datathon-2020/Part 1.csv")
part2 = read.csv("/Users/yangyunye/Desktop/Illinois-Datathon-2020/Part 2.csv")
part3 = read.csv("/Users/yangyunye/Desktop/Illinois-Datathon-2020/Part 3.csv")
part4 = read.csv("/Users/yangyunye/Desktop/Illinois-Datathon-2020/Part 4a.csv")
part5 = read.csv("/Users/yangyunye/Desktop/Illinois-Datathon-2020/Part 4b.csv")
part6 = read.csv("/Users/yangyunye/Desktop/Illinois-Datathon-2020/Part 5.csv")

data = rbind(part1, part2)
data = rbind(data, part3)
data = rbind(data, part4)
data = rbind(data, part5)
data = rbind(data, part6)

state = data$GEO.display.label
state = as.character(state)

library(stringr)

n = length(state)
for (i in 2:n) {
  state_name = str_sub(state[i], start = -3, end = -2)
  state[i] = state_name
}

data = cbind(data, state)

state_set = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
              "HI", "ID", "IL", "IN", "IA", "KS", "KY","LA", "ME", "MD",
              "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
              "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
              "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

path = "/Users/yangyunye/Desktop/state_data"

for(i in 1:length(state_set)) {
  index = which(data$state == state_set[i])
  state_data = data[index, ]
  write.csv(state_data, str_c(paste(path, state_set[i], sep = '/'), ".csv"))
}

state_set_new = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                  "HI", "ID", "IL", "IN", "IA", "KS", "KY","LA", "ME", "MD",
                  "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
                  "NY", "NC", "OH", "PA", "RI", "SC", "SD", "TN", "TX", "UT", 
                  "VT", "VA", "WA", "WV", "WI", "WY")

revenue = numeric(length(state_set_new))

for (i in 1:length(state_set_new)) {
  state_info = read.csv(str_c(paste(path, state_set_new[i], sep = '/'), ".csv"))
  benefit = state_info$RCPSZFE.display.label
  state_revenue = 0
  for (j in 1:length(benefit)) {
    if (benefit[j] == "Establishments operated entire year with sales/receipts/revenue less than $100,000") {
      state_revenue = state_revenue + 100000
    }
    if (benefit[j] == "Establishments operated entire year with sales/receipts/revenue of $100,000 to $249,999") {
      state_revenue = state_revenue + (100000 + 249999)/2
    }
    if (benefit[j] == "Establishments operated entire year with sales/receipts/revenue of $250,000 to $499,999") {
      state_revenue = state_revenue + (250000 + 499999)/2
    }
    if (benefit[j] == "Establishments operated entire year with sales/receipts/revenue of $500,000 to $999,999") {
      state_revenue = state_revenue + (500000 + 999999)/2
    }
    if (benefit[j] == "Establishments operated entire year with sales/receipts/revenue of $1,000,000 or more") {
      state_revenue = state_revenue + 1000000
    }
  }
  revenue[i] = state_revenue
}

state_revenue = cbind(state_set_new, revenue)
write.csv(state_revenue, "/Users/yangyunye/Desktop/state_revenue.csv")

FL = read.csv("/Users/yangyunye/Desktop/FL_data.csv")
industry = data.frame(table(FL$NAICS.display.label))
industry_rev = numeric(nrow(industry))


for (i in 1:nrow(industry)) {
  ind_revenue = 0
  for (j in 1:nrow(FL)) {
    if (FL$NAICS.display.label[j] == industry$Var1[i]) {
      if (FL$RCPSZFE.display.label[j] == "Establishments operated entire year with sales/receipts/revenue less than $100,000") {
        ind_revenue = ind_revenue + 100000
      }
      if (FL$RCPSZFE.display.label[j] == "Establishments operated entire year with sales/receipts/revenue of $100,000 to $249,999") {
        ind_revenue = ind_revenue + (100000 + 249999)/2
      }
      if (FL$RCPSZFE.display.label[j] == "Establishments operated entire year with sales/receipts/revenue of $250,000 to $499,999") {
        ind_revenue = ind_revenue + (250000 + 499999)/2
      }
      if (FL$RCPSZFE.display.label[j] == "Establishments operated entire year with sales/receipts/revenue of $500,000 to $999,999") {
        ind_revenue = ind_revenue + (500000 + 999999)/2
      }
      if (FL$RCPSZFE.display.label[j] == "Establishments operated entire year with sales/receipts/revenue of $1,000,000 or more") {
        ind_revenue = ind_revenue + 1000000
      }
    }
  }
  industry_rev[i] = ind_revenue
}

industry_rev = data.frame(industry$Var1, industry_rev)

