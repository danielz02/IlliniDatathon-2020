
#PCA

data = read.csv("/Users/guannanzhai/Desktop/state_data.csv")
data1 = read.csv("/Users/guannanzhai/Desktop/state_data1.csv")
data1[which(is.na(data1$per_capital_personal_expenditure)), 4] = mean(data1$per_capital_personal_expenditure, na.rm = TRUE)
scale_data = scale(data1[, -c(6, 7, 8)])
sigma = cor(scale_data)
eigen = eigen(sigma)

fit = principal(scale_data, nfactors = 2, rotate = "varimax", score = T)

fa.diagram(fit, digits = 2)
new_data = as.data.frame(cbind(scale_data, fit$scores))
new_data = as.data.frame(cbind(data1[,8], new_data))
names(new_data) = c("revenue", "population", "GDP(millions)", "personal_income", "personal_expenditure",
                    "GDP_from_lastYear", "RC1", "RC2")
new_data$revenue = new_data$revenue / 1000000
#data1 = new_data[order(new_data$RC1, decreasing = T),]

lm_mod = lm(revenue ~ RC1 + RC2, data = new_data)
mod_coef = coef(lm_mod)
loadings_RC1 = c(0.99, 0.99, 0.19, -0.03, 0.99)
loadings_RC2 = c(0.02, 0.11, 0.74, 0.82, 0.12)
importance = mod_coef[1] * loadings_RC1 + mod_coef[2] * loadings_RC2

plot(importance, type = "l")

data2 = data1[order(data1$revenue, decreasing = T),]
weight = importance / sum(importance)

data1[,1:5] %*% weight 


#choose five states which own first five score above

#eco and population shuffle

CA_data = alldata[which(alldata$state == "CA"), ] 
TX_data = alldata[which(alldata$state == "TX"), ]
NC_data = alldata[which(alldata$state == "NC"), ]
FL_data = alldata[which(alldata$state == "FL"), ]
IL_data = alldata[which(alldata$state == "IL"), ]

eco_data = rbind(CA_data, TX_data, NC_data, FL_data, IL_data)

#max100
level_name = names(table(eco_data$RCPSZFE.display.label))
eco_data[which(eco_data$RCPSZFE.display.label == level_name[2]), 28] = 1
eco_data[which(eco_data$RCPSZFE.display.label == level_name[3]), 28] = 2
eco_data[which(eco_data$RCPSZFE.display.label == level_name[4]), 28] = 6
eco_data[which(eco_data$RCPSZFE.display.label == level_name[5]), 28] = 3
eco_data[which(eco_data$RCPSZFE.display.label == level_name[6]), 28] = 4
eco_data[which(eco_data$RCPSZFE.display.label == level_name[7]), 28] = 5
eco_data[which(eco_data$RCPSZFE.display.label == level_name[1]), 28] = NA
eco_data[which(eco_data$RCPSZFE.display.label == level_name[8]), 28] = NA

eco_data = eco_data[which(!is.na(eco_data[, 28])),] #remove NA


#max10

eco_data[which(eco_data$RCPSZFE.display.label == level_name[2]), 28] = 1
eco_data[which(eco_data$RCPSZFE.display.label == level_name[3]), 29] = 100000 / 2
eco_data[which(eco_data$RCPSZFE.display.label == level_name[4]), 29] = 1000000 / 2
eco_data[which(eco_data$RCPSZFE.display.label == level_name[5]), 29] = (100000 + 249999) / 2
eco_data[which(eco_data$RCPSZFE.display.label == level_name[6]), 29] = (250000 + 499999) / 2
eco_data[which(eco_data$RCPSZFE.display.label == level_name[7]), 29] = (500000 + 999999) / 2
eco_data[which(eco_data$RCPSZFE.display.label == level_name[1]), 29] = NA
eco_data[which(eco_data$RCPSZFE.display.label == level_name[8]), 29] = NA
eco_data$prod = eco_data[,29] * eco_data$ESTAB

na = which(is.na(eco_data$prod))
eco_data2 = eco_data[-na, ]

summed = read.csv("/Users/guannanzhai/Desktop/datathon-2020/eco_data2_summed.csv")
all_category1 = names(table(summed$NAICS.display.label))
data4 = data.frame()
i = 1
for(i in 1:105){
  data = summed[which(summed$NAICS.display.label == all_category1[i]), ]
  data2 = data[order(data[, 33], decreasing = T),]
  data3 = data2[1:10, ]
  data4 = rbind(data3, data4)
}

na.value = which(is.na(data4$GEO.id))
data5 = data4[-na.value,]

write.csv(data5, file = "/Users/guannanzhai/Desktop/datathon-2020/Ifinaldata_max103.csv")

#final result
zipcode = data5$GEO.id2
zipcode = as.data.frame(zipcode)
write.csv(zipcode, file = "/Users/guannanzhai/Desktop/datathon-2020/zipcode.csv")