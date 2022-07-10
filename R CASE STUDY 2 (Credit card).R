getwd()
setwd("C:/Users/ACER/Downloads/R/R/R case study 2 (Credit card)")

library(dplyr)
library(ggplot2)
library(scales)
cust_acq = read.csv("Customer Acqusition.csv")
repayment = read.csv("Repayment.csv")
spend = read.csv("spend.csv")
repayment = repayment[ , c(1:4)]

#1.In the above dataset,
#a.Incase age is less than 18, replace it with mean of age values

cust_acq$Age[cust_acq$Age < 18]  = mean(cust_acq$Age)

#b.	Incase spend amount is more than the limit, replace it with 
#50% of that customer’s limit. (customer’s limit provided in 
#acquisition table is the per transaction limit on his card)

a = inner_join(cust_acq , repayment , by ="Customer")

final = inner_join(a, spend , by = "Customer")

colnames(final)[10] = "repayment_date" 
colnames(final)[11] = "repayment_amount" 
colnames(final)[13] = "spend_date"
colnames(final)[15] = "spend_amount"

final$spend_amount[final$spend_amount>final$Limit] = (final$Limit)*0.5

#c.Incase the repayment amount is more than the limit, replace the 
#repayment with the limit.

final$repayment_amount[final$repayment_amount>final$Limit] = final$Limit

#2.From the above dataset create the following summaries
# a.How many distinct customers exist?

sum(table(unique(final$Customer)))
## 100 customrs 

#b.	How many distinct categories exist?

final = final[,c(-1,-9,-12)]

colnames(final)

# 12 distinct category 


#c.	What is the average monthly spend by customers?

final$spend_date =  as.Date(final$spend_date,format = "%d-%b-%y" )


View(summarise(group_by(final ,Customer ,months(spend_date) ), avg_spend = mean(spend_amount)))

#d.	What is the average monthly repayment by customers?

final$repayment_date = as.Date(final$repayment_date,format = "%d-%b-%y" )

View(summarise(group_by(final ,Customer ,months(repayment_date) ), avg_repayment = mean(repayment_amount)))


#e.	If the monthly rate of interest is 2.9%, what is the profit for the 
#bank for each month?(Profit is defined as interest earned on Monthly 
#Profit. Monthly Profit = Monthly repayment – Monthly spend.
#Interest is earned only on positive profits and not on negative amounts)

final$spend_year = format(final$spend_date, format="%Y") 
final$spend_month =format(final$spend_date, format="%m") 

interest_cal =  summarise(group_by(final , spend_year ,spend_month), spend_amount = sum(spend_amount), repayment_amount = sum(repayment_amount))

interest_cal$monthly_profit   =interest_cal$repayment_amount - interest_cal$spend_amount

interest_cal$monthly_profit[8] = 0

interest_cal$interest_earn_monthly = (interest_cal$monthly_profit*2.9)/100

#f.	What are the top 5 product types?
 
head(table(final$Type)[order(table(final$Type), decreasing = T)], 5)


#g.	Which city is having maximum spend?

data_1 = summarise(group_by(final , City), spend_amount = sum(spend_amount))

data_1[data_1$spend_amount == max(data_1$spend_amount),]
# Cochin city spend max 

#h.	Which age group is spending more money?

data_2 = summarise(group_by(final , Age), spend_amount = sum(spend_amount))
data_2[data_2$spend_amount == max(data_2$spend_amount),]

## 28 age group 



#i.	Who are the top 10 customers in terms of repayment?

data_3 = summarise(group_by(final , Customer), repayment_amount = sum(repayment_amount))

data_3 = data_3[order(data_3$repayment_amount, decreasing = T),]

head(data_3, 10) 



#3.	Calculate the city wise spend on each product on yearly basis. 
#Also include a graphical representation for the same.

data_4 = summarise(group_by(final ,spend_year ,City, Product ),total_spend = sum(spend_amount))

ggplot(data_4 , aes(x = City,y = total_spend, fill = Product))+ geom_bar(stat = "identity", width = 0.5, position = "dodge")+scale_y_continuous(labels = comma)+ facet_grid(spend_year~.)


#4.	Create graphs for
#a.	Monthly comparison of total spends, city wise

data_5 = summarise(group_by(final ,spend_month ,City ),total_spend = sum(spend_amount))

ggplot(data_5 , aes(x = City, y=total_spend))+ geom_bar(stat = "identity", width = 0.5, position = "dodge")+scale_y_continuous(labels = comma)+facet_grid(spend_month~.)

#b.	Comparison of yearly spend on air tickets

data_6 = summarise(group_by(final , spend_year ,Type), spend = sum(spend_amount))

data_7 = data_6[data_6$Type== "AIR TICKET", ]

ggplot(data_7, aes(x = Type, y=spend))+ geom_bar(stat = "identity", width = 0.5, position = "dodge")+scale_y_continuous(labels = comma)+facet_grid(.~spend_year)

#c.	Comparison of monthly spend for each product 
#(look for any seasonality that exists in terms of spend)

data_8 = summarise(group_by(final, spend_month, Product ), total_spend = sum(spend_amount))

ggplot(data_8, aes(x = spend_month, y=total_spend))+ geom_bar(stat = "identity", width = 0.5, position = "dodge",aes(fill = Product))+scale_y_continuous(labels = comma)

## By ploting we can make following concussion: 
## a. gold is the higest selling product among three.
## b. silver is the least selling product.
## c. for all 3 product first 4-5 month that is from Jan to may the sales is high then it lower till December.
## d. for all 3 product the last month that is December month sale is the least. 
## e. You can observe a sudden peek in the month of November. 


