# Import libraries
library("xlsx")
library(ggplot2)
library(arules)
library(arulesViz)
library(datasets)
library(plyr)
library("RColorBrewer")
setwd("C:/data")

# Read dataset
HnM_data <- read.xlsx("HM-Sales-2018-2019.xlsx", sheetName="Sheet1")

summary(HnM_data)

# Perform data preprocessing
HnM_data$Ship.Mode <- as.factor(HnM_data$Ship.Mode)
HnM_data$Country <- as.factor(HnM_data$Country)
HnM_data$City <- as.factor(HnM_data$City)
HnM_data$State <- as.factor(HnM_data$State)
HnM_data$Region <- as.factor(HnM_data$Region)
HnM_data$Category <- as.factor(HnM_data$Category)
HnM_data$Sub.Category <- as.factor(HnM_data$Sub.Category)
HnM_data$Sales <- as.numeric(HnM_data$Sales)
HnM_data$Quantity <- as.numeric(HnM_data$Quantity)
HnM_data$Discount <- as.numeric(HnM_data$Discount)
HnM_data$Profit <- as.numeric(HnM_data$Profit)
HnM_data$Order.ID <- as.factor(HnM_data$Order.ID)
HnM_data$Customer.ID <- as.factor(HnM_data$Customer.ID)

# Changing column names to appropriate names
names(HnM_data) [1] <- "Order_ID"
names(HnM_data) [2] <- "Order_Date"
names(HnM_data) [3] <- "Ship_Mode"
names(HnM_data) [4] <- "Customer_ID"
names(HnM_data) [9] <- "Product_ID"
names(HnM_data) [11] <- "Sub_Category"

# Drop columns that are not neccessary which will not effect the analysis and model
HnM_data <- HnM_data[-c(3,5,6,7,9)]


# Visualization analysis

#Number of consumers by region
grouped_cus_state <-  aggregate(Customer_ID~Region, HnM_data, FUN=function(x) length(unique(x)))

plottheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (18), hjust = 0.5),
                   legend.title = element_text(colour = "#656568",  face = "bold.italic", family = "Helvetica"), 
                   legend.text = element_text(face = "bold.italic",family = "Helvetica"), 
                   axis.title = element_text(family = "Helvetica", size = (14)),
                   axis.text = element_text(family = "Courier", size = (14)))


#We will make this as pie chart
ggplot(grouped_cus_state, aes(x = "", y = Customer_ID, fill = Region)) + labs(title="Number of customers by region")+
  geom_col() + geom_text(aes(label = Customer_ID),
                         position = position_stack(vjust = 0.5)) + coord_polar(theta="y") +theme_void()


#Grouping number of items sold by region based on categories
grouped_items_category <-  aggregate(Quantity~Category+Region, HnM_data, sum)

ggplot(grouped_items_category, aes(fill=Category, y=Quantity, x=Region)) + 
  geom_bar(position="dodge", stat="identity") + 
  labs(title= "Grouped barplot for number of items sold by region based on categories", 
       y="Quantity of Items", x = "Regions") +
  geom_text(aes(label=Quantity), vjust=-1, position = position_dodge(0.9)) + plottheme


#Sales earned from items sold in west region by category
grouped_west_category <- HnM_data[HnM_data$Region == 'Central',] 

grouped_west_category <- aggregate(Sales~Sub_Category, grouped_west_category, sum)

p <- ggplot(grouped_west_category, aes(Sub_Category, Sales, fill=factor(Sub_Category)))
p + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90)) + 
  labs(title= "Barplot for sales of items in central region by sub categories", 
       y="Sales Amount", x = "Sub Category", fill="Sub Category") + plottheme +
  geom_text(aes(label=Sales), vjust=-0.6, position = position_dodge(0.9))


# Find number of occurence in sub category
table(HnM_data$Sub_Category)

# Group the data to merge sub category with similar customer ID
trans <- ddply(HnM_data,c("Order_Date","Customer_ID","Order_ID"),
               function(df1)paste(df1$Sub_Category,collapse = ","))
summary(trans)
head(trans)

# Remove the columns that will not be used
trans$Order_Date <- NULL
trans$Customer_ID<- NULL
trans$Order_ID <- NULL

# rename the column name from V1 to sub category
colnames(trans) <- c("Sub_Category")

# write the trans data into new csv file
write.csv(trans,"newHnM.csv", quote = FALSE, row.names = FALSE)

# read the written csv file
tr <- read.transactions('newHnM.csv', format = 'basket', sep=',')
summary(tr)


# Visualize the top 10 frequency of sub category
itemFrequencyPlot(tr,topN=10,type="absolute",
                  main="Absolute Sub-Category Frequency Plot",
                  col=brewer.pal(12,'Set3'),
                  ylab="Sub-Category(absolute)")

# Apply Apriori algorithm to find association sub category
scenario1 <- apriori(tr,parameter=list(support=0.05,confidence=0.5,
                                       target = "rules",minlen=2))
inspect(scenario1)

scenario2 <- apriori(tr,parameter=list(support=0.01,confidence=0.3,
                                       target = "rules",minlen=4))
inspect(sort(scenario2, by="lift"))

scenario3 <-apriori(tr,parameter=list(support=0.02,confidence=0.5,
                                      target = "rules",minlen=3))
inspect(sort(scenario3, by="lift"))

scenario4 <- apriori(tr,parameter=list(support=0.05,confidence=0.2,
                                       target = "rules",minlen=2),
                     appearance = list(lhs="Dresses",default="rhs"))
inspect(sort(scenario4, by="lift"))


# Apply Eclat algorithm to find association sub category
rules <- eclat(tr,parameter=list(support=0.05,minlen=2))
rules1 <- eclat(tr,parameter=list(support=0.01,minlen=4))
rules2 <- eclat(tr,parameter=list(support=0.02,minlen=3))

inspect(sort(rules,by="support"))
inspect(sort(rules1,by="support"))
inspect(sort(rules2,by="support"))

rules_1 <- ruleInduction(rules,tr,confidence=0.5)
rules_2 <- ruleInduction(rules1,tr,confidence=0.3)
rules_3 <- ruleInduction(rules2,tr,confidence=0.5)

inspect(sort(rules_1,by="lift"))
inspect(sort(rules_2,by="lift"))
inspect(sort(rules_3,by="lift"))