# Pizza sales

##### 1. Load packages #######
library(tidyverse)
library(lubridate)

##### 2. Load data #######
all_sales <- list.files(path = "./sales", pattern = ".csv")

# use a loop to load all datasets 
i <- 1
for (salesdata in all_sales){
  datafile <- read_csv(paste0("./sales/", salesdata))
  assign(paste0("dataset_", i), datafile) # assigns=>creates an file
  i <- i + 1
}

############## JOIN ###############
# Use a tidyverse join to join all the data together into one file
# called sales_data, then run the rest of the code
sales_data <- full_join(dataset_1, dataset_2)
sales_data <- full_join(sales_data, dataset_3)
sales_data <- full_join(sales_data, dataset_4)
sales_data <- full_join(sales_data, dataset_5)
sales_data <- full_join(sales_data, dataset_6)
sales_data <- full_join(sales_data, dataset_7)
sales_data <- full_join(sales_data, dataset_8)
sales_data <- full_join(sales_data, dataset_9)
sales_data <- full_join(sales_data, dataset_10)
sales_data <- full_join(sales_data, dataset_11)
sales_data <- full_join(sales_data, dataset_12)
sales_data <- full_join(sales_data, dataset_13)
sales_data <- full_join(sales_data, dataset_14)
sales_data <- full_join(sales_data, dataset_15)
join_sales_ANACUBAS <- full_join(sales_data, dataset_16)

########################################

##### 3. Create summaries #####
sales_summary <- join_sales_ANACUBAS %>%
  group_by(pizza, month) %>% 
  summarize(total_sales = sum(number))

ggplot(data = sales_summary, aes(x = pizza, y = total_sales))+
  geom_bar(stat = "identity")

# Daily sales
# Create "proper" dates
join_sales_ANACUBAS$date <- ymd(paste(join_sales_ANACUBAS$year, "/", join_sales_ANACUBAS$month, "/", join_sales_ANACUBAS$day))

# Summarize data
sales_summary_daily <- join_sales_ANACUBAS %>%
  group_by(pizza, date) %>% 
  summarize(total_sales = sum(number))

# Plot
ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, color = pizza))+
  geom_line()

ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, fill = pizza))+
  geom_bar(stat = "identity")

# Average data
sales_ave_daily <- join_sales_ANACUBAS %>%
  group_by(pizza, date) %>% 
  summarize(ave_sales = mean(number))

ggplot(data = sales_ave_daily, aes(x = date, y = ave_sales, fill = pizza))+
  geom_bar(stat = "identity", position = "dodge")