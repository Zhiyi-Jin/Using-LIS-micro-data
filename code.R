## Read household data for 3 countries and calculate mean income by country
library(dplyr)
LIS <- read.LIS(c("NO16h", "KR16h", "TW16h"), vars=c("dname", "dhi")) 
LIS %>% 
  group_by(dname) %>% 
  summarise(mean(dhi))

## Finding:
## The average household income of South Korea, Taiwan, and Norway is 37193393 won, 
## 843198 New Taiwan Dollar, and 591843 Norwegian Krone respectively.

## Compute the GINI coefficient for inequality across households in equivalized 
## disposable household income for 3 countries.
library(dplyr)
library(ineq)
LIS <- read.LIS(c("NO16h", "KR16h", "TW16h"), vars=c("dname", "dhi", "nhhmem"))
LIS %>% mutate(edhi = dhi/sqrt(nhhmem)) %>% 
        group_by(dname) %>% 
        summarise(Gini(edhi))

## Finding:
## The Gini coefficient for South Korea, Taiwan, and Norway is 0.374, 0.329, and
## 0.329 in 2016. South Korea has the highest level of income inequality among the 
## three counties.

## Compute and compare the GINI coefficient for inequality across households, 
## using 2 different income concepts - before and after redistribution.
library(dplyr)
library(ineq)
LIS <- read.LIS(c("NO16h", "KR16h", "TW16h"), vars=c("dname", "dhi", "nhhmem", "hitotal"))

#income after redistribution
LIS %>% mutate(edhi = dhi/sqrt(nhhmem)) %>% 
  group_by(dname) %>% 
  summarise(Gini(edhi))
#before the redistribution
LIS %>% mutate(ehitotal = hitotal/sqrt(nhhmem)) %>% 
  group_by(dname) %>% 
  summarise(Gini(ehitotal))

## Finding:
## The Gini coefficient tends to be lower after income redistribution in South Korea and Norway.
## This might be because transfers and taxes can limits disposal income among households in 
## the highest quantile thus reduce income inequality to some extent. And this reduction is 
## more effective in Norway than in South Korea. However, in Taiwan, the Gini coefficient after 
## redistribution becomes a little bit higher. It indicates that its tax act does not have a 
## significant effect on reducing inequality.

## Compute (individual-level) relative poverty (AROP60 = equivalized disposable income < 60% of national median)
## Do this for 3 different years in a country
library(dplyr)

# read individual-level data
P <- read.LIS(c("TW10p", "TW13p", "TW16p"), 
              vars=c("dname", "pid", "hid", "sex"))
# read household-level data
H <- read.LIS(c("TW10h", "TW13h", "TW16h"), 
              vars=c("dname", "hid", "dhi", "nhhmem"))
# merge two data sets
LIS <- merge(P, H, by = c("dname", "hid")) 

At_risk <- LIS %>% 
  mutate(edhi = dhi/sqrt(nhhmem)) %>% #calculate equivalized household income
  group_by(dname) %>% 
  mutate(t = median(edhi)*0.6) %>%  #calculate poverty line
  filter(edhi < t) %>% 
  summarise(n=n()) #count number of households under poverty line

Total <- LIS %>%
  group_by(dname) %>% 
  summarise(n=n()) #count the number of households by year

# calculate percentage of households under poverty line in each year
Rate <- At_risk$n / Total$n
Rate

## Finding:
## Results show that the AROP of Taiwan has been decreased from 2010 to 2016. In 2010,
## there are 18.96% of households at risk of poverty. However, this value decreases to 17.10%
## in 2013, and a further 16.13% in 2016. Taiwan seems to have made progress in poverty 
## reduction across these years.

# Another method for this question using `laeken` package
library(laeken)

tw10 <- LIS %>% 
  mutate(edhi = dhi/sqrt(nhhmem)) %>% 
  filter(dname == "tw10")

r10 <- arpr(tw10$edhi)
r10

tw13 <- LIS %>% 
  mutate(edhi = dhi/sqrt(nhhmem)) %>% 
  filter(dname == "tw13")
r13 <- arpr(tw13$edhi)
r13

tw16 <- LIS %>% 
  mutate(edhi = dhi/sqrt(nhhmem)) %>% 
  filter(dname == "tw16")
r16 <- arpr(tw16$edhi)
r16


## Differentiate the trends by gender
library(ggplot2)
library(dplyr)

# read and merge the data sets
P <- read.LIS(c("TW10p", "TW13p", "TW16p"), 
              vars=c("dname", "pid", "hid", "sex"))
H <- read.LIS(c("TW10h", "TW13h", "TW16h"), 
              vars=c("dname", "hid", "dhi", "nhhmem"))
LIS <- merge(P, H, by = c("dname", "hid"))

# calculate AROP by gender and year
Risk_by_gender <- LIS %>% 
  mutate(edhi = dhi/sqrt(nhhmem)) %>% 
  group_by(dname, sex) %>% 
  mutate(t = median(edhi)*0.6) %>% 
  filter(edhi < t) %>% 
  summarise(n=n()) %>% 
  head()

Total_by_gender <- LIS %>%
  group_by(dname, sex) %>% 
  summarise(n=n())

Rate_by_gender <- Risk_by_gender$n / Total_by_gender$n
Rate_by_gender

# visualize the result
Total_by_gender$AROP <- Rate_by_gender
DT <- Total_by_gender %>%
  select(dname, sex, AROP)

ggplot(DT, aes(x=sex)) +
  geom_point(aes(y=AROP, color=dname), size = 5)

## Findings:
## Women have a higher percentage of AROP than men in 2010 and 2013 in Taiwan. 
## The gender gap decrease in 2016. However, this subtle difference can not be seen 
## from the index in Exercise 4. Women's AROP is higher than the general AROP resulted 
## from Exercise 4 in both 2010 and 2013. As Bennett claims that equalized household 
## income assumes equal sharing of resources, ignoring predominantly women’s economic 
## dependence on men. Thus, only calculating general household income will ignore the 
## gender difference in income and poverty. And in 2016, when women have more equal 
## income distribution, the AROP is close to the general AROP resulted in Exercise 4. 
## It seems that Taiwan has made progress not only in poverty reduction but also in 
## gender equality across these years.






