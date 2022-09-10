# LSE Data Analytics Online Career Accelerator

# DA301: Advanced Analytics for Organisational Impact

# Week 5: Wrangling data with R

###############################################################################

# This week, you will build on your R knowledge and learn to wrangle and 
# manipulate complex data sets in R using the aggregate(), apply(), and
# str_ functions. You will also perform descriptive and inferential statistical
# operations on data sets in R to deduce business insights to aid data-informed
# decision-making.

# Additionally, you will create advanced visualisations using ggplot2 to present 
# your insights to stakeholders. You will end this week by combining R, Python,
# and SQL to perform complex data analysis. 

# This is your R Script. Use it to follow along with the demonstrations, to test
# ideas and explore what is possible. The hands-on experience of writing your
# own code will accelerate your learning!
  
###############################################################################

# 5.1 Analysing data in R

# 5.1.7 Practical activity: Data manipulation in R ----------------------------

# 1. Prepare your workstation

# Import libraries - this might take a minute or so.
# The whole tidyverse package.
library(tidyverse)
# Create insightful summaries of the data set.
library(skimr)
# Create insightful reports on the data set.
library(DataExplorer)


# Import the data set (seatbelt.csv).
seatbelt <- read.csv(file.choose(), header=T)


# Sense-check  the data set.
as_tibble(seatbelt)
View(seatbelt)

##############################################################################

# 2. Explore the data set

# Determine the sum of missing values (data frame and seatbelt column). 
sum(is.na(seatbelt))
sum(is.na(seatbelt$seatbelt))


# Replace NA with 0.
seatbelt[is.na(seatbelt)] = 0


# View the result.
head(seatbelt)
sum(is.na(seatbelt$seatbelt))


# Determine the descriptive statistics.
summary(seatbelt)


# Create a data profile report.
DataExplorer::create_report(seatbelt)

###############################################################################

# 3. Manipulate (data wrangling) the data set

# Drop unnecessary columns (e.g. column X).
seatbelt_df <- subset(seatbelt, select=-c(1))

# Create a subset of the data frame with only numeric columns.
seatbelt1 <- seatbelt_df %>% keep(is.numeric)

# View the result.
head(seatbelt1)


# Round all the columns to 2 decimal places.
seatbelt1 <- round(seatbelt1, 2)

# View the result.
head(seatbelt1)

##############################################################################

# 4. Visualise data set

# visualise data with boxplot to determine normal 
# distribution of separate columns.
boxplot(seatbelt1$miles)
boxplot(seatbelt1$fatalities)
boxplot(seatbelt1$income)
boxplot(seatbelt1$age)
boxplot(seatbelt1$seatbelt)


###############################################################################

# 5. Perform data manipulation

# Calculate the sum of all the columns.
aseatbelt1 <- apply(seatbelt1, 2, sum)

head(aseatbelt1)

aseatbelt1 <- round(aseatbelt1, 2)

# View the results.
head(aseatbelt1)
# It only make sense to use the sum for columns fatalities and income.


# Calculate the min of all the columns.
sseatbelt1 <- sapply(seatbelt1, min)
sseatbelt1 <- round(sseatbelt1, 2)

# View the results.
sseatbelt1
# We can use all the columns to draw conclusions.


# Calculate the max of all the columns.
bseatbelt1 <- sapply(seatbelt1, max)
bseatbelt1 <- round(bseatbelt1, 2)

# View the results.
bseatbelt1
# We can use all the columns to draw conclusions.

##########################################################################

# Advanced exercise

# Focus on specific variables with select() function.
seatbelt_agg <- select(seatbelt, c('state',
                                   'year',
                                   'miles'))

# View the result.
as_tibble(seatbelt_agg)



# Focus on specific variables with the select() function.
seatbelt_agg2 <- select(seatbelt, c('drinkage',
                                    'year',
                                    'miles'))

# View the result.
as_tibble(seatbelt_agg2)


# Can you think of other columns to compare?

###############################################################################

# 6. Save your R script

# 5.1.9 Practical activity: Wrangling raw data --------------------------------

# 1. Prepare your workstation

# Import the necessary libraries.
library(tidyverse)
# Create insightful summaries of data set.
library(skimr)
# Create insightful reports of data set.
library(DataExplorer)


# Import the police.csv file.
police <- read.csv(file.choose(), header=T)


# View the data frame.
as_tibble(police)
dim(police)

View(police)

##############################################################################

# 2. Explore the data set

# Determine if there are missing values. 
police[is.na(police)] 
sum(is.na(police))


# Delete all the records with missing values.
police_new <-na.omit(police)

# View the result.
head(police_new)
dim(police_new)
sum(is.na (police_new))


# Determine the descriptive statistics.
summary(police_new)

DataExplorer::create_report(police_new)


##############################################################################

# 3. Data manipulation

# Drop unnecessary columns.
police_df <- select(police_new, -c('X', 'idNum', 'date', 'MDC', 'preRace',
                                   'race', 'lat', 'long', 'policePrecinct',
                                   'citationIssued', 'personSearch', 
                                   'vehicleSearch'))

# View the result.
colnames(police_df)
dim(police_df)


# Rename column names with first letter to uppercase.
names(police_df) <- str_to_title(names(police_df))

# View the result.
colnames(police_df)
View(police_df)


# Determine the unique values in each column
unique(police_df$Problem)

unique(police_df$Gender)

unique(police_df$Neighborhood)

###############################################################################

# 4. Visualise data

# Create barplots.
# How many offences were suspicious compared to traffic?
barplot(table(police_df$Problem),
        main='Police reports',
        xlab='Offense',
        ylab='Count',
        col='red')

# How many offences were gender based?
barplot(table(police_df$Gender),
        main='Police reports',
        xlab='Gender',
        ylab='Count',
        col='blue')

# How do the neighbourhoods compare?
barplot(table(police_df$Neighborhood),
        main='Police reports',
        xlab='Neighbourhood',
        ylab='Count',
        col='green')

###############

# Is the number of occurrences (problems and traffic) gender based?
# Determine the number of occurrences for gender and problems.
table(police_df$Gender)            
table(police_df$Problem)
table(police_df$Neighborhood)

###############

# Determine only females with traffic.
nrow(subset(police_df,
            Gender=='Female' & Problem=='traffic'))

# Determine only males with traffic.
nrow(subset(police_df, Gender=='Male' & Problem=='traffic'))

###############

# What is the total number of occurrences per neighbourhood?
# Determine neighbourhoods with occurrences.
police_df %>% count(Neighborhood, sort=T)


# 5.2.3 Practical activity: Statistical analysis in R -------------------------

# 1. Prepare your workstation

# Import the necessary libraries.
library(tidyverse)


# Import the insurance data set (insurance.csv).
health <- read.csv(file.choose(), header=T)

# View the data frame.
head(health)

str(health)

View(health)

###############################################################################

# 2. Explore the data set

# Determine descriptive statistics of the data set.
summary(health)
summary(health$bmi)


# Measure central tendencies of BMI with mean and median.
mean(health$bmi)
median(health$bmi)


# Statistics of extreme values (max and min).
min (health$bmi)
max (health$bmi)


# Measure the variability of BMI values.
# Range = Maximum - Minimum.
max(health$bmi)- min(health$bmi)  

# Function to calculate Q1.
quantile(health$bmi, 0.25)  

# Function to calculate Q2.
quantile(health$bmi, 0.75)   

# Function to calculate IQR.
IQR(health$bmi)    

# Function to determine the variance.
var(health$bmi)

# Function to return the standard deviation.
sd(health$bmi) 

###############################################################################

# 3. Determine if data is normally distributed

# Measure normality in BMI values.
# Q-Q plot:
qqnorm(health$bmi)
# Add a reference line:
qqline(health$bmi, col='red')


# Shapiro-Wilk test:
shapiro.test((health$bmi))
# Our p-value is <0.05, and we can conclude that the sample data is not 
# normally distribution.


# Check for skewness.
# First import the moments package and library.
library(moments)

# Now we can check for skewness.
skewness(health$bmi)
# Our output suggests a positive skewness.


#Check for kurtosis.
kurtosis(health$bmi)
# Our kurtosis value is less than 3, suggesting our data is platykurtic (thin-tailed).

###############################################################################

# 4. Determine if there are any correlation(s)

# Check correlation between BMI and client age.
# Let's first check for normality in the client age values.
shapiro.test(health$age)
# Our output is greater than 0.5, and we can assume normality.


# Check correlation between BMI and age using Pearson's correlation.
cor(health$bmi, health$age)

# Our correlation coefficient of 0.11 suggests a weak positive correlation.

################################################################################

# 5. Save your R script



# 5.2.7 Practical activity: Using ggplot2 to visualise data -------------------

# 1. Prepare your workstation

# Import the necessary libraries.
library(tidyverse)


# Import the insurance data set (insurance.csv).
health <- read.csv(file.choose(), header=T)

# View the data frame.
head(health)
str(health)
summary(health)

###############################################################################

# 2. Create visualisations

# 2.a) Create a scatterplot
# Compare age (x-variable) and charges (y-variable).
ggplot(health,
       mapping=aes(x=age, y=charges)) +
  geom_point()

#####

# 2b) Remove the outliers

# Remove outliers (>50,000).
# Create a new data frame to store the data set.
# Create a new scatteplot.
new_health <- filter(health, charges<50000)

# View result.
ggplot(new_health,
       mapping=aes(x=age, y=charges)) +
  geom_point()

####

# 2c) Create three new scatterplots

## i - Compare age and charges based on sex.
# Change colours, adjust size and alpha of points.
ggplot(new_health,
       mapping=aes(x=age, y=charges)) +
  geom_point(color='purple',
             alpha=0.75,
             size=2.5) +
  # Add labels and change axes marks.
  scale_x_continuous(breaks=seq(0, 70, 5), "Age of the Individual") +
  scale_y_continuous(breaks=seq(0, 55000, 5000), "Monthly charges (in $)") +
  # Add a title and subtitle.
  labs(title="Relationship between age and charges",
       subtitle="A survey from a health insurance provider") +
  # Facet by sex.
  facet_wrap(~sex)


## ii - Compare age and charges based on region.
# Facet by region
ggplot(new_health,
       mapping=aes(x=age, y=charges)) +
  geom_point(color='blue',
             alpha=0.75,
             size=2.5) +
  scale_x_continuous(breaks=seq(0, 70, 5), "Age of the Individual") +
  scale_y_continuous(breaks=seq(0, 55000, 5000), "Monthly charges (in $)") +
  labs(title="Relationship between age and charges",
       subtitle="A survey from a health insurance provider") + 
  facet_wrap(~region)


## iii - Compare age and charges based on children.
# Facet by children
ggplot(new_health,
       mapping=aes(x=age, y=charges)) +
  geom_point(color='red',
             alpha=0.75,
             size=2.5) +
  scale_x_continuous(breaks=seq(0, 70, 5), "Age of the Individual") +
  scale_y_continuous(breaks=seq(0, 55000, 5000), "Monthly charges (in $)") +
  labs(title="Relationship between age and charges",
       subtitle="A survey from a health insurance provider") + 
  facet_wrap(~children)

####

# 2d) Add a third variable (smoker)

# Create a scatterplot to view result.
ggplot(new_health, 
       mapping=aes(x=age, y=charges, col=smoker)) +
  geom_point() +
  scale_x_continuous(breaks=seq(0, 70, 5), "Age of member") +
  scale_y_continuous(breaks=seq(0, 55000, 5000), "Monthly charges")

###############################################################################

# 3. Save your R script


# 5.2.10 Practical activity: Choosing the right graphic -----------------------

# 1. Prepare your workstation

# Import the necessary libraries.
library(tidyverse)


# Import the insurance data set (insurance.csv).
health <- read.csv(file.choose(), header=T)

# View the data frame.
head(health)
str(health)
summary(health)

###############################################################################

# 2. Create visualisations

# The plots below are suggestions and other plots may work better for you!
# Remember to motivate your choice of visualisation.
# Feel free to play with colours, labels and titles.

# Plot age on a histogram.
ggplot(health, aes(x=age)) +
  geom_histogram(stat='count',
                 fill='blue')


# Plot children on a histogram.
ggplot(health, aes(x=children)) +
  geom_histogram(stat='count',
                 fill='red')


# Plot region and sex on a stacked barplot.
ggplot(health, aes(x=region, fill=sex)) +
  geom_bar()



# Plot smoker and sex on a grouped barplot.
ggplot(health, aes(x=smoker, fill=sex)) +
  geom_bar(position='dodge')



# Plot BMI and sex on a side-by-side boxplot.
ggplot(health, aes(x=sex, y=bmi)) +
  geom_boxplot()



# Plot BMI and region on a side-by-side violinplot.
ggplot(health, aes(x=region, y=bmi)) +
  geom_violin(fill='orange')



# Plot BMI and smoker on a side-by-side boxplot.
ggplot(health, aes(x=smoker, y=bmi)) +
  geom_boxplot(fill='purple')

###############################################################################

# 3. Select two plots to add colour, titles, and labels.

# We have chosen the BMI vs smoker plot.
ggplot(health, aes(x=smoker, y=bmi)) +
  geom_boxplot(fill='green',
               notch=TRUE,
               outlier.color='blue')

# We have chosen smoker vs sex.
ggplot(health, aes(x=smoker, fill=sex)) +
  geom_bar(position='dodge') +
  scale_fill_manual(values=c('purple', 'orange')) +
  labs(title="Count of male and female smokers")

###############################################################################

# 4. Select two plots and and add a suitable theme

# For a website:
ggplot(health, aes(x=region, y=bmi)) +
  geom_violin() +
  theme_dark()



# For a publication:
ggplot(health, aes(x=region, fill=sex)) +
  geom_bar() +
  theme_classic()

###############################################################################

# 5. Save your R script
