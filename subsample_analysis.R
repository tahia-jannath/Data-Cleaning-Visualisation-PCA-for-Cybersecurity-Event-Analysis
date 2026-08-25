#Name: Tahiatul Jannath Bhuiyan
#Student_ID: 10637997
# Load required libraries
library(dplyr) #This library is used for data manipulation
library(moments) 
library(ggplot2)#This library is used for Graphical representation
library(factoextra) # This library is used to extract and visualize the Results of Multivariate Data Analyses

# path of working directory
dat <- read.csv("F:/Semester_02/Data_visualaization/Assignment_01/HealthCareData_2024_me.csv", stringsAsFactors = TRUE)


# Separate samples of normal and malicious events
dat.class0 <- dat %>% filter(Classification == "Normal") # normal
dat.class1 <- dat %>% filter(Classification == "Malicious") # malicious
# Randomly select 400 samples from each class, then combine them to form a working dataset
#Here seed command is used, so that every time it creates same data
set.seed(10637997)  # This is my student ID here
rand.class0 <- dat.class0[sample(1:nrow(dat.class0), size = 400, replace = FALSE),]
rand.class1 <- dat.class1[sample(1:nrow(dat.class1), size = 400, replace = FALSE),]

# My sub-sample of 800 observations
# rowbinding of two data frame into one, combining the random dataset 1&2
mydata <- rbind(rand.class0, rand.class1)
dim(mydata) # Check the dimension of your sub-sample

# Check the data type for each feature
str(mydata)

# Exploratory Data Analysis -----------------------------------------------
#--------Part_1 (i)Ans:---------------
# calculate frequencies and percentages
#Feature_01 AlertCategory
table(mydata$AlertCategory)
prop.table(table(mydata$AlertCategory))
#Feature_02 NetworkEventType
table(mydata$NetworkEventType)
prop.table(table(mydata$NetworkEventType))
#Feature_03 NetworkInteractionType
table(mydata$NetworkInteractionType)
prop.table(table(mydata$NetworkInteractionType))
#Feature_04 Classification
table(mydata$Classification)
prop.table(table(mydata$Classification))
#Feature_05 SessionIntegrityCheck
table(mydata$SessionIntegrityCheck)
prop.table(table(mydata$SessionIntegrityCheck))
#Feature_06 ResourceUtilizationFlag
table(mydata$ResourceUtilizationFlag)
prop.table(table(mydata$ResourceUtilizationFlag))

#--------Part_1 (ii)Ans:---------------
# calculate number of observations, missing values, missing values percentages,
# minimum, maximum, mean, median and skewness

# number of missing
mydata %>% select(where(is.numeric)) %>% apply(., MARGIN = 2, function(x) sum(!is.na(x))) # number of missing
# number of complete
mydata %>% select(where(is.numeric)) %>% apply(., MARGIN = 2, function(x) sum(is.na(x)))  # number of complete
# percentage missing
mydata %>% select(where(is.numeric)) %>% apply(., MARGIN = 2, function(x) sum(is.na(x))/length(x))
# minimum
mydata %>% select(where(is.numeric)) %>% apply(., MARGIN = 2, function(x) min(x, na.rm = T))
# maximum
mydata %>% select(where(is.numeric)) %>% apply(., MARGIN = 2, function(x) max(x, na.rm = T))
# mean
mydata %>% select(where(is.numeric)) %>% apply(., MARGIN = 2, function(x) mean(x, na.rm = T))
# median
mydata %>% select(where(is.numeric)) %>% apply(., MARGIN = 2, function(x) median(x, na.rm = T))
# skewness
mydata %>% select(where(is.numeric)) %>% apply(., MARGIN = 2, function(x) skewness(x))  

# Merger Policy_Violation and PolicyViolation into PolicyViolation  
mydata$NetworkEventType[mydata$NetworkEventType == "Policy_Violation"] <- "PolicyViolation"

#-----------Part_1(iii)Ans:--------------------------------------

# identify outliers --------------------------------------------------------

# observing violin plots for outlier

ggplot(mydata) +
  geom_violin(aes(x = NetworkAccessFrequency, y = Classification), fill = "royalblue", alpha = 0.6) +
  theme_bw() +
  labs(title = "Violin plot of NetworkAccessFrequency")

ggplot(mydata) +
  geom_violin(aes(x = ResponseTime, y = Classification), fill = "royalblue", alpha = 0.6) +
  theme_bw() +
  labs(title = "Violin plot of ResponseTime", y = "Frequency")

# outlier exists



# Identifying outliers in NetworkAccessFrequency using IQR method
q <- quantile(mydata$NetworkAccessFrequency, probs = c(0.25, 0.75), na.rm = TRUE)
iqr <- q[2] - q[1]
lower_bound <- q[1] - 1.5 * iqr
upper_bound <- q[2] + 1.5 * iqr
outliers <- mydata$NetworkAccessFrequency < lower_bound | mydata$NetworkAccessFrequency > upper_bound
prop.table(table(outliers)) # 4.9% outliers

#----------------Part_2(i)Ans:-----------------------
# Replacing outliers in NetworkAccessFrequency with NA
mydata$NetworkAccessFrequency <- replace(mydata$NetworkAccessFrequency, outliers, NA)


# Identifying outliers in ResponseTime using IQR method
q <- quantile(mydata$ResponseTime, probs = c(0.25, 0.75), na.rm = TRUE)
iqr <- q[2] - q[1]
lower_bound <- q[1] - 1.5 * iqr
upper_bound <- q[2] + 1.5 * iqr
outliers <- mydata$ResponseTime < lower_bound | mydata$ResponseTime > upper_bound
prop.table(table(outliers))   # 8.1% outliers

# Replacing outliers in ResponseTime with missing value
mydata$ResponseTime <- replace(mydata$ResponseTime, outliers, NA)



# plot again to observe the change in distribution
ggplot(mydata) +
  geom_violin(aes(x = NetworkAccessFrequency, y = Classification), fill = "royalblue", alpha = 0.6) +
  theme_bw() +
  labs(title = "Violin plot of NetworkAccessFrequency (no outlier)")

ggplot(mydata) +
  geom_violin(aes(x = ResponseTime, y = Classification), fill = "royalblue", alpha = 0.6) +
  theme_bw() +
  labs(title = "Violin plot of ResponseTime (no outlier)", y = "Frequency")

mydata$SystemAccessRate <- NULL  # removing a feature with too many outliers



#---------Part_2(ii)Ans:--------------------------------
#Cleaned data is saved in mydata csv -------------------------------------------------------------

write.csv(mydata,"mydata.csv")

#---------Part_2(iii)Ans:--------------------------------
#----PCA ---------------------------------------------------------------------

# Selecting numeric features and Classification
pcdata <- mydata %>% 
  select(where(is.numeric), Classification) %>%  # selecting relevant columns
  select(-X) %>%  # Do not include index column
  na.omit()  

1 - nrow(pcdata)/nrow(mydata) # 12.9% removed

pcdata_num <- pcdata %>% select(-Classification)  # select only numeric columns for PCA

# principal component analysis on scaled data
pca_res <- prcomp(pcdata_num, center = TRUE, scale. = TRUE)

summary(pca_res)  # proportion of variation

pca_res$rotation %>% round(3)  # loadings


#---------Part_2(iv)Ans:--------------------------------
# Biplot ------------------------------------------------------------------
fviz_pca_biplot(pca_res,
                axes = c(1,2),  #Specifying the PCs to be plotted. 
                #Parameters for samples
                col.ind=pcdata$Classification,  #Outline colour of the shape
                fill.ind=pcdata$Classification,  #fill colour of the shape
                alpha=0.7,  #transparency of the fill colour
                pointsize=1,  #Size of the shape
                pointshape=21,  #Type of Shape
                #Parameter for variables
                col.var="red",  #Colour of the variable labels
                label="var",  #Show the labels for the variables only
                repel=TRUE,  #Avoid label overplotting
                addEllipses=TRUE,  #Add ellipses to the plot
                legend.title=list(colour="Classification",fill="Classification",alpha="Classification"))

#------------ans(v)------------
#histogram to visualaize the contribution of PC1
ggplot(pca_res$x) +
  geom_histogram(aes(x = PC1, fill = pcdata$Classification), alpha = 0.5) +
  theme_bw()
#histogram to visualaize the contribution of PC2
ggplot(pca_res$x) +
  geom_histogram(aes(x = PC2, fill = pcdata$Classification), alpha = 0.5) +
  theme_bw()
