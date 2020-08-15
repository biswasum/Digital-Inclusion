#install.packages("XLConnect")
require(XLConnect)
library(XLConnect)
library(XLConnectJars)


# Load the MAHARASHTRA State data
workbook <- loadWorkbook(file.choose(),create = TRUE)

workbook1 <- readWorksheet(workbook,sheet = "Sheet1",header = TRUE)
workbook

names(workbook1)
head(workbook)
# OPTINAL: Select all the relevant features for PCA
df <- workbook1[,10:20]

##=======================
## Applying PCA Algorithm
##=======================

prin_comp <- prcomp(df,scale=TRUE)

print(prin_comp)


head(prin_comp)
names(prin_comp)

## Find the measures of PCA:
## center and scale refers to respective mean and standard deviation of the variables that are used for normalization .

prin_comp$scale


#outputs the mean of variables

prin_comp$center
#outputs the standard deviation of variables
prin_comp$scale

# Distribution of total variance for the features

plot(prin_comp,type = "l")


## Use ggbiplot screen  ;Contact  Ravi

biplot(prin_comp, scale = TRUE)

prin_comp_decluttered <- princomp(df)

names(prin_comp_decluttered)

biplot(prin_comp_decluttered,scale = TRUE, xlabs=rep("",nrow(df)))
summary(prin_comp)

# Find the important measure of Principal Components
##The rotation measure provides the principal component loading

prin_comp$rotation

## Look at first few principal components and first 10 rows.
prin_comp$rotation[1:10,1:10]


# Compute standard deviation of each principal component
std_dev <- prin_comp$sdev


# Compute variance
pr_var <- std_dev^2



#  Proportion of variance- To compute the proportion of variance explained by each Principal components,
 ## simply divide the variance by sum of total variance.

prop_varex <- pr_var/sum(pr_var)

prop_varex[1:20]

# 'x' = a numeric matrix or data frame which provides the data for the principal components analysis.
dim(prin_comp$x)

#scree plot - plots the variances against the number of the principal component
## A scree plot is used to access components or factors which explains the most of variability in the data.

plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")



# Cumulative scree plot
##========================

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")



