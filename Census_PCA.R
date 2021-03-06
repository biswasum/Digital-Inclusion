#install.packages("XLConnect")
require(XLConnect)
library(XLConnect)
library(XLConnectJars)

# Load the MAHARASHTRA State data
workbook <- loadWorkbook(file.choose(),create = TRUE)

workbook1<-readWorksheet(workbook,sheet = "Sheet1",header = TRUE)
workbook

names(workbook1)
head(workbook)
# OPTINAL: Select all the relevant features for PCA
df<-workbook1[,10:90]

##=======================
## Applying PCA Algorithm
##=======================

prin_comp<-prcomp(df,scale=TRUE)

print(prin_comp)
prin_comp

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

biplot(prin_comp, scale = TRUE)

summary(prin_comp)

dimdesc(prin_comp, axes=c(1,2))
# Find the important measure of Principal Components
##The rotation measure provides the principal component loading

prin_comp$rotation

## Look at first few principal components and first 10 rows.
prin_comp$rotation[10:90,10:90]

prin_comp


# Compute standard deviation of each principal component
std_dev <- prin_comp$sdev
std_dev

# Compute variance
pr_var <- std_dev^2



#pr_var[1:10]

#  Proportion of variance- To compute the proportion of variance explained by each Principal components,
 ## simply divide the variance by sum of total variance.

prop_varex <- pr_var/sum(pr_var)

prop_varex[1:20]

# 'x' = a numeric matrix or data frame which provides the data for the principal components analysis.
dim(prin_comp$x)
#scree plot - file:///C:/Users/Biswajeet/Desktop/CensusProject/Reports/Census_Project Report_ver1.3.docx
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")



# Cumulative scree plot
##========================


plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

summary(prop_varex)

##  END OF CODE ####
#####################


