#Course            : Selected Topics in Computational Statistics



# To import the active_dataset
dataset <- read.csv("C:/Users/prati/Documents/A hochschule mittweida/computational statistics/active.csv") ##Use path for data

dataset 


#QN_1. Read the article describing the study, so you can understand as much as possible concerning the data.

##===========================================================================================================================

#QN_2.  What are the scales of each variable?

names(Dataset)
#site     nominally scaled variable,categorical variable i.e. 'UAB','BHRCA','IU','JHU','PenState','WSU'
#age      metric variable 
#edu      metric variable based on data 
#group    nominally scaled variable,categorical variable i.e. control,memory,reasoning,and speed
#booster  nominally scaled variable,categorical variable i.e 1 and 0
#sex      nominally scaled variable,categorical variable i.e. Male and Female 
#reason   metric variable
#ufov     metric variable
#hvltt    metric variable
#hvltt2   metric variable
#hvltt3   metric variable
#hvltt4   metric variable
#mmse     metric variable
#id       nominally scaled variable 

#Nominal and ordinal scaled variable are categorical variable

##===========================================================================================================================

#QN_3. Change the format in R, i.e., code each categorical variable as factor.
#For the variable "sex" code it as "M" and "F" instead of 1 and 2.

#3 #converting each categorical variable as a factor
#site
Data <- as.data.frame(Dataset) 
Data

#3 # Converting to factors
Data$site <- as.factor(Data$site)
Data$group <- as.factor(Data$group)
Data$booster <- as.factor(Data$booster)
Data$sex <- as.factor(Data$sex)
Data$id  <- as.factor(Data$id)

# Assigning levels
levels(Data$site) <- c('UAB','BHRCA','IU','JHU','PenState','WSU')
levels(Data$group) <- c('control','memory','reasoning','speed')
levels(Data$booster) <- c('No','Yes')
levels(Data$sex) <- c("M","F")
Data

summary(Data)

##==========================================================================================================================


#QN_4. Provide a frequency table (absolute and relative counts + marginals) of the variable "sex" alongside with 
#bar charts. 

#4 # absolute frequency #number of times the value is repeated in the data vector
# f1+f2+..+fn = N
# fi....... frequency of each value
# N.........total no. of data values
#table()/length(x) # x is a vector where we assign values
sex_absfr <- table(Data$sex) # absolute frequency table
sex_absfr #M->373 #F->1202


#4 #relative frequency = fi/length(fi) # the absolute frequency of that event divided by the total number of events
sex_rf <- sex_absfr/length(Data$sex) 
sex_rf #printing frequency table #M->0.2368254 #F->0.7631746

#4 #marginal #the sum of any one of the rows or columns in a data matrix
marginal <- margin.table(sex_absfr,1) # M->373 F->1202
marginal


#plotting in barplot
barplot(sex_absfr)
barplot(sex_rf)


#=================================================================================================================================


#QN_5. Provide summary statistics for the variables "age", "reason", "hvltt", "hvltt2", "hvltt3", "hvltt4",boxplots, and histograms.


#5 #summary statistics #provides the gist of the information about the sample data
#summary for different variables
summary(Data$age)     #summary for age
summary(Data$reason)  #summary for reason
summary(Data$hvltt)   #summary for hvltt
summary(Data$hvltt2)  #summary for hvltt2
summary(Data$hvltt3)  #summary for hvltt3
summary(Data$hvltt4)  #summary for hvltt4

#5 #boxplots #shows the five-number summary of a set of data: 
#including the minimum score, first (lower) quartile, median, third (upper) quartile, and maximum score.

boxplot(Data$age, Data$reason,Data$hvltt,Data$hvltt2, Data$hvltt3,Data$hvltt4)

#5 #histograms #a bar graph representation of data
#histogram for age
age <- Data$age
hist(age)
#histogram for reason
reason <- Data$reason  
hist(reason)
#histogram for hvltt
hvltt <- Data$hvltt
hist(hvltt)
#histogram for hvltt2
hvltt2 <- Data$hvltt2
hist(hvltt2)
#histogram for hvltt3
hvltt3 <- Data$hvltt3
hist(hvltt3)
#histogram for hvltt4
hvltt4 <- Data$hvltt4 
hist(hvltt4)


##=========================================================================================================================



#QN_6.Investigate any if there is a correlation between "age" and "reason". 
#     Intuitively reason should decline with increasing age, so we expect a negative correlation.
#     Provide a scatter plot. Perform a one-sided test.
#     Which correlation coefficient do you prefer in this case?

attach(Data) #accessing the variables present in the data framework without calling the data frame


#6 #correlation coefficient #a number between -1 and +1
#correlation between age and reason
#Pearson correlation 
Pear <-cor(age,reason,use = "complete", method ="pearson") #-0.3471376 
Pear
#Spearman's Correlation 
#rank correlation coefficient,uses the rankings of data from each variable (e.g.from lowest to highest) 
Spear <- cor(age,reason,use = "complete", method ="spearman") #-0.3380581 
Spear

#Kendall rank correlation
Kendal <- cor(age,reason,use = "complete", method ="kendall") #-0.2374444
Kendal

#Pearson's correlation is preferable because,
  #data does not have much outlier,
  #data is not monotonic


#6 #scatter plot between age and reason
plot(age,reason) #age and reason #hence shows negative correlation

#6 one sided test #sample being tested falls into the one-sided critical area,
# either greater than or less than a certain value, but not both

# H0 : age and reason are uncorrelated
# H1 : age and reason are correlated
# significance level alpha = 0.05

Dataset

x1 <- Dataset$age[Dataset$reason]
t.test(x1,alternative = "two.sided") #p-value < 2.2e-16
#this means null hypothesis rejected and they are correlated


##============================================================================================================================


# QN_7. To see whether the age distributions is comparable among the sexes, 
#       perform a two-sided test (t-test and a non-parametric alternative). 
#       Check whether you can safely assume equal variances (test for this too).
#       If the variances are equal, would you use the Welch-correction (yes or no and why)


# H0 : age distribution can't be comparable among males and females
# H1 : age distribution can be comparable among males and females
# significance level alpha = 0.05


#comparing #assigning age with male and female respectively 
x1 <- Dataset$age[Dataset$sex =="M"]
x2 <- Dataset$age[Dataset$sex =="F"]


plot(age,sex) #scatter plot between age and sex
#age and sex are independent variable

#testing for normality: #create Q-Q plot #or create histogram
qqnorm(x1) 
hist(x1) #somewhat normal

qqnorm(x2)
hist(x2) # not normal
#since data in not normal we perform nonparametric test that does not perform normalty


# To check with test function #assume variances are equal
t.test(x1,x2, alternative = "less",var.equal = TRUE) #t = -0.04121 

#to check variances are equal or not we assume 
var(x1,na.rm = TRUE) #23.83376
var(x2,na.rm = TRUE) #34.68234
#variances are not equal

#to check variances manually, we perform test statistics as follows
n <- sum(!is.na(x1)) #1575 #sum of age and Male
m <- sum(!is.na(x2)) #220  #sum of age and Female 

sp <- sqrt((n-1)/(n+m-2)*var(x1,na.rm = TRUE) + (m-1)/(n+m-2)*var(x2,na.rm = TRUE)) #pooled standard deviation

#checking the means between two groups
X <- mean(x1,na.rm = TRUE)  #73.82603
Y <- mean(x2,na.rm = TRUE)  #73.84091

# Test statistics
(X-Y-0)/(sp*sqrt(1/n+1/m)) #t = -0.04121 

# Variances are equal based on test statistics
# Since variances are equal we do not need to perform "welch correction" test



#nonparametric test to two-sample t-test
#Mann Whitney U test - Wilcoxon rank sum test

wilcox.test(x1,x2,alternative = "two.sided") 
#p value: 0.4191 > 0.05 we fail to reject the null hypothesis #thus means between two variables are equal  


#==============================================================================================================================


#QN_8. Introduce a new variable called "sex-boost". 
#      The factors should be "M-No", "M-Yes", "F-No", "F-Yes"for males or females with or without the booster training. 
#      Compare the age distribution across the new variable.
#      Perform a 1-Way ANOVA and a nonparametric alternative. Which would you prefer in this case?
#      (Argue based on whether the assumptions for the respective tests hold - this might require even more testing.) 
#      If age cannot be assumed to be identical across the groups, perform pairwise t-tests (with a common estimate for variance) 
#      and identify which groups are different. Apply a multiple test-correction. 
#      Which test correction would you prefer?



for (i in 1:length(Data$sex)){
  if (Data$sex[i] == "M" && Data$booster[i] == "Yes") {
    
    Data$sex_boost[i] <- "M-Yes" 
  } else if (Data$sex[i] == "M" && Data$booster[i] == "No") {
    
    Data$sex_boost[i] <- "M-No" 
  } else if (Data$sex[i] == "F" && Data$booster[i] == "Yes") {
    
    Data$sex_boost[i] <- "F-Yes" 
  } else if (Data$sex[i] == "F" && Data$booster[i] == "No") {
    
    Data$sex_boost[i] <- "F-No"
    
  }else{
    print("error")
  }
}

Data$sex_boost #view new variable

Data # view data frame with new variable
attach(Data)

#comparing age with new variable sex_boost
x1 <- Dataset$age[Dataset$sex_boost =="M-Yes"]
x2 <- Dataset$age[Dataset$sex_boost =="M-No"]
x3 <- Dataset$age[Dataset$sex_boost =="F-Yes"]
x4 <- Dataset$age[Dataset$sex_boost =="F-No"]

#to compare we assumed variances are equal
t.test(x1,x2,var.equal = TRUE ) # p-value = 0.041540.0 < 0.05 
t.test(x1,x3,var.equal = TRUE ) # p-value = 0.7467 > 0.05 
t.test(x1,x4,var.equal = TRUE ) # p-value = 0.4188 < 0.05 

t.test(x2,x3,var.equal = TRUE ) #p-value = 0.0162 < 0.05
t.test(x2,x4,var.equal = TRUE ) #p-value = 0.05905 = 0.05 

t.test(x3,x4,var.equal = TRUE ) #p-value = 0.4488 > 0.05

#1-way ANOVA, to check the distribution more than two groups
# Null hypothesis,H0: means of all samples are equal

# Alternative hypothesis,HA: at least one sample mean is not equal to the others



#boxplot
boxplot(age ~ sex_boost) #check if the datas are independent or not 


# Compute the analysis of variance
res <- anova(lm(Data$age ~ Data$sex_boost))
summary(res) #Summary of the analysis

# p - value 0.0805 > 0.05 which means that means are unequal in different groups



# To check whether the variances are equal or not, we perform Bartlett test
bartlett.test(age ~ sex_boost) #p-value = 0.3151 #thus, variances are equal

# install.packages("lawstat")
library(lawstat) #library for variances

sb <- Data[!is.na(age) & !is.na(sex_boost),]


levene.test(sb$age,sb$sex_boost)#p-value = 0.536 #thus, variances are equal 


#nonparametric alternative to one-way anova #Kruskal test 
?kruskal.test
kruskal.test(age~sex_boost)

#Here we can choose nonparametric test because variables are not normally distributed.


#check if the two variables are identical or independent
# paired samples t-test is used to compare the means between two related groups of samples.

#multiple correction
pairwise.t.test(age, sex_boost, p.adj = "BH")       #Benjamini-Hochberg
pairwise.t.test(age, sex_boost, p.adj = "bonf")     #Bonferon
pairwise.t.test(age, sex_boost, p.adj = "hochberg") #Hochberg
pairwise.t.test(age, sex_boost, p.adj = "holm")     #Holm

##================================================================================================================================


#QN_09.  Perform a 2-Way-ANOVA to compare the measurements "hvltt", "hvltt2", "hvltt3", "hvltt4" at the 4 measurement times. 
#        Can they be assumed to be identical? Also perform a non-parametric alternative to the 2-Way-ANOVA.


# Null hypothesis,H0: means of all samples are equal
# Alternative hypothesis,HA: at least one sample mean is not equal to the others


Data$hvltt <- as.factor(Data$hvltt)

summary(Data)


anova(lm(Data$hvltt2 ~ Data$hvltt3 + Data$hvltt))   #p:2.2e-16 < 0.05
anova(lm(Data$hvltt2 ~ Data$hvltt4 + Data$hvltt3))  #p:2.2e-16 < 0.05
anova(lm(Data$hvltt3 ~ Data$hvltt4 + Data$hvltt))   #p:2.2e-16 < 0.05
anova(lm(Data$hvltt3 ~ Data$hvltt2 + Data$hvltt4))  #p:2.2e-16 < 0.05

#p-value 2.2e-16 < 0.05 thus we reject H0 and accept HA i.e. means are not equal


#nonparametric alternative 
?friedman.test
friedman.test(hvltt~hvltt2|hvltt3,data = Data) 
#p:2.2e-16 < 0.05 thus we reject H0 and accept HA i.e. means are not equal

#=================================================================================================================================

#QN_10. Compare only "hvltt" with "hvltt4" using a t-test (and a non-parametric alternative). 
#       Which test would you prefer? Provide 90% confidence intervals for the true difference between "hvltt" and "hvltt4


# To check manually whether the Confidence interval true difference in two groups

# Confidence Interval #90% CI

Data$hvltt <- as.numeric(Data$hvltt)

k <- length(Data$hvltt)
dif <- Data$hvltt - Data$hvltt4
alpha <- 0.1  # 90% CI 
mean(dif) - sd(dif)/sqrt(k)*qt(1-alpha/2,k-1) #-5.375549    #lower confidence point
mean(dif) - sd(dif)/sqrt(k)*qt(alpha/2,k-1) #-4.995245      #upper confidence point


t.test(gl,alternative="two.sided",conf.level = 0.90)
#90 percent confidence interval:
#  -5.375549 -4.995245


#10 #nonparametric test to t-test
wilcox.test(hvltt,hvltt4,alternative ="two.sided") 
# p-value = 0.002308 

#10 #hist
    hist(hvltt) #not normal,left skewed
    
    hist(hvltt4) #not normal,left skewed
#The data is not normally distributed so we choose non-parametric test.





