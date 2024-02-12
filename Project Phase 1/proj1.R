library(magrittr) 
library(dplyr)
library(ggfortify)
library(ggplot2)
library(plyr)
library(gridExtra)
library(ggpubr)
require(qqplotr)
library("ggpubr")
library(moments)
library(hexbin)
library(ggmosaic)
library(GGally)

#####Question 0

UniversityAdmissions <- read.csv("F:\\Downloads\\UniversityAdmissions.csv", header = TRUE)







#Question 1






selected.numerical <- UniversityAdmissions$CGPA

#A

binsize <- 2 * IQR(selected.numerical) / length(selected.numerical)^(1/3)

selected.numerical.hist <- ggplot(as.data.frame(selected.numerical),
                                  aes(selected.numerical)) +
geom_histogram(aes(y=..density..) , binwidth = binsize, alpha = 0.4)  + geom_density(linetype="dashed", alpha = 0.3, size=1) + labs(title = "CGPA", x = "Score", y="Density")+ theme(plot.title = element_text(hjust = 0.5))

selected.numerical.hist

#B

selected.numerical.qq <- ggplot(as.data.frame(selected.numerical),
                                aes(sample = selected.numerical))+
  geom_qq()+
  geom_qq_line()+
  labs(title="QQ-plot of CGPA")+
  theme(plot.title = element_text(hjust = 0.5))

selected.numerical.qq

#C

print(skewness(selected.numerical))

#D

selected.numerical.boxplot <- ggplot(as.data.frame(UniversityAdmissions),
                                     aes(x = selected.numerical))+
  geom_boxplot()+
  labs(title="Boxplot for CGPA ",
       xlab="score")+
  theme(plot.title = element_text(hjust = 0.5))

selected.numerical.boxplot

#E

selected.numerical.mu <- mean(selected.numerical)
selected.numerical.median <- median(selected.numerical)
selected.numerical.var <- var(selected.numerical)
selected.numerical.std <- sd(selected.numerical)

#F

selected.numerical.density <- ggplot(UniversityAdmissions,
                                     aes(x = selected.numerical)) +
  geom_vline(xintercept = selected.numerical.mu,
             linetype="dashed",
             color = "darkorchid3") +
  geom_vline(xintercept = selected.numerical.median,
             linetype="dashed",
             color = "darkred") +
  geom_density(color = "darkgreen", size = 1)+
  stat_function(fun = dnorm, n = 101, args = list(mean = selected.numerical.mu,
                                                  sd = selected.numerical.std)  )+
  annotate("text", x = 5.2 , label = "Normal", y = 0.2, size = 3, angle = 0 , color="black") +
  annotate("text", x = 6.5 , label = "Density", y = 0.2, size = 3, angle = 0, color="darkgreen") +
  annotate("text", x = 3.7 , label = "Mean", y = 0.1, size = 3, angle = 0, color="darkorchid3") +
  annotate("text", x = 4.4 , label = "Median", y = 0.1, size = 3, angle = 0, color="darkred") +
  labs(title="Density of CGPA")+
  theme(plot.title = element_text(hjust = 0.5))

selected.numerical.density

#G

CGPA <- UniversityAdmissions$CGPA
mu<- mean(CGPA)
Partial_CGPA <- c(length(CGPA[CGPA<=0.5*mu]) ,
                  length(CGPA[CGPA>0.5*mu & CGPA <= mu]) ,
                  length(CGPA[CGPA>mu & CGPA <= 1.5*mu]) ,
                  length(CGPA[CGPA>1.5*mu & CGPA <=2*mu]))

percentage<-round(100*Partial_CGPA/sum(Partial_CGPA) , 1 )
labels<- c("First" ,"Second" , "Third" , "Fourth")

pie(Partial_CGPA , labels=paste(paste0(labels , ":" , percentage , "%") , sep= " ") , col = Partial_CGPA)
title ("Pie Chart Of 4 Mean-length Parts")

#H

boxplot.stats(selected.numerical)
selected.numerical.boxplot








#Question 2








internshipYes <- dplyr::filter(UniversityAdmissions, internship_abroad=="1")
internshipNo <- dplyr::filter(UniversityAdmissions, internship_abroad=="0")
student.internship_abroad <- UniversityAdmissions$internship_abroad

##Part a

length(internshipYes$internship_abroad)
length(internshipNo$internship_abroad)
internshipYes.percentage <- length(internshipYes$internship_abroad)/length(student.internship_abroad)
internshipNo.percentage <- length(internshipNo$internship_abroad)/length(student.internship_abroad)
internshipYes.percentage
internshipNo.percentage

##Part b

data <- data.frame( internship_abroad = c("Yes", "No"),
  Value = c(internshipYes.percentage*100, internshipNo.percentage*100))

internship_abroad.barplot <- ggplot(data , aes( fill=internship_abroad , x = " " , y = Value )) + 
  geom_bar(position = "fill" , stat="identity") +
  
  labs(title="Stacked Barplot of internship abroad", y = 'Frequency')+
  annotate("text", x = 1 , label = paste(toString(round(internshipNo.percentage*100, digit=3)),"%"), y = 1-internshipNo.percentage/2 , size = 3)+ 
  annotate("text", x = 1 , label = paste(toString(round(internshipYes.percentage*100, digit=3)),"%"), y = internshipYes.percentage/2 , size = 3) 

internship_abroad.barplot



##Part c


internship_abroad.hbarplot <- ggplot(data, aes(x =internship_abroad, y=Value  , fill=internship_abroad)) + 
  geom_bar( stat="identity") +
  labs(title="Barplot of internship abroad", y = 'Frequency')+
  annotate("text", x = 1 , label = paste(toString(round( internshipNo.percentage*100, digit=3)),"%"), y =  internshipNo.percentage*50 , size = 3 ) + 
  annotate("text", x = 2 , label = paste(toString(round(internshipYes.percentage*100, digit=3)),"%"), y = internshipYes.percentage*50 , size = 3 ) +
  xlab("internship abroad")+
  ylab("percentage")



internship_abroad.hbarplot



##Part d

temp <- data.frame(UniversityAdmissions)
q2.violin <- ggplot(temp , aes( x=internship_abroad, y = CGPA,  fill = internship_abroad)) + 
  geom_violin(aes ( fill = factor(internship_abroad)))+
  ylab("CGPA scores")+
  labs(title="Violin plot of internship abroad VS CGPA scores")+
  theme(plot.title = element_text(hjust = 0.5))

q2.violin





#Question 3




numerical.second <- UniversityAdmissions$TOEFL
numerical.first <- UniversityAdmissions$GRE


##Part b

temp <- data.frame(UniversityAdmissions)

q3.scatter <- ggplot(temp, aes( x = GRE, y = TOEFL ))+
  geom_point(color="orange")+
  labs(title="scatterplot for TOEFL Score VS GRE Score")+
  theme(plot.title = element_text(hjust = 0.5))

q3.scatter


##Part c

q3.corr <- cor(numerical.first, numerical.second)
q3.corr



##part E
q3.test.corr <- cor.test(numerical.first, numerical.second,
                         alternative = "less",
                         method = "pearson",
                         conf.level = 0.95)

q3.test.corr

##Part F

twonum.and1cat.scatter <- ggplot(temp, aes(x= GRE, y=TOEFL, color= Research , fill= Research ))+
  geom_point()+
  labs(title = "TOEFL Score vs GRE Score with respect to having research scatter plot")+
  theme(plot.title = element_text(hjust = 0.5))

twonum.and1cat.scatter


##Part G

library(ggExtra)


q3.densigram.hex <- ggMarginal(ggplot(temp, aes(x = TOEFL, y = GRE)) +  geom_point(col="transparent")+geom_hex(bins=45), type= "densigram", margins = "both")
q3.densigram.hex



#Part H
temp <- data.frame(UniversityAdmissions)
q3.2ddenisty.hex <- ggMarginal(ggplot(temp, aes(x = TOEFL, y = GRE)) +
                    ylim(c(290,350))+xlim(c(93,123))+
                    geom_point(col="transparent")+
                    stat_density2d(aes(fill=..level..), geom="polygon", color="red"), type= "densigram", margins = "both")

q3.2ddenisty.hex







#Question 4








#a

library(GGally)

featurePlot(x=temp[,1:5], y=temp[,5:10], plot="pairs")
ggpairs(dplyr::select_if(UniversityAdmissions, is.numeric), title = "Correlogram")
list.of.num <- c(2, 3, 5, 6, 7, 8 , 9, 10)

#density, without failure
ggpairs(UniversityAdmissions[, list.of.num],
        upper = list(continuous = wrap("density", colour="blue" , size = 0.5 )),
        lower = list(continuous = wrap("points", colour="red" , size = 0.5 )))

#linear relationship
ggpairs(UniversityAdmissions[, list.of.num],
        upper = list(continuous = wrap("smooth", colour="blue", size = 0.5 )),
        lower = list(continuous = wrap("points", colour="red", size = 0.5 )))




#b

library(Hmisc)
col <- colorRampPalette(c("blue", "red"))
UniversityAdmissions.corr <- rcorr(as.matrix(dplyr::select_if(UniversityAdmissions, is.numeric)))
UniversityAdmissions.corr.p <- UniversityAdmissions.corr$P
UniversityAdmissions.corr.p[is.na(UniversityAdmissions.corr.p)] <- 1

M <- cor(dplyr::select_if(UniversityAdmissions, is.numeric))
library(corrplot)
corrplot(M, method = "color", col = col(400), type = "upper", order = "hclust", addCoef.col = "black", 
         tl.col = "blue", tl.srt = 45, p.mat = UniversityAdmissions.corr.p, sig.level = 0.05, diag = FALSE)


#c.

cols <- c("Green", "Blue" , "red" , "black" , "orange3")
cols <- cols[as.numeric(as.factor(UniversityAdmissions$University))]
library(scatterplot3d)
scatterplot3d(UniversityAdmissions$SOP ,UniversityAdmissions$LOR , UniversityAdmissions$TOEFL , color = cols )
legend("right" , legend = c("A" , "B" , "C" , "D" , "E") ,  col = c("Green", "Blue" , "red" , "black" , "orange3") , pch = 10)



#Question 5







##Part a
q5.cat1 <- UniversityAdmissions$Research
q5.cat2 <- UniversityAdmissions$University
Research <- UniversityAdmissions$Research
University <- UniversityAdmissions$University
table <- table(Research, q5.cat2)
print.table(table)
head(data.frame(table))




##Part b
combined.barplot.RS <- ggplot(temp, aes(x = Research,color = University, fill = University)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  labs(title="Research grouped barplot with University", x="Research")+
  theme(plot.title = element_text(hjust = 0.5))

combined.barplot.RS

##Part c
combined.segbarplot.RS <- ggplot(temp, aes(x = Research,color = University, fill = University)) +
  geom_bar(alpha = 0.7) +
  labs(title="Research grouped barplot with University", x="Research")+
  theme(plot.title = element_text(hjust = 0.5))

combined.segbarplot.RS


#Question 6






calculate_ci <- function(sampled_data, confidence_level) {
  sample_mean <- mean(sampled_data)
  stdDev <- sd(sampled_data)
  
  z_value <- qnorm((1 + confidence_level)/2)
  stdError <- stdDev / sqrt(length(sampled_data))
  CI <- c(sample_mean - z_value * stdError, sample_mean + z_value * stdError)
  return(CI)
}
#Part a
sampled_data <- sample(UniversityAdmissions$TOEFL, 100)
TOEFL.CI <- calculate_ci(sampled_data, 0.95)
TOEFL.CI


#Part c
selected.numerical <- UniversityAdmissions$TOEFL
bwidth <- 2 * IQR(selected.numerical) / length(selected.numerical)^(1/3)
q6.TOEFL.hist <- ggplot(UniversityAdmissions, aes(x = TOEFL)) +
  geom_histogram(binwidth = 0.9, alpha = 0.4, color="lightsteelblue2", fill="lightsteelblue1") +
  labs(title = "TOEFL Histogram", x = "TOEFL") +
  geom_vline(xintercept =  mean(UniversityAdmissions$TOEFL), color = "orange") +
  geom_vline(xintercept =  TOEFL.CI[1], color = "blue") +
  geom_vline(xintercept =  TOEFL.CI[2], color = "blue")+
  theme(plot.title = element_text(hjust = 0.5))


q6.TOEFL.hist




#Part d
zdist.2tail.meantest <- function(sampled_data, null_value, alpha) {
  n <- length(sampled_data)
  x_bar <- mean(sampled_data)
  S <- sd(sampled_data)
  z_score <- abs((x_bar - null_value)) / (S/sqrt(n))
  p_value <- pnorm(z_score, lower.tail = FALSE)
  print(paste("p-value =", p_value))
}
zdist.2tail.meantest(sampled_data, 105 , 0.05)

#Part f
null.value <- 105
z_value <- abs(qnorm((1-0.05)/2))
errorTypeII <- pnorm(null.value + z_value * sd(sampled_data)/sqrt(length(sampled_data)) - mean(sampled_data))
errorTypeII

#part g
power <- 1 - errorTypeII
power






#Question 7




#Part a
UniversityAdmissions.sampled <- sample_n(UniversityAdmissions, 25)
x_bar <- mean(UniversityAdmissions.sampled$CGPA) - mean(UniversityAdmissions.sampled$SOP)
s1 <- sd(UniversityAdmissions.sampled$CGPA)
s2 <- sd(UniversityAdmissions.sampled$SOP)
s <- abs((x_bar - 0)) / (sqrt((s1^2/25) + (s2^2/25)))
pvalue <- 2*pt(s, df = 24, lower.tail = FALSE)
print(paste("p-value =", pvalue))


#Part b
CGPA.sample <- sample(UniversityAdmissions$CGPA, 100)
SOP.sample <- sample(UniversityAdmissions$SOP, 100)
zdist.2tail.meantest(CGPA.sample - SOP.sample, 0, 0.05)



#Question 8





library(bootstrap)
#Part a
q8.chosen.numerical <- UniversityAdmissions$CGPA
boxplot(q8.chosen.numerical)

mean.CI <- c()
for(i in 1:30){
  bootsamp <- sample(q8.chosen.numerical, 100) 
  mean.CI <- c(mean.CI, mean(bootsamp)) 
}
q8.mean.Pa.CI <- quantile(mean.CI, c(0.025,0.975))

calculate_ci(q8.mean.Pa.CI , 0.95)


#Part b

get_mean <- function(x){
  mean(x)
}

boot.q8.mean <- bootstrap(x=q8.chosen.numerical, nboot=20, get_mean)
temp <- boot.q8.mean$thetastar
se <- sd(temp)
mu <- mean(temp)
t_s <- qt(0.975, df=19)
q8.mean.Pb.CI <- c(mu-t_s*se, mu+t_s*se)
q8.mean.Pb.CI <- quantile(boot.q8.mean$thetastar, c(0.025,0.975))

calculate_ci(q8.mean.Pb.CI , 0.95)




#Question 9



result <- aov(UniversityAdmissions$Chance.of.Admit~as.factor(UniversityAdmissions$University))
print(summary(result))
thsd <- TukeyHSD(result, conf.level = 0.95)
plot(thsd)
print(thsd)
