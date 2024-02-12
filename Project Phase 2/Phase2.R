library("dplyr")
library("lmvar")
library("ggplot2")
library("GGally")
library("pROC")
library("oddsratio")
library("readxl")

UniversityAdmissions <- read.csv("C:\\Users\\Asus\\Desktop\\Phase 2 R code Mirhaji\\UniversityAdmissions.csv")
levels(UniversityAdmissions$University)[levels(UniversityAdmissions$University) == "a"] <- "a"
levels(UniversityAdmissions$University)[levels(UniversityAdmissions$University) == "b"] <- "b"
levels(UniversityAdmissions$University)[levels(UniversityAdmissions$University) == "c"] <- "c"
levels(UniversityAdmissions$University)[levels(UniversityAdmissions$University) == "d"] <- "d"
levels(UniversityAdmissions$University)[levels(UniversityAdmissions$University) == "e"] <- "e"
summary(UniversityAdmissions)

### 1

#A 

CI <- function(mean, SE, alpha) {
  z <- qnorm(alpha/2 + 0.5)
  return(c(mean - z*SE, mean + z*SE))
}
n <- nrow(UniversityAdmissions)
p1 <- nrow(UniversityAdmissions[which(UniversityAdmissions$University == "b"),])/n 
p2 <- nrow(UniversityAdmissions[which(UniversityAdmissions$SOP == "3.5"),])/n
mean <- p1 - p2
SE <- sqrt(p1*(1-p1) + p2*(1-p2))/sqrt(n)
CI <- CI(mean, SE, 0.95)
cat("\nConfidence Interval:", CI, "\n")

#B

tbl = table(UniversityAdmissions$University, UniversityAdmissions$SOP)
tbl
chisq.test(tbl) 

### 2

sample <- sample_n(na.omit(UniversityAdmissions), 10)$Research
psub <- sum(sample == "1")/10
cat("\n\nSample proportion =", psub, "\n")
sim <- c()
for (i in 1:1000) {
  sim <- append(sim, sum(sample(c(0, 1), 10, replace = TRUE))/10)
}
cat("P-value =", sum(sim >= psub)/1000, "\n")

### 3

#A

dataSOP <- UniversityAdmissions$SOP
n <- nrow(data)
x <- summary(dataSOP)
x
pop <- sample(dataSOP , size = 100)
pop
x1 <- sample(dataSOP , size = 100)
x1
x2 <- sample(dataSOP[which(dataSOP < "2.5")] , size =100)
x2

chisq.test(pop , x1)
chisq.test(pop , x2)


#B

data1 <- UniversityAdmissions$SOP
data2 <- UniversityAdmissions$LOR
tbl <- table(data1 , data2)
tbl
chisq.test(tbl)

### 4

#B a

SOP <- UniversityAdmissions$SOP 
TOEFL <-UniversityAdmissions$TOEFL
CGPA <-UniversityAdmissions$CGPA 
model1 <- lm(CGPA ~ SOP   , UniversityAdmissions )
model2 <- lm(CGPA ~ TOEFL , UniversityAdmissions )
summary(model1)
summary(model2)

#B c

ggplot(UniversityAdmissions, aes(SOP, CGPA)) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed") +
  ggtitle("CGPA : SOP")
ggplot(UniversityAdmissions, aes(TOEFL, CGPA)) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed") +
  ggtitle("CGPA : TOEFL")

#D

anova(model1)
anova(model2)

#F


xx1 <- sample_n( UniversityAdmissions , size = 100)
xx1
models <- lm(CGPA ~ SOP , sample)
summary(models)



t <- abs(qt(0.025, 25))
sd <- coef(summary(models))[2, 2]
b1 <- coef(summary(models))[2, 1]
CI <- c(b1 - t*sd, b1 + t*sd)
cat("\nConfidence Interval for slope:", CI, "\n\n")

models2 <- lm(CGPA ~ Research + model, sample)
summary(models2)

anova(models, models2)


###5 

#A

data <- UniversityAdmissions
expvars <- data[c("SOP", "GRE", "LOR", "Research",
                  "Chance_of_Admit", "TOEFL", "internship_abroad")]
ggpairs(expvars, title="Correlogram of variables")

#B

GRE <-UniversityAdmissions$GRE 
Research <-UniversityAdmissions$Research
internship_abroad <-UniversityAdmissions$internship_abroad
modelg <- lm( CGPA ~ GRE + Research + internship_abroad , UniversityAdmissions )
summary(modelg)

#E

data <- UniversityAdmissions
modelfull <- lm(CGPA ~ SOP + internship_abroad+ Chance_of_Admit +
                    GRE + LOR + Research + TOEFL, data)
modelnull <- lm(CGPA ~ 1, data)
cat("\n\n*** Backward elimination ***\n\n")
bestbw <- step(modelfull, direction = "backward")
data <- UniversityAdmissions
cat("\n\n*** Forward selection ***\n\n")
bestfw <- step(modelnull, direction = "forward", scope = (~ LOR + SOP + GRE + Chance_of_Admit  + Research + internship_abroad  + TOEFL) ) 

#F

plot(model$residuals ~ data$SOP, main = "Residuals vs SOP")
plot(model$residuals ~ data$GRE, main = "Residuals vs GRE")
plot(model$residuals ~ data$TOEFL, main = "Residuals vs TOEFL")
plot(model$residuals ~ data$Chance_of_Admit, main = "Residuals vs Chance_of_Admit")



### 6

#A

data <- UniversityAdmissions
gmodel <- glm(internship_abroad ~ LOR + GRE + TOEFL ,
              data, family = "binomial")
summary(gmodel)

#b

data <- UniversityAdmissions
roc <- roc(data$internship_abroad, data$LOR)
plot.roc(roc)


#C

roc <- roc(data$internship_abroad, gmodel$fitted)
plot.roc(roc)
auc <- roc$auc
auc

#D

data <- UniversityAdmissions
roc <- roc(data$internship_abroad, data$TOEFL)
plot.roc(roc)

#E

data <- UniversityAdmissions
gmodel <- glm(internship_abroad ~  TOEFL ,
              data, family = "binomial")
summary(gmodel)
roc <- roc(data$internship_abroad, gmodel$fitted)
plot.roc(roc)
auc <- roc$auc
auc

### 7 

SOP <- UniversityAdmissions$SOP 
TOEFL <-UniversityAdmissions$TOEFL
CGPA <-UniversityAdmissions$CGPA 
LOR <-UniversityAdmissions$LOR
GRE <-UniversityAdmissions$GRE 
Research <-UniversityAdmissions$Research
internship_abroad <-UniversityAdmissions$internship_abroad
Chance_of_Admit <-UniversityAdmissions$Chance_of_Admit 

modelx <- lm( Chance_of_Admit ~ SOP + TOEFL +  CGPA + LOR + GRE + Research + internship_abroad , UniversityAdmissions )
summary(modelx)
