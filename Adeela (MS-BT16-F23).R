read.csv("smoking_dataset.csv")
x<-read.csv("smoking_dataset.csv")
str(x)
q<-na.omit(x)
str(x)
library(ggplot2)
ggplot(x,aes(x=))+geom_boxplot()
IQR_age<-IQR(x$age)
IQR_age
lower_bound<-quantile(x$age,0.25)-1.5*IQR_age
upper_bound<-quantile(x$age,0.25)+1.5*IQR_age
y<-x[x$age>=lower_bound&x$age<=upper_bound,]
boxplot(y$age)
mean(y$Gtp)
median(y$hemoglobin)
var(y$age)
sd(y$age)
max(y$age)
min(y$Gtp)
ggplot(y,aes(x=hearing.left.))+geom_histogram()
ggplot(y,aes(x=serum.creatinine))+geom_density()
ggplot(y,aes(x=gender))+geom_density()
ggplot(y,aes(x=age))+geom_bar()
ggplot(y,aes(x=oral))+geom_point()
ggplot(y, aes(x = tartar, y = smoking)) + geom_point()
  geom_point(position = position_jitter(width = 0.2, height = 0.1), size = 3) +  # Jitter points for better visibility
  labs(title = "Scatter Plot of Height vs Smoking",
       x = "Height",
       y = "Smoking Status")
ggplot(y,aes(x=oral))+geom_point()
mean(y$triglyceride)
model <- glm(smoking ~ tartar, data =y , family =  binomial())
summary(model)
y$tartar <- ifelse(y$tartar =='Y',0,1)
model <- glm(smoking ~ gender+age+height.cm.+weight.kg.+waist.cm.+eyesight.left.+eyesight.right.+hearing.left.+hearing.right.+systolic+relaxation+fasting.blood.sugar+Cholesterol+triglyceride+HDL+LDL+hemoglobin+Urine.protein+serum.creatinine+AST+ALT+Gtp+dental.caries+tartar, data = y, family = binomial)
summary(model)
model <- glm(smoking ~ gender+height.cm.+triglyceride+hemoglobin+Gtp, data = y, family = binomial)
summary(model)
min(y$LDL)
