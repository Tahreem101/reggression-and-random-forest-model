t <- read.csv("smoking_dataset.csv")
str(t)
summary(t)
t$gender<-as.factor(t$gender)
t$hearing.left.<-as.factor(t$hearing.left.)
t$hearing.right.<-as.factor(t$hearing.right.)
t$Urine.protein<-as.factor(t$Urine.protein)
t$oral<-as.factor(t$oral)
t$tartar<-as.factor(t$tartar)
t$dental.caries<-as.factor(t$dental.caries)
t$smoking<-as.factor(t$smoking)
summary(t)
str(t)
t<-na.omit(t)
library(ggplot2)
ggplot(t,aes(x=age))+geom_boxplot()
ggplot(t,aes(x=height.cm.))+geom_boxplot()
ggplot(t,aes(x=weight.kg.))+geom_boxplot()
ggplot(t,aes(x=waist.cm.))+geom_boxplot()
ggplot(t,aes(x=eyesight.left.))+geom_boxplot()
ggplot(t,aes(x=eyesight.right.))+geom_boxplot()
ggplot(t,aes(x=systolic))+geom_boxplot()
ggplot(t,aes(x=relaxation))+geom_boxplot()
ggplot(t,aes(x=fasting.blood.sugar))+geom_boxplot()
ggplot(t,aes(x=Cholesterol))+geom_boxplot()
ggplot(t,aes(x= triglyceride))+geom_boxplot()
ggplot(t,aes(x= HDL))+geom_boxplot()
ggplot(t,aes(x= LDL))+geom_boxplot()
ggplot(t,aes(x= hemoglobin ))+geom_boxplot()
ggplot(t,aes(x= serum.creatinine ))+geom_boxplot()
ggplot(t,aes(x= AST ))+geom_boxplot()
ggplot(t,aes(x= ALT ))+geom_boxplot()
ggplot(t,aes(x= Gtp ))+geom_boxplot()

IQR_age<-IQR(t$age)
IQR_age 
lower_bound<-quantile(t$age,0.25)-1.5*IQR_age 
upper_bound<-quantile(t$age ,0.25)+1.5*IQR_age 
x<-t[t$age >=lower_bound&t$age <=upper_bound,]
boxplot(x$age )

IQR_height.cm.<-IQR(t$height.cm.)
IQR_height.cm. 
lower_bound<-quantile(t$height.cm.,0.25)-1.5*IQR_height.cm. 
upper_bound<-quantile(t$height.cm. ,0.25)+1.5*IQR_height.cm.
x<-t[t$height.cm. >=lower_bound&t$height.cm. <=upper_bound,]
boxplot(x$height.cm. )

IQR_weight.kg.<-IQR(t$weight.kg.)
IQR_weight.kg. 
lower_bound<-quantile(t$weight.kg.,0.25)-1.5*IQR_weight.kg. 
upper_bound<-quantile(t$weight.kg. ,0.25)+1.5*IQR_weight.kg.
x<-t[t$weight.kg. >=lower_bound&t$weight.kg. <=upper_bound,]
boxplot(x$weight.kg. )

IQR_waist.cm.<-IQR(t$waist.cm.)
IQR_waist.cm. 
lower_bound<-quantile(t$waist.cm.,0.25)-1.5*IQR_waist.cm. 
upper_bound<-quantile(t$waist.cm. ,0.25)+1.5*IQR_waist.cm.
x<-t[t$waist.cm. >=lower_bound&t$waist.cm. <=upper_bound,]
boxplot(x$waist.cm. )

IQR_eyesight.left. <-IQR(t$eyesight.left. )
IQR_eyesight.left. 
lower_bound<-quantile(t$eyesight.left. ,0.25)-1.5*IQR_eyesight.left. 
upper_bound<-quantile(t$eyesight.left.  ,0.25)+1.5*IQR_eyesight.left. 
x<-t[t$eyesight.left.  >=lower_bound&t$eyesight.left.  <=upper_bound,]
boxplot(x$eyesight.left.  )

IQR_eyesight.right. <-IQR(t$eyesight.right. )
IQR_eyesight.right. 
lower_bound<-quantile(t$eyesight.right. ,0.25)-1.5*IQR_eyesight.right. 
upper_bound<-quantile(t$eyesight.right.  ,0.25)+1.5*IQR_eyesight.right. 
x<-t[t$eyesight.right.  >=lower_bound&t$eyesight.right.  <=upper_bound,]
boxplot(x$eyesight.right.  )

IQR_systolic <-IQR(t$systolic )
IQR_systolic 
lower_bound<-quantile(t$systolic ,0.25)-1.5*IQR_systolic
upper_bound<-quantile(t$systolic  ,0.25)+1.5*IQR_systolic
x<-t[t$systolic  >=lower_bound&t$systolic  <=upper_bound,]
boxplot(x$systolic  )

IQR_relaxation  <-IQR(t$relaxation )
IQR_relaxation 
lower_bound<-quantile(t$relaxation  ,0.25)-1.5*IQR_relaxation 
upper_bound<-quantile(t$relaxation   ,0.25)+1.5*IQR_relaxation 
x<-t[t$relaxation   >=lower_bound&t$relaxation   <=upper_bound,]
boxplot(x$relaxation   )

IQR_fasting.blood.sugar  <-IQR(t$fasting.blood.sugar )
IQR_fasting.blood.sugar
lower_bound<-quantile(t$fasting.blood.sugar  ,0.25)-1.5*IQR_fasting.blood.sugar 
upper_bound<-quantile(t$fasting.blood.sugar   ,0.25)+1.5*IQR_fasting.blood.sugar 
x<-t[t$fasting.blood.sugar  >=lower_bound&t$fasting.blood.sugar   <=upper_bound,]
boxplot(x$fasting.blood.sugar   )

IQR_Cholesterol   <-IQR(t$Cholesterol  )
IQR_Cholesterol 
lower_bound<-quantile(t$Cholesterol   ,0.25)-1.5*IQR_Cholesterol  
upper_bound<-quantile(t$Cholesterol    ,0.25)+1.5*IQR_Cholesterol  
x<-t[t$Cholesterol   >=lower_bound&t$Cholesterol    <=upper_bound,]
boxplot(x$Cholesterol   )

IQR_triglyceride   <-IQR(t$triglyceride  )
IQR_triglyceride
lower_bound<-quantile(t$triglyceride   ,0.25)-1.5*IQR_triglyceride
upper_bound<-quantile(t$triglyceride   ,0.25)+1.5*IQR_triglyceride  
x<-t[t$triglyceride   >=lower_bound&t$triglyceride   <=upper_bound,]
boxplot(x$triglyceride   )

ggplot(t,aes(x=HDL ))+geom_boxplot()
IQR_HDL   <-IQR(t$HDL  )
IQR_HDL
lower_bound<-quantile(t$HDL   ,0.25)-1.5*IQR_HDL
upper_bound<-quantile(t$HDL   ,0.25)+1.5*IQR_HDL  
x<-t[t$HDL   >=lower_bound&t$HDL   <=upper_bound,]
boxplot(x$HDL   )


IQR_LDL   <-IQR(t$LDL  )
IQR_LDL
lower_bound<-quantile(t$LDL   ,0.25)-1.5*IQR_HDL
upper_bound<-quantile(t$LDL   ,0.25)+1.5*IQR_HDL  
x<-t[t$LDL   >=lower_bound&t$LDL   <=upper_bound,]
boxplot(x$LDL   )


IQR_hemoglobin   <-IQR(t$hemoglobin  )
IQR_hemoglobin
lower_bound<-quantile(t$hemoglobin  ,0.25)-1.5*IQR_hemoglobin
upper_bound<-quantile(t$hemoglobin   ,0.25)+1.5*IQR_hemoglobin  
x<-t[t$hemoglobin   >=lower_bound&t$hemoglobin   <=upper_bound,]
boxplot(x$hemoglobin   )

IQR_serum.creatinine    <-IQR(t$serum.creatinine   )
IQR_serum.creatinine  
lower_bound<-quantile(t$serum.creatinine   ,0.25)-1.5*IQR_serum.creatinine  
upper_bound<-quantile(t$serum.creatinine     ,0.25)+1.5*IQR_serum.creatinine  
x<-t[t$serum.creatinine    >=lower_bound&t$serum.creatinine    <=upper_bound,]
boxplot(x$serum.creatinine   )

IQR_AST    <-IQR(t$AST   )
IQR_AST 
lower_bound<-quantile(t$AST  ,0.25)-1.5*IQR_AST  
upper_bound<-quantile(t$AST     ,0.25)+1.5*IQR_AST  
x<-t[t$AST    >=lower_bound&t$AST    <=upper_bound,]
boxplot(x$AST   )

IQR_ALT    <-IQR(t$ALT   )
IQR_ALT 
lower_bound<-quantile(t$ALT  ,0.25)-1.5*IQR_ALT  
upper_bound<-quantile(t$ALT     ,0.25)+1.5*IQR_ALT  
x<-t[t$ALT    >=lower_bound&t$ALT    <=upper_bound,]
boxplot(x$ALT   )

IQR_Gtp    <-IQR(t$Gtp   )
IQR_Gtp 
lower_bound<-quantile(t$Gtp ,0.25)-1.5*IQR_Gtp  
upper_bound<-quantile(t$Gtp     ,0.25)+1.5*IQR_Gtp 
x<-t[t$Gtp   >=lower_bound&t$Gtp   <=upper_bound,]
boxplot(x$Gtp  )



str(x)

summary(x)

sd(x$age)
sd(x$height.cm.)
sd(x$weight.kg.)
sd(x$waist.cm.)
sd(x$eyesight.right.)
sd(x$eyesight.left.)
sd(x$systolic)
sd(x$relaxation)
sd(x$fasting.blood.sugar)
sd(x$Cholesterol)
sd(x$triglyceride)
sd(x$HDL)
sd(x$LDL)
sd(x$hemoglobin)
sd(x$serum.creatinine)
sd(x$AST)
sd(x$ALT)
sd(x$Gtp)

var(x$age)
var(x$height.cm.)
var(x$weight.kg.)
var(x$waist.cm.)
var(x$eyesight.right.)
var(x$eyesight.left.)
var(x$systolic)
var(x$relaxation)
var(x$fasting.blood.sugar)
var(x$Cholesterol)
var(x$triglyceride)
var(x$HDL)
var(x$LDL)
var(x$hemoglobin)
var(x$serum.creatinine)
var(x$AST)
var(x$ALT)
var(x$Gtp)


ggplot(x,aes(x=age))+geom_histogram()
ggplot(x,aes(x=height.cm.  ))+geom_histogram()
ggplot(x,aes(x=weight.kg. ))+geom_histogram()
ggplot(x,aes(x=waist.cm. ))+geom_histogram()
ggplot(x,aes(x=eyesight.left. ))+geom_histogram()
ggplot(x,aes(x=eyesight.right. ))+geom_histogram()
ggplot(x,aes(x=systolic ))+geom_histogram()
ggplot(x,aes(x=relaxation ))+geom_histogram()
ggplot(x,aes(x=fasting.blood.sugar ))+geom_histogram()
ggplot(x,aes(x=Cholesterol ))+geom_histogram()
ggplot(x,aes(x=triglyceride  ))+geom_histogram()
ggplot(x,aes(x=HDL ))+geom_histogram()
ggplot(x,aes(x=LDL ))+geom_histogram()
ggplot(x,aes(x=hemoglobin  ))+geom_histogram()
ggplot(x,aes(x=serum.creatinine ))+geom_histogram()
ggplot(x,aes(x=AST  ))+geom_histogram()
ggplot(x,aes(x=ALT  ))+geom_histogram()
ggplot(x,aes(x=Gtp  ))+geom_histogram()

ggplot(x,aes(x=gender ))+geom_bar()
ggplot(x,aes(x=hearing.right. ))+geom_bar()
ggplot(x,aes(x=hearing.left. ))+geom_bar()
ggplot(x,aes(x= Urine.protein   ))+geom_bar()
ggplot(x,aes(x=dental.caries ))+geom_bar()
ggplot(x,aes(x=tartar ))+geom_bar()
ggplot(x,aes(x=smoking ))+geom_bar()


abc<-table(x$smoking,x$gender)
abc
barplot(abc,legend.text=TRUE)


abc<-table(x$smoking,x$hearing.left.)
abc
barplot(abc,legend.text=TRUE)


abc<-table(x$smoking,x$Urine.protein )
abc
barplot(abc,legend.text=TRUE)


abc<-table(x$smoking,x$hearing.right.)
abc
barplot(abc,legend.text=TRUE)

abc<-table(x$smoking,x$tartar)
abc
barplot(abc,legend.text=TRUE)

abc<-table(x$smoking,x$dental.caries)
abc           
barplot(abc,legend.text=TRUE)

ggplot(x, aes(y = age)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = age)) + geom_boxplot(fill = 'brown', color = 
                                                      'black', alpha = 0.7) + theme_minimal()
                                    
ggplot(x, aes(y = height.cm.)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = height.cm.)) + geom_boxplot(fill = 'brown', color = 
                                                      'black', alpha = 0.7) + theme_minimal()

ggplot(x, aes(y = weight.kg.)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = weight.kg.)) + geom_boxplot(fill = 'brown', color = 
                                                      'black', alpha = 0.7) + theme_minimal()

ggplot(x, aes(y = waist.cm.)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = waist.cm.)) + geom_boxplot(fill = 'brown', color = 
                                                            'black', alpha = 0.7) + theme_minimal()

ggplot(x, aes(y = eyesight.left.)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = eyesight.left. )) + geom_boxplot(fill = 'brown', color = 
                                                                  'black', alpha = 0.7) + theme_minimal()
ggplot(x, aes(y = eyesight.right.)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = eyesight.right. )) + geom_boxplot(fill = 'brown', color = 
                                                                  'black', alpha = 0.7) + theme_minimal()

ggplot(x, aes(y = systolic)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = systolic )) + geom_boxplot(fill = 'brown', color = 
                                                                   'black', alpha = 0.7) + theme_minimal()


ggplot(x, aes(y = relaxation)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = relaxation )) + geom_boxplot(fill = 'brown', color = 
                                                                  'black', alpha = 0.7) + theme_minimal()

ggplot(x, aes(y = fasting.blood.sugar)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = fasting.blood.sugar )) + geom_boxplot(fill = 'brown', color = 
                                                                  'black', alpha = 0.7) + theme_minimal()


ggplot(x, aes(y = Cholesterol   )) + geom_boxplot()
ggplot(x, aes(x =smoking , y = Cholesterol   )) + geom_boxplot(fill = 'brown', color = 
                                                            'black', alpha = 0.7) + theme_minimal()


ggplot(x, aes(y = triglyceride )) + geom_boxplot()
ggplot(x, aes(x =smoking , y = triglyceride )) + geom_boxplot(fill = 'brown', color = 
                                                            'black', alpha = 0.7) + theme_minimal()


ggplot(x, aes(y = HDL)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = HDL)) + geom_boxplot(fill = 'brown', color = 
                                                            'black', alpha = 0.7) + theme_minimal()


ggplot(x, aes(y = LDL)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = LDL)) + geom_boxplot(fill = 'brown', color = 
                                                            'black', alpha = 0.7) + theme_minimal()

ggplot(x, aes(y = serum.creatinine )) + geom_boxplot()
ggplot(x, aes(x =smoking , y = serum.creatinine )) + geom_boxplot(fill = 'brown', color = 
                                                            'black', alpha = 0.7) + theme_minimal()

ggplot(x, aes(y = AST)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = AST)) + geom_boxplot(fill = 'brown', color = 
                                                            'black', alpha = 0.7) + theme_minimal()

ggplot(x, aes(y = ALT)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = ALT)) + geom_boxplot(fill = 'brown', color = 
                                                            'black', alpha = 0.7) + theme_minimal()

ggplot(x, aes(y = Gtp)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = Gtp)) + geom_boxplot(fill = 'brown', color = 
                                                            'black', alpha = 0.7) + theme_minimal()

ggplot(x, aes(y = hemoglobin)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = hemoglobin)) + geom_boxplot(fill = 'brown', color = 
                                                            'black', alpha = 0.7) + theme_minimal()
model<-glm(smoking~height.cm.+hemoglobin+triglyceride+Gtp+ALT+gender+tartar+dental.caries+weight.kg.,data=x,family=binomial())
summary(model)
predictions <- predict(model, newdata = x, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
conf_matrix <- table(predicted_classes, x$smoking) 
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
print(accuracy)

