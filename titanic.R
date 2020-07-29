titanic=read.csv("titanicformated.csv")
newdata=read.csv("morevars.csv")
data2=newdata[c(1:2,4:8)]
data=titanic[c(1:2,4:8)]



#pairwise correlation
cor(data)

attach(data)
#regression models 

justSex=glm(Survived~Sex.numeric., data=data, family=binomial)
fit=glm(Survived~Sex.numeric.+ Age, data=data, family=binomial)
fit2=glm(Survived~Age+Pclass, data=data, family=binomial)
fit3=glm(Survived~Sex.numeric.+Age+Pclass, data=data, family=binomial)
summary(fit)
summary(justSex)
summary(fit2)
summary(fit3)
coef(fit)

#percent chance myself and fiance will survive
mySelf=data.frame(Sex.numeric.=1, Age=25)
fiance=data.frame(Sex.numeric.=0, Age=23)
predict(fit,mySelf, type="response")
predict(fit,fiance, type="response")

#probability of survival based on sex age and class
probsex=predict(justSex, type="response")
prob=predict(fit,type="response")
prob2=predict(fit2,type="response")
prob3=predict(fit3,type="response")

#accuracy of model
glm.pred=rep("Dead",887)
glm.pred[prob>.5]="Survived"
table(glm.pred,Survived)

glm.predSex=rep("Dead",887)
glm.predSex[probsex>.5]="Survived"
table(glm.predSex,Survived)

glm.pred2=rep("Dead",887)
glm.pred2[prob2>.5]="Survived"
table(glm.pred2,Survived)

glm.pred3=rep("Dead",887)
glm.pred3[prob3>.5]="Survived"
table(glm.pred3,Survived)

#predictions with new data
attach(data2)
testfit=glm(Survived~Sex.numeric.+Age, data=newdata, family = binomial)
summary(testfit)
prob=predict(testfit,type="response")
summary(prob)
prob[887]
