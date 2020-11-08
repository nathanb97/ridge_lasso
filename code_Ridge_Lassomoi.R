setwd("C:/Users/s002139/Documents/Ecoles/2020-2021/Saclay/cours/Ridge_Lasso_MARS")
Hitters=read.csv("reg_penalise/Hitters.csv")
str(Hitters)
Hitters[1:13] <- lapply(Hitters[1:13], as.numeric)
Hitters[16:19] <- lapply(Hitters[16:19], as.numeric)
str(Hitters)


#analyse des donn?es
summary(Hitters)
Hitters =na.omit(Hitters )

library(corrplot)

M <- cor(Hitters[,c(1:13,16:19)])
corrplot(M, method = "circle")
corrplot.mixed(M)

#################
#regression MCO #
#################
# 2 min 15
mod_MCO = lm(Salary ~ ., data=Hitters)
summary(mod_MCO)#MCO
par(mfrow = c(2,2))
plot(mod_MCO, las=1)

###################
#regression Ridge #
###################
x=model.matrix(Salary~.,Hitters)[,-1] # codage variable qualitative en quantitative
y=Hitters$Salary
grid =10^ seq (10,-2, length =100) #100 valeurs de lambda de 10**10 ? 0.01

library(glmnet)
#3 min 18
ridge.mod =glmnet (x,y,alpha =0, lambda =grid) #standardization des donn?es par d?faut
par(mfrow = c(1,1))
plot(ridge.mod)
grid()
dim(coef(ridge.mod ))
coef(ridge.mod)[,1] # modele 1 pour lambda = 10**10
sqrt(sum(coef(ridge.mod)[ -1 ,1]^2) ) #penalit?


ridge.mod$lambda[50] # valeur de lambda
coef(ridge.mod )[,50]
sqrt(sum(coef(ridge.mod)[ -1 ,1]^2) )


# pour toutes les valeurs de lambda
coef(ridge.mod)[ -1 ,]^2
l2_norm=colSums(coef(ridge.mod)[ -1 ,]^2)
par(mfrow = c(1,1))
plot(l2_norm)

####################################
#echantillon apprentissage et test #
####################################
set.seed (111)
train=sample (1: nrow(x), nrow(x)/2)
test=(- train )
y.test=y[test]

ridge.mod =glmnet (x[train ,],y[train],alpha =0, lambda =grid ,thresh =1e-12) #convergence 1e-12, alpha = 0 (Ridge), alpha = 1 (Lasso)
ridge.pred=predict (ridge.mod ,s=4, newx=x[test ,]) #coeff pour lambda = 4
mean((ridge.pred -y.test)^2) #Erreur quadratique moyenne

#EQM sur Y et EQM sur grande valeur de lambda (quand lambda grand, estimateurs sauf constante = 0) estimation =constante
#mean((mean(y[train ])-y.test)^2)
ridge.pred=predict(ridge.mod ,s=1e10 ,newx=x[test ,])
mean((ridge.pred -y.test)^2)

#NB : MCO obtenu avec lambda = 0 (s=0)
# choix de lambda avec validation crois?e
#3min 50
cv.out =cv.glmnet (x[train ,],y[train],alpha =0) # alpha = 0 (Ridge), alpha = 1 (Lasso)
plot(cv.out)
best_lambda =cv.out$lambda.min
best_lambda

#calcul regression p?nalis?e avec valeur optimis?e de lambda
ridge.pred=predict(ridge.mod ,s=best_lambda, newx=x[test ,])
mean(( ridge.pred -y.test)^2) #SCR
ridge.mod =glmnet (x,y,alpha =0, lambda =best_lambda)
coef(ridge.mod)
# 4 :20
ridge_fit = predict(ridge.mod,x,s=best_lambda,type="response")
MCO_fit=mod_MCO$fitted.values
indice=seq(1:length(y))
out=data.frame(indice, y, MCO_fit,ridge_fit)
plot(out$indice,out$y, main="dsffds")
points(out$indice,out$MCO_fit, col="red", pch=19)
points(out$indice,out$X1, col="blue", pch=17)
legend(1, 2400, legend=c("True", "MCO", "Penalisee"),
       col=c("black", "red", "blue"), lty=1:2, cex=0.8)


plot(out$y,out$MCO_fit, main="ajustement donn?es mod?les MCO, Ridge", col="red", pch=19, ylim=c(0,2500))
points(out$y,out$X1, pch=17, col="blue")
abline(0,1)
legend(1, 2400, legend=c("MCO", "Penalisee"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

#LASSO
#alpha = 1

#MARS
library(earth)
a <- earth(x, y, deg=1, keepxy=TRUE, nfold=5)
b <- earth(x, y, deg=2, keepxy=TRUE, nfold=5)
c <- earth(x, y, deg=3, keepxy=TRUE, nfold=5)
par(mfrow = c(1,1))

plot(a, which = 1)
plot(b, which = 1)
plot(c, which = 1)
plot.earth.models(list(a,b,c))
plot.earth.models(list(a,b,c),which=1)
grid()
plot.earth.models(list(a,b,c),which=2)
grid()
#8:52
ev <- evimp(b, trim=T)
plot(ev)
print(ev)

plotmo(b,ylim=NA)
mars_fit=b$fitted.values
#11 min
plot(out$y,out$MCO_fit, main="ajustement donn?es mod?les MCO, Ridge", col="red", pch=19, ylim=c(0,2500))
points(out$y,out$X1, pch=17, col="blue")
points(out$y,mars_fit, pch=19, col="green")
abline(0,1)
grid()

summary(b)


