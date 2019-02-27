require(ISLR)
require(leaps)
require(glmnet)
set.seed(1)
train=sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)
test=(!train)
regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
test.mat=model.matrix(Salary~.,data=Hitters[test,])
val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  #取intercept和构成size的另外几列从test data中
  pred=test.mat[,names(coefi)]%*%coefi #表示用已知的x值乘以estimated
                                        #coefficient
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
  
}
nrow(test.mat)
pred=test.mat[,names(coef(regfit.best,id=1))]%*%coefi
length(pred)
nrow(Hitters$Salary[test])
nrow(test.mat)
length(train)
nrow(coef(regfit.best,id=1))
grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)

############
#6.6 Lab2  #
############

#create x matrix, delete the intercept column
#(model matrix)will remove na value automatically
x = model.matrix(Salary~.,data=Hitters)[,-1]
y = Hitters$Salary


y = y[!is.na(y)]

#6.6.1 Ridge Regression
grid = 10^seq(10,-2,length=100)
ridge.mod = glmnet(x,y,alpha=0,lambda=grid)

#row-num of predictors + intercept; col=each value of lambda
dim((coef(ridge.mod)))

#value of lambda 50
ridge.mod$lambda[50]

#l2 norm (associated with 50th lamda so we only calculated 50th col)
sqrt(sum(coef(ridge.mod)[-1,50]^2))

#cal when lambda=705
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

####################
#test error of     #
#ridge regression  #         
####################
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]
y.test=y[test][!is.na(y[test])]
x.train=x[train,][!is.na(x[train,])]
#fit a ridge regression model on train data
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)

#test our estimation(using lambda=4)
pred=predict(ridge.mod,s=4,newx=x[test,])
mse=mean((pred-y.test)^2)

#if we simply fit a model with just an intercept
mse2=mean((mean(y[train])-y.test)^2)

#we could also get mse2 by using very large lambda
pred=predict(ridge.mod,s=1e10,newx=x[test,])
pred=predict(ridge.mod,newx=x[test,])
mse3=mean((pred-y.test)^2)

# fitting a ridge regression model with lambda=4 leads to much
# lower mse than fitting a model with just an intercept

# compare ridge with ols
pred=predict(ridge.mod,s=0,newx=x[test,])
mse4=mean((pred-y.test)^2)

#two ways to check what ols look like 
#lm(preferred)
lm(y~x,subset = train)
predict(ridge.mod,s=0,type="coefficients")[1:20,]

#################################
#Use cross-validation           #
#to choose the tuning parameter #
#################################
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
names(cv.out)
bestlam=cv.out$lambda.min
#test mse with best lambda
pred=predict(ridge.mod,newx=x[test,],s=bestlam)
mse_best=mean((pred-y.test)^2)

#fit our model on full data set
out=glmnet(x,y,alpha=0)
#way to check coefficients in ridget regression
predict(out,s=bestlam,type="coefficients")[1:20,]

################
#6.2  The lasso#
################
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

#perform cv and test mse
lasso.cv=cv.glmnet(x[train,],y[train],alpha=1)
bestlam=lasso.cv$lambda.min
pred=predict(lasso.cv,s=bestlam,newx=x[test,])
mean((pred-y.test)^2)

out=glmnet(x,y,alpha=0,lambda=grid)
pred=predict(out,type="coefficients",s=bestlam)[1:20,]
