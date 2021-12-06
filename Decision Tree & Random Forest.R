########################### Questions on decision tree ###########################

# 1.Building a decision tree model: a.Start off by dividing the 'customer_churn' data into train & test sets in 70:30 ratio. The split-criteria would be determined by the 'Dependents' column
library(readr)
churn = read.csv("C://Users//user//Documents//Working Directory//Working Directory_Intellipaat//R Language//Practice Session//1st  Stimulus//customer_churn.csv")

library(caTools)
split1 = sample.split(churn$Dependents,SplitRatio = 0.70)
train1 = subset(churn, split1==T)
test1 = subset(churn, split1==F)


# b.Build a decision tree model on top of the 'train' set, where the dependent variable is 'Dependents' & the independent variable is 'Partner'. Store the result in 'mod_tree1'
library(tree)
mod_tree1 = tree(Dependents~Partner, data=train1) 


# c.Plot the tree and add text
plot(mod_tree1)
text(mod_tree1)


# d.Predict the values on the test set and store the result in 'result_tree1'
result_tree1 = predict(mod_tree1,newdata=test1,type="class")
head(result_tree1)


# e.Build a confusion matrix for the actual values & the predicted values
con_mat1 = table(test1$Dependents, result_tree1)
con_mat1


# f.Calculate the accuracy from the confusion matrix
accuracy1 = sum(diag(con_mat1)/sum(con_mat1))
accuracy1 # 0.7160435

acc1 = (990+523)/(990+490+110+523) # 0.7160435
acc1

error1 = 1 - accuracy1
error1 # 0.2839565


# 2.Building 2nd decision tree model on same 'train' & 'test' sets: a.In this case the dependent variable is 'Dependents' & the independent variables are 'Partner' & 'InternetService'. Store the result in 'mod_tree2'
library(caTools)
split2 = sample.split(churn$Dependents,SplitRatio = 0.70)
train2 = subset(churn, split2==T)
test2 = subset(churn, split2==F)

mod_tree2 = tree(Dependents~Partner+InternetService, data=train2)


# b.Plot the tree & add text
plot(mod_tree2)
text(mod_tree2)


# c.Predict the values on the test set & store the result in 'result_tree2'
result_tree2 = predict(mod_tree2,newdata=test2,type="class")
head(result_tree2)


# d.Build a confusion matrix for the actual values & the predicted values
con_mat2 = table(test2$Dependents, result_tree2)
con_mat2


# e.Calculate the accuracy from the confusion matrix
accuracy2 = sum(diag(con_mat2)/sum(con_mat2))
accuracy2 # 0.7808803

acc2 = (1282+368)/(1282+198+265+368) # 0.7808803
acc2

error2 = 1 - accuracy2
error2 # 0.2191197




############################## Questions on random forest ############################## 

# 1.Building the first "Random Forest" model: a.Start off by dividing the 'customer_churn' data into train & test sets in 65:35 ratio. The split-criteria would be determined by the 'gender' column
library(caTools)  
split3 = sample.split(churn$gender,SplitRatio = 0.65)
train3 = subset(churn, split3==T)
test3 = subset(churn, split3==F)


# b.Build a random forest model on top of the 'train' set, where the dependent variable is 'gender' & the independent variables are 'MonthlyCharges' & 'tenure'. The number of decision trees in the random forest would be 35. Store the result in 'mod_forest1'
library(randomForest)
mod_forest1 = randomForest(gender~MonthlyCharges+tenure, data=train3, ntree=35)


# c.Find the importance of the independent variables and also plot it
importance(mod_forest1)
varImpPlot(mod_forest1)
varImpPlot(mod_forest1, col="palegreen4")


# d.Predict the values on top of the test set & store the result in 'result_forest1'
result_forest1 = predict(mod_forest1,newdata=test3,type="class")
head(result_forest1)


# e.Build a confusion matrix for the actual values & the predicted values
con_mat3 = table(test3$gender, result_forest1)
con_mat3


# f.Find out the accuracy from the confusion matrix
accuracy3 = sum(diag(con_mat3)/sum(con_mat3))
accuracy3 # 0.5172414

acc3 = (578+697)/(578+697+643+547) # 0.5172414
acc3

error3 = 1 - accuracy3
error3 # 0.4827586


# 2.Build a 2nd 'Random forest' model on the same train & test sets: a.The dependent & the independent variables would be same. The number of decision trees would be 350. Store the result in 'mod_forest2'
library(caTools)  
split4 = sample.split(churn$gender,SplitRatio = 0.65)
train4 = subset(churn, split4==T)
test4 = subset(churn, split4==F)

mod_forest2 = randomForest(gender~MonthlyCharges+tenure, data=train4, ntree=350)


# b.Find the importance of the independent variables & also plot it
importance(mod_forest2)
varImpPlot(mod_forest2)
varImpPlot(mod_forest2, col="red")


# c.Predict the values on top of test set & store the result in 'result_forest2'
result_forest2 = predict(mod_forest2,newdata=test4,type="class")
head(result_forest2)


# d.Build a confusion matrix for the actual values & predicted values
con_mat4 = table(test4$gender, result_forest2)
con_mat4


# e.Find out the accuracy from the confusion matrix
accuracy4 = sum(diag(con_mat4)/sum(con_mat4))
accuracy4 # 0.4920892

acc4 = (555+658)/(555+658+666+586) # 0.4920892
acc4

error4 = 1 - accuracy4
error4 # 0.5079108

