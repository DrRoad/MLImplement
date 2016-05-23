set.seed(1)

# 0. EDA
summary(iris)
plot(iris)

# 1. split data into test/train
samp <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))

# 2. train model
ir.model <- train.dnn(x=1:4, y=5, traindata=iris[samp,], testdata=iris[-samp,], hidden=10, maxit=2000, display=50)
# ir.model <- train.dnn(x=1:4, y=5, traindata=iris[samp,], hidden=6, maxit=2000, display=50)

# 3. prediction
# NOTE: if the predict is factor, we need to transfer the number into class manually.
#       To make the code clear, I don't write this change into predict.dnn function.
labels.dnn <- predict.dnn(ir.model, iris[-samp, -5])

# 4. verify the results
table(iris[-samp,5], labels.dnn)

#accuracy
mean(as.integer(iris[-samp, 5]) == labels.dnn)
# 0.98

# 5. compare with nnet
library(nnet)
ird <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                  species = factor(c(rep("s",50), rep("c", 50), rep("v", 50))))
ir.nn2 <- nnet(species ~ ., data = ird, subset = samp, size = 6, rang = 0.1,
               decay = 1e-2, maxit = 2000)


labels.nnet <- predict(ir.nn2, ird[-samp,], type="class")
table(ird$species[-samp], labels.nnet)

# accuracy
mean(ird$species[-samp] == labels.nnet)
# 0.96


# Visualization
# the output from screen, copy and paste here.
data1 <- ("i loss accuracy
          50 1.098421 0.3333333 
          100 1.098021 0.3333333 
          150 1.096843 0.3333333 
          200 1.093393 0.3333333 
          250 1.084069 0.3333333 
          300 1.063278 0.3333333 
          350 1.027273 0.3333333 
          400 0.9707605 0.64 
          450 0.8996356 0.6666667 
          500 0.8335469 0.6666667 
          550 0.7662386 0.6666667 
          600 0.6914156 0.6666667 
          650 0.6195753 0.68 
          700 0.5620381 0.68 
          750 0.5184008 0.7333333 
          800 0.4844815 0.84 
          850 0.4568258 0.8933333 
          900 0.4331083 0.92 
          950 0.4118948 0.9333333 
          1000 0.392368 0.96 
          1050 0.3740457 0.96 
          1100 0.3566594 0.96 
          1150 0.3400993 0.9866667 
          1200 0.3243276 0.9866667 
          1250 0.3093422 0.9866667 
          1300 0.2951787 0.9866667 
          1350 0.2818472 0.9866667 
          1400 0.2693641 0.9866667 
          1450 0.2577245 0.9866667 
          1500 0.2469068 0.9866667 
          1550 0.2368819 0.9866667 
          1600 0.2276124 0.9866667 
          1650 0.2190535 0.9866667 
          1700 0.2111565 0.9866667 
          1750 0.2038719 0.9866667 
          1800 0.1971507 0.9866667 
          1850 0.1909452 0.9866667 
          1900 0.1852105 0.9866667 
          1950 0.1799045 0.9866667 
          2000 0.1749881 0.9866667  ")

data.v <- read.table(text=data1, header=T)
par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(x=data.v$i, y=data.v$loss, type="o", col="blue", pch=16, 
     main="IRIS loss and accuracy by 2-layers DNN",
     ylim=c(0, 1.2),
     xlab="",
     ylab="",
     axe =F)
lines(x=data.v$i, y=data.v$accuracy, type="o", col="red", pch=1)
box()
axis(1, at=seq(0,2000,by=200))
axis(4, at=seq(0,1.0,by=0.1))
axis(2, at=seq(0,1.2,by=0.1))
mtext("training step", 1, line=3)
mtext("loss of training set", 2, line=2.5)
mtext("accuracy of testing set", 4, line=2)

legend("bottomleft", 
       legend = c("loss", "accuracy"),
       pch = c(16,1),
       col = c("blue","red"),
       lwd=c(1,1)
)