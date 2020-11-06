data <- read.table("IBAT.csv", sep = ",", header = TRUE)
data
#descriptive statistics

#finding min
min(data$Runs)
min(data$Ave)
min(data$SR)
min(data$Fours)
min(data$Sixes)
min(data$HF)

#finding max
max(data$Runs)
max(data$Ave)
max(data$SR)
max(data$Fours)
max(data$Sixes)
max(data$HF)

#finding mean
mean(data$Runs)
mean(data$Ave)
mean(data$SR)
mean(data$Fours)
mean(data$Sixes)
mean(data$HF)

#finding median
median(data$Runs)
median(data$Ave)
median(data$SR)
median(data$Fours)
median(data$Sixes)
median(data$HF)

#finding variance
var(data$Runs)
var(data$Ave)
var(data$SR)
var(data$Fours)
var(data$Sixes)
var(data$HF)

#finding standard deviation
sd(data$Runs)
sd(data$Ave)
sd(data$SR)
sd(data$Fours)
sd(data$Sixes)
sd(data$HF)

#finding IQR
IQR(data$Runs)
IQR(data$Ave)
IQR(data$SR)
IQR(data$Fours)
IQR(data$Sixes)
IQR(data$HF)

# plots
hist(data$Runs)
hist(data$Ave)
hist(data$SR)
hist(data$Fours)
hist(data$Sixes)
hist(data$HF)

#MATRIX PLOT b/w VARAIBLES 
pairs(data[,2:7], pch=20, col="#FC4E07")
d <- data[,-1]
head(d)

#finding Correlation matrix b/w varaiables
cor(d)

#finding Covariance Matrix b/w variables
d1 <- cov(d)
head(d1)



#finding Principal components by using prcomp() and princomp()

pca <- prcomp(t(d), scale = TRUE)
pca$x
plot(pca$x[,1], pca$x[,2])
pca.var <- pca$sdev^2
pca.var


pc <- princomp(d, cor = TRUE, scores = TRUE)
dim(d)
attributes(pc)
pc$loadings
pc$scores
pc$call
pc$sdev
pc$center
pc$scale
pc$n.obs
str(pc)

#plotting principal components
plot(pc)
plot(pc,type="l")
biplot(pc)

#summary of each component:
summary(pc)
#finding eigen vectors of variables and each component value contributed
d2 <- eigen(d1)$vectors
head(d2)

#calcutaing points for each batsman by using the eigen vectors obtained
Bpoints <- ((data$Runs*0.991243095)+(data$Ave*0.064861229)+(data$SR*0.054430368)+(data$Fours*0.091385991)+(data$Sixes*0.042737098)+(data$HF*0.009466959))
Bpoints
data1 <- read.table("DA_lab/lab2/IBAT.csv", sep = ",", header = TRUE)
data1
data1$bat = Bpoints
data1
data1[with(data1, order(bat)),]
data1
rum1 <- data1[order(data1$bat, decreasing = TRUE),]
rum1

#ordering and printing batsmans accor. to their ranks
rum1$Ranks <- 101 - rank(rum1$bat)
rum1 <- rum1[order(rum1$Ranks),]
cbind(rum1,rum1$Ranks)
print(rum1)

#ploting bargraph for points calculated for each player
poin <- c(Bpoints)
barplot(poin)





data2 <- read.table("ball.csv", sep = "," , header = TRUE)
data2
#descriptive statistics

#finding min
min(data2$Wkts)
min(data2$Avg)
min(data2$Eco)
min(data2$SR)


#finding max
max(data2$Wkts)
max(data2$Avg)
max(data2$Eco)
max(data2$SR)

#finding mean
mean(data2$Wkts)
mean(data2$Avg)
mean(data2$Eco)
mean(data2$SR)

#finding median
median(data2$Wkts)
median(data2$Avg)
median(data2$Eco)
median(data2$SR)

#finding variance
var(data2$Wkts)
var(data2$Avg)
var(data2$Eco)
var(data2$SR)

#finding standard deviation
sd(data2$Wkts)
sd(data2$Avg)
sd(data2$Eco)
sd(data2$SR)

#finding IQR
IQR(data2$Wkts)
IQR(data2$Avg)
IQR(data2$Eco)
IQR(data2$SR)


# plots
hist(data2$Wkts)
hist(data2$Avg)
hist(data2$SR)
hist(data2$Eco)

#Matrix Plot b/w variables of Bowlers
pairs(data2[,2:5], pch=20, col="#FC4E07")

d3 <- data2[,-1]
head(d3)

#finding Correlation Matrix of Variables for Bowlers Data
cor(d3)

#finding Covariance Matrix of Variables for Bowlers Data
d4 <- cov(d3)
head(d4)

#Finding Principal Components
pc1 <- princomp(d3, cor = TRUE, scores = TRUE)
pca1 <- prcomp(t(d3), scale = TRUE)
pca1$x
plot(pca1$x[,1], pca1$x[,2])
pca1.var <- pca1$sdev^2
pca1.var

plot(pc1)
plot(pc1,type="l")
biplot(pc1)
dim(d3)
attributes(pc1)
pc1$loadings
pc1$scores
pc1$call
pc1$sdev
pc1$center
pc1$scale
pc1$n.obs
str(pc1)
d4
#summary of Principal Components:
summary(pc1)

#Finding Eigen Vectors for each Component
d5 <- eigen(d4)$vectors
head(d5)

Bpoints1 <- (((data2$Wkts*0.27635634)-(data2$Avg*0.81662416)-(data2$SR*0.5057089)-(data2$Eco*0.03177509))*0.6654867682)+(((data2$Wkts*0.94630900)-(data2$Avg*0.15584264)+(data2$SR*0.27070635)-(data2$Eco*0.08324913))*0.334513232)
Bpoints1
data3 <- read.table("ball.csv", sep = ",", header = TRUE)
data3
data3$ball = Bpoints1
data3
data3[with(data3, order(ball)),]
data3
rum2 <- data3[order(data3$ball, decreasing = TRUE),]
rum2

rum2$Ranks <- 51 - rank(rum2$ball)
rum2 <- rum2[order(rum2$Ranks),]
cbind(rum2,rum2$Ranks)
print(rum2)

point <- c(Bpoints1)

barplot(point)


