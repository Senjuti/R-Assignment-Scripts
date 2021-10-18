data <- read.csv(file="C:\\Users\\senju\\Documents\\R-Projects\\socexpdata.csv")
#print(data)

df <- data.frame(data)

socexp <- df[, 6]
unemp <- df[, 4]
govleft <- df[, 3]
proprep <- df[, 5]

avg_socexp <- mean(socexp)
avg_unemp <- mean(unemp)
avg_govleft <- mean(govleft)

max_socexp <- max(socexp)
max_unemp <- max(unemp)
max_goveleft <- max(govleft)

min_socexp <- min(socexp)
min_unemp <- min(unemp)
min_govleft <- min(govleft)

sd_socexp <- sd(socexp)
sd_unemp <- sd(unemp)
sd_govleft <- sd(govleft)

tbl_stats <- matrix(c(avg_socexp, avg_unemp, avg_govleft, max_socexp, max_unemp, max_goveleft, min_socexp, min_unemp, min_govleft, sd_socexp, sd_unemp, sd_govleft), ncol=3, byrow=TRUE)
colnames(tbl_stats) <- c("Socexp", "Unemp", "Govleft")
rownames(tbl_stats) <- c("Average", "Maximum", "Minimum", "Std. Deviation")
tbl_stats <- as.table(tbl_stats)
print("Table of Statistics")
print(tbl_stats)

reg1 <- lm(socexp~govleft)
reg2 <- lm(socexp~unemp)
reg3 <- lm(socexp~proprep)

print("Table of Regression Estimation Results")
print("Regression Coefficient for Socexp vs Govleft")
print (coefficients(reg1))
print("Regression Coefficient for Socexp vs Unemp")
print (coefficients(reg2))
print("Regression Coefficient for Socexp vs Proprep")
print (coefficients(reg3))

#Arrange plots in 2x2 with par(mfrow)
par(mfrow=c(2,2))
# abline() can be used to add vertical, horizontal or regression lines to a graph
plot(govleft, socexp, main = "Socexp vs Govleft")
abline(reg1)
plot(unemp, socexp, main = "Socexp vs Unemp")
abline(reg2)
plot(proprep, socexp, main = "Socexp vs Proprep")
abline(reg3)

print("Multivariate Linear Regression Model using all 3 variables")
multiple_linear_reg <- lm(socexp~govleft+unemp+proprep)
print("Socexp vs Govleft, Unemp, Proprep")
print(coefficients(multiple_linear_reg))

# predict() is a generic function with a single method for lm objects
prediction1 <- predict(multiple_linear_reg, data.frame(govleft=75, unemp=5, proprep=1))
prediction2 <- predict(multiple_linear_reg, data.frame(govleft=25, unemp=10, proprep=1))

print("Predicted level of social expenditures for 2 countries with:")
print("Govleft: 75% left, Unemp: 5%, Proprep")
print (prediction1)
print("Govleft: 25% left, Unemp: 10%, Proprep")
print (prediction2)

if (prediction1 > prediction2) {
  print("Model predicts higher social expenditure in 1st country")
} else {
  print("Model predicts higher social expenditure in 2nd country")
}