?mean
?sort
sort(c(4,5,6), decreasing = TRUE)
install.packages("dpylr")
help("dplyr")
library("dplyr")
?dplyr

help.search("visualization")
logical = T
class(logical)
vec = c(1,2,"a")
vec
vec3 = c("a", "b", "c", "d")
vec3
1:5
c(1:5)
seq(from=2, to=5, by=2)
rep(1:3, each = 2)
vec5 = c(1,2,3)
vec5
rep(vec5, 2)
vec5 = NULL
vec5
vec6 = c(1,2,3)
vec7 = c(vec6, 4,5,6)
vec7
vec8 = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")
vec8
names(vec8)
names(vec8) = c("n1", "n2", "n3", "n4", "n5", "n6", "n7")
names(vec8)
vec8
vec8[2]
vec8[c(1,3,5)]
vec8["n2"]
vec8[c("n1", "n3", "n5")]
vec8[6]
vec8[6] = "Saturday"
vec8
vec8[c(-6,-7)]

vec9 = c(1,2,3)
vec10 = c(4,5,6)
log(vec9)
vec9 + 1
vec9 * 2
vec9 + vec10
vec9 * vec10
vec9 == vec10
vec11 = 1:10
length(vec11)
min(vec11)
max(vec11)
?any
any(vec11)
quantile(vec11)
summary(vec11)
table(vec11)
vec15 = c(5,6,2,3,4,8,1,3,7,15)
vec15
vec15>5
vec15[vec15>5]
subset(vec15, vec15>5)
which(vec15>5)
names(vec15) = NULL
vec15
vec15[which(vec15>5)]
vec17 = c(1,1,1,2,2,3)
vec17
str(vec17)
factor1 = as.factor(vec17)
factor1
summary(vec17)
summary(factor1)
factor1[1] = 0
factor1
str(factor1)
levels(factor1)
levels(factor1) = c(levels(factor1), 0)
factor1[1] = 0
factor1
list1 = list(1,2, "a")
list1
str(list1)

matrix1 = matrix(1:15, nrow = 5, ncol = 3)
matrix1
matrix1 = matrix(1:15, nrow = 5, ncol = 3, byrow = T)
matrix1
matrix1[1,3]
matrix1[c(1,2), 3]
matrix1[c(1,2), c(2,3)]
matrix1[, 3]
matrix3 = matrix(1:12, nrow = 3, ncol = 4)
matrix3
rownames(matrix3)
rownames(matrix3) = c("row1", "row2", "row3")
rownames(matrix3)
matrix3
colnames(matrix3) = c("col1", "col2", "col3", "col4")
matrix3
matrix3[c("row1", "row2"), c("col1", "col2")]
matrix4 = matrix(1:6, nrow = 3, ncol = 2)
matrix4
matrix5 = matrix(11:16, nrow = 3, ncol = 2)
matrix5
rbind(matrix4, matrix5)
cbind(matrix4, matrix5)
matrix6 = matrix(1:6, nrow = 3, ncol = 2)
matrix6
matrix6 + 1
matrix6 * 2
vec1 = c(1:5)
vec2 = rep(1:2, length = 5)
vec3 = c("one", "two", "three", "four", "five")
df1 = data.frame(vec1, vec2, vec3)
df1
str(df1)
View(df1)
df2 = data.frame(vec1, vec2, vec3, stringsAsFactors = F)
df2
str(df2)
dim(df1)
head(df2)
df2[2,3]
variable1 = c(1,2,3,4,5)
variable2 = c(T, T, F, T, F)
variable3 = c("one", "two", "three", "four", "five")
df3 = data.frame(variable1,variable2, variable3)
df3
df3[2,3]
df3[df3$variable1>3, c(1,2)]
df3[df3$variable1>3, c("variable1", "variable2")]
df3[df3$Variable1>3,]
subset(df3, variable2 == TRUE)
subset(df3, variable2 == T & variable1>3)
subset(df3, variable1>3)

factor10 = c(2,3,4,5,2,3,4,5,2,3,4,5)
factor10 = as.factor(factor10)
str(factor10)
levels(factor10)
names(factor10)
names(factor10) = NULL
factor10
table(factor10)
