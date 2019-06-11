#BGGN213 - Lecture 4 Hand-On

dbl_var <- c(1, 2.5, 4.5)
dbl_var
log_var <- c(TRUE, FALSE, T, F)
log_var
chr_var <- c("these are", "some", "strings")
chr_var
var <- c(1, "G", "4", "0.05", TRUE)
var
x <- c(a = 1, b = 2, c = 3)
x
x <- 1:3; names(x) <- c("a", "b", "c")
x
x[c("b", "a")]
grades <- c(alice=80, barry=99, chandra=60, chris=100)
grades["barry"]
which.max(grades)
sort(grades)
x <- 1:3; names(x) <- c("a", "b", "c", "d")
x <- 1:3; names(x) <- 3:1; x[3]
x["3"]
dat <- data.frame(id = letters[1:10], x = 1:10, y = 11:20)
dat
dep <- read.csv2("http://bio3d.uib.no/data/pdb_deposition2.csv")

#swcarpentry exercise
#section 1

read.csv(file = "inflammation-01.csv", header = FALSE)
read.csv(file = "inflammation-01.csv")
?read.csv

weight_kg <- 55
weight_kg

weight_kg <- 57.5
#weight in kilograms is now
weight_kg

weight_lb <-  2.2*weight_kg
#weight in kg...
weight_kg
#...and in lbs
weight_lb

weight_kg <- 100.0
#weight in kg now...
weight_kg
#...and weight in pounds still
weight_lb

dat <- read.csv(file = "inflammation-01.csv", header = FALSE)
head(dat)

class(dat)
dim(dat)
#first value in dat, row1, column 1
dat[1,1]
#middle value in dat, row 30, column 20
dat[30,20]
#select multiple rows/columns
dat[c(1, 3, 5), c(10, 20)]
#selecting contiguous rows/columns
dat[1:4, 1:10]
dat[5:10, 1:10]
#selecting all rows/columns (below: row 5, all columns)
dat[5, ]
#selecting all rows/columns(below: all rows, columns 16-18)
dat[, 16:18]
#selecting entire data frame, leave both indices blank
dat[,]
#can also address columns by name or $ (below, all of V16 column within dataframe dat)
dat$V16

#operations in the data.frame
#naming all columns of data from from row 1 as patient_1
patient_1 <- dat[1,]
#max inflammation for patient 1
max(patient_1)
#combine selection and function call, max inflammation for patient 2
max(dat[2,])
#minimum inflammation on day 7
min(dat[,7])
# mean inflammation on day 7
mean(dat[,7])
#median inflammation on day 7
median(dat[,7])
#standard deviation of inflammation on day 7
sd(dat[,7])
#Summarize function
summary(dat[, 1:4])
#using the apply function
avg_patient_inflammation <- apply(dat, 1, mean)
avg_day_inflammation <- apply(dat, 2, mean)
avg_patient_inflammation
rowMeans(dat)
avg_day_inflammation
colMeans(dat)

#subsetting
animal <- c("m", "o", "n", "k", "e", "y")
#first three characters
animal[1:3]
#last three characters
animal[4:6]
#reverse first 4 characters
animal[3:1]
#selecting out a character from the vector
animal[-1]
animal[-4]
animal[-1:-4]
#result "e" "y"
#want result c("e", "o", "n")
animal[c(5, 2, 3)]

?seq
#subsetting and re-assignment: using the inflammation data fram "dat" from above: Let's pretend there was something wrong with the instrument on the first five days for every second patient(#2, 4, 6, etc.), which resulted int he measurements being twice as large as they should be.
#write a new vector containing each affected patient
#needed hint - patients
patient_sub <- seq(2, 60, 2)
#needed hint - days
day_sub <- seq(1,5)
dat2 <- dat
#check the size of your subset: returns '39 5', that is 30 [rows = patients] by 5 [columns=days]
dim(dat2[patient_sub, day_sub])
dat2[patient_sub, day_sub] <- dat2[patient_sub, day_sub]/2
dat2

#calculating the mean inflammation for patients 1 to 5 over the whole 40 days
apply(dat, ))
?apply

#optional exercise
1 + 2*(3+4)
log(4^3 + 3^(2+1))
sqrt((4+3)*(2+1))
((1+2)/(3+4))^2
