## 사칙연산
7+4   # 덧셈
7-4   # 뺄셈
7*4   # 곱셈
7/4   # 나눗셈
7%/%4 # 몫
7%%4  # 나머지


## 조건문
# 예시1 
if(7%%4==15%%4){
  print('나머지가 같습니다.')
}else{
  print('나머지가 다릅니다.')
}


# 예시2
(19961011%%10000)
(19961011%%10000)%/%100
if((19961011%%10000)%/%100==10){
  print('10월에 태어났습니다.')
}


# 예시3
if(30%%10==0){
  print('10의 배수입니다.')
  if(30%%7==0){
    print('7의 배수입니다.')
  }else{
    print('7의 배수가 아닙니다.')
  }
}

# QUIZ1 : if 문 연습
birth <- 940726
sex <- 'female'

if(sex == 'female'){
  if(birth%/%10000 >= 24){
    print("뒷자리는 2로 시작합니다.")
  }else{
    print("뒷자리는 4로 시작합니다.")
  }
}else{
  if(birth%/%10000 >= 24){
    print("뒷자리는 1로 시작합니다.")
  }else{
    print("뒷자리는 3으로 시작합니다.")
  }
}


## 반복문
# 예시1
for(i in 1:5){
  print(i)
}

for(i in 'hello'){
  print(i)
}

for(i in c('happy','new','year')){
  print(i)
}

for(i in strsplit('hello', split = '')){
  print(i)
}

# 예시2
for(i in 1:10){
  if(i%%2==0){
    print(i)
  }
}

################################################################################
## 변수의 종류
a <- 5
b <- 3
a*b
c <- a+b

x <- 'A'
y <- 'B'
x+y

m <- FALSE
n <- TRUE


# vector 
x <- c('HELLO','WORLD','!')         # vector
y <- c('MY','NAME','IS','SNUH')     # vector
z <- c(x, y)                        # vector
w <- x+y                            # error

x <- c('HELLO','WORLD','!')         # vector - character
y <- c(1,2,3,4)                     # vector - integer
w <- c(x,y)                         # vector - character


# list
a <- list('HELLO','WORLD','!')      # list - character
b <- list('MY','NAME','IS','SNUH')  # list - character
c <- list(1,2,3,4)                  # list - integer


# matrix
mat1 <- matrix(1:6, nrow = 3, ncol = 2)
mat1

mat1 <- matrix(1:6, nrow = 4, ncol = 2)
mat1

mat2 <- matrix(c(x=c('HELLO','WORLD','!'), y=c('MY','NAME','IS','SNUH')))
mat2

mat3 <- matrix(c(x=c('HELLO','WORLD','!'), y=c('MY','NAME','IS','SNUH')), nrow=2)
mat3

mat4 <- matrix(c(x=c('HELLO','WORLD','!'), 1:6))
mat4

mat5 <- matrix(c(x=c('HELLO','WORLD','!'), 1:6), nrow=3)
mat5

# data.frame
dat1 <- data.frame(1:6)
dat1

dat2 <- data.frame(base=1:6)
dat2

dat1 <- data.frame(col1=1:6, col2=c('MY','NAME','IS','SNUH')) # error
dat1 <- data.frame(col1=1:6, col2=c('MY','NAME','IS','SNU','Hospital','!'))
dat1

dat2 <- data.frame(col1=1:6, col2=list('MY','NAME','IS','SNU','Hospital','!'))
dat2

################################################################################
# function
data <- c(2,5,7,10,15)

# 예시1
mymean <- function(vec){
  num <- 0
  sum <- 0
  for(i in vec){
    num <- num + 1
    sum <- sum + i
  }
  return(sum/num)
}
mymean(data)

# 예시2
mymean <- function(vec){
  sum <- 0
  for(i in vec){
    sum <- sum + i
  }
  return(sum/length(vec))
}
mymean(data)

# 예시3
mymean <- function(vec){
  return(sum(vec)/length(vec))
}
mymean(data)

# 예시4
mean(data)


# Quiz2 : median
mymedian <- function(vec){
  # vector 안에 몇 개의 숫자가 있는지 확인
  num <- 0
  for(i in vec){
    num <- num + 1
  }
  
  # 홀수개 => 가운데 / 짝수개 => 가운데 두 개의 평균
  if(num%%2==1){
    med <- 1+num%/%2
    result <- vec[med]
  }else{
    med <- num/2
    result <- (vec[med]+vec[med+1]/2)
  }
  
  return(result)
}
data
mymedian(data)

################################################################################
data <- c(-15,7,-1,-10,5,-8)

length(data)
sort(data)
sort(data, decreasing = TRUE)

min(data)
max(data)
mean(data)
median(data)
sum(data)

ceiling(mean(data))
floor(mean(data))
abs(mean(data))
round(mean(data), digits = 2)

seq(from = 0, to = 1, length.out = 11)
seq(from = 1, to = 9, by = 2)
rep(c('hello','everyone'), each = 2)
rep(c('hello','everyone'), times = 2)
rep(c('hello','everyone'), times = 2, each=2)

getwd()
setwd('./data')
getwd()

lst1 <- c(1,2,3,6)
lst2 <- c(1,2,4,8)
intersect(lst1, lst2)
setdiff(lst1, lst2)
union(lst1, lst2)


paste0(data, collapse = ', ')
paste0('hello','everyone')
paste0('hello',' ','everyone')


for(i in c(2,4,6,8,10)){
  print(paste0(i, ' is a even number.'))
}

nchar("Hello Everyone")
tolower("Hello Everyone")
toupper("Hello Everyone")

help('setdiff')
?setdiff


## Packages
install.packages('dplyr')
library(dplyr)

# package를 불러오기 전 
str_replace('Hello Everyone','H','h') # Error
?str_replace

install.packages('stringr')
install.packages('reshape2')
install.packages('openxlsx')
install.packages('ggplot2')

library(stringr)
library(reshape2)
library(openxlsx)
library(ggplot2)

## stringr function 
str_replace("Hello.Everyone.", "\\.", "_")
str_replace_all("Hello.Everyone.", "\\.", "_")

str_trim("    Hello.Everyone.    ", side = "left")
str_trim("    Hello.Everyone.    ", side = "right")
str_trim("    Hello.Everyone.    ", side = "both")

str_sub("Hello.Everyone.", start = 1, end = 5)

str_split("Hello.Everyone.", pattern = "\\.")
str_count("Hello.Everyone.", pattern = "\\.")
str_detect("Hello.Everyone.", pattern = "\\.")
str_locate("Hello.Everyone.", pattern = "\\.")
str_locate_all("Hello.Everyone.", pattern = "\\.")


################################################################################
