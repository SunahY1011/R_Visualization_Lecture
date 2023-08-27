library(dplyr)
library(stringr)
library(reshape2)
library(openxlsx)
library(ggplot2)


# 작업중인 폴더 경로 확인
getwd()

# 현재폴더경로가 다를 경우
setwd("./DataVisualization/")
getwd()


## 1. TCGA-BLCA data 불러오기 
patients <- read.xlsx(xlsxFile = './data/blca_tcga_pub_2017_clinical_data.xlsx', sheet = 1)

colnames(patients) # 열 이름 확인
colnames(patients) <- str_replace_all(colnames(patients), "\\.", "_") # 열이름의 .을 _로 바꿔주기
colnames(patients) # 잘 바뀌었는지 확인

# Q1. 현재 데이터에 있는 환자들은 몇 개의 ICD10 코드로 구분될까?
patients$`ICD-10_Classification`
unique(patients$`ICD-10_Classification`)
length(unique(patients$`ICD-10_Classification`))

# Q2. 데이터에 있는 남성 환자 수는 몇 명일까?
head(patients)
tail(patients)
table(patients$Sex)
patients %>% filter(Sex == 'Male')
patients %>% filter(Sex == 'Male') %>% head()
patients %>% filter(Sex == 'Male') %>% tail()
patients %>% filter(Sex == 'Male') %>% nrow()

## 2. dataframe : mutate, str_sub
# Overall Survival Status
patients$Overall_Survival_Status
unique(patients$Overall_Survival_Status) # 1:DECEASED / 0:LIVING
str_sub(patients$Overall_Survival_Status,1,1)
patients$OS_STATUS <- str_sub(patients$Overall_Survival_Status,1,1)
patients$OS_STATUS <- as.numeric(patients$OS_STATUS)
unique(patients$OS_STATUS)

# Disease Free Status
patients$Disease_Free_Status # 1:Recurred/Progressed / 0:DiseaseFree
patients <- patients %>% mutate(DFS_STATUS = str_sub(Disease_Free_Status, 1, 1))
unique(patients$DFS_STATUS)
patients <- patients %>% mutate(DFS_STATUS = as.numeric(DFS_STATUS))
unique(patients$DFS_STATUS)


patients <- patients %>% mutate(OS_STATUS2 = as.numeric(str_sub(Overall_Survival_Status, 1, 1)))
patients <- patients %>% mutate(DFS_STATUS2 = as.numeric(str_sub(Disease_Free_Status, 1, 1)))


## subset
ncol(patients) # 62
unique(patients$Last_Alive_Less_Initial_Pathologic_Diagnosis_Date_Calculated_Day_Value)
patients <- patients %>% subset(select = -c(Last_Alive_Less_Initial_Pathologic_Diagnosis_Date_Calculated_Day_Value))
ncol(patients) # 61

# 1-2 성별에 따라 평균 나이를 알 수 있을까?
patients %>% group_by(Sex) %>% slice(match(min(Diagnosis_Age), Diagnosis_Age))
patients %>% group_by(Sex) %>% filter(Diagnosis_Age == min(Diagnosis_Age))

#patients %>% group_by(Sex)
patients %>% group_by(Sex) %>% summarise(cnt = n())
patients <- patients %>% filter(!is.na(Sex))
patients %>% group_by(Sex) %>% summarise(count = n())
patients %>% group_by(Sex) %>% summarise(count = n(), age_mean = mean(Diagnosis_Age))

patients %>% group_by(American_Joint_Committee_on_Cancer_Tumor_Stage_Code) %>% summarise(count = n())

patients %>% group_by(Sex, American_Joint_Committee_on_Cancer_Tumor_Stage_Code) %>% summarise(count = n())
patients %>% group_by(Sex, American_Joint_Committee_on_Cancer_Tumor_Stage_Code) %>% summarise(count = n()) %>% View()
table(patients$Sex, patients$American_Joint_Committee_on_Cancer_Tumor_Stage_Code)


# Quiz3 : data.frame
patients <- patients %>% mutate(TSS = str_sub(Patient_ID,6,7))
patients %>% group_by(TSS) %>% summarise(count = n()) %>% View()


## 3. Basic Visualization
methods("plot") # 어떤 plot들을 그릴 수 있는지 확인

# 1) scatter plot
# 1-1) basic
plot.default(x = patients$Patient_Height, y=patients$Patient_Weight)

plot(x = patients$Patient_Height, y=patients$Patient_Weight)

# 1-2) lab title
plot(x = patients$Patient_Height, y=patients$Patient_Weight,
     xlab = "Height", ylab = "Weight")

# 1-3) plot title
plot(x = patients$Patient_Height, y=patients$Patient_Weight,
     xlab = "Height", ylab = "Weight",
     main = "TCGA-BLCA scatter plot")

# 1-4) xlim, ylim
plot(x = patients$Patient_Height, y=patients$Patient_Weight,
     xlab = "Height", ylab = "Weight",
     main = "TCGA-BLCA scatter plot",
     xlim = c(140,190), ylim = c(40,200))

# 1-5) dot shape
plot(x = patients$Patient_Height, y=patients$Patient_Weight,
     xlab = "Height", ylab = "Weight",
     main = "TCGA-BLCA scatter plot",
     xlim = c(140,190), ylim = c(40,200),
     pch = 2)

# 1-6) dot size
plot(x = patients$Patient_Height, y=patients$Patient_Weight,
     xlab = "Height", ylab = "Weight",
     main = "TCGA-BLCA scatter plot",
     xlim = c(140,190), ylim = c(40,200),
     pch = 2, cex = 2)

# 1-7) dot color
plot(x = patients$Patient_Height, y=patients$Patient_Weight,
     xlab = "Height", ylab = "Weight",
     main = "TCGA-BLCA scatter plot",
     xlim = c(140,190), ylim = c(40,200),
     pch = 2, cex = 0.5, col = "#E74C3C")


# scatter plot을 위한 데이터 만들기
test3 <- patients %>% group_by(Patient_Height) %>% 
  summarise(Patient_Weight_mean = mean(Patient_Weight, na.rm = T))
test3 <- test3 %>% filter(!is.na(Patient_Height))
head(test3)

# basic scatter plot
plot(x = test3$Patient_Height, y=test3$Patient_Weight_mean,
     xlab = "Height", ylab = "Weight Mean",
     main = "TCGA-BLCA")

# 1-8) dot type
plot(x = test3$Patient_Height, y=test3$Patient_Weight_mean,
     xlab = "Height", ylab = "Weight Mean",
     main = "TCGA-BLCA", type = "b") #l, o, b

# 1-9) line type
plot(x = test3$Patient_Height, y=test3$Patient_Weight_mean,
     xlab = "Height", ylab = "Weight Mean",
     main = "TCGA-BLCA", type = "b", lty = "dashed") # dashed

# 1-10) plot 구분하기
opar <- par(mfrow = c(1, 2))
plot(x = test3$Patient_Height, y=test3$Patient_Weight_mean,
     xlab = "Height", ylab = "Weight Mean",
     main = "TCGA-BLCA", type = "b", lty = "dashed")
plot(x = test3$Patient_Height, y=test3$Patient_Weight_mean,
     xlab = "Height", ylab = "Weight Mean",
     main = "TCGA-BLCA", type = "b", lty = "dashed")
par(opar)


# 원래대로 복구
par(mfrow = c(1, 1))

# 2. plot + 다른 plot 추가하기
# 2-1) 추세선 추가 (lowess = locally weighted polynomial regression)
plot(x = test3$Patient_Height, y=test3$Patient_Weight_mean,
     xlab = "Height", ylab = "Weight Mean",
     main = "TCGA-BLCA")
lines(lowess(x = test3$Patient_Height, y=test3$Patient_Weight_mean))

# 2-2) 직선 추세선 추가 (a = y절편, b = 기울기)
plot(x = test3$Patient_Height, y=test3$Patient_Weight_mean,
     xlab = "Height", ylab = "Weight Mean",
     main = "TCGA-BLCA")
abline(a=-100, b=1.05)

# 2-3) 추세선 색, 무늬 변경
plot(x = test3$Patient_Height, y=test3$Patient_Weight_mean,
     xlab = "Height", ylab = "Weight Mean",
     main = "TCGA-BLCA")
abline(a=-100, b=1.05, col="red")
abline(v=mean(test3$Patient_Height), lty=2)
abline(h=mean(test3$Patient_Weight_mean), lty=2)

# 2-4) 텍스트 추가하기1 - 자동
plot(x = test3$Patient_Height, y=test3$Patient_Weight_mean,
     xlab = "Height", ylab = "Weight Mean",
     main = "TCGA-BLCA")
abline(a=-100, b=1.05, col="red")
abline(v=mean(test3$Patient_Height), lty=2)
abline(h=mean(test3$Patient_Weight_mean), lty=2)
text(x = test3$Patient_Height, y=test3$Patient_Weight_mean, 
     labels = test3$Patient_Height, pos = 4)

# 2-5) 텍스트 추가하기2 - 수동
plot(x = test3$Patient_Height, y=test3$Patient_Weight_mean,
     xlab = "Height", ylab = "Weight Mean",
     main = "TCGA-BLCA")
abline(a=-100, b=1.05, col="red")
abline(v=mean(test3$Patient_Height), lty=2)
abline(h=mean(test3$Patient_Weight_mean), lty=2)
text(x = mean(test3$Patient_Height), y = mean(test3$Patient_Weight_mean), label = "Mean")

# 3. Boxplot
# 3-1) basic box plot
boxplot(patients$Diagnosis_Age)

boxplot(patients$Diagnosis_Age,
        main = "TCGA-BLCA Diagnosis Age")

# 3-2) boxplot outlier 표시하기
boxstats <- boxplot(patients$Diagnosis_Age)
boxstats
boxplot(patients$Diagnosis_Age,
        main = "TCGA-BLCA Diagnosis Age")
text(boxstats$out, labels = boxstats$out, pos = c(4))

# 3-3) 그룹간 비교하기 & notch boxplot 
boxplot(Diagnosis_Age ~ Sex, data = patients,
        main = "TCGA-BLCA Diagnosis Age by Sex")

boxplot(Diagnosis_Age ~ Sex, data = patients,
        main = "TCGA-BLCA Diagnosis Age by Sex",
        notch=TRUE)

# Quiz
boxstats <- boxplot(Diagnosis_Age ~ Sex, data = patients)
boxstats

boxplot(Diagnosis_Age ~ Sex, data = patients,
        main = "TCGA-BLCA Diagnosis Age by Sex",
        notch=TRUE,
        col = c("#E74C3C","#2980B9"),
        xlab = "", ylab = "Diagnosis Age")
text(boxstats$stats[3,], labels = boxstats$stats[3,], pos = c(1))


boxplot(Diagnosis_Age ~ Sex, data = patients,
        main = "TCGA-BLCA Diagnosis Age by Sex",
        notch=TRUE,
        col = c("#E74C3C","#2980B9"),
        xlab = "", ylab = "Diagnosis Age")
text(x=1, y=72.5, labels = "72.5", pos = c(1))
text(x=2, y=68, labels = "68", pos = c(1))

# 4. Density plot
patients <- patients %>% filter(!is.na(Diagnosis_Age))

# 4-1) Density plot (histogram)
hist(patients$Diagnosis_Age, # density
     main = "TCGA-BLCA Diagnosis Age", freq = FALSE) 
hist(patients$Diagnosis_Age, # frequency
     main = "TCGA-BLCA Diagnosis Age", freq = TRUE) 


# density plot (line)
plot(density(patients$Diagnosis_Age),
     main = "TCGA-BLCA Diagnosis Age")

# density plot (histogram+line)
hist(patients$Diagnosis_Age, freq = FALSE,
     main = "TCGA-BLCA Diagnosis Age")
lines(density(patients$Diagnosis_Age))

# Quiz
hist(patients$Diagnosis_Age, freq = FALSE,
     breaks = 6,
     main = "TCGA-BLCA Diagnosis Age",
     xlab = "Diagnosis Age", ylab = "Density")
lines(density(patients$Diagnosis_Age))


# barplot
tapply(patients$Diagnosis_Age, patients$American_Joint_Committee_on_Cancer_Tumor_Stage_Code, mean)
data <- round(tapply(patients$Diagnosis_Age, patients$American_Joint_Committee_on_Cancer_Tumor_Stage_Code, mean), digits = 2)
plt_col <- c(rep("grey",5),"red",rep("grey",6))
bp <- barplot(data, main = "Mean of Diagnosis Age by Tumor Stage",
              col = plt_col)
text(x = bp, y = data, labels = data, pos = c(1)) 


# pie chart
pie(table(patients$American_Joint_Committee_on_Cancer_Tumor_Stage_Code),
    main = "TCGA-BLCA Tumor Stage Code")

# pairs
pairs( ~ Diagnosis_Age + `Disease_Free_(Months)` + `Overall_Survival_(Months)` + Days_to_Sample_Collection_,
       data = patients,
       col = c("#E74C3C","#2980B9")[as.factor(patients$Sex)])

?pairs
