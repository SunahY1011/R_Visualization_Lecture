library(openxlsx)
library(dplyr)
library(stringr)
library(ggplot2)

# 파일이 data 폴더에 있는 경우
patients <- read.xlsx(xlsxFile = './data/blca_tcga_pub_2017_clinical_data.xlsx', sheet = 1)

# 파일이 기본 폴더에 있는 경우
patients <- read.xlsx(xlsxFile = './blca_tcga_pub_2017_clinical_data.xlsx', sheet = 1)

# data preprocessing
patients <- patients %>% filter(!is.na(Sex))
colnames(patients)
colnames(patients) <- str_replace_all(colnames(patients), "\\.", "_") # 열이름의 .을 _로 바꿔주기
colnames(patients)

# 사용할 열만 남겨놓음
ncol(patients)
patients <- patients[, c(1:7, 42:46, 55)]
ncol(patients)


# 열 이름 바꾸기
colnames(patients)[7]
colnames(patients)[7] <- "Tumor_Stage_Code"
colnames(patients)[9]
colnames(patients)[9] <- "Disease_Free_Months"
colnames(patients)[11]
colnames(patients)[11] <- "Overall_Survival_Months"

# 1. Density plot
# 1-1) Basic density plot
ggplot(data = patients)+
  geom_density(mapping = aes(x = Diagnosis_Age))

# 1-2) group 별 density plot이 그려지게 하기 
ggplot(data = patients)+
  geom_density(mapping = aes(x = Diagnosis_Age, group = Sex))

# 1-3) color, size, linetype 이해하기
# color = 선 색깔 변경
ggplot(data = patients)+
  geom_density(mapping = aes(x = Diagnosis_Age, group = Sex, color = Sex))

# size = 선 굵기 변경
ggplot(data = patients)+
  geom_density(mapping = aes(x = Diagnosis_Age, group = Sex, color = Sex), 
               size = 1)

# linetype = 선 종류 변경
ggplot(data = patients)+
  geom_density(mapping = aes(x = Diagnosis_Age, group = Sex, color = Sex), 
               size = 1, linetype = "longdash")
# linetype : dashed, twodash, longdash, dotted, dotdash, blank, solid


# 1-4) fill, alpha 이해하기
# fill = density curve 아래 채우기
ggplot(data = patients)+
  geom_density(mapping = aes(x = Diagnosis_Age, group = Sex, fill = Sex))

# alpha = 투명도 조절
ggplot(data = patients)+
  geom_density(mapping = aes(x = Diagnosis_Age, group = Sex, fill = Sex), 
               alpha = 0.4)


# 1-5) position : stack, fill
# 기본 plot에 선 굵기와 투명도를 변경해놓음(color, fill 수정)
ggplot(data = patients)+
  geom_density(mapping = aes(x = Diagnosis_Age, group = Sex, color = Sex, fill = Sex),
               size = 1, alpha = 0.4)

# stack : 하나의 plot에 쌓여서 그려짐
ggplot(data = patients)+
  geom_density(mapping = aes(x = Diagnosis_Age, group = Sex, color = Sex, fill = Sex),
               size = 1, alpha = 0.4, position = "stack")

# fill : male, female의 density 값을 비례하게 채우는 과정
ggplot(data = patients)+
  geom_density(mapping = aes(x = Diagnosis_Age, group = Sex, color = Sex, fill = Sex),
               size = 1, alpha = 0.4, position = "fill")

# 2. Histogram
# 2-1) 기본 histogram
ggplot(data = patients)+
  geom_histogram(mapping = aes(x = Diagnosis_Age))

# 2-2) bin 갯수 바꾸기
ggplot(data = patients)+
  geom_histogram(mapping = aes(x = Diagnosis_Age),
                 bins = 20)

# 2-3) group 에 따라 plot 그리기
ggplot(data = patients)+
  geom_histogram(mapping = aes(x = Diagnosis_Age, group = Sex),
                 bins = 20)

# 2-4) group 별로 color 입히기 (color = 선 색, bar 테두리색)
ggplot(data = patients)+
  geom_histogram(mapping = aes(x = Diagnosis_Age, group = Sex, color = Sex),
                 bins = 20)

# 2-5) group 별로 color 입히기 (fill = bar 색)
ggplot(data = patients)+
  geom_histogram(mapping = aes(x = Diagnosis_Age, group = Sex, fill = Sex),
                 bins = 20)

# 2-6) position 
# position 변경 : stack(default) = 쌓아주기
ggplot(data = patients)+
  geom_histogram(mapping = aes(x = Diagnosis_Age, group = Sex, fill = Sex),
                 bins = 20, position = "stack") # default

# frequency -> density 로 변경
# "..density.."는 ggplot2의 특수코드로 빈도수 대신 밀도로 그리도록 지정하는데 사용
ggplot(data = patients)+
  geom_histogram(mapping = aes(x = Diagnosis_Age, y = ..density.. , group = Sex, fill = Sex),
                 bins = 20, position = "stack")

# position : fill 
ggplot(data = patients)+
  geom_histogram(mapping = aes(x = Diagnosis_Age, group = Sex, fill = Sex),
                 bins = 20, position = "fill")

# position(fill), density 적용
ggplot(data = patients)+
  geom_histogram(mapping = aes(x = Diagnosis_Age, y = ..density.. , group = Sex, fill = Sex),
                 bins = 20, position = "fill")

# position : identity
ggplot(data = patients)+
  geom_histogram(mapping = aes(x = Diagnosis_Age, group = Sex, fill = Sex),
                 bins = 20, position = "identity")

# position(identity), alpha(투명도) 적용
ggplot(data = patients)+
  geom_histogram(mapping = aes(x = Diagnosis_Age, group = Sex, fill = Sex),
                 bins = 20, position = "identity", alpha = 0.4)

# position(identity), density 적용
ggplot(data = patients)+
  geom_histogram(mapping = aes(x = Diagnosis_Age, y = ..density.. , group = Sex, fill = Sex),
                 bins = 20, position = "identity")

# position(identity), alpha, density 적용
ggplot(data = patients)+
  geom_histogram(mapping = aes(x = Diagnosis_Age, y = ..density.. , group = Sex, fill = Sex),
                 bins = 20, position = "identity", alpha = 0.4)

# position : dodge
ggplot(data = patients)+
  geom_histogram(mapping = aes(x = Diagnosis_Age, group = Sex, fill = Sex),
                 bins = 20, position = "dodge")

# position(dodge), density 적용
ggplot(data = patients)+
  geom_histogram(mapping = aes(x = Diagnosis_Age, y = ..density.. , group = Sex, fill = Sex),
                 bins = 20, position = "dodge")

# 3. frequency polygon
# 3-1) frequency polygon : color 적용 가능
ggplot(data = patients)+
  geom_freqpoly(mapping = aes(x = Diagnosis_Age, color = Sex), size = 1)

# 3-2) frequency polygon : fill 적용 불가능
ggplot(data = patients)+
  geom_freqpoly(mapping = aes(x = Diagnosis_Age, fill = Sex), size = 1)


# 4. plot + plot
# 4-1) histogram + density plot
ggplot(data = patients)+
  geom_histogram(mapping = aes(x = Diagnosis_Age, y = ..density.. , group = Sex, fill = Sex), 
                 alpha = 0.4, position = "identity")+
  geom_density(mapping = aes(x = Diagnosis_Age, group = Sex, color = Sex), 
               size = 1.2)

# 4-2) histogram + freqpoly plot
ggplot(data = patients)+
  geom_histogram(mapping = aes(x = Diagnosis_Age, group = Sex, fill = Sex), 
                 alpha = 0.4, position = "identity")+
  geom_freqpoly(mapping = aes(x = Diagnosis_Age, group = Sex, color = Sex), 
                size = 1)


# 5. bar plot
# 5-1) basic plot
ggplot(data = patients)+
  geom_bar(mapping = aes(x = Sex))

# 5-2) bar plot + color -> 테두리만 색이 바뀜
ggplot(data = patients)+
  geom_bar(mapping = aes(x = Sex, color = Sex))

# 5-3) bar plot + fill -> bar 내부를 채워줌
ggplot(data = patients)+
  geom_bar(mapping = aes(x = Sex, fill = Sex))

# Quiz SH
# 1) 괄호 안의 환자 수 text 를 bar plot에 기입하세요
ggplot(data = patients, aes(x = Sex, fill = Sex))+
  geom_bar()+
  geom_text(aes(label=paste0("(", Sex, ")")),stat='count')

# 2) bar 위로 text 위치를 조정하세요
ggplot(data = patients, aes(x = Sex, fill = Sex))+
  geom_bar()+
  geom_text(aes(label=paste0("(", Sex, ")")), stat='count', vjust = -0.5)


ggplot(data = patients, aes(x = Sex, fill = Sex))+
  geom_bar()+
  geom_text(aes(label=..count..), stat='count', vjust = -0.5)




# 5-4) 테두리 색 바꾸기
ggplot(data = patients)+
  geom_bar(mapping = aes(x = Sex, fill = Sex), 
           color = 'black')

# 5-5) 테두리 색 바꾸기2
ggplot(data = patients)+
  geom_bar(mapping = aes(x = Sex, fill = Tumor_Stage_Code), 
           color = "white")

# 5-6) position 변경
ggplot(data = patients)+
  geom_bar(mapping = aes(x = Sex, fill = Tumor_Stage_Code), 
           color = "white", position = "fill")

# 5-7) axis title 
ggplot(data = patients)+
  geom_bar(mapping = aes(x = Sex, fill = Tumor_Stage_Code), color = "white")+
  ylab("Patients")

# 5-8) axis title to bold
ggplot(data = patients)+
  geom_bar(mapping = aes(x = Sex, fill = Tumor_Stage_Code), color = "white")+
  ylab("Patients")+
  theme(axis.title.y = element_text(face = "bold"))

# 5-9) remove axis title
ggplot(data = patients)+
  geom_bar(mapping = aes(x = Sex, fill = Tumor_Stage_Code), color = "white")+
  ylab("Patients")+
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_blank())

# 5-10) axis text to bold
ggplot(data = patients)+
  geom_bar(mapping = aes(x = Sex, fill = Tumor_Stage_Code), color = "white")+
  ylab("Patients")+
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"))

# 5-11) plot title
ggplot(data = patients)+
  geom_bar(mapping = aes(x = Sex, fill = Tumor_Stage_Code), color = "white")+
  labs(title = "TCGA-BLCA patients by Sex")+
  ylab("Patients")+
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

# 5-12) subtitle 
ggplot(data = patients)+
  geom_bar(mapping = aes(x = Sex, fill = Tumor_Stage_Code), color = "white")+
  labs(title = "TCGA-BLCA patients by Sex",
       subtitle = "colored by Tumor Stage")+
  ylab("Patients")+
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

# 5-13) legend title 
ggplot(data = patients)+
  geom_bar(mapping = aes(x = Sex, fill = Tumor_Stage_Code), color = "white")+
  labs(title = "TCGA-BLCA patients by Sex",
       subtitle = "colored by Tumor Stage")+
  ylab("Patients")+
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank())

# ppt 53p

# 5-14) legend border color
ggplot(data = patients)+
  geom_bar(mapping = aes(x = Sex, fill = Tumor_Stage_Code), color = "white")+
  labs(title = "TCGA-BLCA patients by Sex",
       subtitle = "colored by Tumor Stage")+
  ylab("Patients")+
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank(),
        legend.background = element_rect(color = "red"))

# 5-15) legend box background
ggplot(data = patients)+
  geom_bar(mapping = aes(x = Sex, fill = Tumor_Stage_Code), color = "white")+
  labs(title = "TCGA-BLCA patients by Sex",
       subtitle = "colored by Tumor Stage")+
  ylab("Patients")+
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank(),
        legend.background = element_rect(color = "red", fill = "yellow"))


# 6. Boxplot
# 6-1) basic plot
ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age))

# Quiz1
ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), 
               alpha = 0.3, size = 1)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.position = "none")

# 6-2) plot background
# theme_bw
ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), 
               alpha = 0.3, size = 1)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.position = "none")

# theme_classic
ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), 
               alpha = 0.3, size = 1)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.position = "none")

# theme_dark
ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3, size = 1)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_dark()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.position = "none")

# theme_grey
ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3, size = 1)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_grey()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.position = "none")

# theme_minimal
ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3, size = 1)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_minimal()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.position = "none")

# theme_void
ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3, size = 1)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_void()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.position = "none")

######### 60쪽
# background edit
# theme_bw base
ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3, size = 1)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.position = "none")

# 6-3) x축 grid 제거하기
ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), 
               alpha = 0.3, size = 1)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.position = "none",
        
        panel.grid.major.x = element_blank())

# y축 grid 제거하기
ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3, size = 1)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())

# y축 minor grid 제거하기
ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3, size = 1)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

# y축 minor grid 제거 & major grid 수정하기
ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3, size = 1)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", size = 1),
        panel.grid.minor.y = element_blank())

# 축 ticks 바꾸기
ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3, size = 1)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", size = 1),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_blank())

# 축 ticks 수정하기
ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3, size = 1)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", size = 1),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.length.y = unit(0.3, "in"))

# y축 제목 떨어트려놓기
ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3, size = 1)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, margin = margin(r = 10, unit = 'pt')),
        axis.text.x = element_text(face = "bold", size = 15),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", size = 1),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank())

# plot border 변경하기
ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3, size = 1)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, margin = margin(r = 10, unit = 'pt')),
        axis.text.x = element_text(face = "bold", size = 15),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", size = 1),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank(),
        
        panel.border = element_rect(color = "red", size = 2, linetype = "dashed"))

# Quiz2
ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3, size = 1)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, margin = margin(r = 10, unit = 'pt')),
        axis.text.x = element_text(face = "bold", size = 15),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", size = 1),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank(),
        
        panel.border = element_blank(),
        axis.line.x = element_line(color = "red", size = 2),
        axis.line.y = element_line(color = "red", size = 2))



# median 값 annotation
plotData <- tapply(patients$Diagnosis_Age, patients$Sex, median)
plotData
plotData['Female']

ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3, size = 1)+
  annotate("text", x = 1, y = plotData['Female'], label = plotData['Female'])+
  annotate("text", x = 2, y = plotData['Male'], label = plotData['Male'])+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, margin = margin(r = 10, unit = 'pt')),
        axis.text.x = element_text(face = "bold", size = 15),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", size = 1),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank())


ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3, size = 1)+
  annotate("text", x = 1, y = plotData['Female']-2, label = plotData['Female'])+
  annotate("text", x = 2, y = plotData['Male']-2, label = plotData['Male'])+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, margin = margin(r = 10, unit = 'pt')),
        axis.text.x = element_text(face = "bold", size = 15),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", size = 1),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank())

######### 75쪽
# Violin plot
ggplot(data = patients)+
  geom_violin(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3, size = 1)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, margin = margin(r = 10, unit = 'pt')),
        axis.text.x = element_text(face = "bold", size = 15),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", size = 1),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank())

# mean annotation 
plotData <- tapply(patients$Diagnosis_Age, patients$Sex, mean)
plotData
plotData['Female']

ggplot(data = patients)+
  geom_violin(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3, size = 1)+
  annotate("text", x = 1, y = plotData['Female']-2, label = plotData['Female'])+
  annotate("text", x = 2, y = plotData['Male']-2, label = plotData['Male'])+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, margin = margin(r = 10, unit = 'pt')),
        axis.text.x = element_text(face = "bold", size = 15),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", size = 1),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank())

# round(mean)
ggplot(data = patients)+
  geom_violin(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3, size = 1)+
  annotate("text", x = 1, y = plotData['Female']-2, label = round(plotData['Female'], digits = 2))+
  annotate("text", x = 2, y = plotData['Male']-2, label = round(plotData['Male'], digits = 2))+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, margin = margin(r = 10, unit = 'pt')),
        axis.text.x = element_text(face = "bold", size = 15),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", size = 1),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank())

# line annotation
ggplot(data = patients)+
  geom_violin(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3, size = 1)+
  annotate("text", x = 1, y = plotData['Female']-2, label = round(plotData['Female'], digits = 2))+
  annotate("text", x = 2, y = plotData['Male']-2, label = round(plotData['Male'], digits = 2))+
  annotate("segment", x = 0.68, xend = 1.32, y = plotData['Female'], yend = plotData['Female'])+
  annotate("segment", x = 1.6, xend = 2.4, y = plotData['Male'], yend = plotData['Male'])+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, margin = margin(r = 10, unit = 'pt')),
        axis.text.x = element_text(face = "bold", size = 15),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", size = 1),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank())

# line annotation
ggplot(data = patients)+
  geom_violin(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3, size = 1)+
  annotate("text", x = 1, y = plotData['Female']-2, label = round(plotData['Female'], digits = 2))+
  annotate("text", x = 2, y = plotData['Male']-2, label = round(plotData['Male'], digits = 2))+
  geom_segment(x = 0.68, xend = 1.32, y = plotData['Female'], yend = plotData['Female'], color = "#F8766D", size = 1)+
  geom_segment(x = 1.6, xend = 2.4, y = plotData['Male'], yend = plotData['Male'], color = "#00BFC4", size = 1)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, margin = margin(r = 10, unit = 'pt')),
        axis.text.x = element_text(face = "bold", size = 15),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", size = 1),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank())

######### 80쪽
# dotplot
ggplot(data = patients)+
  geom_dotplot(mapping = aes(x = factor(Sex), y = Diagnosis_Age, fill = factor(Sex)), 
               binaxis = "y", stackdir = "center", binwidth = 1)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, margin = margin(r = 10, unit = 'pt')),
        axis.text.x = element_text(face = "bold", size = 15),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", size = 1),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank())

# jitter plot
ggplot(data = patients)+
  geom_jitter(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex))+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, margin = margin(r = 10, unit = 'pt')),
        axis.text.x = element_text(face = "bold", size = 15),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", size = 1),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank())

# jitter + violin
ggplot(data = patients)+
  geom_jitter(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex))+
  geom_violin(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.4)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, margin = margin(r = 10, unit = 'pt')),
        axis.text.x = element_text(face = "bold", size = 15),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", size = 1),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank())

# dotplot + violin
ggplot(data = patients)+
  geom_dotplot(mapping = aes(x = factor(Sex), y = Diagnosis_Age, fill = factor(Sex)), 
               binaxis = "y", stackdir = "center", binwidth = 1)+
  geom_violin(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.4)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, margin = margin(r = 10, unit = 'pt')),
        axis.text.x = element_text(face = "bold", size = 15),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", size = 1),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank())

# Quiz3
ggplot(data = patients)+
  geom_violin(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.4)+
  geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0, size = 1, width = 0.3)+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, margin = margin(r = 10, unit = 'pt')),
        axis.text.x = element_text(face = "bold", size = 15),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "none",
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", size = 1),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank())

# scatter plot을 위한 데이터 만들기
plotData <- patients %>% group_by(Sex, Patient_Height) %>% summarise(Patient_Weight_mean = mean(Patient_Weight, na.rm = T))
plotData <- plotData %>% filter(!is.na(Patient_Height)) %>% mutate(Patient_Weight_mean = round(Patient_Weight_mean, digits = 2))
head(plotData)

# scatter plot
# 따라하기
ggplot(data = plotData) +
  geom_point(mapping = aes(x = Patient_Height, y = Patient_Weight_mean, color = Sex))+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 10, unit = 'pt')),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, unit = 'pt')),
        plot.title = element_text(face = "bold", size = 15))

# 단순히 보여지는 범위를 늘리는 것
ggplot(data = plotData) +
  geom_point(mapping = aes(x = Patient_Height, y = Patient_Weight_mean, color = Sex))+
  xlim(c(135, 200))+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 10, unit = 'pt')),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, unit = 'pt')),
        plot.title = element_text(face = "bold", size = 15))

# 나눠지는걸 바꿔보는 것
ggplot(data = plotData) +
  geom_point(mapping = aes(x = Patient_Height, y = Patient_Weight_mean, color = Sex))+
  scale_x_continuous(breaks = seq(140, 200, 10))+
  scale_y_continuous(breaks = seq(40, 160, 20))+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 10, unit = 'pt')),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, unit = 'pt')),
        plot.title = element_text(face = "bold", size = 15))

# Quiz SH
plotData_mean <- tapply(patients$Patient_Height, patients$Sex, mean, na.rm = T)

# 1) Age scale 바꾸기
ggplot(data = plotData) +
  geom_point(mapping = aes(x = Patient_Height, y = Patient_Weight_mean, color = Sex))+
  scale_x_continuous(breaks = seq(140, 200, 10))+
  scale_y_continuous(breaks = c(40, 50, 60, 70, 80, 100, 150))+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 10, unit = 'pt')),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, unit = 'pt')),
        plot.title = element_text(face = "bold", size = 15))

# 2) Age에 yr 붙이기
ggplot(data = plotData) +
  geom_point(mapping = aes(x = Patient_Height, y = Patient_Weight_mean, color = Sex))+
  scale_x_continuous(breaks = seq(140, 200, 10))+
  scale_y_continuous(breaks = c(40, 50, 60, 70, 80, 100, 150), labels = paste0(c(40, 50, 60, 70, 80, 100, 150), " yr"))+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 10, unit = 'pt')),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, unit = 'pt')),
        plot.title = element_text(face = "bold", size = 15))

# 3) linear model 점선 얇게 긋기
ggplot(data = plotData, aes(Patient_Height, Patient_Weight_mean, color = Sex)) +
  geom_point()+
  scale_x_continuous(breaks = seq(140, 200, 10))+
  scale_y_continuous(breaks = c(40, 50, 60, 70, 80, 100, 150), labels = paste0(c(40, 50, 60, 70, 80, 100, 150), " yr"))+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 10, unit = 'pt')),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, unit = 'pt')),
        plot.title = element_text(face = "bold", size = 15))+
  geom_smooth(method = lm, se = F, linetype = 2, size=0.5)


# color 다시 입히기
ggplot(data = plotData) +
  geom_point(mapping = aes(x = Patient_Height, y = Patient_Weight_mean, color = Sex))+
  scale_x_continuous(breaks = seq(140, 200, 10))+
  scale_y_continuous(breaks = seq(40, 160, 20))+
  scale_color_manual(values = c("Red", "Blue"))+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 10, unit = 'pt')),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, unit = 'pt')),
        plot.title = element_text(face = "bold", size = 15))

# color 다시 입히기2
ggplot(data = plotData) +
  geom_point(mapping = aes(x = Patient_Height, y = Patient_Weight_mean, color = Patient_Weight_mean))+
  scale_x_continuous(breaks = seq(140, 200, 10))+
  scale_y_continuous(breaks = seq(40, 160, 20))+
  scale_color_continuous()+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 10, unit = 'pt')),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, unit = 'pt')),
        plot.title = element_text(face = "bold", size = 15))

# color 다시 입히기3
ggplot(data = plotData) +
  geom_point(mapping = aes(x = Patient_Height, y = Patient_Weight_mean, color = Patient_Weight_mean))+
  scale_x_continuous(breaks = seq(140, 200, 10))+
  scale_y_continuous(breaks = seq(40, 160, 20))+
  scale_color_gradientn(colors = c("red","blue"))+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 10, unit = 'pt')),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, unit = 'pt')),
        plot.title = element_text(face = "bold", size = 15))


# geom smooth
ggplot(data = plotData) +
  geom_smooth(mapping = aes(x = Patient_Height, y = Patient_Weight_mean, color = Sex))+
  scale_x_continuous(breaks = seq(140, 200, 10))+
  scale_y_continuous(breaks = seq(40, 160, 20))+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 10, unit = 'pt')),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, unit = 'pt')),
        plot.title = element_text(face = "bold", size = 15))

# facet grid
ggplot(data = plotData) +
  geom_smooth(mapping = aes(x = Patient_Height, y = Patient_Weight_mean, color = Sex))+
  facet_grid(cols = vars(Sex))+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 10, unit = 'pt')),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, unit = 'pt')),
        plot.title = element_text(face = "bold", size = 15))

# facet grid scale 자동으로 바뀌는 것
ggplot(data = plotData) +
  geom_smooth(mapping = aes(x = Patient_Height, y = Patient_Weight_mean, color = Sex))+
  facet_grid(cols = vars(Sex), scales = "free")+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 10, unit = 'pt')),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, unit = 'pt')),
        plot.title = element_text(face = "bold", size = 15))

# facet grid scale 자동으로 바뀌는 것
ggplot(data = plotData) +
  geom_smooth(mapping = aes(x = Patient_Height, y = Patient_Weight_mean, color = Sex))+
  facet_grid(rows = vars(Sex), scales = "free")+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 10, unit = 'pt')),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, unit = 'pt')),
        plot.title = element_text(face = "bold", size = 15))

# facet grid
ggplot(data = plotData) +
  geom_smooth(mapping = aes(x = Patient_Height, y = Patient_Weight_mean, color = Sex))+
  facet_grid(cols = vars(Sex), scales = "free")+
  scale_y_continuous(breaks = seq(40,160,20))+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 10, unit = 'pt')),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, unit = 'pt')),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "none")

# Quiz4
ggplot(data = patients) +
    geom_boxplot(mapping = aes(x = Sex, y = Diagnosis_Age, color = Sex, fill = Sex), alpha = 0.3)+
  facet_grid(cols = vars(Overall_Survival_Status), scales = "free")+
  scale_y_continuous(breaks = seq(30,90,10))+
  scale_color_manual(values = c("red","blue"))+
  scale_fill_manual(values = c("red","blue"))+
  ylab("Diagnosis Age")+
  labs(title = "TCGA-BLCA patients' Diagnosis Age by Sex")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, unit = 'pt')),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "none",
        panel.grid.major.x = element_blank())


# Quiz5
ggplot(data = plotData) +
  geom_point(mapping = aes(x = Patient_Height, y = Patient_Weight_mean, color = Patient_Weight_mean, size = Patient_Weight_mean/80))+
  scale_x_continuous(breaks = seq(140, 200, 10))+
  scale_y_continuous(breaks = seq(40, 160, 20))+
  scale_color_gradientn(colors = c("red","blue"))+
  facet_grid(cols = vars(Sex), scales = "free")+
  ylab("Patient Weight Mean")+
  labs(title = "TCGA-BLCA patients by Sex",
       color = "Patient Weight Mean",
       size = "Patient Weight Mean")+
  theme_light()+
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 10, unit = 'pt')),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, unit = 'pt')),
        plot.title = element_text(face = "bold", size = 15))


# Quiz SH
# 1) 괄호 안의 환자 수 text 를 bar plot에 기입하세요
# 2) bar 위로 text 위치를 조정하세요
ggplot(data = patients, aes(x = Sex, fill = Sex))+
  geom_bar()+
  geom_text(aes(label=..count..), stat='count', vjust = -0.5)


ggplot(data = patients, aes(x = Sex, fill = Sex))+
  geom_bar()+
  geom_text(aes(label=paste0("(", Sex, ")")),stat='count')


ggplot(data = patients, aes(x = Sex, fill = Sex))+
  geom_bar()+
  geom_text(aes(label=paste0("(", Sex, ")")), stat='count', vjust = -0.5)





# Quiz SH
# 1) Age scale 바꾸기
# 2) y축 라벨 없에고 값에 kg 붙이기
# 3) legend color name 바꾸기 (Female -> female, Male -> male)
# 4) linear model 점선 얇게 긋기
ggplot(data = plotData, aes(Patient_Height, Patient_Weight_mean, color = Sex)) +
  geom_point()+
  scale_x_continuous(breaks = seq(140, 200, 10))+
  scale_y_continuous(breaks = c(40, 50, 60, 70, 80, 100, 150), 
                     labels = paste0(c(40, 50, 60, 70, 80, 100, 150), " kg"))+
  scale_color_discrete(labels = c("female", "male"))+
  ylab("")+
  labs(title = "TCGA-BLCA patients by Sex")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 10, unit = 'pt')),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, unit = 'pt')),
        plot.title = element_text(face = "bold", size = 15))+
  geom_smooth(method = lm, se = F, linetype = 2, size=0.5)

