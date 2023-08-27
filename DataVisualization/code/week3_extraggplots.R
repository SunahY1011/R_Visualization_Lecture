library(openxlsx)
library(dplyr)
library(stringr)
library(ggplot2)

# 파일이 data 폴더에 있는 경우
patients <- read.xlsx(xlsxFile = './data/blca_tcga_pub_2017_clinical_data.xlsx', sheet = 1)

# 파일이 기본 폴더에 있는 경우
patients <- read.xlsx(xlsxFile = './blca_tcga_pub_2017_clinical_data.xlsx', sheet = 1)

# data preprocessing
colnames(patients)
colnames(patients) <- str_replace_all(colnames(patients), "\\.", "_") # 열이름의 .을 _로 바꿔주기
colnames(patients)

# colnames 변경
colnames(patients)[7] <- "AJCC_Tumor_Stage_Code"
colnames(patients)[8] <- "NAJCC_Tumor_Stage_Code"
colnames(patients)[12] <- 'AJCC_Pub_Version'
colnames(patients)[43] <- "DFS_Months"
colnames(patients)[45] <- "OS_Months"

# Data filtering
patients <- patients %>% filter(!is.na(Sex))
patients <- patients %>% filter(!is.na(Diagnosis_Age)) 
patients <- patients %>% filter(!is.na(Patient_Weight))
patients <- patients %>% filter(!is.na(Patient_Height))

# BMI 계산
patients <- patients %>% mutate(BMI = Patient_Weight/((Patient_Height/100)^2))
patients <- patients %>% filter(!is.na(BMI))

# cancer 진단일
table(patients$Year_Cancer_Initial_Diagnosis)
patients <- patients %>% mutate(Year_Cancer_Initial_Diagnosis = ifelse(Year_Cancer_Initial_Diagnosis < 2010, '< 2010', Year_Cancer_Initial_Diagnosis))
table(patients$Year_Cancer_Initial_Diagnosis)
patients <- patients %>% filter(!is.na(Year_Cancer_Initial_Diagnosis))



######## 1. gganimate
install.packages("gganimate")
install.packages("gifski")
library(gganimate)
library(gifski)


# 1-1) basic ggplot2 plot
ggplot(patients)+
  geom_point(aes(x=Patient_Height, y=Patient_Weight))

# 1-2) basic gganimate plot
plot <- ggplot(patients)+
  geom_point(aes(x=Patient_Height, y=Patient_Weight))+
  transition_states(Sex, transition_length = 2, state_length = 1)

# 1-3) Easing : 어떻게 움직일 것인지를 정하는 옵션
# cubic in out
ggplot(patients)+
  geom_point(aes(x=Patient_Height, y=Patient_Weight))+
  transition_states(Year_Cancer_Initial_Diagnosis, 
                    transition_length = 2, state_length = 1)+
  ease_aes('cubic-in-out')

# bounce out
ggplot(patients)+
  geom_point(aes(x=Patient_Height, y=Patient_Weight))+
  transition_states(Year_Cancer_Initial_Diagnosis, 
                    transition_length = 2, state_length = 1)+
  ease_aes('bounce-out')

# help("ease_aes") 참조
# 바꿀 수 있는 옵션 다양함

# 1-4) title에 정보 반영하기
ggplot(patients)+
  geom_point(aes(x=Patient_Height, y=Patient_Weight))+
  transition_states(Year_Cancer_Initial_Diagnosis, 
                    transition_length = 2, state_length = 1)+
  ease_aes('cubic-in-out')+
  
  # title : species 에 해당하는 애들이 들어오고
  # subtitle : 전체 nframe 중에 몇 번째 frame이 들어오는지
  ggtitle("Initial Cancer Diagnosis in {closest_state}", 
          subtitle = "Frame {frame} of {nframes}")

# 1-5) 각 샘플마다 color 입히기 => 이어지지 않음
ggplot(patients)+
  geom_point(aes(x=Patient_Height, y=Patient_Weight, 
                 color=Year_Cancer_Initial_Diagnosis))+
  transition_states(Year_Cancer_Initial_Diagnosis, 
                    transition_length = 2, state_length = 1)+
  ease_aes('cubic-in-out')+
  ggtitle("Initial Cancer Diagnosis in {closest_state}", 
          subtitle = "Frame {frame} of {nframes}")

# 1-6) 각 샘플마다 color를 입히고 이어지도록 함
ggplot(patients)+
  geom_point(aes(x=Patient_Height, y=Patient_Weight, 
                 color=Year_Cancer_Initial_Diagnosis, group = 1L))+
  transition_states(Year_Cancer_Initial_Diagnosis, 
                    transition_length = 2, state_length = 1)+
  ease_aes('cubic-in-out')+
  ggtitle("Initial Cancer Diagnosis in {closest_state}", 
          subtitle = "Frame {frame} of {nframes}")


# 1-7) Tweening : 이어지지 않는 점 fade in, fade out
ggplot(patients)+
  geom_point(aes(x=Patient_Height, y=Patient_Weight, 
                 color=Year_Cancer_Initial_Diagnosis))+
  transition_states(Year_Cancer_Initial_Diagnosis, 
                    transition_length = 2, state_length = 1)+
  ease_aes('cubic-in-out')+
  enter_appear()+exit_shrink()+
  ggtitle("Initial Cancer Diagnosis in {closest_state}", 
          subtitle = "Frame {frame} of {nframes}")

# enter_appear, drift, fade, fly, grow, recolour, reset
# exit_disappear, drift, fade, fly, grow, recolour, reset, shrink


# Quiz
ggplot(patients)+
  geom_point(aes(x = Patient_Height, y = Patient_Weight, 
                 color = Year_Cancer_Initial_Diagnosis, group = 1L, size = BMI))+
  facet_grid(cols = vars(Sex), scales = "free")+
  transition_states(Year_Cancer_Initial_Diagnosis, 
                    transition_length = 2, state_length = 1)+
  ease_aes('cubic-in-out')+
  ggtitle("Initial Cancer Diagnosis in {closest_state}")+
  guides(color = guide_legend(title = "Year"))+
  theme_bw()+
  theme(axis.title.x = element_text(size = 12, face = 'bold', margin = margin(t = 10, unit = 'pt')),
        axis.title.y = element_text(size = 12, face = 'bold', margin = margin(r = 10, unit = 'pt')),
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5)
  )

# 1-8) save plot
plot <- ggplot(patients)+
  geom_point(aes(x=Patient_Height, y=Patient_Weight, 
                 color=Year_Cancer_Initial_Diagnosis))+
  transition_states(Year_Cancer_Initial_Diagnosis, 
                    transition_length = 2, state_length = 1)+
  ease_aes('cubic-in-out')+
  enter_appear()+exit_shrink()+
  ggtitle("Initial Cancer Diagnosis in {closest_state}", 
          subtitle = "Frame {frame} of {nframes}")

anim_save(filename = "./test.gif", animation = plot, path = NULL, width = 600, height = 600, res = 100)

rm(plot)


######### 2. GGally
# install & load package
install.packages('GGally')
library(GGally)

# 2-1) basic form
ggpairs(data = patients, 
        columns = c("Sex","Diagnosis_Age","BMI", "Year_Cancer_Initial_Diagnosis"),
        mapping = aes(colour = Sex))

# 2-2) 각 plot에 뭘 넣을지 선택할 수 있음
# default 
ggpairs(data = patients, 
        columns = c("Sex","Diagnosis_Age","BMI", "Year_Cancer_Initial_Diagnosis"),
        mapping = aes(colour = Sex),
        upper = list(continuous = "cor", combo = "box_no_facet", discrete = "count", na = "na"),
        lower = list(continuous = "points", combo = "facethist", discrete = "facetbar", na = "na"))

# upper = lower
ggpairs(data = patients, columns = c("Sex","Diagnosis_Age","BMI", "Year_Cancer_Initial_Diagnosis"),
        mapping = aes(colour = Sex),
        upper = list(continuous = "cor", combo = "box_no_facet", discrete = "count", na = "na"),
        lower = list(continuous = "cor", combo = "box_no_facet", discrete = "count", na = "na"))

# blank upper
ggpairs(data = patients, columns = c("Sex","Diagnosis_Age","BMI", "Year_Cancer_Initial_Diagnosis"),
        mapping = aes(colour = Sex),
        upper = "blank",
        lower = list(continuous = "cor", combo = "box_no_facet", discrete = "count", na = "na"))

# plot types
ggpairs(data = patients, columns = c("Sex","Diagnosis_Age","BMI", "Year_Cancer_Initial_Diagnosis"),
        mapping = aes(colour = Sex),
        upper = list(continuous = "points", combo = "box", discrete = "table", na = "na"),
        lower = list(continuous = "density", combo = "facethist", discrete = "rowbar", na = "na"))

# continuous : cor, points, density, dot, dot_no_facet, 
# combo : box, box_no_facet, facethist, facetdensity, denstrip, 
# discrete : facetbar, count, table, cross, crosstable, colbar, rowbar, ratio, 


# box, box_no_facet, colbar, rowbar, count, cor, cross, crosstable, density, denstrip, 
# diagAxis, dot, dot_no_facet, facetbar, facetdensity, facetdensitystrip, facethist, 
# points, ratio, smooth, smooth_loess, smooth_lm, statistic, table, text, trends

# 2-3) 수정하기 (binwidth, plot proportions)
ggpairs(data = patients, 
        columns = c("Sex","Diagnosis_Age","BMI", "Year_Cancer_Initial_Diagnosis"),
        mapping = aes(colour = Sex),
        upper = list(continuous = wrap("points", alpha = 0.4), combo = "box", 
                     discrete = "table", na = "na"),
        lower = list(continuous = "density", combo = wrap("facethist", binwidth = 10), 
                     discrete = "rowbar", na = "na"),
        proportions = "auto")

# 2-4) 수정하기 (plot proportions customize)
ggpairs(data = patients, 
        columns = c("Sex","Diagnosis_Age","BMI", "Year_Cancer_Initial_Diagnosis"),
        mapping = aes(colour = Sex),
        upper = list(continuous = wrap("points", alpha = 0.4), combo = "box", 
                     discrete = "table", na = "na"),
        lower = list(continuous = "density", combo = wrap("facethist", binwidth = 10), 
                     discrete = "rowbar", na = "na"),
        proportions = c(1,2,2,4))


# 2-5) 수정하기 (column label, title)
ggpairs(data = patients, 
        columns = c("Sex","Diagnosis_Age","BMI", "Year_Cancer_Initial_Diagnosis"),
        mapping = aes(colour = Sex),
        upper = list(continuous = wrap("points", alpha = 0.4), combo = "box", 
                     discrete = "table", na = "na"),
        lower = list(continuous = "density", combo = wrap("facethist", binwidth = 10), 
                     discrete = "rowbar", na = "na"),
        proportions = c(1,2,2,4),
        columnLabels = c("Sex", "Diagnosis Age", "BMI", "Year of Cancer Initial Diagnosis"),
        title = "TCGA-BLCA")

# 2-6) theme 수정 (ggplot2 기반이기 때문에 기존에 쓰던대로 + 하여 추가할 수 있음)
plot <- ggpairs(data = patients, 
                columns = c("Sex","Diagnosis_Age","BMI", "Year_Cancer_Initial_Diagnosis"),
                mapping = aes(colour = Sex),
                upper = list(continuous = wrap("points", alpha = 0.4), combo = "box", 
                             discrete = "table", na = "na"),
                lower = list(continuous = "density", combo = wrap("facethist", binwidth = 10), 
                             discrete = "rowbar", na = "na"),
                proportions = c(1,2,2,4),
                columnLabels = c("Sex", "Diagnosis Age", "BMI", "Year of Cancer Initial Diagnosis"),
                title = "TCGA-BLCA")
plot <- plot + theme_bw()
plot

# 2-7) 여러가지 수정하기
plot <- ggpairs(data = patients, 
                columns = c("Sex","Diagnosis_Age","BMI", "Year_Cancer_Initial_Diagnosis"),
                mapping = aes(colour = Sex),
                upper = list(continuous = wrap("points", alpha = 0.4), combo = "box", 
                             discrete = "table", na = "na"),
                lower = list(continuous = "density", combo = wrap("facethist", binwidth = 10), 
                             discrete = "rowbar", na = "na"),
                proportions = c(1,2,2,4),
                columnLabels = c("Sex", "Diagnosis Age", "BMI", "Year of Cancer Initial Diagnosis"),
                title = "TCGA-BLCA")

plot <- plot+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 20),
        strip.text = element_text(face = "bold", colour = "red", size = 10))

plot

rm(plot)

# Quiz
plot <- ggpairs(data = patients, 
                columns = c("Patient_Height","Patient_Weight", "BMI"),
                mapping = aes(color = Sex),
                upper = list(continuous = "cor"),
                lower = list(continuous = wrap("smooth_lm", alpha = 0.3)),
                diag = list(continuous = wrap("densityDiag", alpha = 0.4)),
                labeller = "label_parsed",
                proportions = c(1,1,3),
                columnLabels = c("Height", "Weight", "BMI"),
                title = "TCGA-BLCA")

plot + 
  theme_bw()+
  theme(plot.title = element_text(face = "bold"))


######### 3. ggside
install.packages('ggside')
library(ggside)

# 3-1) basic ggplot2 plot
# 기존 방식
ggplot(data = patients)+ 
  geom_point(mapping = aes(x = Diagnosis_Age, y = BMI, colour = Sex), size = 2)

# 전체에 적용하기 위함 (plot 자체에는 차이가 없음)
ggplot(data = patients, mapping = aes(x = Diagnosis_Age, y = BMI, colour = Sex)) + 
  geom_point(size = 2)

# color만 geom_point에 적용 (plot 자체에는 차이가 없음)
ggplot(data = patients, mapping = aes(x = Diagnosis_Age, y = BMI)) + 
  geom_point(mapping = aes(colour = Sex), size = 2)

# 3-2) ggside plot : xsidedensity
# 기존방식
ggplot(data = patients)+ 
  geom_point(mapping = aes(x = Diagnosis_Age, y = BMI, colour = Sex), size = 2)+
  geom_xsidehistogram(aes(x = Diagnosis_Age, y = after_stat(count)), position = "stack")+
  geom_ysidedensity(aes(x = after_stat(density), y = BMI), position = "stack")

# geom_xsidebar(), geom_xsideboxplot(), geom_xsidecol(), geom_xsidedensity()
# geom_xsidefreqpoly(), geom_xsidehistogram(), geom_xsidelabel(), geom_xsideline()
# geom_xsidepath(), geom_xsidepoint(), geom_xsidesegment(), geom_xsidetext()
# geom_xsidetile(), geom_xsideviolin()

# 전체적용
ggplot(data = patients, mapping = aes(x = Diagnosis_Age, y = BMI, colour = Sex)) + 
  geom_point(size = 2)+
  geom_xsidehistogram(aes(y = after_stat(count)), position = "stack")+
  geom_ysidedensity(aes(x = after_stat(density)), position = "stack")

# color는 point에만 적용
ggplot(data = patients, mapping = aes(x = Diagnosis_Age, y = BMI)) + 
  geom_point(mapping = aes(colour = Sex), size = 2)+
  geom_xsidehistogram(aes(y = after_stat(count)), position = "stack")+
  geom_ysidedensity(aes(x = after_stat(density)), position = "stack")


# 3-3) fill 채우기
# x축 histogram에만
ggplot(data = patients, mapping = aes(x = Diagnosis_Age, y = BMI)) + 
  geom_point(mapping = aes(colour = Sex), size = 2)+
  geom_xsidehistogram(aes(y = after_stat(count), fill = Sex), position = "stack")+
  geom_ysidedensity(aes(x = after_stat(density)), position = "stack")

# 전체에 fill 적용
ggplot(data = patients, mapping = aes(x = Diagnosis_Age, y = BMI, fill = Sex)) + 
  geom_point(mapping = aes(colour = Sex), size = 2)+
  geom_xsidehistogram(aes(y = after_stat(count)), position = "stack")+
  geom_ysidedensity(aes(x = after_stat(density)), position = "stack")


# 3-4) scale
# boxplot을 side에 추가할 경우 error 발생
ggplot(data = patients, mapping = aes(x = Diagnosis_Age, y = BMI, fill = Sex)) + 
  geom_point(mapping = aes(colour = Sex), size = 2)+
  geom_xsidedensity(aes(y = after_stat(count)), position = "stack")+
  geom_ysideboxplot(aes(x = Sex), orientation = "x")

# scale_ysidex_discrete
ggplot(data = patients, mapping = aes(x = Diagnosis_Age, y = BMI, fill = Sex)) + 
  geom_point(mapping = aes(colour = Sex), size = 2)+
  geom_xsidedensity(aes(y = after_stat(count)), position = "stack")+
  geom_ysideboxplot(aes(x = Sex), orientation = "x")+
  scale_ysidex_discrete()

#color <- c('#E74C3C','#2980B9',"#27AE60","#E67E22","#F1C40F","#7D3C98","#F08080")
#color <- c('red','blue','green','orange','yellow','purple','pink')

# 다른 기존 scale을 추가해서 반영
ggplot(data = patients, mapping = aes(x = Diagnosis_Age, y = BMI, fill = Sex)) + 
  geom_point(mapping = aes(colour = Sex), size = 2)+
  geom_xsidedensity(aes(y = after_stat(count)), position = "stack")+
  geom_ysideboxplot(aes(x = Sex), orientation = "x")+
  scale_ysidex_discrete(guide = guide_axis(angle = 90, position = "top"))+
  scale_color_manual(values = c('red','blue'))+
  scale_fill_manual(values = c('red','blue'))


# 3-5) theme
# side size 변경
ggplot(data = patients, mapping = aes(x = Diagnosis_Age, y = BMI, fill = Sex))+ 
  geom_point(mapping = aes(colour = Sex), size = 2)+
  geom_xsidedensity(aes(y = after_stat(count)), position = "stack")+
  geom_ysideboxplot(aes(x = Sex), orientation = "x")+
  scale_ysidex_discrete(guide = guide_axis(angle = 90, position = "bottom"))+
  scale_color_manual(values = c('red','blue'))+
  scale_fill_manual(values = c('red','blue'))+
  theme_bw()+
  theme(ggside.panel.scale = 0.3)

# side size 개별 변경
ggplot(data = patients, mapping = aes(x = Diagnosis_Age, y = BMI, fill = Sex)) + 
  geom_point(mapping = aes(colour = Sex), size = 2)+
  geom_xsidedensity(aes(y = after_stat(count)), position = "stack")+
  geom_ysideboxplot(aes(x = Sex), orientation = "x")+
  scale_ysidex_discrete(guide = guide_axis(angle = 90, position = "bottom"))+
  scale_color_manual(values = c('red','blue'))+
  scale_fill_manual(values = c('red','blue'))+
  ggtitle("TCGA-BLCA")+
  theme_bw()+
  theme(ggside.panel.scale.x = 0.2,
        ggside.panel.scale.y = 0.3,
        ggside.panel.background = element_rect(fill = "lightgrey"),
        ggside.panel.grid = element_line(linetype = 'dotted', color = 'black'),
        plot.title = element_text(size = 15, face = "bold"))

# Quiz
ggplot(data = patients, mapping = aes(x = Diagnosis_Age, y = BMI, fill = Sex))+ 
  geom_point(mapping = aes(colour = Sex), size = 2)+
  geom_xsidedensity(aes(y = after_stat(count)), position = "stack")+
  geom_ysideboxplot(aes(x = Sex), orientation = "x")+
  geom_encircle(mapping = aes(x=Diagnosis_Age, y = BMI), 
                size = 2, alpha = 0.3, expand = -0.1, s_shape = 2)+
  scale_ysidex_discrete(guide = guide_axis(angle = 90, position = "bottom"))+
  scale_color_manual(values = c('red','blue'))+
  scale_fill_manual(values = c('red','blue'))+
  ggtitle("TCGA-BLCA")+
  xlab('Diagnosis Age')+
  theme_bw()+
  theme(ggside.panel.scale.x = 0.2,
        ggside.panel.scale.y = 0.3,
        ggside.panel.grid = element_line(linetype = 'dotted', color = 'black'),
        plot.title = element_text(size = 15, face = "bold"))

######### 4. ggalluvial
install.packages('ggalluvial')
library(ggalluvial)

# Examples
data(Titanic)
Titanic <- data.frame(Titanic)
ggplot(data = Titanic,
       aes(axis1 = Class, axis2 = Sex, axis3 = Age,
           y = Freq)) +
  scale_x_discrete(limits = c("Class", "Sex", "Age"), expand = c(.2, .05)) +
  xlab("Demographic") +
  geom_alluvium(aes(fill = Survived)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_void() +
  ggtitle("passengers on the maiden voyage of the Titanic",
          "stratified by demographics and survival")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.margin = margin(l = -50, unit = 'pt'))

# data format 만들기
data <- patients %>% group_by(Sex, Year_Cancer_Initial_Diagnosis, Overall_Survival_Status) %>% 
  summarise(count = n())
head(data)

# format check
is_alluvia_form(data, axes = 1:3)

# 4-1) basic form
ggplot(data = as.data.frame(data), mapping = aes(y = count, axis1 = Sex, axis2 = Overall_Survival_Status))+
  geom_alluvium(aes(fill = Year_Cancer_Initial_Diagnosis))

# 4-2) label 붙일 틀 만들기기
ggplot(data = as.data.frame(data), mapping = aes(y = count, axis1 = Sex, axis2 = Overall_Survival_Status))+
  geom_alluvium(aes(fill = Year_Cancer_Initial_Diagnosis), width = 1/12)+
  geom_stratum(width = 1/12, fill = "white", color = "grey")

# 4-3) 틀에 label 넣기기
ggplot(data = as.data.frame(data), mapping = aes(y = count, axis1 = Sex, axis2 = Overall_Survival_Status))+
  geom_alluvium(aes(fill = Year_Cancer_Initial_Diagnosis), width = 1/12)+
  geom_stratum(width = 1/12, fill = "white", color = "grey")+
  geom_label(stat = "stratum", aes(label = after_stat(stratum)))

# 4-4) label text로 넣기기
ggplot(data = as.data.frame(data), mapping = aes(y = count, axis1 = Sex, axis2 = Overall_Survival_Status))+
  geom_alluvium(aes(fill = Year_Cancer_Initial_Diagnosis), width = 1/12)+
  geom_stratum(width = 1/12, fill = "white", color = "grey")+
  geom_text(stat = "stratum", aes(label = after_stat(stratum)))

# 4-5) label이 잘리니까 가로로 넣자
ggplot(data = as.data.frame(data), mapping = aes(y = count, axis1 = Sex, axis2 = Overall_Survival_Status))+
  geom_alluvium(aes(fill = Year_Cancer_Initial_Diagnosis), width = 1/10)+
  geom_stratum(width = 1/10, fill = "white", color = "grey")+
  geom_text(stat = "stratum", aes(label = after_stat(stratum)))+
  coord_flip()

# 4-6) 축을 세 개로 만들자
ggplot(data = as.data.frame(data), mapping = aes(y = count, 
                                                 axis1 = Sex, 
                                                 axis2 = Year_Cancer_Initial_Diagnosis,
                                                 axis3 = Overall_Survival_Status))+
  geom_alluvium(aes(fill = Year_Cancer_Initial_Diagnosis), width = 1/10)+
  geom_stratum(width = 1/10, fill = "white", color = "grey")+
  geom_text(stat = "stratum", aes(label = after_stat(stratum)))+
  coord_flip()

# 4-7) legend 없애고, 축 수정하기 
ggplot(data = as.data.frame(data), mapping = aes(y = count, 
                                                 axis1 = Sex, 
                                                 axis2 = Year_Cancer_Initial_Diagnosis,
                                                 axis3 = Overall_Survival_Status))+
  geom_alluvium(aes(fill = Year_Cancer_Initial_Diagnosis), width = 1/10)+
  geom_stratum(width = 1/10, fill = "white", color = "grey")+
  geom_text(stat = "stratum", aes(label = after_stat(stratum)))+
  coord_flip()+
  guides(fill = "none")+
  scale_x_discrete(limits = c("Sex", "Initial Cancer Diagnosis Year", "Overall Survival"), 
                   expand = c(0.1, 0.1))

# 4-8) 컬러 수정하고, 모양 바꾸고 등
ggplot(data = as.data.frame(data), mapping = aes(y = count, 
                                                 axis1 = Sex, 
                                                 axis2 = Year_Cancer_Initial_Diagnosis,
                                                 axis3 = Overall_Survival_Status))+
  geom_alluvium(aes(fill = Year_Cancer_Initial_Diagnosis), width = 1/10, knot.pos = 0)+
  geom_stratum(width = 1/10, fill = "white", color = "black", alpha = 0.2)+
  geom_text(stat = "stratum", aes(label = after_stat(stratum)))+
  coord_flip()+
  guides(fill = "none")+
  scale_x_discrete(limits = c("Sex", "Initial Cancer Diagnosis Year", "Overall Survival"), 
                   expand = c(0.01, 0.01))+
  scale_fill_manual(values = c("#E74C3C", "#F1C40F", "#27AE60", "#2980B9", "#7D3C98"))

# 4-9) flow로 변경
ggplot(data = as.data.frame(data), mapping = aes(y = count, 
                                                 axis1 = Sex, 
                                                 axis2 = Year_Cancer_Initial_Diagnosis,
                                                 axis3 = Overall_Survival_Status))+
  geom_flow(aes(fill = Year_Cancer_Initial_Diagnosis), width = 1/10, knot.pos = 0)+
  geom_stratum(width = 1/10, fill = "white", color = "black", alpha = 0.5)+
  geom_text(stat = "stratum", aes(label = after_stat(stratum)))+
  coord_flip()+
  guides(fill = "none")+
  scale_x_discrete(limits = c("Sex", "Initial Cancer Diagnosis Year", "Overall Survival"), 
                   expand = c(0.01, 0.01))+
  scale_fill_manual(values = c("#E74C3C", "#F1C40F", "#27AE60", "#2980B9", "#7D3C98"))

# 4-10) theme 수정하기
ggplot(data = as.data.frame(data), mapping = aes(y = count, 
                                                 axis1 = Sex, 
                                                 axis2 = Year_Cancer_Initial_Diagnosis,
                                                 axis3 = Overall_Survival_Status))+
  geom_flow(aes(fill = Year_Cancer_Initial_Diagnosis), width = 1/10, knot.pos = 0)+
  geom_stratum(width = 1/10, fill = "white", color = "black", alpha = 0.5)+
  geom_text(stat = "stratum", aes(label = after_stat(stratum)))+
  coord_flip()+
  guides(fill = "none")+
  scale_x_discrete(limits = c("Sex", "Initial Cancer Diagnosis Year", "Overall Survival"), 
                   expand = c(0.01, 0.01))+
  scale_fill_manual(values = c("#E74C3C", "#F1C40F", "#27AE60", "#2980B9", "#7D3C98"))+
  ggtitle("TCGA-BLCA")+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size = 12, margin = margin(r = -14, unit = "pt")),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5))


# 4-11) examples : Others 시계열 데이터의 경우 
data(majors)
majors
majors$curriculum <- as.factor(majors$curriculum)
ggplot(majors,
       aes(x = semester, stratum = curriculum, alluvium = student,
           fill = curriculum, label = curriculum)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",color = "darkgray") +
  geom_stratum() +
  ggtitle("Student Curricula Across Several Semesters")+
  theme_minimal()+
  theme(legend.position = "top",
        plot.title = element_text(size = 15, hjust = 0.5, margin = margin(b = 5, unit = 'pt')),
        axis.text.y = element_blank(),
        panel.grid = element_blank())

rm(majors, data, Titanic)

################################### 예희쌤!!

######### 5. ggalt
install.packages("tidyverse")
install.packages("ggalt")
library(tidyverse)
library(ggalt)

# 5-1) 기본 plot
ggplot(data = patients) +
  geom_point(mapping = aes(x=Patient_Weight, y =Patient_Height, col=Sex))


# 5-2) 일부 테마 수정
ggplot(data = patients)+
  geom_point(mapping = aes(x=Patient_Weight, y =Patient_Height, col=Sex))+
  xlim(25, 150) + ylim(130, 190)+
  ggtitle("Height and weight by gender")+ 
  xlab("Weight(kg)") + ylab("Height(cm)")

# 5-3) color 변경
ggpatients <- ggplot(data = patients)+
  geom_point(mapping = aes(x=Patient_Weight, y =Patient_Height, col=Sex))+
  xlim(25, 150) + ylim(130, 190)+
  ggtitle("Height and weight by gender")+ 
  xlab("Weight(kg)") + ylab("Height(cm)")+
  scale_color_manual(values = c("orange", "skyblue"),
                     labels = c("female", "male"), name = "Sex")

ggpatients

# 5-4) 예시 그림 넣기
ggpatients +
  geom_encircle(data=patients_encircle,
                mapping=aes(x=Patient_Weight, y=Patient_Height), 
                fill = 'orange',
                color = 'orange',
                size = 3,
                expand=0.1,
                s_shape=2,
                alpha=0.4)


# 5-4) Female 정보만 불러오기
patients_encircle <- patients %>% filter(Sex=='Female')

# 5-5) Circle 추가
ggpatients + geom_encircle(data=patients_encircle, 
                           mapping=aes(x=Patient_Weight, y=Patient_Height))

# 5-6) circle color, 선 size 변경
ggpatients + geom_encircle(data=patients_encircle,
                           mapping=aes(x=Patient_Weight, y=Patient_Height), 
                           color = 'orange', size = 3)


# 5-7) 원 확장
ggpatients + geom_encircle(data=patients_encircle,
                           mapping=aes(x=Patient_Weight, y=Patient_Height), 
                           color = 'orange', size = 3, expand = 0.1)


# 5-8) shape 완만하게 변경하기 
# 1 기본
ggpatients + geom_encircle(data=patients_encircle,
                           mapping=aes(x=Patient_Weight, y=Patient_Height), 
                           color = 'orange', size = 3,
                           s_shape = 0.1)

# 2 완만하게
ggpatients + geom_encircle(data=patients_encircle,
                           mapping=aes(x=Patient_Weight, y=Patient_Height), 
                           color = 'orange', size = 3,
                           s_shape = 2)

# 5-9) 원 안에 채우기
ggpatients + geom_encircle(data=patients_encircle,
                           mapping=aes(x=Patient_Weight, y=Patient_Height), 
                           color = 'orange', size = 3, s_shape = 2,
                           fill = 'orange')

# 5-10) 투명도 조절
ggpatients + geom_encircle(data=patients_encircle,
                           mapping=aes(x=Patient_Weight, y=Patient_Height), 
                           color = 'orange', size = 3, s_shape = 2,
                           fill = 'orange', alpha = 0.4)


# 5-13) female, male 각각 추가하기
patients_encircle_female <- patients %>% filter(Sex=='Female')
patients_encircle_male <- patients %>% filter(Sex=='Male')

# 완성!
ggpatients +
  geom_encircle(data=patients_encircle_female,
                mapping=aes(x=Patient_Weight, y=Patient_Height),
                color='orange', s_shape=2, size=2, 
                fill = 'orange', alpha=0.4)+
  geom_encircle(data=patients_encircle_male,
                mapping=aes(x=Patient_Weight, y=Patient_Height),
                color='skyblue', s_shape=2, size=2, 
                fill='skyblue', alpha=0.4)+
  theme(plot.title = element_text(face = 'bold'))


rm(patients_encircle, patients_encircle_female, patients_encircle_male, ggpatients)

# Quiz 1
# patients 데이터에서 Diagnosis_Age, Patient_Height, NAJCC_Tumor_Stage_Code가 na가 아닌 값을   
# patients_1으로 지정한 뒤, 아래의 기본 ggplot 형식을 따라 그래프를 그려라.
# ggplot(data = patients_1) +
# geom_point(mapping = aes(x=Diagnosis_Age,
#                         y=Patient_Height,col=AJCC_Tumor_Stage_Code)) 
# 1) 제목: Height by Age (Tumor Stage Code)
#    x 축 이름: Age
#    y 축 이름: Height(cm)
#    범례 이름: Tumor Stage 
# 2) ggalt 라이브러리를 이용해 geom_encircle 함수로 T4 에 해당하는
#    Tumor Stage Code 를 원형으로 표시하기

patients_1 <- patients %>% filter(!is.na(NAJCC_Tumor_Stage_Code))

ggheight_age <- ggplot(data = patients_1) +
  geom_point(mapping = aes(x=Diagnosis_Age,y=Patient_Height,
                           col=NAJCC_Tumor_Stage_Code))+
  scale_color_manual(values = rainbow(12))+
  xlim(40,90)+ylim(145,190)+
  ggtitle("Height by Age")+
  xlab("Age")+ylab("Height(cm)")+
  theme_classic()+
  theme(plot.title = element_text(face = 'bold', hjust = 0.5),
        legend.position = 'top',
        legend.title = element_blank(),
        legend.background = element_rect(color = 'black'))

ggheight_age

color <- c('yellowgreen','green','lightblue')
names(color) <- c('T3', 'T3a', 'T3b')

for(TS in c('T3', 'T3a', 'T3b')){
  print(TS)
  patients_encircle <- patients_1 %>% filter(NAJCC_Tumor_Stage_Code == TS)
  
  ggheight_age <- ggheight_age +
    geom_encircle(data=patients_encircle,
                  mapping=aes(x=Diagnosis_Age, y=Patient_Height), 
                  color=color[TS], size=2, s_shape=2,
                  fill=color[TS], alpha=0.3)
  }

ggheight_age <- ggheight_age + 
  annotate('text', x=63, y=171, label='T3')+
  annotate('text', x=75, y=172, label='T3a')+
  annotate('text', x=66, y=180, label='T3b')

ggheight_age

rm(color, TS, ggheight_age)

######### 6. ggpubr
install.packages("colorspace")
install.packages("ggpubr")
library(colorspace)
library(ggpubr)


# Perform the test
compare_means(Diagnosis_Age ~ NAJCC_Tumor_Stage_Code,  data = patients,
              ref.group = ".all.", method = "t.test")

# 6-1) basic plot
ggboxplot(patients, x = "NAJCC_Tumor_Stage_Code", y = "Diagnosis_Age", 
          color = "NAJCC_Tumor_Stage_Code")

# 6-2) jitter
ggboxplot(patients, x = "NAJCC_Tumor_Stage_Code", y = "Diagnosis_Age", 
          color = "NAJCC_Tumor_Stage_Code", 
          add = "jitter")

# 6-3) 전체평균 위치 추가
ggboxplot(patients, x = "NAJCC_Tumor_Stage_Code", y = "Diagnosis_Age", 
          color = "NAJCC_Tumor_Stage_Code", add = "jitter")+
  geom_hline(yintercept = mean(patients$Diagnosis_Age), linetype = 2)


# 6-4) 전체와 각 그룹간 차이를 확인하기 위한 t-test 결과 표시
ggboxplot(patients, x = "NAJCC_Tumor_Stage_Code", y = "Diagnosis_Age", 
          color = "NAJCC_Tumor_Stage_Code", add = "jitter")+
  geom_hline(yintercept = mean(patients$Diagnosis_Age), linetype = 2)+
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.")


# 6-5) 나머지 디자인 추가(제목, 축 제목 수정, x축 텍스트 회전, legend 제거)하고 변수에 저장
boxplot_draft <- ggboxplot(patients, x = "NAJCC_Tumor_Stage_Code", y = "Diagnosis_Age", 
                           color = "NAJCC_Tumor_Stage_Code", add = "jitter") +
  geom_hline(yintercept = mean(patients$Diagnosis_Age), linetype = 2)+
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.",
                     hide.ns = T, size = 5)+
  labs(title="Plot of age by tumor code", x ="Tumor stage", y = "Age")+
  rotate_x_text(angle = 45)+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        #axis.text.x = element_text(angle = 45, vjust = 0.5),
        plot.title = element_text(face="bold"),
        legend.position = 'none')

boxplot_draft

# 아래과정을 왜 하는지, 이렇게 만들고 싶다고 하나 보여주고 시작하자
ggarrange(boxplot_draft, text.p,
          ncol = 1, nrow = 2,
          heights = c(1.3, 0.3))


# 6-6) plot에 넣을 텍스트 추가
text <- paste("Patients data set gives the measurements",
              "of the variables Age, Tumor stages",
              "and statistical p-value of t-test, respectively,",
              "from each of 10 categories of Tumor stages.",
              "The categories are T2, TX, T3a, T2a, T1, T4a, T3b, T2b, T3, T4.", sep = " ")

# 6-7) 텍스트를 plot format으로 변경, 스타일 지정
text.p <- ggparagraph(text, face = "italic", size = 12)

# 6-8) Arrange the plots on the same page
ggarrange(boxplot_draft, text.p)

# 6-9) 배치방법 변경
ggarrange(boxplot_draft, text.p,
          ncol = 1, nrow = 2)

# 6-10) 배치할 비율 변경
ggarrange(boxplot_draft, text.p,
          ncol = 1, nrow = 2,
          heights = c(1.3, 0.3))

# Quiz 2
# t.test결과를 그래프 하단에 위치시키시오.
# 그래프의 폭과 그래프 하단의 폭의 길이를 일치시키시오.
# 그래프 형식은 하단의 텍스트를 포함하여 위의 그래프와 같게 할 것.
boxplot_draft <- ggboxplot(patients, x = "NAJCC_Tumor_Stage_Code", y = "Diagnosis_Age", 
                           color = "NAJCC_Tumor_Stage_Code", add = "jitter") +
  geom_hline(yintercept = mean(patients$Diagnosis_Age), linetype = 2)+
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.",
                     hide.ns = T, size = 5)+
  labs(title="Plot of age by tumor code", x ="Tumor stage", y = "Age")+
  rotate_x_text(angle = 45)+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.title = element_text(face="bold"),
        legend.position = 'none')

ggarrange(boxplot_draft, text.p,
          ncol = 1, nrow = 2,
          heights = c(1.3, 0.3),
          align = "v")

rm(text, text.p, boxplot_draft)

######### 7. ggridges
install.packages("ggridges")
library(ggridges)

# patients 데이터에서 AJCC_Pub_Version 변수가 6th, 7th 인 것들만 남기기
table(patients$AJCC_Pub_Version)
patients_for_graph <- patients %>% filter(AJCC_Pub_Version %in% c('6th','7th'))
table(patients_for_graph$AJCC_Pub_Version)

# 7-1) geom_density_ridges
ggplot(patients_for_graph, aes(x = Patient_Height, y = AJCC_Pub_Version)) + 
  geom_density_ridges()

# 7-2) geom_density_ridges2
ggplot(patients_for_graph, aes(x = Patient_Height, y = AJCC_Pub_Version)) + 
  geom_density_ridges2()

# 7-3) scale 조정
ggplot(patients_for_graph, aes(x = Patient_Height, y = AJCC_Pub_Version)) + 
  geom_density_ridges(scale =0.9)

# scale = 5
ggplot(patients_for_graph, aes(x = Patient_Height, y = AJCC_Pub_Version)) + 
  geom_density_ridges(scale = 5)


# 7-4) quantile_lines = TRUE: 분위수 선을 그래프에 표시해준다.
ggplot(patients_for_graph, aes(x = Patient_Height, y = AJCC_Pub_Version)) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = 4)

# 7-5) Jittered points
ggplot(patients_for_graph, aes(x = Patient_Height, y = AJCC_Pub_Version)) +
  geom_density_ridges(jittered_points = TRUE)

# 7-6) point_shape 변경
ggplot(patients_for_graph, aes(x = Patient_Height, y = AJCC_Pub_Version)) +
  geom_density_ridges(jittered_points = TRUE, point_shape = '*', 
                      point_size = 3, point_alpha = 1, alpha = 0.7)


# Quiz 3 => 너무 약해
# 위의 그래프에서 median 만 표기하는 옵션 추가하기
ggplot(patients_for_graph, aes(x = Patient_Height, y = AJCC_Pub_Version)) +
  geom_density_ridges(
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0),
    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,
    quantile_lines = TRUE, quantiles = 2)

# 7-7) 특정 위치에 annotation 추가 : 2.5% 와 97.5%
ggplot(patients_for_graph, aes(x = Patient_Height, y = AJCC_Pub_Version)) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = c(0.025, 0.975), alpha = 0.7)

# geom = "density_ridges_gradient"
# ecdf: (empirical cumulative distribution function) 경험적 누적 분포 함수.그라디언트 컬러의 적용 기준
# calc_ecdf = TRUE: stat_density_ridges() 함수가 경험적 누적 분포 함수(empirical cumulative distribution function, ecdf)를 
# 계산하고, ecdf 변수와 quantile 변수를 반환

# 7-9) stat 적용한 그래프 그리기 : color를 입히기 위해
ggplot(patients_for_graph, aes(x=Patient_Height, y=AJCC_Pub_Version, 
                               fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, 
                      quantile_lines = TRUE, quantiles = 4, scale = 1)

ggplot(patients_for_graph, aes(x = Patient_Height, y = AJCC_Pub_Version, 
                               fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, 
                      quantile_lines = TRUE, quantiles = c(0.025, 0.975), scale = 1)

# 7-10) 직접 color 입히기
ggplot(patients_for_graph, aes(x = Patient_Height, y = AJCC_Pub_Version, 
                               fill = factor(stat(quantile)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, 
                      quantile_lines = TRUE, quantiles = c(0.025, 0.975), scale = 1)+
  scale_fill_manual(name = "Probability", values = c("red", "grey", "blue"),
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]"))


# 7-11) median에 가까운 정도에 대해 color 입히기
ggplot(patients_for_graph, aes(x = Patient_Height, y = AJCC_Pub_Version, 
                               fill = 0.5 - abs(0.5 - stat(ecdf))))+
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, scale = 1)

# viridis : direction 1, -1만 가능
ggplot(patients_for_graph, aes(x = Patient_Height, y = AJCC_Pub_Version, 
                               fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, scale = 1) +
  scale_fill_viridis_c(name = "Tail probability", direction = 1)

ggplot(patients_for_graph, aes(x = Patient_Height, y = AJCC_Pub_Version, 
                               fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, scale = 1) +
  scale_fill_viridis_c(name = "Tail probability", direction = -1)


# 7-12) expand : 여백조절
ggplot(patients_for_graph, aes(x = Patient_Height, y = AJCC_Pub_Version, 
                               fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, 
                      scale = 1) +
  scale_fill_viridis_c(name = "Tail probability", direction = -1)+
  scale_x_continuous(expand = c(0.5, 0.5)) + # 좌우여백
  scale_y_discrete(expand = expand_scale(mult = 0.1)) # 값을 몇 배해서 보여줄건지


# 7-13) ridge theme 적용, grid 선들 제거
ggplot(patients_for_graph, aes(x = Patient_Height, y = AJCC_Pub_Version, 
                               fill = 0.5 - abs(0.5 - stat(ecdf))))+
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, 
                      scale = 1)+
  scale_fill_viridis_c(name = "Tail probability", direction = -1)+
  scale_x_continuous(expand = c(0.1, 0.1))+
  scale_y_discrete(expand = expand_scale(mult = 0.15))+
  theme_ridges()+
  theme(panel.grid.major.y = element_blank(),
        axis.title.y = element_text(hjust = 0.5))

# Quiz 4
# 위의 그래프에서
# 1) 제목, 부제목 넣기
#    제목: Density plots 
#    부제목: Weight density plots by publication versions
# 2) x축, y축 이름이 축 가운데에 위치하도록 조정하기. bold하게 넣기
# 3) y축 이름이 y축과 약간 떨어지게 조정하기
ggplot(patients_for_graph, aes(x=Patient_Weight, y=AJCC_Pub_Version, 
                               color=AJCC_Pub_Version, 
                               fill=AJCC_Pub_Version))+
  geom_density_ridges(alpha=0.4, jittered_points=TRUE, quantile_lines = TRUE)+
  theme(legend.title = element_blank())+
  xlab("Weight") + ylab("Publication Version")+
  theme_ridges()+
  labs(title = 'Density plots',
       subtitle = 'Weight density plots by publication versions')+
  theme(legend.title = element_blank(),
        axis.title.y = element_text(hjust = 0.5, face='bold', margin = margin(r = 14)),
        axis.title.x = element_text(hjust = 0.5, face='bold'))
  
rm(patients_for_graph)

######### 8. patchwork
install.packages("patchwork")
library(patchwork)

# plot1 : basic
p1 <- ggplot(patients, aes(x=Diagnosis_Age, y=OS_Months)) + 
  geom_point() +
  xlab("Diagnosis age") + ylab("Overall survival months")  
p1

# plot2 : stat_smooth(loess)
p2 <- ggplot(patients, aes(x=Diagnosis_Age, y=OS_Months)) + 
  geom_point() + 
  stat_smooth()+
  xlab("Diagnosis age") + ylab("Overall survival months") 
p2

# plot3 : stat_smooth(lm)
p3 <- ggplot(patients, aes(x=Diagnosis_Age, y=OS_Months)) + 
  geom_point() + 
  stat_smooth(method='lm')+
  xlab("Diagnosis age") + ylab("Overall survival months")
p3


# 8-1) plot 합치기 : 열로 만들기
p1 + p2
p1 - p2
p1 * p2
p1 + p2 + p3
p1 | p2 | p3

# 8-2) plot 합치기
p1 / p2
p1 / p2 / p3

# 8-3) 연산자 적용 확인
p1 + p2 / p3
p1 * p2 / p3
p1 / (p2+p3)
p1 - p2 * p3

p1/p2*p3 
p1/p2+p3
p1/p2|p3

p <- p1/p2
p+p3
p


# 8-4) plot annotation
plot <- p1 / (p2+p3)
plot + plot_annotation(
  title = 'Survival months and age',
  caption = 'made with patchwork',
  theme = theme(plot.title = element_text(size = 16)))
  

# 8-5) 논문에 넣을 때의 ABC 번호 붙이기
plot + plot_annotation(
  title = 'Survival months and age',
  caption = 'made with patchwork',
  theme = theme(plot.title = element_text(size = 16)),
  tag_levels = 'A')
  

# Quiz
# plot1 : histogram
plot1 <- ggplot(data = patients)+
  geom_density(mapping = aes(x = Diagnosis_Age, y = ..count.., fill = Sex), position = 'stack')+
  scale_fill_manual(values = c("red", "blue"))+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")

plot1

# plot2 : scatter plot
plot2 <- ggplot(data = patients)+
  geom_point(mapping = aes(x = Diagnosis_Age, y = BMI, color = Sex))+
  scale_color_manual(values = c("red", "blue"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(t = 10, unit = 'pt'), hjust = 0.7),
        axis.title.y = element_text(margin = margin(r = 5, unit = 'pt'), hjust = 0.7))

plot2

# plot3 : boxplot
plot3 <- ggplot(data = patients)+
  geom_boxplot(mapping = aes(x = Sex, y = BMI, fill = Sex))+
  scale_fill_manual(values = c("red", "blue"))+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

plot3

plot <- plot1+guide_area()+plot2+plot3
plot <- plot + 
  plot_layout(ncol = 2, widths = c(3, 1), heights = c(1, 3), guides = 'collect')
plot

