library(openxlsx)
library(dplyr)
library(stringr)
library(ggplot2)

###################################
# RNA seq data
expr <- read.csv('./data/TCGA_BLCA_RNAexpression2.csv')
colnames(expr) <- str_replace_all(colnames(expr), "\\.", "-")
expr <- tibble::column_to_rownames(expr, var='X')

# Patient clinical data
patients <- read.xlsx('./data/TCGA_BLCA_SampleInfo.xlsx', sheet = 1)

# group1 = T2
group1 <- patients %>% filter(AJCC_Tumor_Stage_Code == 'T2')
group1 <- group1$Sample_ID

# group2 = T3
group2 <- patients %>% filter(AJCC_Tumor_Stage_Code == 'T3')
group2 <- group2$Sample_ID

# DEG 계산
expr$group1 <- apply(expr[group1], 1, sum)
expr$group2 <- apply(expr[group2], 1, sum)
expr <- expr %>% mutate(log2FC = log2(group2/group1))
expr$ttest <- apply(expr, 1, function(x){t.test(x[group1], x[group2])$p.value})

expr %>% filter(abs(log2FC) >= 2, ttest < 0.05) %>% nrow() # 136
expr %>% filter(abs(log2FC) >= 2, ttest < 0.05) %>% View()

###################################
# 1. Enhanced Volcano
install.packages('BiocManager')
BiocManager::install('EnhancedVolcano')
library(EnhancedVolcano)

# plot function

# 1-1) basic form
EnhancedVolcano(toptable = expr,
                lab = rownames(expr),
                x ='log2FC',
                y = 'ttest')

# 1-2) set cutoff
EnhancedVolcano(toptable = expr,
                lab = rownames(expr),
                x ='log2FC',
                y = 'ttest',
                pCutoff = 0.05,
                FCcutoff = 2)

# 1-3) set title
EnhancedVolcano(toptable = expr,
                lab = rownames(expr),
                x ='log2FC',
                y = 'ttest',
                pCutoff = 0.05,
                FCcutoff = 2,
                title = 'TCGA-BLCA : T2 vs T3',
                subtitle = "abs(log2FC) > 2, p-value < 0.05",
                ylim = c(0, 6))

# 1-4) set final
EnhancedVolcano(toptable = expr,
                lab = rownames(expr),
                x ='log2FC',
                y = 'ttest',
                pCutoff = 0.05,
                FCcutoff = 2,
                title = 'TCGA-BLCA : T2 vs T3',
                subtitle = "abs(log2FC) > 2, p-value < 0.05",
                ylim = c(0, 6),
                gridlines.minor = F,
                legendPosition = 'none')

###################################
# 2. Complex Heatmap
install.packages('devtools')
devtools::install_github("jokergoo/ComplexHeatmap")
devtools::install_github("jokergoo/circlize")
library(ComplexHeatmap)
library(circlize)

# DEG 추출하기
DEG <- expr %>% filter(abs(log2FC) >= 2, ttest < 0.05)
DEG <- DEG[order(DEG$ttest),]
DEG <- DEG[1:10,]
DEG_norm <- t(apply(DEG[1:72], 1, function(x){scale(x, center = T, scale = T)}))
colnames(DEG_norm) <- colnames(DEG)[1:72]


# 2-1) basic form
Heatmap(DEG_norm,
        col = colorRamp2(c(-1,0,1), c('blue','white','red')),
        
        # cell 채우기
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.1f", DEG_norm[i, j]), x, y, gp = gpar(fontsize = 7))
        })

# 2-2) 칸 테두리
Heatmap(DEG_norm,
        col = colorRamp2(c(-1,0,1), c('blue','white','red')),
        
        # cell 채우기
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.1f", DEG_norm[i, j]), x, y, gp = gpar(fontsize = 7))
        },
        
        # 칸 테두리 채워넣기
        rect_gp = gpar(col = "white", lwd = 2)
        )

# 2-3) clustering 1
Heatmap(DEG_norm,
        col = colorRamp2(c(-1,0,1), c('blue','white','red')),
        
        # cell 채우기
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.1f", DEG_norm[i, j]), x, y, gp = gpar(fontsize = 7))
        },
        
        rect_gp = gpar(col = "white", lwd = 2),
        
        ## clustering
        cluster_rows = F, # row
        show_column_dend = T # column
)

# 2-4) clustering 2 - column clustering 변경
Heatmap(DEG_norm,
        col = colorRamp2(c(-1,0,1), c('blue','white','red')),
        
        # cell 채우기
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.1f", DEG_norm[i, j]), x, y, gp = gpar(fontsize = 7))
        },
        
        rect_gp = gpar(col = "white", lwd = 2),
        
        ## clustering
        cluster_rows = F, # row
        show_column_dend = T, # column
        
        column_dend_side = "bottom",
        column_dend_height = unit(2, 'cm')
        )

# 2-5) clustering 3 - column clustering 변경
Heatmap(DEG_norm,
        col = colorRamp2(c(-1,0,1), c('blue','white','red')),
        
        # cell 채우기
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.1f", DEG_norm[i, j]), x, y, gp = gpar(fontsize = 7))
        },
        
        rect_gp = gpar(col = "white", lwd = 2),
        
        ## clustering
        cluster_rows = F, # row
        show_column_dend = T, # column
        
        column_dend_side = "top",
        column_dend_height = unit(2, 'cm'),
        
        clustering_distance_columns = "spearman", 
        column_km = 2 # k-means partitioning
        )

# 2-6) plot 수정하기
Heatmap(DEG_norm,
        col = colorRamp2(c(-1,0,1), c('blue','white','red')),
        
        # cell 채우기
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.1f", DEG_norm[i, j]), x, y, gp = gpar(fontsize = 7))
        },
        
        rect_gp = gpar(col = "white", lwd = 2),
        
        ## clustering
        cluster_rows = F, # row
        show_column_dend = T, # column
        
        column_dend_side = "top",
        column_dend_height = unit(2, 'cm'),
        
        clustering_distance_columns = "spearman", 
        column_km = 2, 
        
        ## plot 수정하기
        row_title = "p-value top10 DEGs",
        column_title = "TCGA-BLCA : T2 vs T3",
        column_title_gp = gpar(fontsize = 15, fontface = "bold"),
        name = " " # legend name
)

# 2-7) annotation 붙이기
# annotation function
heatmap_anno <- HeatmapAnnotation(bar = patients$AJCC_Tumor_Stage_Code,
                                  col = list(bar = c("T2" = "orange", "T3" = "skyblue")))

# heatmap function 
Heatmap(DEG_norm,
        col = colorRamp2(c(-1,0,1), c('blue','white','red')),
        
        # cell 채우기
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.1f", DEG_norm[i, j]), x, y, gp = gpar(fontsize = 7))
        },
        
        rect_gp = gpar(col = "white", lwd = 2),
        
        ## clustering
        cluster_rows = F, # row
        show_column_dend = T, # column
        
        column_dend_side = "top",
        column_dend_height = unit(2, 'cm'),
        
        clustering_distance_columns = "spearman", 
        column_km = 2,
        
        ## plot 수정하기
        row_title = "p-value top10 DEGs",
        column_title = "TCGA-BLCA : T2 vs T3",
        column_title_gp = gpar(fontsize = 15, fontface = "bold"),
        name = " ", 
        
        ## annotation
        bottom_annotation = heatmap_anno
)

###################################
# 3. Kaplan-meier
install.packages("survminer")
install.packages("survival")
library(survminer)
library(survival)

# data preprocessing
unique(patients$OS_Status) 
patients <- patients %>% mutate(OS_Status = as.integer(str_sub(OS_Status, 1, 1)))
unique(patients$OS_Status)

# 3-1) basic data format
# 생존 모델 1 : 시간에 따른 생존 확률 
fit1 <- survfit(Surv(OS_Months, OS_Status) ~ 1, data = patients)
ggsurvplot(fit1, data = patients)

# 생존 모델 2 : 각 성별의 시간에 따른 생존 확률
fit2 <- survfit(Surv(OS_Months, OS_Status) ~ AJCC_Tumor_Stage_Code, data = patients)
ggsurvplot(fit2, data = patients)


# 3-2) facet grid 수행 가능
ggsurvplot_facet(fit2, data = patients, facet.by = "Sex")


# 3-3) plot 수정하기
ggsurvplot(fit2, data = patients,
           size = 0.5,     
           palette = c("orange", "skyblue"),
           
           pval = TRUE,
           conf.int = TRUE # confidence interval 추가
)

# 3-4) plot 수정하기2
ggsurvplot(fit2, data = patients,
           size = 0.5,     
           palette = c("orange", "skyblue"),
           
           pval = TRUE,
           conf.int = TRUE,
           
           risk.table = TRUE,
           risk.table.height = 0.35,
           risk.table.col = "strata",# Risk table color by groups
           risk.table.y.text = FALSE # show bars instead of names in text annotations
)

# 3-5) plot 수정하기3
p <- ggsurvplot(fit2, data = patients,
                size = 0.5,                
                palette = c("orange", "skyblue"),
                
                pval = TRUE,
                conf.int = TRUE, 
                
                risk.table = TRUE,      
                risk.table.height = 0.35, 
                risk.table.col = "strata",
                risk.table.y.text = FALSE,
                
                xlab = "Time in months",  
                xlim = c(0,100),         
                break.time.by = 10,    
                
                legend.labs =  c("T2", "T3"),
                ggtheme = theme_bw()
)

p

# 3-6) plot 수정하기4
p <- ggsurvplot(fit2, data = patients,
                size = 0.5,               
                palette = c("orange", "skyblue"),
                
                pval = TRUE,
                conf.int = TRUE,
                
                risk.table = TRUE,      
                risk.table.height = 0.35, 
                risk.table.col = "strata",
                risk.table.y.text = FALSE,
                
                xlab = "Time in days",  
                xlim = c(0,100),         
                break.time.by = 10,    
                
                legend.labs =  c("T2", "T3"),
                ggtheme = theme_bw(), 
                
                fun = "event"
)

p

# 3-6) ggsurvplot 구성 
p$data.survplot
p$data.survtable
p$plot
p$table

p$plot <- p$plot + labs(
  title    = "Survival curves",
  subtitle = "TCGA-BLCA : T2 vs T3"
)

p

################################### Quiz 
colon <- survival::colon
colon_rec <- colon[which(colon$etype==1),]
colon_rec <- colon %>% filter(etype==1)

fit5 <- survfit(Surv(time, status) ~ sex, data = colon_rec)

p <- ggsurvplot(fit5, data = colon_rec,
                size = 0.5,     
                palette = c("darkgreen", "purple"),
                fun = "event",
                ylim = c(0, 1),
                xlim = c(0, 365.25*8),
                xlab = "time in days",
                
                legend.labs = c("Male","Female"),
                legend.title = "",
                risk.table = T,
                risk.table.col = "strata")

p

p$plot <- p$plot + labs(title = "Survival Curves",
                        subtitle = "event : recurrence",
                        y = "cumulative event")
p


# 3-7) save files
# 고화질 크기 설정한 저장 
png(filename="고화질이지롱.png", width=300, height=500, unit="px", bg="transparent")
p
dev.off() 

###################################
# 4. VennDiagram 
install.packages('VennDiagram')
library(VennDiagram)

# 4-1) basic form
grid.newpage()
draw.pairwise.venn(area1 = 25, area2 = 20, cross.area = 10, 
                   category = c("diagram A", "diagram B"))

# 4-2) 테두리 제거, color 변경, 투명도 조절
grid.newpage()
draw.pairwise.venn(area1 = 25, area2 = 20, cross.area = 10, 
                   category = c("diagram A", "diagram B"), 
                   lty = c("blank", "blank"), 
                   fill = c("skyblue", "pink"), 
                   alpha = c(0.5, 0.5))

# 4-3) category position
grid.newpage()
draw.pairwise.venn(area1 = 25, area2 = 20, cross.area = 10, 
                   category = c("diagram A", "diagram B"), 
                   lty = c("blank", "blank"), 
                   fill = c("skyblue", "pink"), 
                   alpha = c(0.5, 0.5), 
                   cat.pos = c(0,180))


# 4-4) cat.dist
grid.newpage()
draw.pairwise.venn(area1 = 25, area2 = 20, cross.area = 10, 
                   category = c("diagram A", "diagram B"), 
                   lty = c("blank", "blank"), 
                   fill = c("skyblue", "pink"), 
                   alpha = c(0.5, 0.5), 
                   cat.pos = c(0,180), 
                   cat.dist = c(0.1, -0.1))

# 4-5) 원 사이즈 고정
grid.newpage()
draw.pairwise.venn(area1 = 25, area2 = 20, cross.area = 10, 
                   category = c("bigger", "smaller"), 
                   lty = c("blank", "blank"), 
                   fill = c("skyblue", "pink"), 
                   alpha = c(0.5, 0.5),
                   cat.pos = c(0, 0),
                   scaled = FALSE)



# 4-6) triple venn diagram
grid.newpage()
draw.triple.venn(
  area1 = 1000, area2 = 500, area3 = 100,
  n12 = 30, n23 = 20, n13 = 15, n123 = 10,
  category = c('Set1', 'Set2', 'Set3')
)

# 4-7) plot 수정하기
grid.newpage()
draw.triple.venn(
  area1 = 1000, area2 = 500, area3 = 100,
  n12 = 30, n23 = 20, n13 = 15, n123 = 10,
  category = c('Set1', 'Set2', 'Set3'),
  fill = RColorBrewer::brewer.pal(3, 'Accent'),
  lty = 'blank'
)

# 4-8) 원 내부 수치, 카테고리 이름 텍스트 크기
grid.newpage()
draw.triple.venn(
  area1 = 1000, area2 = 500, area3 = 100,
  n12 = 30, n23 = 20, n13 = 15, n123 = 10,
  category = c('Set1', 'Set2', 'Set3'),
  fill = RColorBrewer::brewer.pal(3, 'Accent'),
  lty = 'blank',
  cex = rep(1.2, 7),
  cat.cex = rep(1.5, 3)
)


# Quiz 1 
# 위 그래프에제목 추가
# Title: Venn Diagrams (위치: 벤 다이어그램 위)
# 폰트 사이즈: 20
# 힌트: 새로운 library 설치 가능!

grid.newpage()
venn <- draw.triple.venn(
  area1 = 1000, area2 = 500, area3 = 100,
  n12 = 30, n23 = 20, n13 = 15, n123 = 10,
  category = c('Set1', 'Set2', 'Set3'),
  fill = RColorBrewer::brewer.pal(3, 'Accent'),
  lty = 'blank',
  cex = rep(1.2, 7),
  cat.cex = rep(1.5, 3)
)

install.packages("gridExtra")       
library("gridExtra") 
grid.arrange(gTree(children = venn), 
             top = textGrob("Venn Diagrams",gp=gpar(fontsize=20,font=3)))

# 숫자 지정 X
# 200 words 3 sets 생성
set1 <- paste(rep("word_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
set2 <- paste(rep("word_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
set3 <- paste(rep("word_" , 200) , sample(c(1:1000) , 200 , replace=F) , sep="")
list(set1, set2, set3)

# 
venn.plot <- venn.diagram(
  x = list(set1, set2, set3),
  category.names = c("Set 1", "Set 2", "Set 3"),
  filename = '#1_venn_diagramm.png',
  output = TRUE
)

#
venn.diagram(
  x = list(set1, set2, set3),
  category.names = c("Set 1" , "Set 2 " , "Set 3"),
  filename = '#2_venn_diagramm.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  compression = "lzw"
)

# R colorbrewer: palette of 3 colors
#install.packages("RColorBrewer")
library(RColorBrewer)
myCol <- brewer.pal(3, "Pastel2")

venn.diagram(
  x = list(set1, set2, set3),
  category.names = c("Set 1" , "Set 2 " , "Set 3"),
  filename = '#3_venn_diagramm.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = myCol
)

#
venn.diagram(
  x = list(set1, set2, set3),
  category.names = c("Set 1" , "Set 2 " , "Set 3"),
  filename = '#4_venn_diagramm.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans"
)

#
venn.diagram(
  x = list(set1, set2, set3),
  category.names = c("Set 1" , "Set 2 " , "Set 3"),
  filename = '#5_venn_diagramm.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1
) 

# Quiz 2
# 위의 벤다이어그램을 파일 생성 없이
# 바로 보게 할 것
# 단, 숫자와 카테고리 이름의 텍스트 크기를 0.9로 각각 조정할 것
grid.newpage()
venn.plot <- venn.diagram(
  x = list(set1, set2, set3),
  category.names = c("Set 1" , "Set 2 " , "Set 3"),
  filename = NULL,
  output=TRUE,
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .9,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.9,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1
) 

grid.draw(venn.plot)

