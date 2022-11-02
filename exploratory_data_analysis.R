# LIBRARY

library(readr)
library(knitr)
library(car)
library(data.table)
library(plotrix)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(lubridate)

# IMPORT AND CLEANING DATA

model_data = data.table(read.csv("home_insurance.csv",
                        header = TRUE,sep = ",",na.strings = c("NA","NULL")))
model_data$CAMPAIGN_DESC <- NULL
model_data$i <- NULL
total = nrow(model_data)
str(model_data)

# DATA WRANGLING

status_groups <- model_data[, .(count=.N, percent=round((.N/total)*100, 2)), by = POL_STATUS]
status_groups <- status_groups[order(count, decreasing = TRUE)]
status_groups
pieLabels <- paste(status_groups$POL_STATUS,' ', status_groups$percent, '%')

# 
pie3D(status_groups$count,labels=pieLabels,explode=0.1, radius=0.8,height=0.1, col=rainbow(length(status_groups$POL_STATUS)),
      main="Pie Chart of Policy Status ")


status_groups[POL_STATUS != 'Lapsed', POL_STATUS:= "Non Resiliated"]
status_groups[POL_STATUS == 'Lapsed', POL_STATUS:= "Resiliated"]
status_groups <- status_groups[, .(count=sum(count)), by = POL_STATUS]
status_groups[,percent := round((count*100)/sum(count), 2)]
status_groups <- status_groups[order(count, decreasing = TRUE)]
status_groups
#pieLabels <- paste(status_groups$POL_STATUS,' ', status_groups$percent, '%')
#pie <- pie3D(status_groups$percent,labels=pieLabels,explode=0.1, radius=0.8,height=0.1, col=rainbow(length(status_groups$POL_STATUS)),
#             main="Pie Chart of Resiliation")

# EXPLORATORY DATA ANALYSIS
# ...... # Policy best covered

model_data$Resiliated[model_data$POL_STATUS == 'Lapsed'] <- 1
model_data$Resiliated[model_data$POL_STATUS != 'Lapsed'] <- 0 

dt <- model_data[, .(SUM_INSURED_BUILDINGS, SUM_INSURED_CONTENTS, SPEC_SUM_INSURED)]
dt
name <- 'total_coverage'
model_data[, (name):= SUM_INSURED_BUILDINGS+ SUM_INSURED_CONTENTS+ SPEC_SUM_INSURED]
model_data$total_coverage
ordered_table <- model_data[order(total_coverage, decreasing = TRUE),
                            .(Police, SUM_INSURED_BUILDINGS, SUM_INSURED_CONTENTS,
                              SPEC_SUM_INSURED, total_coverage)]

ordered_table <- data.table(ordered_table)

# CLIENT PROFESSIONAL STATUS

status_client <- model_data[!is.na(P1_EMP_STATUS), .(count=.N), by = P1_EMP_STATUS]
status_client <- status_client[order(count, decreasing = TRUE)]
status_client

palette_colors <-  c("#5e482c", "#f7d2a7", "#df45a4", "#d1223d", "#fbdb50",
                     "#cd500d", "#d5addf", "#206536", "#b98d9b", "#ebaa7b", "#85a664", "#ef99fa")
ordered_status <- status_client$P1_EMP_STATUS

# BARPLOT

#ggbarplot(status_client, x= "P1_EMP_STATUS", y= "count", xlab="Client's professional status",
#          ylab ="quantity policies", fill = "P1_EMP_STATUS", label=TRUE, title = "Clients professional status",
#         label.pos = "out", order = ordered_status, palette = palette_colors)

