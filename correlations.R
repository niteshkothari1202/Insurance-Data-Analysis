# CORRELATIONS
source('exploratory_data_analysis.R')

#ggscatter(model_data, x = "SUM_INSURED_BUILDINGS", y = "RISK_RATED_AREA_B", 
#          add = "reg.line", conf.int = TRUE, 
#          cor.coef = TRUE, cor.method = "pearson",
#          xlab = "Assured Sum - Building", ylab = "Geographical Classification of Risk - Building")

risk <- model_data[LAST_ANN_PREM_GROSS > 0]
#head(risk, 2)
#ggqqplot(model_data$LAST_ANN_PREM_GROSS, ylab = "Premium - Total for the previous year")




month_order <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
day_order <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
model_data$quotemonth_n <- month(as.POSIXlt(model_data$QUOTE_DATE, format="%m/%d/%Y"))
model_data$covermonth_n <- month(as.POSIXlt(model_data$COVER_START, format="%d/%m/%Y"))
#i name the columns

quotesmonthDF <- data.frame(month_n = model_data$quotemonth_n)
coversmonthDF <- data.frame(month_n = model_data$covermonth_n)

#head(quotesmonthDF, 2)
#i avoid the null values and make the group by each month to get the monthly total

quotesmonthgroup <- data.table(quotesmonthDF)
quotesmonthgroup <-quotesmonthgroup[month_n <= 12]
quotesmonthgroup <-quotesmonthgroup[(order(month_n)), .(count=.N), by=month_n]

coversmonthgroup <- data.table(coversmonthDF)
coversmonthgroup <-coversmonthgroup[month_n <= 12]
coversmonthgroup <-coversmonthgroup[(order(month_n)), .(count=.N), by=month_n]

quotesmonthgroup$month_s <- month_order[quotesmonthgroup$month_n ]
coversmonthgroup$month_s <- month_order[coversmonthgroup$month_n ]
head(quotesmonthgroup, 12)
head(coversmonthgroup, 12)

model_data$quotemonth_s <- month_order[model_data$quotemonth_n]
model_data$covermonth_s <- month_order[model_data$covermonth_n]
str(model_data)

# Quotes
#ggbarplot(quotesmonthgroup, x= "month_s" , y= "count", xlab="Month",
#          ylab ="quantity policies", label=TRUE, title = "Most successful months in Quotation date",
#          label.pos = "out", fill = "month_s", color = "month_s", palette = palette_colors)

# Cover
#ggbarplot(coversmonthgroup, x= "month_s" , y= "count", xlab="Month",
#          ylab ="quantity policies", fill = "month_s", palette = palette_colors,
#          label=TRUE, title = "Most successful months in Coverage date", label.pos = "out")

# boxplot

#boxplotDF <- model_data[covermonth_n <= 12, .(covermonth_n, covermonth_s, total_coverage)]
#boxplotDF
#ggboxplot(boxplotDF, x = "covermonth_s", y = "total_coverage", xlab="Coverage month",
#          ylab="total coverage amount", width = 0.8,
#          fill="covermonth_s", palette = palette_colors, order=month_order)

# Number of buildings by year

year_built <- model_data[model_data$YEARBUILT != 'null' & model_data$YEARBUILT > 0,
                         .(count=.N), by = YEARBUILT]
year_built <- year_built[order(YEARBUILT)]
year_built

#qplot(x = year_built$YEARBUILT, y = year_built$count, xlab="Year of building construction",
#      ylab="Number of Policies",main = "Number of buildings by year construction" , geom="line")


#set client age
birthday_year <- year(as.POSIXlt(model_data$P1_DOB, format="%d/%m/%Y"))
cover_year <- year(as.POSIXlt(model_data$COVER_START, format="%d/%m/%Y"))
model_data$client_age <- cover_year-birthday_year

cancelation_year <- year(as.POSIXlt(model_data$MTA_DATE, format="%d/%m/%Y"))
model_data$police_duration <- cancelation_year-cover_year


age_bar <- model_data[!is.na(client_age) & client_age > 0, .(count=.N), by = client_age]
age_bar <- age_bar[order(client_age)]
head(age_bar, 20)


years_table <- model_data[!is.na(police_duration) & police_duration > 0 & !is.na(client_age) & client_age > 0, .(client_age,police_duration)]
#ggdensity(years_table, x = "client_age", y = "..count..", xlab = "Age of client", ylab="Policies",
#          add = "mean", fill="#00AFBB", rug = TRUE, palette = c("#00AFBB", "#E7B800"))

#ggscatter(years_table, x = "client_age", y = "police_duration", 
#          add = "reg.line", conf.int = TRUE, 
#          cor.coef = TRUE, cor.method = "pearson",
#         xlab = "Client Age", ylab = "Policy duration")

