# IST 687 Project
# Gregory Richardson
# What Makes a Good School District?

# Part 1 - Data

# For this project, I am using the district report card data freely
# available online.  I am using the most recent available dats
# for analysis.  The first step is bringing the data into R and cleaning 
# it up.  The data file is an Excel spreadsheet with data across 10 tabs.
# In addition, there is a lot of extraneous data that must be removed as
# well as duplicate data.

# The variable 'xl2018' stores the link to the dataset.
xl2018 <- "C:/Users/Greg/Documents/School/IST 687/2018 data.xlsx"
file.exists(xl2018)

# The data is spread over 10 tabs in the Excel file.  I created a variable
# 'tab_nums' with a vector of the relevant tabs.  I then used the 
# lapply() function to bring the sheets into R as a list called 'tab_nums'.
tab_nums <- c(2,3,4,5,6,7,8,9,10,11)
tab_list <- lapply(tab_nums, function (x) readxl::read_xlsx(xl2018, 
                                                            sheet=x))

# I then sought to merge the 10 lists into a single dataframe that could be
# easily worked.  There were 5 columns that serve as ID columns across the
# different lists: ReportCardYear, DistrictNm, SchoolNm, SCHOOLID, and 
# SCHOOLTYPECD.  I created the dataframe 'df2018' by merging the first 2
# lists using these 5 ID variables.  I discovered later that in data was
# duplicated in other sheets, creating a mess out of the dataframe.  To 
# restrict the amount of duplicate and garbage data, I limited the merge
# to all rows on the first list (the accurate list of all districts) and
# joined all other sheets to that one on only rows that matched the correct
# ID variables.

df2018 <- data.frame(merge(x=tab_list[[1]], y=tab_list[[2]], 
                           by=c("ReportCardYear", "DistrictNm", 
                                "SchoolNm","SCHOOLID", "SCHOOLTYPECD"), 
                           all.x=TRUE))

# To then add the other 8 lists into the dataframe, I used a for loop which
# loops over each list, merging it to the dataframe using the same logic as
# above.
for (i in 3:10) {
  df2018 <- merge(x=df2018, y=tab_list[[i]], by=c("ReportCardYear", 
                  "DistrictNm", "SchoolNm", "SCHOOLID", "SCHOOLTYPECD"), 
                  all.x=TRUE)
}

# The newly created dataframe housed data for all districts, as well as 
# data for every individual school in the state.  Since I am only looking
# at the data at a District level, I removed all rows that were not for
# school districts.  

df2018 <- df2018[df2018$SCHOOLTYPECD == "D",]

# There are 179 variables in the original dataset.  I also know that there 
# are 86 school districts in the state.  At this point, my dataframe showed
# 93 obs x 179 vars.  So obviously I had a few duplicate districts from
# somewhere in the data.  I reviewed these duplicates and removed them to
# get a fully clean dataset

rownames(df2018) <- NULL

df2018 <- df2018[-c(13,18,42,58,61,67,87),]
rownames(df2018) <- NULL


# There is a second file with additional datapoints that I needed to add
# to my dataset.  The second file location is stored in xl2018b, and the
# appropriate pages are stored, brought in, and merged to the set as above.
xl2018b <- "C:/Users/Greg/Documents/School/IST 687/2018 data 1.xlsx"
file.exists(xl2018b)
tab_nums_b <- c(4,5,6,7,9)
tab_list_b <- lapply(tab_nums_b, function (x) readxl::read_xlsx(xl2018b, 
                                                                sheet=x))

for (i in 1:5) {
  df2018 <- merge(x=df2018, y=tab_list_b[[i]], 
      by=c("ReportCardYear", "DistrictNm", "SchoolNm", "SCHOOLID", 
           "SCHOOLTYPECD"), all.x=TRUE)
}


# Removing columns that are not necessary for the data analysis

df2018 <- df2018[,-c(1,3,5:8,10,12:16,22:24,31,33,50:52,59:61,64:90,92,94,
  96,98,100,102,104:108,110:112,126,128,130,132,134,137:141,143,169,171,
  173,175,177,179,181:185,187:191,194:198,200:204,207:211,213:217,220:223,
  225:228,230:371,373:377,379:383,386:390,392:396,399:403,405:409,412:416,
  418:422,430:431,433:452)]

# Need to fix some missing data for Zip codes in order to get a good map of
# the scores.  This section is for manually fixing the data using zip codes
# found online for the districts without.

df2018[20, "zip"] <- 29162
df2018[23, "zip"] <- 29488
df2018[37, "zip"] <- 29692
df2018[38, "zip"] <- 29666
df2018[39, "zip"] <- 29918
df2018[46, "zip"] <- 29325
df2018[77, "zip"] <- 29150

str(df2018)
summary(df2018)

# Removing school districts with not enough information to remain in the 
# database.

df2018 <- df2018[-c(59,63:65),]

# Quick analysis of Classification Data.  Summary and Histograms on size of
# district and # of Teachers

class <- c("Enrollment", "TeacherCount")
summary(df2018[,class])

hist(df2018$Enrollment, breaks=50, main="Histogram of # of Students")
hist(df2018$TeacherCount, breaks=30, main="Histogram of # of Teachers")

# Quick analysis of select Indicator variables.  

indices <- c("PctOnTrackELA", "PctOnTrackMATH", "DropoutRateCurrYr", 
  "ACT_Avg_CompositeScore", "ACT_Pct_BENCHMARK_All4", 
  "SAT_Avg_CompositeScore", "AP_PctPass", "Scholarships_PctSnrEligCurrYr", 
  "COLLEGE_PctEnrolledCurrYr", "PctCollORCareer")

str(df2018, list.len=200)
summary(df2018[,indices])

# I wanted to look at the early readiness indicators, including looking at
# the readiness in 2nd Grade vs. ACT scores in the same subject - Math in
# this case.

hist(df2018$PctOnTrackELA, main= "% of 2nd Graders On Track in ELA" )

plot(df2018$PctOnTrackMATH, df2018$ACT_Avg_MathScore, pch=16, 
     main= "Plot of ACT Math Scores vs. % 2nd Graders On Track", 
     xlab = "% 2nd Graders on Track in Math", ylab = "Avg ACT Math Score")

# A group of the predictor variables to be used for analysis, and some 
# quick looks at their data.
drivers <- c("KRA_PctDemRdy", "RETAINED_PctCurrYr", "PrincSuperYrs_CurrYr",
  "ChronicAbsenteeismRate", "TCH_SatWithLearningEnvironPct", 
  "STU_SatWithLearningEnvironPct", "PAR_SatWithLearningEnvironPct", 
  "TCH_SatWithSocialPhysEnvironPct", "STU_SatWithSocialPhysEnvironPct", 
  "PAR_SatWithSocialPhysEnvironPct", "TCH_SatWithSchlHomeRelationPct", 
  "STU_SatWithSchlHomeRelationPct", "PAR_SatWithSchlHomeRelationPct", 
  "TCHADVDEGREE_PctCurrYr", "TCHSALARY_AvgCurrYr.x", 
  "StudentsinPoverty_PctCurrYr", "DollarsPerPupil_CurrYr")

summary(df2018[,drivers])

require("ggplot2")

# Using the zipcode database, I merged the geographical information for
# each district into the df2018 dataframe.
library(zipcode)
data("zipcode")
df2018 <- merge(x=df2018, y=zipcode, by.x="zip", by.y="zip", all.x=TRUE)
df2018$state <- tolower(df2018$state)
df2018$region <- "south carolina"

sc <- map_data("state", region = "south carolina")

pmap <- ggplot(sc, aes(map_id=region))
pmap <- pmap + geom_map(map=sc, color="black", fill="white")
pmap <- pmap + expand_limits(x=sc$long, y=sc$lat)
pmap <- pmap + coord_map()

# In order to get the points to plot right, I had to add a dummy column to
# the database called 'region'.  I then plotted the lat/longs for each 
# school district, with their size representing the number of students and
# the color representing their District Score.

pmap <- pmap + geom_point(data=df2018, aes(x=longitude, y=latitude, 
                      color=StudentsinPoverty_PctCurrYr, size=Enrollment))
pmap <- pmap + scale_color_gradientn(colors=
            c("green", "yellow", "orange", "red"), name="% Poverty Level")
pmap <- pmap + scale_size_area(max_size = 20)
pmap <- pmap + ggtitle("% of Students in Poverty in South Carolina")
pmap

plot(df2018$TCHADVDEGREE_PctCurrYr, df2018$TCHSALARY_AvgCurrYr.x)
plot(df2018$TCHSALARY_AvgCurrYr.x, df2018$TCHRETURN3yrAvg_PctCurrYr,
     main="Average Teacher Salary vs. Teacher Retention", 
     xlab = "Average Teacher Salary", 
     ylab="Average Teacher Retention (3-Year Average)")

# -------------------------------------------
# Part 2 - Defining Success

# In order to define success, I narrowed down variables into a list of 
# performance metrics I deemed would reflect a district's success.  I then
# passed the list to several educators as well as our local school
# superintendent for their feedback on which metrics they deemed important
# and which ones less so.  I aggregated their feedback and came up with a 
# basket of 20 metrics that were deemed important for measuring a 
# district's success.  I then weighted these variables based on feedback
# and created a composite score, out of 100, for each school district. This
# score is recorded in a new variable in the dataframe called 'compScore'.

# Initializing variables for Total pts and Score.

df2018$compTotal <- 0
df2018$compScore <- 0

# 2 functions created to help create the composite scores.  The first adds
# the total number of points possible (addTotal).  The second, addScore, 
# adds the total earned points.  Some of the data has NA's.  In some cases
# this is because the metric is not relevant to a particular school
# district (ie - the Governor's School is only for 11/12 grades and so has
# has no result for 2nd grade success.)  These NA's are ignored, reducing
# the total possible points for that district and adding 0 points to the 
# actual score

addTotal <- function (var1, pts) {
  newVal <- ifelse(is.na(var1), df2018$compTotal, df2018$compTotal + pts)
  return(newVal)
}

addScore <- function (var1, pts) {
  newScore <- ifelse(is.na(var1), df2018$compScore,
                     df2018$compScore + var1*pts)
  return(newScore)
}

# The first 2 metrics deemed important was the % of Second Graders on track
# for 3rd grade ELA and Math.  These variables were important, but less 
# critical and so were assigned an importance of 2% each.

df2018$compTotal <- addTotal(df2018$PctOnTrackELA, 2)
df2018$compScore <- addScore(df2018$PctOnTrackELA, .02)

df2018$compTotal <- addTotal(df2018$PctOnTrackMATH, 2)
df2018$compScore <- addScore(df2018$PctOnTrackMATH, 0.02)

# The next variable is Dropout Rate.  This variable was assigned a 2% 
# significance as well based on feedback.  Dropout rates are the percent of
# students who did not come back/finish school.  Thus, a higher % would be
# worse.  Before we add the score, we transform the metric to the inverse -
# a retention rate which is equal to 100% - the dropout rate.  This metric
# is stored in 'retention' variable.

df2018$retention <- 100 - df2018$DropoutRateCurrYr
df2018$compTotal <- addTotal(df2018$retention, 2)
df2018$compScore <- addScore(df2018$retention, 0.02)

# The next 5 metrics are the percent of students meeting college-ready
# ACT benchmarks in English, Reading, Math, Science, and in all 4.  Each of
# the individual subjects are assigned a 3% point value, with the combined
# metric given a value of 2%.

df2018$compTotal <- addTotal(df2018$ACT_Pct_BENCHMARK_English,3)
df2018$compTotal <- addTotal(df2018$ACT_Pct_BENCHMARK_Math, 3)
df2018$compTotal <- addTotal(df2018$ACT_Pct_BENCHMARK_Reading, 3)
df2018$compTotal <- addTotal(df2018$ACT_Pct_BENCHMARK_Sci, 3)
df2018$compTotal <- addTotal(df2018$ACT_Pct_BENCHMARK_All4, 2)

df2018$compScore <- addScore(df2018$ACT_Pct_BENCHMARK_English, .03)
df2018$compScore <- addScore(df2018$ACT_Pct_BENCHMARK_Math, .03)
df2018$compScore <- addScore(df2018$ACT_Pct_BENCHMARK_Reading, .03)
df2018$compScore <- addScore(df2018$ACT_Pct_BENCHMARK_Sci, .03)
df2018$compScore <- addScore(df2018$ACT_Pct_BENCHMARK_All4, .02)

# The next 5 metrics are the average ACT scores for each district in each 
# category - English, Reading, Math, and ELA.  Each score is weighted 5%.
# The scores are based on a 35-point scale, so each value is divided by 35
# to create a percentage which is then applied to the composite score.

df2018$actEnglish <- df2018$ACT_Avg_EnglishScore / 35 * 100
df2018$actMath <- df2018$ACT_Avg_MathScore / 35 * 100
df2018$actReading <- df2018$ACT_Avg_ReadingScore / 35 * 100
df2018$actScience <- df2018$ACT_Avg_ScienceScore / 35 * 100

df2018$compTotal <- addTotal(df2018$actEnglish, 5)
df2018$compTotal <- addTotal(df2018$actMath, 5)
df2018$compTotal <- addTotal(df2018$actReading, 5)
df2018$compTotal <- addTotal(df2018$actScience, 5)

df2018$compScore <- addScore(df2018$actEnglish, .05)
df2018$compScore <- addScore(df2018$actMath, .05)
df2018$compScore <- addScore(df2018$actReading, .05)
df2018$compScore <- addScore(df2018$actScience, .05)

# The next variable is the average SAT composite score.  This variable has
# a 3% weighting.  Values here are out of a 1600 total score, so the 
# average is divided by 1600 - in a new variable called satAvgPct.

df2018$satAvgPct <- df2018$SAT_Avg_CompositeScore / 1600 * 100
df2018$compTotal <- addTotal(df2018$satAvgPct, 3)
df2018$compScore <- addScore(df2018$satAvgPct, 0.03)


# The next 2 metrics pertain to graduating seniors.  The first is the
# the percent of graduates eligible for the LIFE scholarship - an award
# given by the state to students meeting academic eligibility standards for
# traditional college and technical colleges.  The next is a newer metric
# measuring the percentage of graduates who were certified as being College
# OR career ready.  This metric will be invaluable, since college does not
# have to be the end-goal for a student in order to be successful.  However
# the metric is new and may need work, so it's weight is lowered 
# accordingly.  The 2 metrics are worth 10 and 7 pts respectively. 

df2018$compTotal <- addTotal(df2018$Scholarships_PctSnrEligCurrYr, 10)
df2018$compTotal <- addTotal(df2018$PctCollORCareer, 7)

df2018$compScore <- addScore(df2018$Scholarships_PctSnrEligCurrYr, 0.1)
df2018$compScore <- addScore(df2018$PctCollORCareer, 0.07)

# The following 4 metrics are Percent of students meeting or exceeding 
# standards for 4 subjects on a state standardized exam.  The 4 subjects:
# ELA, Math, Science, and Social Studies are each weighted 5 points.

df2018$compTotal <- addTotal(df2018$E_PctME, 5)
df2018$compTotal <- addTotal(df2018$M_PctME, 5)
df2018$compTotal <- addTotal(df2018$SC_PctME, 5)
df2018$compTotal <- addTotal(df2018$SO_PctME, 5)

df2018$compScore <- addScore(df2018$E_PctME, .05)
df2018$compScore <- addScore(df2018$M_PctME, .05)
df2018$compScore <- addScore(df2018$SC_PctME, .05)
df2018$compScore <- addScore(df2018$SO_PctME, .05)

# Finally, the last 20 points are distributed evenly across the pct of
# students scoring A,B, or C on the End of Course exam in 4 subjects: 
# English, Algebra, Biology, and US History.

df2018$compTotal <- addTotal(df2018$E_PctABC, 5)
df2018$compTotal <- addTotal(df2018$M_PctABC, 5)
df2018$compTotal <- addTotal(df2018$SC_PctABC, 5)
df2018$compTotal <- addTotal(df2018$H_PctABC, 5)

df2018$compScore <- addScore(df2018$E_PctABC, .05)
df2018$compScore <- addScore(df2018$M_PctME, .05)
df2018$compScore <- addScore(df2018$SC_PctABC, .05)
df2018$compScore <- addScore(df2018$H_PctABC, .05)

# To determine the scores, the variable compScore was divided by compTotal
# and the value scored in compGrade.

df2018$compGrade <- df2018$compScore / df2018$compTotal * 100 

# A sorted list of the districts by their scores, also showing total number
# of points - some districts had very few points, such as the Governer's 
# Schools, which only serve 11/12 grades and so much of the metrics do not
# apply.
head(df2018[order(-df2018$compGrade),
            c("DistrictNm", "compGrade", "compTotal")],10)
summary(df2018['compGrade'])


# Quick plots of the district scores verses poverty rates, with point size
# depicting the size of the school district.


gplot <- ggplot(df2018, aes(y=compGrade, x=StudentsinPoverty_PctCurrYr,
                            size=Enrollment))
gplot <- gplot + geom_point()
gplot <- gplot + ggtitle("District Score vs. Poverty Rate")
gplot <- gplot + labs(x = "% of Students in Poverty", y="District Score")
gplot

# Boxplot of the School District Scores
boxplot(df2018["compGrade"], ylab="District Score") 
title("Boxplot of School District Scores")

hist(df2018$compGrade, breaks=10, main = "Histogram of District Scores")

# To create a map of SC, I used the map_data library and then plotted it
# using gg_plot, with a white background and a black border.
dmap <- ggplot(sc, aes(map_id=region))
dmap <- dmap + geom_map(map=sc, color="black", fill="white")
dmap <- dmap + expand_limits(x=sc$long, y=sc$lat)
dmap <- dmap + coord_map()

# In order to get the points to plot right, I had to add a dummy column to
# the database called 'region'.  I then plotted the lat/longs for each 
# school district, with their size representing the number of students and
# the color representing their District Score.

dmap <- dmap + geom_point(data=df2018, aes(x=longitude, y=latitude, 
                            color=compGrade, size=Enrollment))
dmap <- dmap + scale_color_gradientn(colors=
              c("red", "orange", "yellow", "green"), name="District Score")
dmap <- dmap + scale_size_area(max_size = 20)
dmap <- dmap + ggtitle("School Districts in South Carolina")
dmap

plot(df2018$TCH_SatWithSchlHomeRelationPct, df2018$compGrade)

plot(df2018$DollarsPerPupil_CurrYr, df2018$compGrade, 
     xlab="Total $ Per Pupil", ylab="District Score")
df2018$InstructDollars <- df2018$DollarsPerPupil_CurrYr * 
                          df2018$ExpendforInstruct_PctCurrYr / 100
plot(df2018$InstructDollars, df2018$compGrade, 
      xlab="$ Per Student for Instruction", ylab="District Score")
# -------------------------------------------------
# Before investigating possible indicators for success, I am consolidating
# some smaller ones into larger bundles.  The first of which is grouping
# incidents of violence.  This includes number of incidents of assaults, 
# robberies, rapes, etc. and dividing by total enrollment, giving an 
# incident rate per 100 students. This variable is called 'violence'.

df2018$violence <- rowSums(df2018[80:92]) / df2018$Enrollment * 100

plot(df2018$violence, df2018$compGrade, 
    main="Violent Incidents vs District Score", 
    xlab="# of Violent Incidents/100 Students", ylab="District Score")

# Student.Teacher variable is number of students / number of teachers
df2018$student.teacher <- df2018$Enrollment / df2018$TeacherCount

plot(df2018$student.teacher, df2018$compGrade, 
     main="Student/Teacher Ratio vs. District Performance",
     xlab="Student:Teacher Ratio", ylab="District Score")

# List of initial variables to include in prediction models are here
vars <- c("compGrade","student.teacher", "KRA_PctDemRdy", "KRA_PctLangRDY",
  "KRA_PctMathRDY", "KRA_PctPhysRDY", "KRA_PctSocRDY", "RETAINED_PctCurrYr", 
  "violence", "PrincSuperYrs_CurrYr", "ChronicAbsenteeismRate",
  "TCH_SatWithLearningEnvironPct", "STU_SatWithLearningEnvironPct",
  "PAR_SatWithLearningEnvironPct", "TCH_SatWithSocialPhysEnvironPct",
  "STU_SatWithSocialPhysEnvironPct", "PAR_SatWithSocialPhysEnvironPct",
  "TCH_SatWithSchlHomeRelationPct", "STU_SatWithSchlHomeRelationPct", 
  "PAR_SatWithSchlHomeRelationPct", "TCHADVDEGREE_PctCurrYr", 
  "TCHATTENDRATE_CurrYr", "TCHSALARY_AvgCurrYr.x", 
  "TCHCONTINUECONTRACT_PctCurrYr", "TCHRETURN_PctCurrYr", 
  "TCHRETURN3yrAvg_PctCurrYr", "PAR_ChildFeelsSafe", "PAR_TchStopBully", 
  "TCH_IFeelSafe", "TCH_RulesEnforced", "OCR_FirearmYN",  
  "StudentsinPoverty_PctCurrYr",  "ExpendforInstruct_PctCurrYr", 
  "ExpendforTchSalaries_PctCurrYr", "AdminSalary_CurrYr", 
  "DollarsPerPupil_CurrYr", "PctCommitTOT", "PctCommitCog",
  "PctCommitEmot", "PctCommitBehav")

# I realized that 4 of the variables for prediction were not numeric, even
# though they ought to be.  I coerced the 4 into a numeric format for 
# analysis
df2018$PAR_ChildFeelsSafe <- as.numeric(df2018$PAR_ChildFeelsSafe)
df2018$PAR_TchStopBully <- as.numeric(df2018$PAR_TchStopBully)
df2018$TCH_IFeelSafe <- as.numeric(df2018$TCH_IFeelSafe)
df2018$TCH_RulesEnforced <- as.numeric(df2018$TCH_RulesEnforced)

# I ran a correlation matrix using the list of variables above.  I selected
# all variables which seemed to have high correlation with compGrade - 
# correlations of +/- 0.6 for further evaluation
cor(df2018[vars], df2018$compGrade, use= "complete.obs")

var1 <- c("compGrade", "TCH_SatWithLearningEnvironPct", 
  "PAR_SatWithLearningEnvironPct", "TCH_SatWithSocialPhysEnvironPct",
  "PAR_SatWithSocialPhysEnvironPct","TCH_SatWithSchlHomeRelationPct", 
  "STU_SatWithSchlHomeRelationPct", "PAR_SatWithSchlHomeRelationPct", 
  "TCHRETURN3yrAvg_PctCurrYr", "PAR_ChildFeelsSafe", 
  "StudentsinPoverty_PctCurrYr",  "ExpendforInstruct_PctCurrYr", 
  "ExpendforTchSalaries_PctCurrYr", "PctCommitTOT", "PctCommitCog", 
  "PctCommitEmot")

df.sub <- df2018[,var1]

lm1 <- lm(formula =  compGrade ~ .,data =  df.sub, na.action = na.exclude)

summary(lm1)

# From the first model, it became clear that there were really only 2 vars
# that were significant: Poverty, Teacher's School/Home satisfaction.  2 
# others, parent's satisfaction with Learning and Social environs were 
# fairly low so I selected them as well.  A second linear
# model was built using only these 4 variables.
lm2 <- lm(formula = compGrade ~ PAR_SatWithLearningEnvironPct + 
  PAR_SatWithSocialPhysEnvironPct + TCH_SatWithSchlHomeRelationPct + 
    StudentsinPoverty_PctCurrYr, data = df.sub, na.action = na.exclude)

summary(lm2)

plot(df2018$PAR_SatWithSocialPhysEnvironPct, df2018$compGrade, 
     xlab= "Parent Satisfaction", ylab = "District Score")

plot(df2018$PAR_SatWithSocialPhysEnvironPct, 
     df2018$PAR_SatWithLearningEnvironPct, 
     xlab="Social Enviro Satisfaction", 
     ylab="Learning Enviro Satisfaction")

# The second model was much better.  All vars were significant and the adj
# R^2 was good.  But I didn't like what the model seemed to be doing with
# the parent's response.  It seemed one of the variables was confounding 
# the model, switching to a negative coefficient and effectively cancelling
# out the other's response.  So I reduced the model to only 2 variables -
# Poverty and Teacher's Satisfaction with School/Home Relations.
lm3 <- lm(formula = compGrade ~ StudentsinPoverty_PctCurrYr + 
      TCH_SatWithSchlHomeRelationPct, data=df.sub, na.action = na.exclude)

summary(lm3)

# Scatter plot of the two variables with the district score defining size
# and color of the points.
lplot <- ggplot(df.sub, aes(y=StudentsinPoverty_PctCurrYr, 
      x=TCH_SatWithSchlHomeRelationPct, color=compGrade, size=compGrade))
lplot <- lplot + geom_point()
lplot <- lplot + scale_size(name="District Score") 
lplot <- lplot + scale_color_gradientn(colors = 
            c("red", "orange", "yellow", "green"), name="District Score") 
lplot <- lplot + labs(x="% Teachers Satisfied with School/Home Relation",
                      y="% of Students in Poverty")
lplot

# Though much lighter in variables than I'd anticipated, this model does an
# excellent job of predicting success.  The R^2 is essentially unchanged
# from the previous model, and all variables are significant.  I then
# used this model to predict the grade for all schools and attached that
# to the dataframe.
df2018$lmpred <- predict.lm(lm3, df2018)

df2018[,c("DistrictNm", "compGrade", "lmpred")]

# Correlation matrix of variables correlated with Teacher's Satisfaction
cor(df2018[,vars], df2018$TCH_SatWithSchlHomeRelationPct, 
    use = "complete.obs")

# -------------------------------------------------------
# Non-linear Model

df.vars <- df2018[,vars]

# Setting up testing and training data to use for SVM models.  The data
# frame uses all predictor variables as inputs.  District data is 
# randomly assigned, with 2/3 used for training and 1/3 for evaluation.
r.index <- sample(1:dim(df.vars)[1], replace = FALSE)
cut.pt <- round(length(r.index) * 2/3)
trainData <- df.vars[r.index[1:cut.pt],]
testData <- df.vars[r.index[(cut.pt+1):length(r.index)],]

# Using the E1071 SVM models.
require("e1071")

# The first model uses all variables available.
sv1 <- svm(compGrade ~ ., data=trainData, na.action = na.exclude)
summary(sv1)

# The model is run against the Test data and the data is appended to the 
# test data frame.
sv1Results <- predict(sv1, testData, type="votes", na.action=na.exclude)
testData$pred1 <- sv1Results

# A second model was created using the 4 key variables identified in the 
# linear model. Model is tested as above.
sv2 <- svm(compGrade ~ PAR_SatWithLearningEnvironPct + 
    PAR_SatWithSocialPhysEnvironPct + TCH_SatWithSchlHomeRelationPct + 
      StudentsinPoverty_PctCurrYr, data=trainData)
summary(sv2)


sv2Results <- predict(sv2, testData, na.action=na.exclude)
testData$pred2 <- sv2Results

# Finally, the predicted results from the final linear model are also 
# added to the test data so they can be compared.
testData$lmpred <- predict(lm3, testData, na.action=na.exclude)

testData[,c(1,41:43)]

# Root Mean Squared Error from all three models are compared.
sv1rmse <- sqrt(mean(testData$pred1^2, na.rm=TRUE))
sv2rmse <- sqrt(mean(testData$pred2^2, na.rm=TRUE))
lmrmse <- sqrt(mean(testData$lmpred^2, na.rm=TRUE))
cat("Linear Model RMSE: ", lmrmse, '\n', "1st SVM RMSE: ", 
    sv1rmse, '\n', "2nd SVM RMSE: ", sv2rmse)