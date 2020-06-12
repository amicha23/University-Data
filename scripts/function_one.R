# Importing necessary libraries
library("jsonlite")
library("openxlsx")
library("dplyr")

# Loading necessary dataframes
df1 <- read.xlsx("data/IPEDS_data.xlsx")
df2 <- data.frame(fromJSON(txt = "data/schoolInfo.json"))
df3 <- read.csv("data/Most-Recent-Cohorts-All-Data-Elements.csv",
                stringsAsFactors = FALSE)

# Joining dataframes by the name of the university.
join_result <- inner_join(df3, df1, by = c("INSTNM" = "Name"))
join_result2 <- left_join(join_result, df2, by = c("INSTNM" = "displayName"))

# Selecting data that invludes the name, the average academic performance,
# and a percentage look at ethnic data.
summary_table <- join_result2 %>%
  select("INSTNM", "SAT_AVG_ALL", "act.avg", "hs.gpa.avg",
  "Percent.of.total.enrollment.that.are.American.Indian.or.Alaska.Native",
         "Percent.of.total.enrollment.that.are.Asian",
         "Percent.of.total.enrollment.that.are.Black.or.African.American",
         "Percent.of.total.enrollment.that.are.Hispanic/Latino",
         "Percent.of.total.enrollment.that.are.White",
         "Percent.of.total.enrollment.that.are.two.or.more.races",
         "Percent.of.total.enrollment.that.are.Race/ethnicity.unknown",
         "Percent.of.total.enrollment.that.are.Nonresident.Alien",
"Percent.of.total.enrollment.that.are.Asian/Native.Hawaiian/Pacific.Islander",
         "Percent.of.total.enrollment.that.are.women",
         "overallRank",
         "acceptance.rate")
