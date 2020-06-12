# Importing necessary libraries.
library("jsonlite")
library("openxlsx")
library("dplyr")

# Loading in necessary dataframes.
df1 <- read.xlsx("data/IPEDS_data.xlsx")
df2 <- data.frame(fromJSON(txt = "data/schoolInfo.json"))
df3 <- read.csv("data/Most-Recent-Cohorts-All-Data-Elements.csv",
                stringsAsFactors = FALSE)

# Joining data based on the name of the university.
join_results1 <- inner_join(df3, df1, by = c("INSTNM" = "Name"))
result_df <- left_join(join_results1, df2, by = c("INSTNM" = "displayName"))

# Creating a summary of the data based on name, academic performance, and
# ethnic diversity.
summary_info <- result_df %>%
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
         "acceptance.rate",
         "tuition",
         "STABBR")

# Summarizing the data based on the acadmic ratings and tuition.
summarized_info <- summary_info %>%
  summarize(
    total_universities = nrow(summary_info),
    ave_tuition = mean(tuition, na.rm = TRUE),
    ave_gpa = mean(hs.gpa.avg, na.rm = TRUE),
    ave_SAT_score = mean(as.numeric(SAT_AVG_ALL), na.rm = TRUE),
    ave_ACT_score = mean(act.avg, na.rm = TRUE),
    ave_acceptance_rate = mean(acceptance.rate, na.rm = TRUE),
    ave_ranking = mean(overallRank, na.rm = TRUE),
  )

# Getting a table with the school with the highest acceptance rate.
highest_accpetance_school <- summary_info %>%
  filter(acceptance.rate == max(acceptance.rate, na.rm = TRUE))
