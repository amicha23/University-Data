# Importing of necessary libraries
library("jsonlite")
library("openxlsx")
library("dplyr")
library("plotly")

# Loading in necessary dataframes
df1 <- read.xlsx("data/IPEDS_data.xlsx")
df2 <- data.frame(fromJSON(txt = "data/schoolInfo.json"))
df3 <- read.csv("data/Most-Recent-Cohorts-All-Data-Elements.csv",
                stringsAsFactors = FALSE)

#combining all three dfs by the university name
#use inner join
inner_join_df <- inner_join(df3, df1, by = c("INSTNM" = "Name"))
#then use left join
universities_df <- left_join(inner_join_df, df2,
                             by = c("INSTNM" = "displayName"))


#create a file with a function that returns a table of summary information
#group_by states
#take avgerages of the summary table created by Aman
summary_table <- universities_df %>%
  select("INSTNM", "STABBR", "SAT_AVG_ALL", "act.avg", "hs.gpa.avg",
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
  "overallRank", "acceptance.rate")


# Making an aggregated dataframe based on state that includes all of the above
# information.
avg_summary_table <- summary_table %>%
  group_by(STABBR, INSTNM) %>%
  summarise(
  act.avg = mean(act.avg, na.rm = TRUE),
  hs.gpa.avg = mean(hs.gpa.avg, na.rm = TRUE),
  percent_of_american_indian_alaskan_native =
  mean(Percent.of.total.enrollment.that.are.American.Indian.or.Alaska.Native,
       na.rm = TRUE),
    percent_of_asian = mean(Percent.of.total.enrollment.that.are.Asian,
                            na.rm = TRUE),
    percent_of_african_american =
    mean(Percent.of.total.enrollment.that.are.Black.or.African.American,
         na.rm = TRUE),
    percent_of_hispanic_or_latino =
    mean(`Percent.of.total.enrollment.that.are.Hispanic/Latino`,
         na.rm = TRUE),
    percent_of_white =
    mean(Percent.of.total.enrollment.that.are.White, na.rm = TRUE),
    percent_of_two_or_more_races =
    mean(Percent.of.total.enrollment.that.are.two.or.more.races,
         na.rm = TRUE),
    percent_of_race_unknown =
    mean(`Percent.of.total.enrollment.that.are.Race/ethnicity.unknown`,
         na.rm = TRUE),
    percent_of_nonresident_alien =
    mean(Percent.of.total.enrollment.that.are.Nonresident.Alien,
         na.rm = TRUE),
    percent_of_asian_native_pacific_islander =
    mean(
`Percent.of.total.enrollment.that.are.Asian/Native.Hawaiian/Pacific.Islander`,
          na.rm = TRUE),
    percent_of_women = mean(Percent.of.total.enrollment.that.are.women)
  ) %>%
  arrange(act.avg)
