# Loading in necessary libraries.
library("jsonlite")
library("openxlsx")
library("dplyr")
library("plotly")

# Loading in necessary dataframes.
df1 <- read.xlsx("data/IPEDS_data.xlsx")
df2 <- data.frame(fromJSON(txt = "data/schoolInfo.json"))
df3 <- read.csv("data/Most-Recent-Cohorts-All-Data-Elements.csv",
                stringsAsFactors = FALSE)

# Joining the dataframes based on the school's name.
join_result <- inner_join(df3, df1, by = c("INSTNM" = "Name"))
join_result2 <- left_join(join_result, df2, by = c("INSTNM" = "displayName"))

# Creating a summary table based on academic performance, institution name,
# and ethnic diversity.
summary_table <- join_result2 %>%
  select("INSTNM", "sat.avg", "act.avg", "hs.gpa.avg",
         "State.abbreviation",
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

# Grouping the data by the state's abbreviation.
test <- summary_table %>%
  group_by(State.abbreviation) %>%
  summarize(mean_SAT = mean(sat.avg, na.rm = TRUE),
            mean_ACT = mean(act.avg, na.rm = TRUE),
            mean_hs_gpa = mean(hs.gpa.avg, na.rm = TRUE),
            mean_acceptance_rate = mean(acceptance.rate, na.rm = TRUE),
            mean_ranking = mean(overallRank, na.rm = TRUE))

# Getting graphs based on mean academic performance.
mean_sat <- plot_ly(data = test,
                    x = ~State.abbreviation,
                    y = ~mean_SAT,
                    type = "bar"
) %>%
  layout(
    title = "Mean SAT score for universities in each state",
    xaxis = list(title = "State"),
    yaxis = list(title = "Mean SAT")
  )

mean_act <- plot_ly(data = test,
        x = ~State.abbreviation,
        y = ~mean_ACT,
        type = "bar"
)  %>%
  layout(
    title = "Mean ACT score for universities in each state",
    xaxis = list(title = "State"),
    yaxis = list(title = "Mean ACT")
  )
mean_hs_gpa <- plot_ly(data = test,
        x = ~State.abbreviation,
        y = ~mean_hs_gpa,
        type = "bar"
)  %>%
  layout(
    title = "Mean High School GPA for universities in each state",
    xaxis = list(title = "State"),
    yaxis = list(title = "Mean High School GPA")
  )
mean_acceptance_rate <- plot_ly(data = test,
        x = ~State.abbreviation,
        y = ~mean_acceptance_rate,
        type = "bar"
)   %>%
  layout(
    title = "Mean Acceptance for universities in each state",
    xaxis = list(title = "State"),
    yaxis = list(title = "Mean Acceptance Rate GPA")
  )
mean_ranking <- plot_ly(data = test,
        x = ~State.abbreviation,
        y = ~mean_ranking,
        type = "bar"
) %>%
  layout(
    title = "Mean Ranking for universities in each state",
    xaxis = list(title = "State"),
    yaxis = list(title = "Mean Ranking")
  )
