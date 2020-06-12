# Loading in necessary libraries.
library("jsonlite")
library("openxlsx")
library("dplyr")
library("plotly")

# Loading in the necessary dataframes.
df1 <- read.xlsx("data/IPEDS_data.xlsx")
df2 <- data.frame(fromJSON(txt = "data/schoolInfo.json"))
df3 <- read.csv("data/Most-Recent-Cohorts-All-Data-Elements.csv",
                stringsAsFactors = FALSE
)

table_join <- right_join(df1, df3, by = c("Name" = "INSTNM"))
table_join2 <- left_join(
  table_join,
  df2,
  by = c("Tuition.and.fees,.2013-14" = "tuition"))


# Table with tution averages per state.
tuition_table <- table_join2 %>%
  select(Name = "Name",
         State = "State.abbreviation",
         Tuition = "Tuition.and.fees,.2013-14") %>%
  group_by(State) %>%
  summarize(Tuition = mean(Tuition, na.rm = T)) %>%
  arrange(-Tuition)



# Produce scatter plot of average tuition of all universities
# in each state.
scatter_plot <- function(df) {
  plot_ly(
    data = df,
    x = ~Tuition,
    y = ~State,
    color = ~Tuition,
    type = "scatter"
  ) %>%
    layout(
      title = "Average Tuition of all Universities per State",
      xaxis = list(
        title = "Average Tuition"
      ),
      yaxis = list(
        title = "",
        showticklabels = F,
        showgrid = F
      ),
      showlegend = F,
      legend = list(
        title = list(text = "<b> Trend </b>")
      )
    )
}

# Calling the chart method to produce a chart for the data.
chart_one <- scatter_plot(tuition_table)