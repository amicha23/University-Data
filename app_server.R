# Load libraries so they are available
library("jsonlite")
library("openxlsx")
library("dplyr")
library(knitr)
library(leaflet)
library("shiny")
library("plotly")
library("ggplot2")
library("DT")
library("knitr")

# Load data tables so that they are available
df1 <- read.xlsx("data/IPEDS_data.xlsx")
df2 <- data.frame(fromJSON(txt = "data/schoolInfo.json"))
df3 <- read.csv("data/Most-Recent-Cohorts-All-Data-Elements.csv",
  stringsAsFactors = FALSE,
  na.strings = "NULL"
)

# Join data tables
join_result <- inner_join(df3, df1, by = c("INSTNM" = "Name"))
join_result2 <- left_join(join_result, df2,
  by = c("INSTNM" = "displayName")
)

# Introduce ethnic categories that we will be measuring
ethnicity_categories <- c(
  "American Indian / Alaska Native",
  "Asian",
  "African American / Black",
  "Hispanic / Latino",
  "White",
  "Two or more race",
  "Unknown Ethnicity",
  "Asian / Native Hawaiian / Pacific Islander"
)

# Get the server running
server <- shinyServer(function(input, output) {
  # render the introduction page
  output$introduction <- renderUI({
    HTML(markdown::markdownToHTML(knit("introduction.Rmd", quiet = TRUE)))
  })

  # Render the map page
  output$map <- renderLeaflet({
    return(draw_map(join_result2, input$state))
  })

  # Render the tuition scatter plot
  output$scatter_plot <- renderPlotly({
    return(draw_scatter(final_tuition_table, input$yaxis))
  })

  # Academics (GPA)
  output$gpa_graph <- renderPlotly({
    return(draw_bar(
      join_result2, input$academicInput,
      "hs.gpa.avg"
    ))
  })

  # Academics (SAT)
  output$sat_graph <- renderPlotly({
    return(draw_bar(
      join_result2, input$academicInput,
      "SAT_AVG_ALL"
    ))
  })

  # Academics (ACT)
  output$act_graph <- renderPlotly({
    return(draw_bar(
      join_result2, input$academicInput,
      "ACT.Composite.75th.percentile.score"
    ))
  })

  # Academics (Acceptance Rate)
  output$acceptance_rate_graph <- renderPlotly({
    return(draw_bar(
      join_result2, input$academicInput,
      "ADM_RATE"
    ))
  })

  # Academics (School Ranking)
  output$ranking_graph <- renderPlotly({
    return(draw_bar(
      join_result2, input$academicInput,
      "overallRank"
    ))
  })

  # Academics (Summary for academic data)
  output$summary <- renderText({
    return(text_summary(join_result2, input$academicInput))
  })

  # Render key takeaways from this project.
  output$takeaways <- renderUI({
    HTML(markdown::markdownToHTML(knit("takeaways.Rmd", quiet = TRUE)))
  })

  # uni summary table
  output$summarystates <- DT::renderDataTable({
    all_states_summary
  })

  # Render ethnic diversity charts
  output$pie_chart <- renderPlotly({
    data_chart <- join_result2 %>%
      group_by(STABBR) %>%
      summarize(
        ave_American_Indian =
          mean(
    Percent.of.total.enrollment.that.are.American.Indian.or.Alaska.Native,
            na.rm = TRUE
          ),
        ave_Asian = mean(Percent.of.total.enrollment.that.are.Asian,
          na.rm = TRUE
        ),
        ave_African_American =
          mean(
            Percent.of.total.enrollment.that.are.Black.or.African.American,
            na.rm = TRUE
          ),
        ave_Latino =
          mean(
            `Percent.of.total.enrollment.that.are.Hispanic/Latino`,
            na.rm = TRUE
          ),
        ave_White = mean(Percent.of.total.enrollment.that.are.White,
          na.rm = TRUE
        ),
        ave_more_race =
          mean(
            Percent.of.total.enrollment.that.are.two.or.more.races,
            na.rm = TRUE
          ),
        ave_unknown =
          mean(
            `Percent.of.total.enrollment.that.are.Race/ethnicity.unknown`,
            na.rm = TRUE
          ),
        ave_Islander =
          mean(
`Percent.of.total.enrollment.that.are.Asian/Native.Hawaiian/Pacific.Islander`,
            na.rm = TRUE
          )
      ) %>%
      filter(STABBR == input$diversityInput)
    return(draw_pie(data_chart, ethnicity_categories, input$diversityInput))
  })

  # Render summary for ethnic diversity
  output$ethnicity_summary <- renderText({
    summarized_pie <- join_result2 %>%
      group_by(STABBR) %>%
      summarize(
        ave_American_Indian =
          mean(
Percent.of.total.enrollment.that.are.American.Indian.or.Alaska.Native,
            na.rm = TRUE
          ),
        ave_Asian = mean(Percent.of.total.enrollment.that.are.Asian,
          na.rm = TRUE
        ),
        ave_African_American =
          mean(
            Percent.of.total.enrollment.that.are.Black.or.African.American,
            na.rm = TRUE
          ),
        ave_Latino =
          mean(
            `Percent.of.total.enrollment.that.are.Hispanic/Latino`,
            na.rm = TRUE
          ),
        ave_White = mean(Percent.of.total.enrollment.that.are.White,
          na.rm = TRUE
        ),
        ave_more_race =
          mean(
            Percent.of.total.enrollment.that.are.two.or.more.races,
            na.rm = TRUE
          ),
        ave_unknown =
          mean(
            `Percent.of.total.enrollment.that.are.Race/ethnicity.unknown`,
            na.rm = TRUE
          ),
        ave_Islander =
          mean(
`Percent.of.total.enrollment.that.are.Asian/Native.Hawaiian/Pacific.Islander`,
            na.rm = TRUE
          )
      ) %>%
      filter(STABBR == input$diversityInput)
    div_percentages <- c(
      summarized_pie$ave_American_Indian,
      summarized_pie$ave_Asian,
      summarized_pie$ave_African_American,
      summarized_pie$ave_Latino,
      summarized_pie$ave_White,
      summarized_pie$ave_more_race,
      summarized_pie$ave_unknown,
      summarized_pie$ave_Islander
    )
    highest_percent <- max(div_percentages)
    pie_data <- data.frame(
      percentages = div_percentages,
      categories = ethnicity_categories
    )
    highest_race <- pie_data %>%
      filter(percentages == highest_percent) %>%
      pull(categories)
    if (highest_percent > 50) {
      paste("From the result of pie chart based on state that you choose,",
        "on average, highest ratio in this state is",
        highest_race,
        "and more than half of students are belong in",
        "that race on average.",
        sep = " "
      )
    } else {
      paste("From the result of pie chart based on state that you choose,",
        "on average, highest ratio in this state is",
        highest_race,
        "and less than half of students are belong in that race on",
        "average. Therefore, we might able to conclude",
        input$diversityInput, "has well diversed universities.",
        sep = " "
      )
    }
  })
})

# Create the tuition scatter plot
draw_scatter <- function(data, graph_var) {
  graph_df <- data %>%
    select("State", graph_var)
  if (graph_var == "In_state") {
    graph <- plot_ly(
      data = graph_df,
      x = ~State,
      y = ~In_state,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = 10
      )
    ) %>%
      layout(
        title = "In-State Tuition vs State",
        xaxis = list(title = "State"),
        yaxis = list(title = "In-State Tuition")
      )
  } else {
    graph <- plot_ly(
      data = graph_df,
      x = ~State,
      y = ~Out_of_State,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = 10
      )
    ) %>%
      layout(
        title = "Out-of-State Tuition vs State",
        xaxis = list(title = "State"),
        yaxis = list(title = "Out-of-State Tuition")
      )
  }
  return(graph)
}

# Create the map
draw_map <- function(data, state) {
  filtered_data <- data %>%
    filter(STABBR == !!as.character(state))
  if (state == "AL") {
    zoom_lat <- 32.318230
    zoom_lng <- -86.902298
    zoom_lvl <- 6
  } else if (state == "AK") {
    zoom_lat <- 66.160507
    zoom_lng <- -153.369141
    zoom_lvl <- 3
  } else if (state == "AZ") {
    zoom_lat <- 34.048927
    zoom_lng <- -111.093735
    zoom_lvl <- 6
  } else if (state == "AR") {
    zoom_lat <- 34.799999
    zoom_lng <- -92.199997
    zoom_lvl <- 6
  } else if (state == "CA") {
    zoom_lat <- 36.778259
    zoom_lng <- -119.417931
    zoom_lvl <- 5
  } else if (state == "CO") {
    zoom_lat <- 39.113014
    zoom_lng <- -105.358887
    zoom_lvl <- 6
  } else if (state == "CT") {
    zoom_lat <- 41.599998
    zoom_lng <- -72.699997
    zoom_lvl <- 8
  } else if (state == "DE") {
    zoom_lat <- 39.000000
    zoom_lng <- -75.500000
    zoom_lvl <- 8
  } else if (state == "DC") {
    zoom_lat <- 38.915204
    zoom_lng <- -77.09291
    zoom_lvl <- 11
  } else if (state == "FL") {
    zoom_lat <- 27.994402
    zoom_lng <- -81.760254
    zoom_lvl <- 5
  } else if (state == "GA") {
    zoom_lat <- 33.247875
    zoom_lng <- -83.441162
    zoom_lvl <- 6
  } else if (state == "HI") {
    zoom_lat <- 19.741755
    zoom_lng <- -155.844437
    zoom_lvl <- 7
  } else if (state == "ID") {
    zoom_lat <- 44.068203
    zoom_lng <- -114.742043
    zoom_lvl <- 5
  } else if (state == "IL") {
    zoom_lat <- 40.000000
    zoom_lng <- -89.000000
    zoom_lvl <- 6
  } else if (state == "IN") {
    zoom_lat <- 40.273502
    zoom_lng <- -86.126976
    zoom_lvl <- 6
  } else if (state == "IA") {
    zoom_lat <- 42.032974
    zoom_lng <- -93.581543
    zoom_lvl <- 6
  } else if (state == "KS") {
    zoom_lat <- 38.500000
    zoom_lng <- -98.000000
    zoom_lvl <- 6
  } else if (state == "KY") {
    zoom_lat <- 37.839333
    zoom_lng <- -84.270020
    zoom_lvl <- 6
  } else if (state == "LA") {
    zoom_lat <- 30.391830
    zoom_lng <- -92.329102
    zoom_lvl <- 6
  } else if (state == "ME") {
    zoom_lat <- 45.367584
    zoom_lng <- -68.972168
    zoom_lvl <- 6
  } else if (state == "MD") {
    zoom_lat <- 39.045753
    zoom_lng <- -76.641273
    zoom_lvl <- 7
  } else if (state == "MA") {
    zoom_lat <- 42.407211
    zoom_lng <- -71.382439
    zoom_lvl <- 7
  } else if (state == "MI") {
    zoom_lat <- 44.182205
    zoom_lng <- -84.506836
    zoom_lvl <- 5
  } else if (state == "MN") {
    zoom_lat <- 46.392410
    zoom_lng <- -94.636230
    zoom_lvl <- 6
  } else if (state == "MO") {
    zoom_lat <- 38.573936
    zoom_lng <- -92.603760
    zoom_lvl <- 6
  } else if (state == "MS") {
    zoom_lat <- 33.000000
    zoom_lng <- -90.000000
    zoom_lvl <- 6
  } else if (state == "MT") {
    zoom_lat <- 46.965260
    zoom_lng <- -109.533691
    zoom_lvl <- 5
  } else if (state == "NE") {
    zoom_lat <- 41.500000
    zoom_lng <- -100.000000
    zoom_lvl <- 6
  } else if (state == "NV") {
    zoom_lat <- 39.876019
    zoom_lng <- -117.224121
    zoom_lvl <- 5
  } else if (state == "NH") {
    zoom_lat <- 44.000000
    zoom_lng <- -71.500000
    zoom_lvl <- 7
  } else if (state == "NJ") {
    zoom_lat <- 39.833851
    zoom_lng <- -74.871826
    zoom_lvl <- 7
  } else if (state == "NM") {
    zoom_lat <- 34.307144
    zoom_lng <- -106.018066
    zoom_lvl <- 6
  } else if (state == "NY") {
    zoom_lat <- 43.000000
    zoom_lng <- -75.000000
    zoom_lvl <- 6
  } else if (state == "NC") {
    zoom_lat <- 35.782169
    zoom_lng <- -80.793457
    zoom_lvl <- 6
  } else if (state == "ND") {
    zoom_lat <- 47.650589
    zoom_lng <- -100.437012
    zoom_lvl <- 6
  } else if (state == "OH") {
    zoom_lat <- 40.367474
    zoom_lng <- -82.996216
    zoom_lvl <- 7
  } else if (state == "OK") {
    zoom_lat <- 36.084621
    zoom_lng <- -96.921387
    zoom_lvl <- 6
  } else if (state == "OR") {
    zoom_lat <- 44.000000
    zoom_lng <- -120.500000
    zoom_lvl <- 6
  } else if (state == "PA") {
    zoom_lat <- 41.203323
    zoom_lng <- -77.194527
    zoom_lvl <- 6
  } else if (state == "RI") {
    zoom_lat <- 41.700001
    zoom_lng <- -71.500000
    zoom_lvl <- 9
  } else if (state == "SC") {
    zoom_lat <- 33.836082
    zoom_lng <- -81.163727
    zoom_lvl <- 7
  } else if (state == "SD") {
    zoom_lat <- 44.500000
    zoom_lng <- -100.000000
    zoom_lvl <- 6
  } else if (state == "TN") {
    zoom_lat <- 35.860119
    zoom_lng <- -86.660156
    zoom_lvl <- 6
  } else if (state == "TX") {
    zoom_lat <- 31.000000
    zoom_lng <- -100.000000
    zoom_lvl <- 5
  } else if (state == "UT") {
    zoom_lat <- 39.419220
    zoom_lng <- -111.950684
    zoom_lvl <- 6
  } else if (state == "VA") {
    zoom_lat <- 37.926868
    zoom_lng <- -78.024902
    zoom_lvl <- 6
  } else if (state == "VT") {
    zoom_lat <- 44.000000
    zoom_lng <- -72.699997
    zoom_lvl <- 7
  } else if (state == "WA") {
    zoom_lat <- 47.751076
    zoom_lng <- -120.740135
    zoom_lvl <- 6
  } else if (state == "WV") {
    zoom_lat <- 39.000000
    zoom_lng <- -80.500000
    zoom_lvl <- 6
  } else if (state == "WI") {
    zoom_lat <- 44.500000
    zoom_lng <- -89.500000
    zoom_lvl <- 6
  } else if (state == "WY") {
    zoom_lat <- 43.075970
    zoom_lng <- -107.290283
    zoom_lvl <- 6
  }
  map <- leaflet(filtered_data) %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = zoom_lng, lat = zoom_lat, zoom = zoom_lvl) %>%
    addMarkers(
      lat = ~LATITUDE,
      lng = ~LONGITUDE,
      popup = paste0("School: ", filtered_data$INSTNM)
    )
  return(map)
}

# Create the summary for the academic section
text_summary <- function(data, search) {
  filtered_data <- data %>%
    filter(data$STABBR == search)
  df <- filtered_data %>%
    select(
      "INSTNM", "hs.gpa.avg", "SAT_AVG_ALL",
      "ACT.Composite.75th.percentile.score",
      "ADM_RATE", "overallRank"
    )
  most_difficult_school_rate <- df %>%
    select("ADM_RATE") %>%
    min(na.rm = TRUE)
  most_diff_school_rate_school <- df %>%
    filter(ADM_RATE == most_difficult_school_rate) %>%
    select("INSTNM") %>%
    pull()
  easiest_school_rate <- df %>%
    select("ADM_RATE") %>%
    max(na.rm = TRUE)
  easiest_school_rate_school <- df %>%
    filter(ADM_RATE == easiest_school_rate) %>%
    select("INSTNM") %>%
    pull()
  ranked_schools <- df %>%
    select("overallRank") %>%
    na.omit() %>%
    nrow()
  if (ranked_schools > 0) {
    highest_ranked <- df %>%
      select("overallRank") %>%
      max(na.rm = TRUE)
    highest_ranked_school <- df %>%
      filter(overallRank == highest_ranked) %>%
      select("INSTNM") %>%
      pull()
    if (ranked_schools > 1) {
      lowest_ranked <- df %>%
        select("overallRank") %>%
        min(na.rm = TRUE)
      lowest_ranked_school <- df %>%
        filter(overallRank == lowest_ranked) %>%
        select("INSTNM") %>%
        pull()
      return(paste0(
        "The following charts show the results for ",
        search, ". What we can see is that",
        "there are a few schools that are ommitted out",
        "of some of the charts. This is because the data",
        "was not available for that school.", "The school",
        "with the lowest admission rate was ",
        most_diff_school_rate_school, " with ",
        most_difficult_school_rate, " rate. The school with the ",
        "easiest admissions rate was ",
        easiest_school_rate_school, " with ", easiest_school_rate,
        " rate. The highest ranked school was ",
        highest_ranked_school, " and ", lowest_ranked_school,
        " was the lowest ranked school."
      ))
    }
    return(paste0(
      "The following charts show the results for ",
      search, ". What we can see is that",
      "there are a few schools that are ommitted out",
      "of some of the charts. This is because the data",
      "was not available for that school.", "The school",
      "with the lowest admission rate was ",
      most_diff_school_rate_school, " with ",
      most_difficult_school_rate, " rate. The school with the ",
      "easiest admissions rate was ",
      easiest_school_rate_school, " with ", easiest_school_rate,
      " rate. The highest ranked school was ",
      highest_ranked_school, "."
    ))
  } else {
    return(paste0(
      "The following charts show the results for ",
      search, ". What we can see is that",
      "there are a few schools that are ommitted out",
      "of some of the charts. This is because the data",
      "was not available for that school.", "The school",
      "with the lowest admission rate was ",
      most_diff_school_rate_school, " with ",
      most_difficult_school_rate, " rate. The school with the ",
      "easiest admissions rate was ",
      easiest_school_rate_school, " with ", easiest_school_rate,
      " rate. Unfortunately, no ranking information is available ",
      "for ", search, "."
    ))
  }
}

# Create the academic bar charts
draw_bar <- function(data, search, graph_var) {
  filtered_data <- data %>%
    filter(data$STABBR == search)
  graph_df <- filtered_data %>%
    select("INSTNM", graph_var)
  graph <- plot_ly(
    data = graph_df,
    x = ~INSTNM,
    y = ~ graph_df[, graph_var],
    type = "bar"
  ) %>%
    layout(
      xaxis = list(tickangle = 45, titlefont = list(size = 30))
    )
  if (graph_var == "hs.gpa.avg") {
    graph <- graph %>%
      layout(
        title = paste("High School GPA vs Schools in", search),
        yaxis = list(title = "High School GPA")
      )
  } else if (graph_var == "SAT_AVG_ALL") {
    graph <- graph %>%
      layout(
        title = paste("SAT Scores vs Schools in", search),
        yaxis = list(title = "SAT Score")
      )
  } else if (graph_var == "ACT.Composite.75th.percentile.score") {
    graph <- graph %>%
      layout(
        title = paste("ACT Scores vs Schools in", search),
        yaxis = list(title = "ACT Score")
      )
  } else if (graph_var == "ADM_RATE") {
    graph <- graph %>%
      layout(
        title = paste("Admissions Rate vs Schools in", search),
        yaxis = list(title = "Admissions Rate")
      )
  } else {
    graph <- graph %>%
      layout(
        title = paste("Rank vs Schools in", search),
        yaxis = list(title = "Rank")
      )
  }
  return(graph)
}

# Ethnicity pie chart
draw_pie <- function(data, category, state) {
  div_percentages <- c(
    data$ave_American_Indian,
    data$ave_Asian,
    data$ave_African_American,
    data$ave_Latino,
    data$ave_White,
    data$ave_more_race,
    data$ave_unknown,
    data$ave_Islander
  )
  pie_df <- data.frame(percentages = div_percentages, categories = category)
  pie_plot <- plot_ly(
    pie_df,
    labels = ~categories,
    values = ~percentages,
    type = "pie"
  ) %>%
    layout(
      title = paste("Ethnic diversity in", state),
      legend = list(orientation = "h")
      )
  return(pie_plot)
}


# all uni summary table
# all states summary table
all_states_summary <- join_result2 %>%
  group_by(INSTNM, STABBR) %>%
  summarise(
    act.avg = mean(act.avg, na.rm = TRUE),
    hs.gpa.avg = mean(hs.gpa.avg, na.rm = TRUE),
    percent_of_american_indian_alaskan_native =
      mean(
        Percent.of.total.enrollment.that.are.American.Indian.or.Alaska.Native,
        na.rm = TRUE
      ),
    percent_of_asian = mean(Percent.of.total.enrollment.that.are.Asian,
      na.rm = TRUE
    ),
    percent_of_african_american =
      mean(Percent.of.total.enrollment.that.are.Black.or.African.American,
        na.rm = TRUE
      ),
    percent_of_hispanic_or_latino =
      mean(`Percent.of.total.enrollment.that.are.Hispanic/Latino`,
        na.rm = TRUE
      ),
    percent_of_white =
      mean(Percent.of.total.enrollment.that.are.White, na.rm = TRUE),
    percent_of_two_or_more_races =
      mean(Percent.of.total.enrollment.that.are.two.or.more.races,
        na.rm = TRUE
      ),
    percent_of_race_unknown =
      mean(`Percent.of.total.enrollment.that.are.Race/ethnicity.unknown`,
        na.rm = TRUE
      ),
    percent_of_nonresident_alien =
      mean(Percent.of.total.enrollment.that.are.Nonresident.Alien,
        na.rm = TRUE
      ),
    percent_of_asian_native_pacific_islander =
      mean(
`Percent.of.total.enrollment.that.are.Asian/Native.Hawaiian/Pacific.Islander`,
        na.rm = TRUE
      ),
    percent_of_women = mean(Percent.of.total.enrollment.that.are.women)
  ) %>%
  arrange(act.avg)

summary_table <- all_states_summary %>%
  mutate(
    "ACT Average" = act.avg,
    "High School Average GPA" = hs.gpa.avg,
    "Native American (% Diversity)" =
      percent_of_american_indian_alaskan_native,
    "Asian (% Diversity)" = percent_of_asian,
    "African American (% Diversity)" = percent_of_african_american,
    "Hispanic or Latino (% Diversity)" = percent_of_hispanic_or_latino,
    "White (% Diversity)" = percent_of_white,
    "Two Or More Races (% Diversity)" = percent_of_two_or_more_races,
    "Race Unknown (% Diversity)" = percent_of_race_unknown,
    "Nonresident Alien (% Diversity)" = percent_of_nonresident_alien,
    "Asian Native Pacific Islander (% Diversity)" =
      percent_of_asian_native_pacific_islander,
    "Women (% Diversity)" = percent_of_women
  )

all_states_summary <- summary_table %>%
  select(
    INSTNM, STABBR,
    `ACT Average`,
    `High School Average GPA`,
    `Native American (% Diversity)`,
    `Asian (% Diversity)`,
    `African American (% Diversity)`,
    `Hispanic or Latino (% Diversity)`,
    `White (% Diversity)`,
    `Two Or More Races (% Diversity)`,
    `Race Unknown (% Diversity)`,
    `Nonresident Alien (% Diversity)`,
    `Asian Native Pacific Islander (% Diversity)`,
    `Women (% Diversity)`
  )
