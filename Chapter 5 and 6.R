
# Chapter 5 - Data tidying ------------------------------------------------


# 5.2 - Tidy data ---------------------------------------------------------

table1 |>
  mutate(rate = cases/population * 10000)

table1 |>
  group_by(year) |>
  summarize(total_cases = sum(cases))

ggplot(table1, aes(x = year, y = cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country, shape = country)) +
  scale_x_continuous(breaks = c(1999, 2000))   

# 5.2.1 - Exercises -------------------------------------------------------
# 1 - Various ways of showing the data.
# 2 - I would do what the instructions say.
# group_by country and year
# math


# 5.3 - Lengthening Data --------------------------------------------------

billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week", values_to = "rank"
  )

billboard_longer <- billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week", values_to = "rank",
    values_drop_na = TRUE
  ) |>
  mutate(
    week = parse_number(week)
  )
billboard_longer |>
  ggplot(aes(x = week, y = rank, group = track)) +
  geom_line(alpha = 0.25) +
  scale_y_reverse()

# 5.3.2 - How does pivoting work? -----------------------------------------

df <- tribble(
  ~id,  ~bp1, ~bp2,
  "A",  100,  120,
  "B",  140,  115,
  "C",  120,  125
)
df |>
  pivot_longer(
    cols = bp1:bp2,
    names_to = "measurement",
    values_to = "value"
  )

# 5.3.3 - Many variables in column names ----------------------------------

who2 |>
  pivot_longer(
    cols = !(country:year),
    names_to = c("diagnosis", "gender", "age"),
    names_sep = "_",
    values_to = "count"
  )


# 5.3.4 - Data and variable names in column headers -----------------------


# 5.4 - Widening Data -----------------------------------------------------

cms_patient_experience |>
  distinct(measure_cd, measure_title)

cms_patient_experience |>
  pivot_wider(
    names_from = measure_cd,
    values_from = prf_rate
  )
cms_patient_experience |>
  pivot_wider(
    id_cols = starts_with("org"),
    names_from = measure_cd,
    values_from = prf_rate
  )


# Chapter 6 ---------------------------------------------------------------


