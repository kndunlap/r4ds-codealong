
# Chapter 7 - Data Import -------------------------------------------------


# 7.2 - Reading data from a file ------------------------------------------------------

students <- read_csv("https://pos.it/r4ds-students-csv")
students <- read_csv("https://pos.it/r4ds-students-csv", na = c("N/A", ""))
students |>
  rename(
    student_id = `Student ID`,
    full_name = `Full Name`
  )
students |> janitor::clean_names()
students <- students |>
  janitor::clean_names() |>
  mutate(
    meal_plan = factor(meal_plan),
    age = parse_number(if_else(age == "five", "5", age))
  )

# 7.2.4 - Exercises --------------------------------------------------------
# 1. - read_delim, delim = |
# 2. - Probably all of them.
# 3. - The width size.
# 4. - Specify the quote argument.
# 5. - Many errors
# 6. - CODE
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
# a. 
annoying |> select(`1`)
# b. 
ggplot(annoying, aes(x = `1`, y = `2`)) + geom_point()
# c.
annoying |>
  mutate(
    `3` = `2`/`1`
  )
# d. 
annoying |>
  mutate(`3` = `2`/`1`) |>
  rename(
    one = `1`,
    two = `2`,
    three = `3`
)

# 7.4 - Reading data from multiple files ------------------------------------------

sales_files <- c(
  "https://pos.it/r4ds-01-sales",
  "https://pos.it/r4ds-02-sales",
  "https://pos.it/r4ds-03-sales"
)
read_csv(sales_files, id = "file")

# 7.6 - Data Entry -------------------------------------------------
tibble(
  x = c(1, 2, 5), 
  y = c("h", "m", "g"),
  z = c(0.08, 0.83, 0.60)
)

tribble(
  ~x, ~y, ~z,
  1, "h", 0.08,
  2, "m", 0.83,
  5, "g", 0.60
)


# Chapter 8 - Getting Help ------------------------------------------------

reprex::reprex()
