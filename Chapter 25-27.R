
# Chapter 25 - Functions --------------------------------------------------


# 25.2 - Vector Functions -------------------------------------------------

rescale01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

rescale01(c(-10,0,10))

df <- tibble(
  a = rnorm(5),
  b = rnorm(5),
  c = rnorm(5),
  d = rnorm(5),
)

df |> mutate(
  a = rescale01(a),
  b = rescale01(b),
  c = rescale01(c),
  d = rescale01(d),
)

z_score <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

clamp <- function(x, min, max) {
  case_when(
    x < min ~ min,
    x > max ~ max,
    .default = x
  )
}
clamp(1:10, min = 2, max = 8)

first_upper <- function(x) {
  str_sub(x, 1, 1) <- str_to_upper(str_sub(x, 2, 1))
  x
}

first_upper("hello")

commas <- function(x) {
  str_flatten(x, collapse = ", ", last = " and ")
}

commas(c("cat", "dog", "pigeon"))


# 25.2.5 - Exercises ------------------------------------------------------

# 1a - CODE

x <- c(NA, 1, 53, NA, 2, 34)
x
mean(is.na(x))

na_mean <- function(x) {
  mean(is.na(x))
}

# 2 - CODE
x <- c(1:10, Inf, -Inf)
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
  if_else(Inf, 1, x)
}
rescale01(x)

# 4 - CODE
x <- sample(1:20, 20, replace = TRUE)
skew <- function(x) {
  mean <- mean(x)
  median <- median(x)
  sd <- sd(x)
  length <- length(x)
  skew <- sum((x - mean) ^ 3)/((length - 1) * (sd^3))
  return(skew)
}
skew(x)

# 5 - CODE
x <- c(1, 2, NA, 3, NA)
y <- c(4, 5, NA, NA, 6)

both_na <- function(x, y) {
  which(is.na(x) & is.na(y))
}

both_na(x, y)


# 25.3 - Data Frame Functions ---------------------------------------------


# 25.3.5 - Exercises ------------------------------------------------------


# 1a - CODE
filter_severe <- function(x) {
  x |>
    filter(is.na(arr_time == TRUE) | dep_delay > 60) |>
    arrange(desc(dep_delay))
}


# 1c - CODE
filter_severe <- function(df, hours) {
  df |>
    filter(is.na(arr_time == TRUE) | dep_delay > hours * 60) |>
    arrange(desc(dep_delay))
}

flights |>
  filter_severe(hours = 2)

# 1d - CODE
summarize_weather <- function(df, varz) {
  df |>
    group_by(origin, month) |>
    summarise(
      min = min({{varz}}, na.rm = TRUE),
      max = max({{varz}}, na.rm = TRUE),
      mean = mean({{varz}}, na.rm = TRUE),
    )
}

weather |>
  summarize_weather(temp) |>
  print(n = 36)

# 3 - CODE

count_prop <- function(df, var, sort = FALSE) {
  df |>
    count(pick({{ var }}), sort = sort) |>
    mutate(prop = n / sum(n))
}

eep <- flights |>
  count_prop(c(dest, origin)) |>
  print(n = 20)

eep |>
  arrange(desc(prop))


# 25.4 - Plot Functions ---------------------------------------------------

histogram <- function(df, var, binwidth = NULL) {
  df |>
    ggplot(aes(x = {{var}})) +
    geom_histogram(binwidth = binwidth)
}

diamonds |>
  histogram(carat, 0.1) +
  labs(x = "Size (in carats)", y = "Number of diamonds")


# 25.4.4 - Exercises ------------------------------------------------------

flights |>
  filter(dest == "DTW") |>
  filter(!is.na(arr_delay)) |>
  filter(!is.na(dep_delay)) |>
  ggplot(aes(x = dep_delay, y = arr_delay)) +
  geom_point(aes(color = carrier)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "delay")



scatter <- function(df, var1, var2, var3) {
  label <- englue("A plot of {{var1}} against {{var2}}")
  df |> 
    filter(dest == "DTW") |>
    filter(!is.na({{var1}})) |>
    filter(!is.na({{var2}})) |>
    ggplot(aes(x = {{ var1 }}, y = {{ var2 }})) + 
    geom_point(aes(color = carrier)) +
    geom_smooth(method = var3, se = FALSE) +
    labs(title = label)
}

flights |> scatter(dep_delay, arr_delay, "lm")


# Chapter 26 - Iteration --------------------------------------------------


# 26.2 - Modifying multiple columns ---------------------------------------

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df |>
  summarize(
    n = n(),
    across(a:d, median),
  )
)

df <- tibble(
  grp = sample(2, 10, replace = TRUE),
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df |>
  group_by(grp) |>
  summarise(across(everything(), median))


# 26.2.8 - Exercises ------------------------------------------------------

# 1a - CODE

penguins |>
  summarize(across(everything(), n_distinct))

# 1b - CODE
mtcars |>
  summarize(across(everything(), mean))

# 1c - CODE
diamonds |>
  group_by(cut, clarity, color) |>
  summarize(across(depth:z, mean))


# 26.3 - Reading Multiple Files -------------------------------------------

paths <- list.files("data/gapminder", pattern = "[.]xlsx$", full.names = TRUE)


# 26.4 - Saving Multiple Outputs ------------------------------------------

by_dest <- flights |>
  group_nest(dest)

by_dest$data[[5]]


# Chapter 27 - A field guide to base R ------------------------------------


# 27.2 - Selecting multiple elements with [ -------------------------------


# 27.2.4 - Exercises ------------------------------------------------------

# 1 - CODE

even_number <- function(x) {
  x <- if_else(x %% 2 == 0, x, NA) 
  x <- x[!is.na(x)]
  return(x)
  
}

vc <- c(2,3,4,5,6,7,8,9,10)    
even_number(vc)


# 27.3 - Selecting a single element with $ and [[ -------------------------

tb <- tibble(
  x = 1:4,
  y = c(10, 4, 1, 21)
)

tb[[1]]

tb[["x"]]
tb$x

max(diamonds$carat) 
diamonds |>
  pull(carat) |>
  max()


# 27.3.4 - Exercises ------------------------------------------------------
l <- list(
  a = 1:3, 
  b = "a string", 
  c = pi, 
  d = list(-1, -5)
)

l[1][[1]]
# pepper[[1]][[1]] pulls out the first element of the first element in the list.


# 27.4 - Apply family -----------------------------------------------------

df <- tibble(a = 1, b = 2, c = "a", d = "b", e = 4)
df
num_cols <- sapply(df, is.numeric)
num_cols
df
df[, num_cols] <- lapply(df[, num_cols, drop = FALSE], \(x) x * 2)
df

tapply(diamonds$price, diamonds$cut, mean)


# 27.5 - For loops --------------------------------------------------------


for (i in 1:10) {
  print(i*2)
}


# 27.6 - Plots ------------------------------------------------------------

hist(diamonds$carat)
plot(diamonds$carat, diamonds$price)




