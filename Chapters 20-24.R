
# Chapter 20 - Spreadsheets -----------------------------------------------


# 20.2 - Excel ------------------------------------------------------------

students <- read_excel("students.xlsx",
           col_names = c("student_id", "full_name", "favorite food", "meal_plan", "age"),
           skip = 1,
           na = c("", "N/A"),
           col_types = c("numeric", "text", "text", "text", "text")
           )

students <- students |>
  mutate(
    age = if_else(age == "five", "5", age),
    age = parse_number(age)
  )

read_excel("penguins.xlsx", sheet = "Torgersen Island")

excel_sheets("penguins.xlsx")

penguins_torgersen <- read_excel("penguins.xlsx", sheet = "Torgersen Island", na = "NA")
penguins_biscoe <- read_excel("penguins.xlsx", sheet = "Biscoe Island", na = "NA")
penguins_dream <- read_excel("penguins.xlsx", sheet = "Dream Island", na = "NA")

dim(penguins_biscoe)
dim(penguins_torgersen)
dim(penguins_dream)

penguins <- bind_rows(penguins_torgersen, penguins_biscoe, penguins_dream)

bake_sale <- tibble(
  item     = factor(c("brownie", "cupcake", "cookie")),
  quantity = c(10, 5, 8)
)


# 20.2.9 - Exercises ------------------------------------------------------

# 1 - CODE
survey1 <- read_xlsx("survey.xlsx",
                    na = c("","N/A"),
                    col_type = c("numeric", "text")) |>
  mutate(
    n_pets = if_else(n_pets == "two", "2", n_pets)
  ) |>
  mutate(
    survey_id = as.character(survey_id),
    n_pets = as.numeric(n_pets)
  )

roster <- read_excel("roster.xlsx") |>
  fill(group, subgroup)

read_excel("sales.xlsx", skip = 3, col_names = c("id", "n")) |>
  mutate(brand = if_else(str_detect(id, "Brand"), id, NA)) |>
  fill(brand) |>
  filter(n != "n") 
  relocate(brand) |>
  mutate(
    id = as.numeric(id),
    n = as.numeric(n)
  ) |>
  print(n = 7)


# 20.3 - Google Sheets ----------------------------------------------------

students_sheet_id <- "1V1nPp1tzOuutXFLb3G9Eyxi3qxeEhnOXUzL5_BcCQ0w"
students <- read_sheet(students_sheet_id)





# Chapter 21 - Databases --------------------------------------------------

# 21.3 - Connecting to a database --------------------------------------------------

con1 <- DBI::dbConnect(
  RMariaDB::MariaDB(), 
  username = "foo"
)
con2 <- DBI::dbConnect(
  RPostgres::Postgres(), 
  hostname = "databases.mycompany.com", 
  port = 1234
)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "duckdb")

dbWriteTable(con, "mpg", ggplot2::mpg)
dbWriteTable(con, "diamonds", ggplot2::diamonds)

dbListTables(con)
con |>
  dbReadTable("diamonds") |>
  as_tibble()

sql <- "
SELECT carat, cut, clarity, color, price
FROM diamonds
WHERE price > 15000"
as_tibble(dbGetQuery(con, sql))


# 21.4 - dbplyr basics ----------------------------------------------------

diamonds_db <- tbl(con, "diamonds")
diamonds_db

big_diamonds_db <- diamonds_db |> 
  filter(price > 15000) |> 
  select(carat:clarity, price)

bigger_diamonds_db |>
  show_query()

bigger_diamonds_db <- diamonds_db |>
  group_by(cut)
  summarize(
    price = price * 2
  )

big_diamonds <- big_diamonds_db |> 
    collect()


# 21.5 - SQL --------------------------------------------------------------

dbplyr::copy_nycflights13(con)

flights <- tbl(con, "flights")
planes <- tbl(con, "planes")

flights |>
  show_query()

flights |>
  filter(dest == "IAH") |>
  arrange(dep_delay) |>
  show_query()

flights |>
  group_by(dest) |>
  summarize(dep_delay = mean(dep_delay, na.rm = TRUE)) |>
              show_query()
       
planes |> 
  select(tailnum, type, manufacturer, model, year) |> 
  rename(year_built = year) |> 
  show_query()     

diamonds_db |> 
  group_by(cut) |> 
  summarize(
    n = n(),
    avg_price = mean(price, na.rm = TRUE),
    median_prie = median(price, na.rm = TRUE)
  ) |> 
  show_query()

flights |> 
  group_by(dest) |> 
  summarize(delay = mean(arr_delay))

flights |> 
  left_join(planes |> rename(year_built = year), by = "tailnum") |> 
  show_query()


# 21.5.10 - Exercises -----------------------------------------------------

# 1 - DISTINCT = DISTINCT, head = LIMIT
flights |>
  distinct(tailnum) |>
  head(n = 20) |>
  show_query()

# 2 - picks rows in flights where arr_delay is greater than dep_delay
flights |>
  filter(arr_delay > dep_delay) |>
  show_query()

# mutates to make a new column.
flights |>
  mutate(speed = distance / (air_time/60)) |>
  show_query()


# 21.6 - Function Translations --------------------------------------------

summarize_query <- function(df, ...) {
  df |> 
    summarize(...) |> 
    show_query()
}
mutate_query <- function(df, ...) {
  df |> 
    mutate(..., .keep = "none") |> 
    show_query()
}


# Chapter 22- Arrow -------------------------------------------------------


# 22.2 - Getting the data -------------------------------------------------


dir.create("data", showWarnings = FALSE)

curl::multi_download(
  "https://r4ds.s3.us-west-2.amazonaws.com/seattle-library-checkouts.csv",
  "data/seattle-library-checkouts.csv",
  resume = TRUE
)



# 22.3 - Opening a dataset ------------------------------------------------
seattle_csv <- open_dataset(
  sources = "data/seattle-library-checkouts.csv", 
  col_types = schema(ISBN = string()),
  format = "csv"
)

seattle_csv

seattle_csv |>
  glimpse()

seattle_csv |>
  group_by(CheckoutYear) |>
  summarize(Checkouts = sum(Checkouts)) |>
  arrange(CheckoutYear) |>
  collect()


# 22.4 - Parquet Format ---------------------------------------------------

pq_path <- "data/seattle-library-checkouts"

seattle_csv |>
  group_by(CheckoutYear) |>
  write_dataset(path= pq_path, format = "parquet")

tibble(
  files = list.files(pq_path, recursive = TRUE),
  size_MB = file.size(file.path(pq_path, files)) / 1024^2
) |>
  summarise(
    sum = sum(size_MB)
  )


# 22.5 - Using dplyr with arrow -------------------------------------------

seattle_pq <- open_dataset(pq_path)

query <- seattle_pq |>
  filter(CheckoutYear >= 2018, MaterialType == "BOOK") |>
  group_by(CheckoutYear, CheckoutMonth) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(CheckoutYear, CheckoutMonth)

query |>
   collect()

seattle_csv |> 
  filter(CheckoutYear == 2021, MaterialType == "BOOK") |>
  group_by(CheckoutMonth) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(desc(CheckoutMonth)) |>
  collect() |> 
  system.time()

seattle_pq |> 
  filter(CheckoutYear == 2021, MaterialType == "BOOK") |>
  group_by(CheckoutMonth) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(desc(CheckoutMonth)) |>
  collect() |> 
  system.time()

seattle_pq |> 
  to_duckdb() |>
  filter(CheckoutYear >= 2018, MaterialType == "BOOK") |>
  group_by(CheckoutYear) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(desc(CheckoutYear)) |>
  collect()


# 22.5.3 - Exercises ------------------------------------------------------

query1 <- seattle_pq |>
  filter(MaterialType == "BOOK") |>
  arrange(desc(Checkouts))

query1 |> collect()

seattle_pq |>
  group_by(CheckoutYear, MaterialType) |>
  summarize(
    sum = sum(Checkouts)
  ) |>
  filter(MaterialType == "BOOK" | MaterialType == "EBOOK") |>
    collect() |>
  ggplot(aes(x = CheckoutYear, y = sum, color = MaterialType)) +
  geom_line()
  
 

# Chapter 23 - Hierarchical data --------------------------------------------


# 23.2 - Lists ------------------------------------------------------------

x1 <- list(1:4, "a", TRUE)
x1

x2 <- list(a = 1:2, b = 1:3, c = 1:4)
x2

str(x1)

x3 <- list(list(1,2), list(3,4))
str(x3)

x5 <- list(1, list(2, list(3, list(4, list(5)))))
str(x5)
View(x5)

df <- tibble(
  x = 1:2, 
  y = c("a", "b"),
  z = list(list(1, 2), list(3, 4, 5))
)
df

df |>
  pull(z) |>
  str()


# 23.3 - Unnesting --------------------------------------------------------

df1 <- tribble(
  ~x, ~y,
  1, list(a = 11, b = 12),
  2, list(a = 21, b = 22),
  3, list(a = 31, b = 32),
)
df1

df2 <- tribble(
  ~x, ~y,
  1, list(11, 12, 13),
  2, list(21),
  3, list(31, 32),
)
df2

df1 |> pull(y)

df1 |>
  unnest_wider(y, names_sep = "_")

df2 |>
  unnest_longer(y)

df4 <- tribble(
  ~x, ~y,
  "a", list(1),
  "b", list("a", TRUE, 5)
)

df4 |>
  unnest_longer(y)

# 23.3.5 - Exercises ------------------------------------------------------

# 1 - CODE - missing values become NA
df2 |>
  unnest_wider(y, names_sep = "_")

# 2 - not sure!
df1 |>
  unnest_longer(y, keep_empty = TRUE)
 
# 3 - CODE
df4 <- tribble(
  ~x, ~y, ~z,
  "a", list("y-a-1", "y-a-2"), list("z-a-1", "z-a-2"),
  "b", list("y-b-1", "y-b-2", "y-b-3"), list("z-b-1", "z-b-2", "z-b-3")
)
df4 |>
  unnest_longer(y) |>
  unnest_longer(z)


# 23.4 - Case Studies -----------------------------------------------------

View(gh_repos)

repos <- tibble(json = gh_repos)
repos |>
  unnest_longer(json) |>
  unnest_wider(json) |>
  select(id, full_name, owner, description) |>
  unnest_wider(owner, names_sep = "_")

chars <- tibble(json = got_chars)
chars |>
  unnest_wider(json) |>
  select(id, name, gender, culture, born, died, alive)

chars |>
  unnest_wider(json) |>
  select(id, where(is.list))

gmaps_cities
View(gmaps_cities)

locations <- gmaps_cities |>
  unnest_wider(json) |>
  select(-status) |>
  unnest_longer(results) |>
  unnest_wider(results)

locations |>
  select(city, formatted_address, geometry) |>
  unnest_wider(geometry) |>
  select(!location:viewport) |>
  unnest_wider(bounds) |>
  rename(ne = northeast, sw = southwest) |>
  unnest_wider(c(ne, sw), names_sep = "_")

locations |> 
  select(city, formatted_address, geometry) |> 
  hoist(
    geometry,
    ne_lat = c("bounds", "northeast", "lat"),
    sw_lat = c("bounds", "southwest", "lat"),
    ne_lng = c("bounds", "northeast", "lng"),
    sw_lng = c("bounds", "southwest", "lng"),
  )


# 23.4.4 - Exercises ------------------------------------------------------

# 1 - CODE - probably 06-24-2012
repos <- tibble(json = gh_repos)
repos_all <- repos |>
  unnest_longer(json) |>
  unnest_wider(json) |>
  select(created_at) |>
  arrange(created_at)
glimpse(repos)                

# 2 - CODE - hard
repos_all <- repos |>
  unnest_longer(json) |>
  unnest_wider(json) |>
  select(owner) |>
  unnest_longer(owner) |>
  unnest_longer(owner)

repos_all |> distinct(full_name)

# 3 - CODE
chars |>
  unnest_wider(json) |>
  select(id, name, gender, culture, allegiances) |>
  unnest_longer(allegiances) |>
  select(name, allegiances) |>
  print(n = Inf)

tibble(json = got_chars) |> 
  unnest_wider(json) |>
  select(id, where(is.list)) |>
  pivot_longer(
    where(is.list), 
    names_to = "name", 
    values_to = "value"
  ) |>
  unnest_longer(value)


# 23.5 - JSON -------------------------------------------------------------

gh_users_json()

gh_users2 <- read_json(gh_users_json())

identical(gh_users, gh_users2)


# 23.5.4 - Exercises ------------------------------------------------------

# 1 - CODE

json_col <- parse_json('
  {
    "x": ["a", "x", "z"],
    "y": [10, null, 3]
  }
')
json_row <- parse_json('
  [
    {"x": "a", "y": 10},
    {"x": "x", "y": null},
    {"x": "z", "y": 3}
  ]
')

df_col <- tibble(json = list(json_col)) 
df_row <- tibble(json = json_row)

df_col |>
  unnest_longer(json) |>
  unnest_wider(json, names_sep = "_") 
  

df_row |>
  unnest_wider(json)


# Chapter 24 - Web Scraping -----------------------------------------------


# 24.4 - Extracting Data ------------------------------------------------------

html <- read_html("http://rvest.tidyverse.org")

html <- minimal_html("
  <h1>This is a heading</h1>
  <p id='first'>This WSis a paragraph</p>
  <p class='important'>This is an important paragraph</p>
")

html |>
  html_elements("p")


# test --------------------------------------------------------------------

url <- "https://www.espn.com/nhl/team/stats/_/name/det/detroit-red-wings"

html <- read_html(url)

table <- html |>
  html_nodes("Table.Table--align-right") |>
  html_table()

