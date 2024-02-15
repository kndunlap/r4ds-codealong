
# Chapter 12 - Logical Vectors --------------------------------------------


# 12.2 - Comparisons ------------------------------------------------------
flights |> 
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
    .keep = "used"
  )
flights |> 
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
  ) |> 
  filter(daytime & approx_ontime)

flights |>
  filter(is.na(dep_time))


# 12.2.4 - Exercises ------------------------------------------------------

# 1 - It gives TRUE for "near" numbers.
x <- 2
near(x, sqrt(3)^2)

# 2 - it looks like dep_time and dep_delay have the same NAs.
# NA = flight cancelled.
flights |>
  mutate(nothere = is.na(dep_time) 
         & is.na(sched_dep_time) 
         & is.na(dep_delay),
         .keep = "used") |>
  arrange(desc(is.na(dep_time))) |>
  count(is.na(sched_dep_time)) 
        

# 12.3 - Boolean algebra --------------------------------------------------

flights |> 
  filter(month %in% c(11, 12))


# 12.3.4 - Exercises ------------------------------------------------------

# 1 - CODE
flights |>
  filter(is.na(arr_delay) == TRUE) |>
  filter(is.na(dep_delay) == FALSE)

flights |>
  filter(is.na(arr_time) == FALSE) |>
  filter(is.na(sched_arr_time) == FALSE) |>
  filter(is.na(arr_delay) == TRUE)

# 2 - represents cancellations
flights |>
  filter(is.na(dep_time) == TRUE) 

# 3 - CODE - tough one but I understand this code. Onwards!
flights %>%
  mutate(dep_date = lubridate::make_datetime(year, month, day)) %>%
  group_by(dep_date) %>%
  summarise(cancelled = sum(is.na(dep_delay)), 
            n = n(),
            mean_dep_delay = mean(dep_delay,na.rm=TRUE),
            mean_arr_delay = mean(arr_delay,na.rm=TRUE)) 
  ggplot(aes(x= cancelled/n)) + 
  geom_point(aes(y=mean_dep_delay), colour='blue', alpha=0.5) + 
  geom_point(aes(y=mean_arr_delay), colour='red', alpha=0.5) + 
  ylab('mean delay (minutes)')
  

# 12.4 - Summaries --------------------------------------------------------

# Gives logical - asks were all flights delayed by less than 60 minutes, or were any flights delayed by over 5 hours.
flights |>
    group_by(year, month, day) |>
    summarize(
      all_delayed = all(dep_delay <= 60, na.rm = TRUE),
      any_long_delay = any(arr_delay >= 300, na.rm = TRUE),
      .groups = "drop"
    ) |>
    min(any_long_delay)
  
flights |>
  group_by(year, month, day) |>
  summarize(
    all_delayed = mean(dep_delay <= 60, na.rm = TRUE),
    any_long_delay = sum(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop" 
    ) |>
  arrange(all_delayed)

# wrote this myself - shows what days had the highest cancellation percentage
# matches up with the blizzard in NYC Feb 8-9 2013
flights |>
  group_by(year, month, day) |>
  summarise(
    cancelled = mean(is.na(dep_time))
  ) |>
  arrange(desc(cancelled))

flights |> 
  filter(arr_delay > 0) |> 
  group_by(year, month, day) |> 
  summarize(
    behind = mean(arr_delay),
    n = n(),
    .groups = "drop"
  )

flights |> 
  group_by(year, month, day) |> 
  summarize(
    behind = mean(arr_delay[arr_delay > 0], na.rm = TRUE),
    ahead = mean(arr_delay[arr_delay < 0], na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )


# 12.4.4 - Exercises ------------------------------------------------------

# 1 - it will tell you the sum or proportion of NA's in "x"
# 2 - prod returns product of all values - Either 1 if all TRUE or 0 if any FALSE. Equal to all()
xx <- c(TRUE, TRUE, TRUE)
min(xx)
# min gives 0 if any FALSE, 1 if all TRUE. Also equal to all()


# 12.5 - Conditional Transformations --------------------------------------

flights |> 
  mutate(
    status = case_when(
      is.na(arr_delay)      ~ "cancelled",
      arr_delay < -30       ~ "very early",
      arr_delay < -15       ~ "early",
      abs(arr_delay) <= 15  ~ "on time",
      arr_delay < 60        ~ "late",
      arr_delay < Inf       ~ "very late",
    ),
    .keep = "used"
  )


# 12.5.4 - Exercises ------------------------------------------------------

# 1 - CODE
nums <- c(0:20)
if_else(nums %% 2 == 0, "EVEN", "ODD")

# 2 - CODE
days <- c("Monday", "Thursday", "Saturday", "Wednesday", "Sunday", "Friday")

if_else(days %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

# 3 - CODE
x <- c(-7:13)
if_else(x > 0, x, -x) |>
  sum()

# 4 - CODE
flights |>
  group_by(month, day, year) |>
  mutate(
    holiday = case_when(
      month == 7 & day == 4 ~ "Indepdendence Day",
      month == 3 & day == 17 ~ "St. Patrick's Day",
      month == 10 & day == 31 ~ "Halloween",
      month == 12 & day == 25 ~ "Christmas",
      month == 2 & day == 14 ~ "Valentine's Day",
      .default = "Regular"
    ),
    .keep = "used" 
  ) 


# Chapter 13 - Numbers ----------------------------------------------------


# 13.3 - Counts ---------------------------------------------------

flights |>
  count(dest, sort = TRUE) |> print(n = Inf)

# other way of doing it
flights |>
  group_by(dest) |>
  summarize(
    n = n(),
    delay = mean(arr_delay, na.rm = TRUE)
  )

flights |>
  group_by(dest) |>
  summarize(
    carriers = n_distinct(carrier),
    carrier = carrier
    ) |>
    arrange(desc(carriers))
  

flights |>
  count(tailnum, wt = distance, sort = TRUE)


# 13.3.1 - Exercises ------------------------------------------------------

# 1 - pair it with is.na()
# 2 - CODE
flights |>
  count(dest, sort = TRUE)

flights |>
  group_by(dest) |>
  summarize(
    n = n() 
  ) |>
  arrange(desc(n))

flights |>
  count(tailnum, wt = distance)

flights |>
  group_by(tailnum) |>
  summarize(
    sum = sum(distance),
    n = n()
  ) 


# 13.4 - Numeric Transformations ------------------------------------------
flights |> mutate(air_time = air_time / 60) |> relocate(air_time)
  
df <- tribble(
  ~x, ~y,
  1, 3,
  5, 2,
  7, NA
  )

df |>
  mutate(
    min = pmin(x, y, na.rm = TRUE),
    max = pmax(x, y, na.rm = TRUE)
  )
  
flights |>
  mutate(
    min = pmin(sched_dep_time, dep_time, na.rm = TRUE),
    max = pmax(sched_dep_time, dep_time, na.rm = TRUE)
  ) |>
  relocate(min, max)

flights |>
  mutate(
    hour = sched_dep_time %/% 100,
    minute = sched_dep_time %% 100,
    .keep = "used"
  )

flights |> 
  group_by(hour = sched_dep_time %/% 100) |> 
  summarize(prop_cancelled = mean(is.na(dep_time)), n = n()) |> 
  filter(hour > 1) |> 
  ggplot(aes(x = hour, y = prop_cancelled)) +
  geom_line(color = "grey50") + 
  geom_point(aes(size = n))

round(123.4567, digits = -1)

x <- c(1, 2, 5, 10, 15, 20)
cut(x, breaks = c(0, 5, 10, 15, 20))


# 13.4.8 - Exercises ------------------------------------------------------

# 1 -
# First we pipe flights in
# Then we group by a new "hour" column, which is the sched dep time divided by 100 with no remainder
# then we make a new row "prop_cancelled" which is the mean NAs in dep time, aka cancelled flights.
# Then filter everything after 1am.
# Plot the hour vs prop_cancelled
# Make a geom_line to connect everything
# then make a geom_point that gets bigger based on the amount of flights at that hour.

# 2 - Gives all the trig functions. They use radians

# 3 - CODE
flights |> 
  filter(month == 1, day == 1) |>
  mutate(sched_dep_time = round(sched_dep_time, -2)) |>
  ggplot(aes(x = sched_dep_time, y = dep_delay)) +
  geom_point(alpha = 0.4)

# 4 -
flights |>
  mutate(dep_time = round(dep_time / 5) * 5)

# 13.5 - General Transformations
xy <- c(1,4,3,6,5,9,7,8,2)
dense_rank(xy)


# 13.5.4 - Exercises ------------------------------------------------------

# 1 - CODE
flights|>
  filter(min_rank(desc(dep_delay))<=10)

# 2 - CODE
flights |>
  group_by(tailnum) |>
  summarize(
    avg_delay = mean(dep_delay),
    n = n()
  ) |>
  filter(n > 3) |>
  filter(min_rank(desc(avg_delay))<=1)

# 3 - CODE
flights |>
  group_by(hour = sched_dep_time %/% 100) |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  ) |>
  filter(hour > 1) |>
  ggplot(aes(x = hour, y = avg_delay)) +
  geom_point(aes(size = n)) +
  geom_smooth()

# 4 - CODE - row_number gives each input a unique rank. ties method = first
flights |>
  group_by(dest) |>
  filter(row_number() < 4)

# 5 - CODE
flights |>
  group_by(dest) |>
  summarize(
    total_delay = sum(dep_delay, na.rm = TRUE)
  ) |>
  filter(min_rank(desc(total_delay)) <=10)

# 6 - CODE
flights |> 
  mutate(hour = dep_time %/% 100) |> 
  group_by(year, month, day, hour) |> 
  summarize(
    dep_delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    lag = dep_delay - lag(dep_delay),
    .groups = "drop"
  ) |> 
  filter(n > 5)

# 8 - CODE
flights |>
  group_by(dest) |>
  summarize(
    distinct_carrier = n_distinct(carrier, na.rm = TRUE)
  ) |>
  filter(distinct_carrier > 1) |>
  arrange(desc(distinct_carrier)) |>
  print(n = Inf)


# 13.6 - Numeric Summaries ------------------------------------------------

flights |> 
  group_by(origin, dest) |> 
  summarize(
    distance_sd = IQR(distance), 
    n = n(),
    .groups = "drop"
  ) |> 
  filter(distance_sd > 0)

flights |>
  filter(dest == "ATL") |>
  relocate(dest, distance)


# 13.6.7 - Exercises -------------------------------------------------------
# 1 -
# median
# mean
# IQR
# Rank
# Flight time

# 2 - CODE - Houston.
flights |>
  group_by(dest) |>
  mutate(airspeed = distance / (air_time / 60)) |>
  relocate(airspeed) |>
  summarize( 
    distance_sd = IQR(airspeed, na.rm = TRUE)
  ) |>
  arrange(desc(distance_sd))

# 3 - the moved the airport one mile closer to JFK in march.
flights |>
  filter(dest == "EGE") |>
  filter(origin == "JFK") |>
  relocate(distance) |> 
  arrange(distance) |> 
  ggplot(aes(x = distance, fill = as.factor(month))) +
  geom_bar()

# Chapter 14 - Strings ----------------------------------------------------


# 14.2 - Creating a String ------------------------------------------------

string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'          
double_quote


# 14.2.4 - Exercises ------------------------------------------------------

string1 <- 'He said """That\'s amazing"'
string2 <- "\\a\\b\\c\\d"


# 14.3 - Creating many strings from data ----------------------------------

str_c("x", "y")

df <- tibble(name = c("Flora", "David", "Terra", NA))
df |> mutate(greeting = str_c("Hi ", name, "!"))

df <- tribble(
  ~ name, ~ fruit,
  "Carmen", "banana",
  "Carmen", "apple",
  "Marvin", "nectarine",
  "Terence", "cantaloupe",
  "Terence", "papaya",
  "Terence", "mandarin"
)


# 14.3.4 - Exercises ------------------------------------------------------

# 1 - CODE - str_c needs to be same length.
str_c("hi ", NA)
paste0("hi ", NA)

str_c(letters[1:2], letters[1:3])
paste0(letters[1:2], letters[1:3])

# 2 - paste0 does not offer a separator operator.

# 3 - CODE

str_c("The price of ", food, " is ", price)


# 14.4 - Extracting data from strings -------------------------------------

df1 <- tibble(x = c("a,b,c", "d,e", "f"))

df1 |>
  separate_longer_delim(x, delim = ",")

df1 |>
  separate_longer_position(x, width = 1)

df3 <- tibble(x = c("a10.1.2022", "b10.2.2011", "e15.1.2015"))

df3 |>
  separate_wider_delim(
    x,
    delim = ".",
    names = c("code", "edition", "year")
  )

df4 <- tibble(x = c("202215TX", "202122LA", "202325CA")) 

df4 |> 
  separate_wider_position(
    x,
    widths = c(year = 4, age = 2, state = 2)
  )


# 14.5 - Letters ----------------------------------------------------------

babynames |>
  count(length= str_length(name), wt = prop)


# 14.5.3 - Exercises ------------------------------------------------------

# 1 - Because it counts each row by whatever is specified at wt.
# 2 - CODE
babynames |>
  mutate(length = str_length(name)) |>
  mutate(middle = str_sub(name, (length %/% 2) + 1, (length %/% 2) + 1)) |>
  filter(length > 9) |>
  print (n = 30)
# 3 - CODE
babynames |>
  group_by(year) |>
  summarize(length = mean(str_length(name)))|>
  ggplot(aes(x = year, y = length)) +
  geom_point()

babynames |>
  group_by(year) |>
  summarize(
    first = str_sub(name, 1, 1),
    gender = toupper(sex)
  ) |>
  filter(year == 1880) |>
  ggplot(aes(x = first, fill = gender)) + 
    geom_bar()

babynames |>
  group_by(year) |>
  summarize(
    first = str_sub(name, 1, 1),
    last = str_sub(name, -1, -1)
  ) |>
  count(last) |>
  print(n = 27)


# 14.6 - Non English Text -------------------------------------------------

ascii <- c("!@#$rR6t?")


# Chapter 15 - Regular Expressions ----------------------------------------


# 15.2 - Pattern Basics ---------------------------------------------------

str_view(fruit, "berry")
str_view(words, "[h]el[p]")


# 15.3 - Key functions ----------------------------------------------------

babynames |>
  filter(year == 1888) |>
  filter(str_detect(name, "x")) |>
  count(name, wt = n, sort = TRUE)
  
babynames |>
  group_by(year) |>
  summarize(prop_x = mean(str_detect(name, "x"))) |>
  ggplot(aes(x = year, y = prop_x)) +
  geom_line()

str_count(fruit, "p")

babynames |>
  count(name) |>
  mutate(
    vowels = str_count(name, "[aeiou]"),
    consonants = str_count(name, "[^aeiou]")
  )

df <- tribble(
  ~str,
  "<Sheryl>-F_34",
  "<Kisha>-F_45", 
  "<Brandon>-N_33",
  "<Sharon>-F_38", 
  "<Penny>-F_58",
  "<Justin>-M_41", 
  "<Patricia>-F_84", 
)

df |>
  separate_wider_regex(
    str,
    patterns = c(
      "<",
      name = "[A-Za-z]+",
      ">-",
      gender = ".",
      "_",
      age = "[0-9]+"
    )
  )


# 15.3.5 - Exercises ------------------------------------------------------

# 1 - CODE 
babynames |>
  count(name) |>
  mutate(
  vowels = str_count(name, "[aeiou]"),
  consonants = str_count(name, "[^aeiou]")
  ) |>
  mutate(
    prop_vowel = vowels/str_length(name)
  ) |>
  arrange(desc(prop_vowel))
  
# 2 - CODE
c <- c("a/b/c/d/e")

str_replace_all(c, "/", "\\")


# 15.6 - Practice --------------------------------------------------

str_view(sentences, "^The\\b")

str_view(sentences, "(She|He|It|They)\\b")

str_view(words[!str_detect(words, "[aeiou]")])

babynames |>
  filter(str_detect(name, "Maria")) |>
  filter(str_length(name) > 7) |>
  count(name, wt = n, sort = TRUE) |>
  separate(
    name,
    into = c("Maria", "othername"),
    sep = 5
  ) |>
  print(n = Inf)


# Chapter 16 - Factors ----------------------------------------------------


# 16.2 - Factor Basics ----------------------------------------------------

x1 <- c("Dec", "Apr", "Jan", "Mar")
sort(x1)

month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

y1 <- factor(x1, levels = month_levels)
sort(y1)


# 16.3 - General Social Survey --------------------------------------------

gss_cat |>
  count(race)


# 16.3.1 - Exercises ------------------------------------------------------

# 1 - CODE - what does "not applicable" mean
gss_cat |>
  ggplot(aes(x = rincome)) +
  geom_bar() +
  coord_flip()

# 2 - CODE - Protestant; Independent
gss_cat |>
  count(relig, sort = TRUE)

gss_cat |>
  count(partyid, sort = TRUE)

# 3 - CODE - Protestant/Christian
gss_cat |>
  ggplot(aes(x = relig, fill = denom)) +
  geom_bar() +
  coord_flip()

gss_cat |>
  group_by(denom) |>
  count(relig) |>
  print(n = Inf)


# 16.4 - Modifying factor order -------------------------------------------

relig_summary <- gss_cat |>
  group_by(relig) |>
  summarize(
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig_summary, aes(x = tvhours, y = relig)) + 
  geom_point()

ggplot(relig_summary, aes(x = tvhours, y = fct_reorder(relig, tvhours))) +
  geom_point()

relig_summary |>
  mutate(
    relig = fct_reorder(relig, tvhours)
  ) |>
  ggplot(aes(x = tvhours, y = relig)) +
  geom_point()

rincome_summary <- gss_cat |>
  group_by(rincome) |>
  summarize(
    age = mean(age, na.rm = TRUE),
    n = n()
  )

ggplot(rincome_summary, aes(x = age, y = fct_reorder(rincome, age))) +
  geom_point(aes(size = n))


# 16.4.1 - Exercises ------------------------------------------------------

# 1 - median would be better.
# 2 -


# 16.5 - Modifying factor levels ------------------------------------------

gss_cat |>
  count(partyid)

gss_cat |>
  mutate(
    partyid = fct_recode(partyid,
                         "Republican, strong"    = "Strong republican",
                         "Republican, weak"      = "Not str republican",
                         "Independent, near rep" = "Ind,near rep",
                         "Independent, near dem" = "Ind,near dem",
                         "Democrat, weak"        = "Not str democrat",
                         "Democrat, strong"      = "Strong democrat"
    )
  ) |>
  count(partyid)

gss_cat |>
  mutate(
    partyid = fct_recode(partyid,
                         "Republican, strong"    = "Strong republican",
                         "Republican, weak"      = "Not str republican",
                         "Independent, near rep" = "Ind,near rep",
                         "Independent, near dem" = "Ind,near dem",
                         "Democrat, weak"        = "Not str democrat",
                         "Democrat, strong"      = "Strong democrat",
                         "Other"                 = "No answer",
                         "Other"                 = "Don't know",
                         "Other"                 = "Other party"
    )
  ) |>
  count(partyid)

gss_cat |>
  mutate(
    partyid = fct_collapse(partyid,
                           "other" = c("No answer", "Don't know", "Other party"),
                           "rep" = c("Strong republican", "Not str republican"),
                           "ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
                           "dem" = c("Not str democrat", "Strong democrat")
    )
  ) |>
  count(partyid)

gss_cat |>
  count(partyid)

gss_cat |>
  mutate(partyid = fct_lump_lowfreq(partyid)) |>
  count(partyid)


# 16.5.1 - Exercises ------------------------------------------------------

# 1 - CODE - spikes in 2006, especially independents.
gss_cat |>
  mutate(
    partyid = fct_collapse(partyid,
                           "other" = c("No answer", "Don't know", "Other party"),
                           "rep" = c("Strong republican", "Not str republican"),
                           "ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
                           "dem" = c("Not str democrat", "Strong democrat")
    )
  ) |>
  group_by(partyid) |>
  count(year) |>
  ggplot(aes(x = year, y = n, color = partyid)) +
  geom_line()

# 2 - 
gss_cat |>
  mutate(rincome = fct_lump_n(rincome, n = 5)) |>
  count(rincome)

gss_cat |>
  count(rincome)


# 16.6 - Ordered Factors --------------------------------------------------

ordered(c("a", "b", "c"))


# Chapter 17 - Dates and Times --------------------------------------------


# 17.2 - Creating date/times ----------------------------------------------

today()
now()

csv <- "
date
01/02/15
"

read_csv(csv, col_types = cols(date = col_date("%y/%m/%d")))

flights |>
  select(year, month, day, hour, minute) |>
  mutate(departure = make_datetime(year, month, day, hour, minute))

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights |> 
  filter(!is.na(dep_time), !is.na(arr_time)) |> 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) |> 
  select(origin, dest, ends_with("delay"), ends_with("time"))
# 17.2.5 - Exercises ------------------------------------------------------

# 1 - CODE - will get "failed to parse" warning.
ymd(c("2010-10-10", "bananas"))

# 2 - CODE
today("CST")

# 3 - CODE
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014
t1 <- "1705"
t2 <- "11:15:10.12 PM"

mdy(d1)
hm(t1)


# 17.3 - Date-Time components ---------------------------------------------

datetime <- ymd_hms("2026-07-08 12:34:56")
yday(datetime)

wday(datetime, label = TRUE)

flights_dt |>
  mutate(wday = wday(dep_time, label = TRUE)) |>
  ggplot(aes(x = wday)) +
  geom_bar()

flights_dt |> 
  count(week = floor_date(dep_time, "week")) 

flights_dt
  

# 17.4 - Time Spans ------------------------------------------------------

h_age <- today() - ymd("1998-07-19")
h_age
as.duration(h_age)
dyears()

one_am <- ymd_hms("2026-03-08 01:00:00", tz = "America/New_York")
one_am + days(1)

days(7)

days(50) + hours(25) + minutes(2)

flights_dt |>
  filter(arr_time < dep_time)

flights_dt <- flights_dt |> 
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight),
    sched_arr_time = sched_arr_time + days(overnight)
  )

flights_dt |>
  filter(arr_time < dep_time)

y2023 <- ymd("2023-01-01") %--% ymd("2024-01-01")
y2024 <- ymd("2024-01-01") %--% ymd("2025-01-01")

y2023/days(1)
y2024/days(1)


# 17.4.4 - Exercises ------------------------------------------------------

# 1 - that ! (not) operator 
# 2 - CODE
days(2015)

# 3 - CODE
f <- ymd("1998-07-19")
t <- today() 
total <- f %--% t
eep <- total/years(1)
floor(eep)

age <- function(x) {
  f <- ymd(x)
  t <- today() 
  total <- f %--% t
  eep <- total/years(1)
  eepy <- floor(eep)
  return(eepy)
}

age("1992-07-19")


# Chapter 18 - Missing Values ---------------------------------------------


# 18.2 - Explicit missing values ------------------------------------------

treatment <- tribble(
  ~person,           ~treatment, ~response,
  "Derrick Whitmore", 1,         7,
  NA,                 2,         10,
  NA,                 3,         NA,
  "Katherine Burke",  1,         4
)

treatment |>
  fill(everything())

x <- c(1, 4, 5, 7, NA)
coalesce(x, 0)


# 18.3 - Implicit Missing Values ------------------------------------------

# Q1 of 2021 is implicitly missing.
stocks <- tibble(
  year  = c(2020, 2020, 2020, 2020, 2021, 2021, 2021),
  qtr   = c(   1,    2,    3,    4,    2,    3,    4),
  price = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

stocks |>
  pivot_wider(
    names_from = qtr,
    values_from = price
  )

stocks |>
  complete(year, qtr)

stocks |>
  complete(year = 2019:2021)

# 1 - CODE
flights |>
  distinct(carrier) |>
  anti_join(planes)


# 18.4 - Factors and empty groups -----------------------------------------

health <- tibble(
  name   = c("Ikaia", "Oletta", "Leriah", "Dashay", "Tresaun"),
  smoker = factor(c("no", "no", "no", "no", "no"), levels = c("yes", "no")),
  age    = c(34, 88, 75, 47, 56),
)

health |>
  count(smoker, .drop = FALSE)

health |> 
  group_by(smoker, .drop = FALSE) |> 
  summarize(
    n = n(),
    mean_age = mean(age),
    min_age = min(age),
    max_age = max(age),
    sd_age = sd(age)
  )


# Chapter 19 - Joins ------------------------------------------------------


# 19.2 - Keys -------------------------------------------------------------

planes |>
  count(tailnum) 

planes |>
  filter(is.na(tailnum))

airports |>
  filter(alt < 5000) |>
  arrange(desc(alt)) |>
  print(n = 50)


# 19.2.4 - Exercises ------------------------------------------------------

# 1 - faa and origin are the connections.
# 2 - would connect via origin.
# 3 - CODE - daylight savings
weather |>
  count(year, month, day, origin, hour) |>
  arrange(desc(n))
# 4 - Add special dates, but use year, month and day to connect to other data frames.
# 5 - Primary key for People could be playerID, for Batting it's also playerID, same with Salaries. Very straightforward.


# 19.3 - Basic Joins ------------------------------------------------------


flights2 <- flights |> 
  select(year, time_hour, origin, dest, tailnum, carrier)
flights2

flights2 |>
  left_join(airlines)

flights2 |>
  left_join(weather |>
              select(origin, time_hour, temp, wind_speed))

flights2 |>
  left_join(planes |> select(tailnum, type, engines, seats)) |>
  count(carrier, wt = seats)

airports |> 
  semi_join(flights2, join_by(faa == dest))


# 19.3.4 - Exercises ------------------------------------------------------

# 1 - CODE - precipitation doesn't really cause delays, but wind speed does.

flights3 <- flights |> 
  select(year, month, day, time_hour, origin, dest, dep_delay)

flights3 |>
  left_join(weather) |>
  select(month, day, dep_delay, time_hour, precip, visib, wind_speed, wind_gust) |>
  group_by(month, day) |>
  summarise(
    delay_per_day = mean(dep_delay, na.rm = TRUE),
    precip = mean(precip), 
    visib = mean(visib),
    wind_speed = mean(wind_speed),
    wind_gust = mean(wind_gust, na.rm = TRUE)
  ) |>
  arrange(desc(precip)) |>
  print(n = 30) |>
  ggplot(aes(x = wind_gust, y = delay_per_day)) +
  geom_point() +
  geom_smooth(method = "lm")

# 2 - CODE
top_dest <- flights2 |>
  count(dest, sort = TRUE) |>
  head(10)

top_dest |>
left_join(flights)

# 3 - CODE - not necessarily
is.na(weather)

# 7 - CODE
airports |>
  semi_join(flights, join_by(faa == dest)) |>
  left_join(dest = faa)
  ggplot(aes(x = lon, y = lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

avg_delay <- flights |>
  group_by(dest) |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
  ) |>
  inner_join(airports, by = c("dest" = "faa"))

avg_delay  %>%
  ggplot(aes(lon, lat, colour = avg_delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()+
  ylab("latitude")+
  xlab("longitude")+
  ggtitle("Average Delay by Arrival Times")


# 19.5 - Non-equi joins ---------------------------------------------------

df <- tibble(name = c("ACACA", "ACLY", "ACO2", "AHCY", "ALDOA", "CS", "DHFR2", "DLAT", "DLD", "DLST", "ENO1", "FASN", "GAPDH", "GPI", "HK2", "IDH3A", "MAT2A", "MDH2", "MTHFD1", "OGDH", "PDHA1", "PDHB", "PFKP", "PGK1", "PKM", "SDHA", "SDHAF2", "SDHB", "SDHC", "SDHD", "SLC25A1"))
df |> cross_join(df)
