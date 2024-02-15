### 3.1.3 Example ### - Says it takes the flights that arrive
# at IAH, sorts them by year then month then day,
# and then calculates the mean of the arrival delay.
flights |>
  filter(dest == "IAH") |> 
  group_by(year, month, day) |> 
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

### 3.2.1 ### - filter
## The below command allows you to keep rows based on the values of the columns - all flights that departed 120 min late.
flights |>
  filter(dep_delay > 120)
# Flights that departed on Jan 1
flights |>
  filter(month == 1 & day == 1)
# flights that departed in Jan or Feb
flights |>
  filter(month == 1 | month == 2)
# Better code
flights |>
  filter(month %in% c(1,2))

### 3.2.3 ###  - arrange
# order by year then month then day then dep_time
flights |> 
  arrange(year, month, day, dep_time)
# order big to small
big <- flights |>
  arrange(desc(dep_delay))

### 3.2.4 ### - distinct
# remove duplicate rows if any
flights |>
  distinct()
# find all unique origin and dest pairs
# keep all keeps the other columns
flights |>
  distinct(origin, dest, .keep_all = TRUE)
# adds an "n" - can pipe to show how many fly to Detroit and were delayed over 60 minutes.
flights |>
  filter(dest == "DTW" & arr_delay > 60) |>
  count(origin, dest, sort = TRUE) 
  

### 3.2.5 Exercises ###
# 1 - Code
#a
flights |>
  filter(arr_delay >= 120)
#b
flights |>
  filter(dest %in% c("IAH", "HOU")) 
#c
flights |>
  filter(carrier == "UA" | carrier == "AA" | carrier == "DL")
#d
flights |>
  filter(month %in% c(7,8,9))
#e
flights |>
  filter(arr_delay >= 120, dep_delay <= 0)
#f
flights |>
  filter(dep_delay >= 60 & dep_delay - arr_delay > 30)
# 2 - CODE
flights |>
  arrange(desc(dep_delay), day) 
# 3 - CODE
flights |>
  arrange(air_time) |>
  relocate(air_time)
# or 
flights |> 
  mutate(speed = distance / (air_time / 60)) |>
  arrange(desc(speed)) |>
  relocate(speed)
# 4 - CODE - answer is yes - can pipe into nrow for an answer
flights |>
  distinct(month, day)
# 5 - CODE - JFK to HNL was the longest (4983 miles) - EWR to LGA was the shortest (17 miles)
flights |>
  arrange(desc(distance)) |>
  relocate(distance, origin, dest)
flights |>
  arrange(distance) |>
  relocate(distance, origin, dest)
# 6 - it would matter because your data might disappear.

### 3.3.1 ### - mutate
# adds two new columns - gain and speed.
flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = month
  )
# keeps only the columns related to the new columns and makes 3 new columns. Cool!
flights |>
  mutate(gain = dep_delay - arr_delay,
         hours = air_time / 60,
         gain_per_hour = gain / hours,
         .keep = "used",
         .before = dep_delay)
# how would I find out which route had the most gain on avg?

### 3.3.2 ### - select (columns)
# select columns by colname
flights |>
  select(year, month,day)
# select all columns between year and day
flights |>
  select(year:day)
# select all columns except those from year to day
flights |>
  select(!year:day)
# select all columns that are characters
flights |>
  select(where(is.character))
# rename a column
flights |>
  select(tail_num = tailnum)

### 3.3.3 ### - rename
# renames tailnum to tail_num
flights |>
  rename(tail_num = tailnum)

### 3.3.4 ### - relocate
# moves time_hour and air_time to the front.
flights |>
  relocate(time_hour, air_time)
# specify where to put them, this one puts year, month, day and dep_time right after arr_time
flights |>
  relocate(year:dep_time, .after = arr_time)
# this one puts ones that start with "arr" before dep_time
flights |>
  relocate(starts_with("arr"), .before = dep_time)

### 3.3.5 - exercises ###
# 1. I would expect sched_dep_time to be the difference between dep_time and dep_delay
# 2. You could do select(4 names), starts_with(dep | arr), select(dep_time:arr_delay, -contains("sched"))
# 3. You only get it once.
# 4. It contains any of those strings. Since it's stored in a vector you can just plug in variables after any_of
# 5 - CODE - surprises me because it matches case. contain ignores case by default
flights |>
  select(contains("TIME"))
# 6 - CODE
flights |>
  rename(air_time_min = air_time) |>
  relocate(air_time_min)
# 7 - because arr_delay is no longer in the dataset.

### 3.5.1 ### - group_by
# groups by month
flights |>
  group_by(month)
### 3.5.2 ### - summarize
# finds average delay by month and removes NA, adds nrow
flights |>
  group_by(month) |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    nrow = n()
  )
### 3.5.3 ### - slice
# following code finds flights that are most delayed
# slice max is important because it finds the representative from each dest.
flights |>
  group_by(dest) |>
  slice_max(arr_delay, n = 1) |>
  relocate(dest)
### 3.5.4 ### - grouping by multiple variables
daily <- flights |>
  group_by(year, month, day)
daily
### 3.5.5 ### - ungrouping
daily |>
  ungroup() |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    flights = n()
  )
### 3.5.6 ### - .by
flights |>
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = carrier
  )
### 3.5.7 Exercises ###
# 1a - CODE
flights |>
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = carrier
  ) |>
  arrange(desc(delay))
# 1b - CODE - tough one?creative
flights |>
  group_by(carrier, dest) |>
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  )
# 2 - CODE - not perfect but it answers what I want, which is the longest delay from each dest.
flights |>
  group_by(dest, carrier) |>
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  ) |>
  slice_max(delay, n = 1) 
# 3 - CODE
flights |>
  group_by(hour) |>
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  ) |>
ggplot(aes(x = hour, y = delay)) + 
  geom_smooth()
# 4 - it doesn't slice the dataframe.
# 5 - it counts how many rows are in each "group" you want to count by.
# the sort argument shows the largest groups at the top if TRUE.
# 6a - I think it will look like
#1 2 3 4 5
#a b a a b
#K K L L K
#group by will divide it into 2 groups a and b
df <- tibble(
  x = 1:5,
   y = c("a", "b", "a", "a", "b"),
   z = c("K", "K", "L", "L", "K")
)
df |>
  group_by(y)
#6b arrange actually will move stuff around, that is why it's different than group by
df |>
  arrange(y)
#6c I think this pipeline will group by y then take the mean of x in each group (8/3, 7/2)
df |>
  group_by(y) |>
  summarize(mean_x = mean(x))
#6d It will group by Y then Z, 3 groups total means 1, 3.5, 3.5
df |>
  group_by(y,z) |>
  summarize(mean_x = mean(x))
#6e all levels of grouping will be dropped.
df |>
  group_by(y,z) |>
  summarize(mean_x = mean(x), .groups = "drop")
#6f I think mutate will add a new column on.
df |>
  group_by(y,z) |>
  summarize(mean_x = mean(x))

df |>
  group_by(y,z) |>
  mutate(mean_x = mean(x))


### 3.6 ### - case study
batters <- Lahman::Batting |>
  group_by(playerID) |>
  summarize(
    performance = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    n = sum(AB, na.rm = TRUE)
  )
  
batters |> 
  filter(n > 100) |> 
  ggplot(aes(x = n, y = performance)) +
  geom_point(alpha = 1 / 10) + 
  geom_smooth(se = FALSE)


#try and answer the question: What route had the most average gain?
h <- flights |>
  group_by(origin,dest) |>
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours
  ) |>
  summarize(
    meangain = mean(gain_per_hour, na.rm = TRUE)
  ) |>
  arrange(desc(meangain))
h

### Practice Problems
#1. What destination received the most flights in June? ORD
flights |>
  filter(month == 6) |>
  group_by(dest) |>
  summarize(
    n = n()
  ) |>
  arrange(desc(n))
#2. Which carrier had the greatest average distance per flight? HA
flights |>
  group_by(carrier) |>
  summarize(
    meandist = mean(distance, na.rm = TRUE)
  ) |>
  arrange(desc(meandist))
#3. Which flight traveled the fastest? (mph) Flight 1499 LGA to ATL 5/25/13
flights |>
  mutate(
    speed = (distance / air_time) * 60
  ) |>
  relocate(flight, speed, origin, dest) |>
  arrange(desc(speed))
#4. What day had the largest average arrival delay for all flights? July 10
flights |>
  filter(arr_delay > 0) |>
  group_by(year, month, day) |>
  summarize(
    mean_delay = mean(arr_delay, na.rm = TRUE)
  ) |>
  arrange(desc(mean_delay))
#5a. What was the total distance for all flights in January? 27188805 miles
flights |>
  filter(month == 1) |>
  summarize(
    total = sum(distance)
  )
#5b. What was the average distance per flight? 1007 miles per flight.
flights |>
  filter(month == 1) |>
  summarize(
    total = sum(distance),
    cols = sum(month),
    avg_dist = total / cols
  )
#6. What day of the week saw the most flights? (needed help) Monday 50690
flights |>
  count(weekdays(time_hour))


# Chapter 4 ---------------------------------------------------------------


# 4.6 - Exercises ---------------------------------------------------------

flights|>
  filter(dest=="IAH")|>
  group_by(year,month,day)|>
  summarize(n=n(), delay=mean(arr_delay,na.rm=TRUE))|>
  filter(n>10)

flights|>
  filter(
    carrier == "UA",
    dest %in% c("IAH","HOU"),
    sched_dep_time > 0900,
    sched_arr_time < 2000)|>
    group_by(flight)|> 
  summarize(
    delay=mean(arr_delay,na.rm=TRUE),
            cancelled=sum(is.na(arr_delay)),
            n=n())|>
  filter(n>10)
