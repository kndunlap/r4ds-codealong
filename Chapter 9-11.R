
# Chapter 9 ---------------------------------------------------------------


# 9.2 - Aesthetic Mappings ------------------------------------------------------
ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, shape = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, size = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, alpha = class)) +
  geom_point()


# 9.2.1 - Exercises -------------------------------------------------------

# 1. - CODE
ggplot(mpg, aes(x = hwy, y = displ)) + geom_point(color = "pink", shape = "triangle")

# 2. - CODE - because it looks for blue in the color column, and color should be set in the geom
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy, color = "blue"))

# 3. - Stroke controls the border of the filled shapes.
# 4. - It will fill all display less than 5 with one color, and anything above it with a different color. Useful!!


# 9.3 - Geometric Objects -------------------------------------------------

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class)) + 
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_area() + 
  geom_point(
    data = mpg |> filter(class == "2seater"), 
    shape = "circle open", size = 3, color = "red"
  )

library(ggridges)

ggplot(mpg, aes(x = hwy, y = drv, fill = drv, color = drv)) +
  geom_density_ridges(alpha = 0.5, show.legend = FALSE)


# 9.3.1 - Exercises -------------------------------------------------------

# 1 - geom_smooth, geom_boxplot, geom_histogram, geom_area

# 2 - Code - If you remove it the legend shows up.
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(color = drv), show.legend = TRUE)

# 3 - se displays the confidence interval around smooth.

# 4 - Codes

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 5) +
  geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 5) +
  geom_smooth(se = FALSE, aes(group = drv))

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(size = 3) +
  geom_smooth(se = FALSE, aes(group = drv, color = drv))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 3, aes(color = drv)) +
  geom_smooth(se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 3, aes(color = drv)) +
  geom_smooth(se = FALSE, aes(linetype = drv))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 4, color = "white") +
  geom_point(aes(color = drv)) 


# 9.4 - Facets ------------------------------------------------------------
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_grid(drv ~ cyl, scales = "free")

# 9.4.1 - Exercises -------------------------------------------------------

# 1. - You get a ton of graphs, but the computer treats it as discrete.
# 2. - It means one of the axes doesn't have data to match the other. Aes should be different from the facet wrap.
# 3. - The dot seems kind of pointless. Means keep everything together.
# 4. - 
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(~cyl)
# 5. - nrow and ncol set the numbers of rows and columns in the grid.
# facet grid can't because it's set by the input.

# 6. - CODE - I like the top one more.
ggplot(mpg, aes(x = cty)) + 
  geom_histogram() + 
  facet_grid(cyl ~ .)

ggplot(mpg, aes(x = displ)) + 
  geom_histogram() +
  facet_grid(. ~ drv)

# 7.- CODE
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) +
  facet_wrap(~drv, nrow = 3)

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)


# 9.5 - Statistical Transformations ---------------------------------------

ggplot(diamonds, aes(x = cut, y = depth)) +
  geom_col()

diamonds |>
  count(cut) |>
  ggplot(aes(x = cut, y = n)) +
  geom_bar(stat = "identity")

ggplot(diamonds, aes(x = cut, y = after_stat(prop), group = 1)) + 
  geom_bar()


# 9.5.1 - Exercises -------------------------------------------------------

# 1 - geom_pointrange
# 2 - geom_col plots bar heights to represent values in the data
# 3 - A lot of them.
# 4 - stat_smooth computes the inner workings of geom_smooth, including the error.
# 5 - 
ggplot(diamonds, aes(x = cut, y = after_stat(prop))) + 
  geom_bar()
ggplot(diamonds, aes(x = cut, fill = color, y = after_stat(prop))) + 
  geom_bar()


# 9.6 - Position Adjustments ----------------------------------------------

ggplot(mpg, aes(x = drv, color = class)) + 
  geom_bar()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_jitter()


# 9.6.1 - Exercises -------------------------------------------------------
# 1 - Fix the overlaps with geom_jitter()
# 2 - No difference. The dots are already where they should be.
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_count()
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_(position = "identity")
# 3 - width and height adjust the spot of the point.
# 4 - Geom count actually uses a value within the dataset, aka how many points are there. Jitter just moves them
# 5 - dodge2
ggplot(mpg, aes(x = cty, y = displ)) +
  geom_boxplot()

ggplot(mpg, aes(x = cty, y = displ)) +
  geom_boxplot(position = "identity")


# 9.7 - Coordinate Systems ------------------------------------------------
nz <- map_data("nz")

ggplot(nz, aes(x = long, y = lat, group = group))+
  geom_polygon(fill = "white", color = "black") +
  coord_quickmap()

bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = clarity, fill = clarity), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1)

bar + coord_flip()
bar + coord_polar()


# 9.7.1 - Exercises -------------------------------------------------------
#1 - CODE
ggplot(mpg, aes(x = drv, fill = class)) +
  geom_bar() + coord_polar()
# or 
ggplot(diamonds, aes(x = "", fill = cut)) +
  geom_bar() + 
  coord_polar(theta = "y")

# 2 - quickmap is better scaled.
# 3 - cty mpg is linear up with hwy mpg.
# abline makes a line at y = x
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() 


# Chapter 10 - Exploratory Data Analysis ----------------------------------


# 10.3 - Variation -----w---------------------------------------------------

ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.5)


# 10.3.3 - Exercises ------------------------------------------------------

# 1 - CODE x is very similar to y, z is different.
diamonds |>
  select(x, y, z) |>
  summarize(
    mean_x = mean(x),
    mean_y = mean(y),
    mean_z = mean(z)
  )

x <- diamonds |>
  filter(x < 10) |>
  ggplot(aes(x = x)) + 
  geom_histogram(binwidth = 0.1)

y <- diamonds |>
  filter(y < 10) |>
  ggplot(aes(x = y)) + 
  geom_histogram(binwidth = 0.1) 

z <- diamonds |>
  filter(z < 10) |>
  ggplot(aes(x = z))  + 
  geom_histogram(binwidth = 0.1) +
  xlim(0, 10)


x + y + z + plot_layout(ncol = 1)

# 2 - CODE - most diamonds are cheap. (like, really cheap).
diamonds |>
  ggplot(aes(x = price)) + 
  geom_histogram(binwidth = 10)

# 3 - CODE - "1" makes more sense than "0.99" for a carat
diamonds |>
  filter(carat == .99 | carat == 1) |>
  group_by(carat) |>
  count()

# 4 - CODE - don't mess around too much or R gets mad.
diamonds |>
  ggplot(aes(x = price)) + 
  geom_histogram() +
  xlim(0, 7500)


# 10.4 - Unusual Values ---------------------------------------------------
diamonds2 <- diamonds |>
  filter(between(y, 3, 20))

diamonds2 <- diamonds |>
  mutate(y = if_else( y < 3 | y > 20, NA, y))

ggplot(diamonds2, aes(x = x, y = y)) +
  geom_point(na.rm = TRUE)

flights |>
  mutate(cancelled = is.na(dep_time),
         sched_hour = sched_dep_time %/% 100,
         sched_min = sched_dep_time %% 100,
         sched_dep_time = sched_hour + (sched_min / 60)
  ) |> 
  ggplot(aes(x = sched_dep_time)) + 
  geom_freqpoly(aes(color = cancelled), binwidth = 1/4)


# 10.4.1 - Exercises ------------------------------------------------------

# 1 - CODE - Both of them remove from plotting.
ggplot(diamonds2, aes(x = y)) +
  geom_histogram()

# 2 - NA.rm will remove values so you can actually calculate the mean or sum.
diamonds2 |>
  reframe(
    y2 = sum(y)
  )

# 3 - CODE - "free" scales makes it better.
flights |>
  mutate(cancelled = is.na(dep_time),
         sched_hour = sched_dep_time %/% 100,
         sched_min = sched_dep_time %% 100,
         sched_dep_time = sched_hour + (sched_min / 60)
  ) |> 
  ggplot(aes(x = sched_dep_time)) + 
  geom_freqpoly(aes(color = cancelled), binwidth = 1/4) +
  facet_wrap(~cancelled, scales = "free")


# 10.5 - Covariation ------------------------------------------------------
# seeing how diamond price varies with quality
ggplot(diamonds, aes(x = price, y = after_stat(density))) +
  geom_freqpoly(aes(color = cut), binwidth = 500, linewidth = 0.75)

ggplot(diamonds, aes(x = cut, y = price)) +
  geom_boxplot()

ggplot(mpg, aes(x = fct_reorder(class, hwy, median), y = hwy)) +
  geom_boxplot()


# 10.5.1.1 - Exercises ----------------------------------------------------

# 2 - CODE - lower quality diamonds are often higher carat
glimpse(diamonds)

ggplot(diamonds, aes(x = price, y = carat)) +
  geom_point()

ggplot(diamonds, aes(x = price, y = clarity)) + 
  geom_boxplot()

ggplot(diamonds, aes(x = carat, y = cut)) +
  geom_boxplot()

ggplot(diamonds, aes(x = x, y = price)) + 
  geom_point()

ggplot(diamonds, aes(x = cut, y = carat)) +
  geom_boxplot() 

# 3 - It doesn't
# 4 - CODE - Good, shows more deciles.
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_lv()
# 5 - CODE - my favorite is the faceted histogram.
p1 <- ggplot(diamonds, aes(x = price, y = cut)) +
  geom_violin()
p1

p2 <- ggplot(diamonds, aes(x = price)) +
  geom_histogram() +
  facet_wrap(~cut)
p2

p3 <- ggplot(diamonds, aes(x = price, color = cut)) +
  geom_freqpoly()
p3

p4 <- ggplot(diamonds, aes(x = price, color = cut)) +
  geom_density()
p4

p1 + p2 + p3 + p4

# 6 - CODE - ggbeeswarm makes geom_jitter "less random"


# 10.5.2 - Two Categorical Variables --------------------------------------

ggplot(diamonds, aes(x = cut, y = color)) +
  geom_count()

diamonds |>
  count(color, cut) |>
  ggplot(aes(x = color, y = cut)) +
  geom_tile(aes(fill = n))


# 10.5.2.1 - Exercises ----------------------------------------------------

# 1 - Change the scale to be different colors, not different shades of the same color.
# 2 - CODE - weird but ok.
diamonds |>
  count(color, cut) |>
  ggplot(aes(x = clarity, y = cut)) +
  geom_bar()

# 3 - some good ideas that I didn't think of.
flights |> 
  group_by(month, dest) |>
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE)) |>
  ggplot(aes(x = month, y = dest, color = avg_delay)) + 
  geom_tile() + 
  scale_fill_viridis_c()

flights %>%
  group_by(month, dest) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  group_by(dest) %>%
  filter(n() == 12) %>%
  ungroup() %>%
  mutate(dest = reorder(dest, dep_delay)) %>%
  ggplot(aes(x = factor(month), y = dest, fill = dep_delay)) +
  geom_tile() +
  scale_fill_viridis_b() +
  labs(x = "Month", y = "Destination", fill = "Departure Delay")


# 10.5.3 - Two numerical variables ----------------------------------------
smaller <- diamonds |> 
  filter(carat < 3)
library(tidyverse)
ggplot(smaller, aes(x = carat, y = price)) +
  geom_point(alpha = 1 / 100)

ggplot(smaller, aes(x = carat, y = price)) +
  geom_bin2d()
install.packages("hexbin")
library(hexbin)
ggplot(smaller, aes(x = carat, y = price)) +
  geom_hex()

ggplot(smaller, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.01, varwidth = TRUE)))


# 10.5.3.1 - Exercises ----------------------------------------------------

# 1 - CODE - can't get cut_number to work
ggplot(smaller, aes(x = carat)) +
  geom_freqpoly(cut_number(diamonds, n = 3))

# 2 - CODE
ggplot(smaller, aes(x = carat, y = price)) +
  geom_bin2d()

# 3 - CODE - not surprising
ggplot(smaller, aes(x = carat, y = price)) +
  geom_point()

# 4 - CODE
ggplot(smaller, aes(x = carat, y = price)) +
  geom_point(color = cut)

# 5 - CODE - Can't really see outliers in the binned plots.
diamonds |> 
  filter(x >= 4) |> 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

# 6 - CODE - can skew things out of proportion, but maybe that's what you want.
ggplot(smaller, aes(x = carat, y = price)) + 
  geom_boxplot(aes(group = cut_number(carat, 20)))

diamonds |>
  group_by(carat) |>
  count() |> 
  arrange(n)


# 10.6 - Patterns and Models ----------------------------------------------


# Chapter 11 - Communication --------------------------------------------------------------
install.packages("scales", "ggrepel", "patchwork")
library(scales)
library(ggrepel)
library(patchwork)


# 11.2 - Labels -----------------------------------------------------------

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    color = "Car type",
    title = "Fuel efficiency generally decreases with engine size",
    subtitle = "Two seaters (sports cars) are an exception because of their light weight",
    caption = "Data from fueleconomy.gov"
  )


# 11.2.1 - Exercises ------------------------------------------------------

# 1 - CODE
ggplot(mpg, aes(x = cyl, y = displ)) +
  geom_point() +
  labs(
    x = "Cylinders",
    y = "Display",
    title = "Cylinders vs Display",
    caption = "More Cylinders",
    subtitle = "Even more cylinders",
    color = "Color Time"
  )

# 2 - CODE
ggplot(mpg, aes(x = cty, y = hwy, shape = drv, color = drv)) +
  geom_point() + 
  labs(
    x = "City MPG",
    y = "Highway MPG",
    color = "Type of drive train",
    shape = "Type of drive train"
  )


# 11.3.1 - Exercises ------------------------------------------------------
# 1- CODE (very annoying)
annot <- data.frame(
  text <- "Hello",
  X = c(-Inf,-Inf,Inf,Inf),
  Y =  c(-Inf, Inf,-Inf,Inf),
  hjustvar = c(0,0,1,1),
  vjustvar = c(0,1,0,1))
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() + 
  labs(
    x = "City MPG",
    y = "Highway MPG",
    color = "Type of drive train",
    shape = "Type of drive train"
  ) +
  geom_text(data = annot, aes(x = X, y = Y, hjust = hjustvar, vjust = vjustvar, label = text))

# 2 - CODE
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() + 
  labs(
    x = "City MPG",
    y = "Highway MPG",
    color = "Type of drive train",
    shape = "Type of drive train"
  ) +
  geom_text(data = annot, aes(x = X, y = Y, hjust = hjustvar, vjust = vjustvar, label = text)) +
  annotate(
    geom = "point", x = 22.5, y = 30,
    color = "red", size = 15
  ) +
  facet_wrap(~cyl)

# 3 - CODE - not sure!
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() + 
  labs(
    x = "City MPG",
    y = "Highway MPG",
    color = "Type of drive train",
    shape = "Type of drive train"
  ) +
  geom_text(data = annot, aes(x = X, y = Y, hjust = hjustvar, vjust = vjustvar, label = text)) +
  annotate(
    geom = "point", x = 22.5, y = 30,
    color = "red", size = 15
  ) +
  facet_wrap(~cyl)

# 4 - padding, size.
# 5 - angle, length, end, type.
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() + 
  labs(
    x = "City MPG",
    y = "Highway MPG",
    color = "Type of drive train",
    shape = "Type of drive train"
  ) +
  annotate(
    geom = "point", x = 22.5, y = 30,
    color = "red", size = 15
  ) + 
  geom_segment(aes(x= 22, y = 28.5, xend = 5, yend = 6),
               arrow = arrow(length = unit(0.19, "cm"), ends = "first", angle = 2))


# 11.4 - Scales -----------------------------------------------------------

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  scale_y_continuous(labels = NULL) 

ggplot(diamonds, aes(x = price, y = cut)) +
  geom_boxplot(alpha = 0.05) +
  scale_x_continuous(
    labels = label_dollar(scale = 1/1000, suffix = "K"), 
    breaks = seq(1000, 19000, by = 4000)
  )

presidential |>
  mutate(id = 33 + row_number()) |>
  ggplot(aes(x = start, y = id)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_x_date(name = NULL, breaks = presidential$start, date_labels = "'%y")

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, override.aes = list(size = 10, alpha = 1)))


# 11.4.6 - Exercises ------------------------------------------------------

# 1 - CODE - needs to be scale_fill not scale_color.
df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)

ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed()

# 2 - CODE - Not sure... seems like it's "name"
# 3 - CODE
presidential2 <- presidential |> 
  mutate(id = 33 + row_number()) |>
  mutate(Startyear = year(as.Date(start))) |>
  mutate(Endyear = year(as.Date(end)))

presidential <- presidential

ggplot(presidential2, aes(x = Startyear, y = id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = Endyear, yend = id)) +
  scale_color_manual(values = c(Republican = "#E81B23", Democratic = "#00AEF3")) +
  geom_text(
    data = presidential2,
    aes(x = Startyear, y = id, label = name),
    fontface = "bold", size = 3, hjust = "left", vjust = "bottom") +
  scale_x_continuous(breaks = seq(1950, 2020, by = 4)) +
  labs(
    title = "Presidents from Ike to Don",
    subtitle = "Who's your favorite?"
  )

# 4 - CODE
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(aes(color = cut), alpha = 1/20) + 
  guides(color = guide_legend(override.aes = list(alpha = 1)))


# 11.5 - Themes -----------------------------------------------------------

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_bw()


# 11.5.1 - Exercises ------------------------------------------------------

# 1 - Done already
# 2 - CODE
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  theme(axis.title.x = element_text(color = "blue", face = "bold")) +
  theme(axis.title.y = element_text(color = "blue", face = "bold"))

# 11.6 - Layout
p1 <- ggplot(mpg, aes(x = drv, y = cty, color = drv)) + 
  geom_boxplot(show.legend = FALSE) + 
  labs(title = "Plot 1")

p2 <- ggplot(mpg, aes(x = drv, y = hwy, color = drv)) + 
  geom_boxplot(show.legend = FALSE) + 
  labs(title = "Plot 2")

p3 <- ggplot(mpg, aes(x = cty, color = drv, fill = drv)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Plot 3")

p4 <- ggplot(mpg, aes(x = hwy, color = drv, fill = drv)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Plot 4")

p5 <- ggplot(mpg, aes(x = cty, y = hwy, color = drv)) + 
  geom_point(show.legend = FALSE) + 
  facet_wrap(~drv) +
  labs(title = "Plot 5")

(guide_area() / (p1 + p2) / (p3 + p4) / p5) +
  plot_annotation(
    title = "City and highway mileage for cars with different drive trains",
    caption = "Source: https://fueleconomy.gov."
  ) +
  plot_layout(
    guides = "collect",
    heights = c(1, 3, 2, 4)
  ) &
  theme(legend.position = "top")


# 11.6.1 - Exercises ------------------------------------------------------

# 1 - CODE - order of operations comes into play
p1 <- ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  labs(title = "Plot 1")
p2 <- ggplot(mpg, aes(x = drv, y = hwy)) + 
  geom_boxplot() + 
  labs(title = "Plot 2")
p3 <- ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_point() + 
  labs(title = "Plot 3")

(p1 | p2) / p3

# 2 - CODE - not sure, this actually seems hard.
p1 / (p2 | p3)
