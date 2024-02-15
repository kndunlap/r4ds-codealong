### 1.2.3 creating a ggplot ###

# Layer 1 - base
ggplot(data = penguins)

# Layer 2 - defining axes
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g))

# Layer 3 - geometric object that a plot uses to represent data
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) + geom_point()

### 1.2.4 ### Adding aesthetics and layers ###

# Layer 4 - adding color to "species" to see what role species plays.
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)) + geom_point()

# Layer 5 - a line for each species based on a linear model.
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)) + geom_point() + geom_smooth(method = "lm")

# Layer 6 - a line overall for everything.
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) + geom_point(mapping = aes(color = species)) + geom_smooth(method = "lm")

# Layer 7 - adding shapes to each group.
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) + geom_point(mapping = aes(color = species, shape = species)) + geom_smooth(method = "lm")

# Layer 8 - adding labels to each axis. (color = and shape = defines the label for the legend)
ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) + geom_point(mapping = aes(color = species, shape = species)) + geom_smooth(method = "lm") + labs(title = "Body mass and flipper length", subtitle = "Dimensions for Adelie, Chinstrap and Gentoo Penguins", x = "Flipper length (mm)", y = "Body mass (g)", color = "Species", shape = "Species") + scale_color_colorblind()

### 1.2.5 Exercises ###
# 1. 344 rows, 8 columns.
# 2. A number denoting the bill depth. Can use ?penguins to find out about the dataset.
# 3 - CODE
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) + geom_point()
# There seems to be a correlation between bill length and depth but separated out by species.
# 4 - CODE
ggplot(penguins, aes(x = bill_depth_mm, y = species)) + geom_point()
# A Geom bar might be better.
# 5 - I would add aesthetic mappings so they know what variables to use.
# 6 - CODE
ggplot(penguins, aes(x = bill_depth_mm, y = species)) + geom_point(na.rm = TRUE)
# Default value of na.rm is FALSE.
# 7 - CODE
ggplot(penguins, aes(x = bill_depth_mm, y = species)) + geom_point(na.rm = TRUE) + labs(caption = "Data came from the palmerpenguins package")
# Note - caption is different than title.
# 8 - CODE
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) + geom_point(mapping = aes(color = bill_depth_mm)) + geom_smooth()
# The bill_depth_mm variable needs to be mapped locally so it doesn't impact the coloring of the line.
# 9 - CODE
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE)
# 10 - No, the second graph just calls locally while the first code is more efficient.

### 1.4.1 ###
# Categorial variable #
ggplot(penguins, aes(x = species)) + geom_bar()

# Ordering it #
ggplot(penguins, aes(x = fct_infreq(species))) + geom_bar()

### 1.4.2 ### 
# Numerical variable #
ggplot(penguins, aes(x = body_mass_g)) + geom_histogram(binwidth = 200)

ggplot(penguins, aes(x = body_mass_g)) + geom_density()

### 1.4.3 Exercises ### 
# 1 - CODE
ggplot(penguins, aes(y = species)) + geom_bar()
# This is different because it's sideways.
# 2 - fill colors the inside- use this.
# 3 - This is the amount of "x axis units" there are. Or better put, how many bars.
# 4 - CODE
ggplot(diamonds, aes(x = carat)) + geom_histogram(binwidth = 1)
# 50 binds is pretty good.

### 1.5.1 ###
ggplot(penguins, aes(x = species, y = body_mass_g)) + geom_boxplot()
ggplot(penguins, aes(x = body_mass_g, color = species)) + geom_density(linewidth = 0.75)
ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) + geom_density(alpha = 0.01)

### 1.5.2 ###
ggplot(penguins, aes(x = island, fill = species)) + geom_bar()
ggplot(penguins, aes(x = island, fill = species)) + geom_bar(position = "fill")

### 1.5.3 ###
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) + geom_point()

### 1.5.4 ###
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) + geom_point(aes(color = species, shape = island))
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) + geom_point(aes(color = island)) + facet_wrap(~species)

### 1.4.3 Exercises ### 
# 1. Categorical: Manufacturer, model, trans, drv, fl, class
# Numerical: displ, year, cyk, cty, hwy
# 2 - CODE - a numerical variable doesn't work with shape, but categorical does.
ggplot(mpg, aes(x = hwy, y = displ)) + geom_point(aes(shape = trans))
# 3 - CODE - Nothing happens, there is no line.
ggplot(mpg, aes(x = hwy, y = displ)) + geom_point() + geom_smooth(aes(linewidth = cyl))
# 4 - CODE - the same variable mapped to different aesthetics makes them live on top of each other.
ggplot(mpg, aes(x = hwy, y = displ)) + geom_point(aes(shape = trans, color = trans))
# 5 - CODE - reveals the dependence on one for another.Facet wrap doesn't improve much but makes it cleaner.
ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm)) + geom_point() + facet_wrap(~species)
# 6 - CODE - add shape onto labs
ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm, 
    color = species, shape = species
  )
) +
  geom_point() +
  labs(color = "Species", shape = "Species")
# 7 - CODE - First one you can answer how many of each species live on each island.
# Second one you can answer what % of each island holds species.
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")
ggplot(penguins, aes(x = species, fill = island)) +
  geom_bar(position = "fill")
