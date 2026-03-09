# https://r4ds.hadley.nz/workflow-scripts.html#projects

# install.packages("ggplot2")

# getOption('timeout')
# options(timeout = 600)

library("conflicted")

conflicts_prefer(dplyr::filter, dplyr::select)

?SpeedSki
??SpeedSki

library(GDAdata)

SpeedSki
head(SpeedSki)
tail(SpeedSki)
str(SpeedSki)
summary(SpeedSki)
View(SpeedSki)
class(SpeedSki)

library(dplyr)

glimpse(SpeedSki)

library(ggplot2)

ggplot(SpeedSki, aes(Speed)) +
  geom_histogram()

SpeedSki %>% ggplot(aes(Speed)) +
  geom_histogram()

SpeedSki |> ggplot(aes(Speed)) +
  geom_histogram()

ggsave("SpeedSki.png")

ggplot(SpeedSki, aes(Speed)) +
  geom_histogram() +
  facet_grid(rows = vars(Sex))

ggplot(SpeedSki, aes(Speed)) +
  geom_histogram() +
  facet_grid(vars(Sex), vars(Event))

ggplot(SpeedSki, aes(Speed)) +
  geom_histogram() +
  facet_grid(rows = vars(Event))

# https://www.cedricscherer.com/2023/07/05/efficiency-and-consistency-automate-subset-graphics-with-ggplot2-and-purrr/

library(stringr)

plot1 <- function(data, var1, var2, var3, save = FALSE) {
  p <- data |>
    filter({{ var2 }} %in% var3) |>
    ggplot(mapping = aes(x = {{ var1 }})) +
    geom_histogram() +
    labs(title = var3)

  if (isTRUE(save)) {
    ggsave(p, filename = paste0(str_replace_all(var3, " ", "_"), ".png"))
  }
  return(p)
}

plot1(SpeedSki, Speed, Event, "Speed Downhill", save = TRUE)

names1 <- levels(SpeedSki$Event)

library(purrr)

group1 <- map(names1, ~plot1(data = SpeedSki, var1 = Speed,
                             var2 = Event, var3 = .x, save = TRUE))

library(patchwork)

wrap_plots(group1, ncol = 1) &
  lims(x = c(160, 215), y = c(0, 9))

?iris
str(iris)
View(iris)
class(iris)

ggplot(iris, aes(Petal.Length)) +
  geom_histogram()

ggplot(iris, aes(Petal.Length, Petal.Width)) +
  geom_point()

ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
  geom_point()

?UCBAdmissions
str(UCBAdmissions)
View(UCBAdmissions)
class(UCBAdmissions)

# ggplot(UCBAdmissions, aes(Dept)) +
#   geom_bar()

ggplot(as.data.frame(UCBAdmissions), aes(Dept)) +
  geom_bar()

# https://ggplot2.tidyverse.org/reference/geom_bar.html

ggplot(as.data.frame(UCBAdmissions), aes(x = Dept, weight = Freq)) +
  geom_bar()

p1 <- ggplot(as.data.frame(UCBAdmissions), aes(Dept)) +
  geom_bar(aes(weight = Freq))

p1

class(p1)

(p2 <- ggplot(as.data.frame(UCBAdmissions), aes(Gender)) +
   geom_bar(aes(weight = Freq)))

p3 <- ggplot(as.data.frame(UCBAdmissions), aes(Admit)) +
  geom_bar(aes(weight = Freq))

p3

p1 + p2 + p3

# https://ggplot2-book.org/arranging-plots.html
# https://patchwork.data-imaginist.com/articles/guides/layout.html

p1 + p2 + p3 + plot_layout(widths = c(3, 1, 1)) &
  scale_y_continuous(limits = c(0, 2800))

library(ggmosaic)

ggplot(data = as.data.frame(UCBAdmissions)) +
  geom_mosaic(
              aes(
                x = product(Admit, Gender, Dept),
                fill = Admit,
                alpha = Gender,
                weight = Freq
              ),
              divider = ddecker()) +
  scale_alpha_manual(values = c(.7, .9)) +
  theme(legend.position = "none") +
  labs(title = "Double Decker Plot")

library(MASS)

?Pima.tr2
str(Pima.tr2)
View(Pima.tr2)
class(Pima.tr2)

plot2 <- function(data, var1) {
  data |>
    ggplot(aes(x = !!sym(var1))) +
    geom_histogram()
}

plot2(Pima.tr2, "glu")

group2 <- map(
  c("glu", "bp", "skin", "bmi", "ped", "age"),
  ~plot2(data = Pima.tr2, var1 = .x)
)

wrap_plots(group2)

ggplot(Pima.tr2, aes(npreg)) +
  geom_bar()

plot3 <- function(data, var1) {
  data |>
    ggplot(aes(y = !!sym(var1))) +
    geom_boxplot()
}

plot3(Pima.tr2, "glu")

group3 <- map(
  c("glu", "bp", "skin", "bmi", "ped", "age"),
  ~plot3(data = Pima.tr2, var1 = .x)
)

wrap_plots(group3, nrow = 1)

library(GGally)

ggpairs(Pima.tr2)

Pima.tr2 |>
  ggpairs(columns = 1:7)

plot4 <- function(data, var1, var2) {
  data |>
    ggplot(aes(x = !!sym(var1), y = !!sym(var2))) +
    geom_point()
}

group4 <- map2(
  c("glu", "glu", "bp"),
  c("bmi", "ped", "age"),
  ~plot4(data = Pima.tr2, var1 = .x, var2 = .y)
)

wrap_plots(group4)

names2 <- names(select(Pima.tr2, where(is.numeric)))

library(tidyr)

names3 <- expand_grid(names2, names2)

group5 <- pmap(
  names3,
  ~plot4(data = Pima.tr2, var1 = .x, var2 = .y)
)

wrap_plots(group5, nrow = 7, byrow = FALSE)

# How would you describe this histogram of sepal width?

?iris
str(iris)
View(iris)

ggplot(iris, aes(Sepal.Width)) +
  geom_histogram(binwidth = .1)

# Summarise what this barchart shows:

?Pima.tr2
str(Pima.tr2)
View(Pima.tr2)

ggplot(Pima.tr2, aes(type)) +
  geom_bar()

# Why is the upper left of this plot of numbers of pregnancies against age
# empty?

ggplot(Pima.tr2, aes(age, npreg)) +
  geom_point()

# There are 100 estimates of the speed of light made by Michelson in 1879,
# composed of 5 groups of 20 experiments each (dataset michelson in the MASS
# package).

# What plot would you draw for showing the distribution of all the values
# together? What conclusions would you draw?

# What plots might be useful for comparing the estimates from the 5 different
# experiments? Do the results from the 5 experiments look similar?

?michelson
str(michelson)
View(michelson)

ggplot(michelson, aes(Speed)) +
  geom_bar()

ggplot(michelson, aes(Speed)) +
  geom_histogram(binwidth = 10)

ggplot(michelson, aes(Speed)) +
  geom_dotplot(binwidth = 10)

ggplot(michelson, aes(Speed)) +
  geom_boxplot()

ggplot(michelson, aes(Speed)) +
  geom_bar()

ggplot(michelson, aes(Speed, fill = Expt)) +
  geom_bar()

ggplot(michelson, aes(Speed)) +
  geom_bar() +
  facet_grid(rows = vars(Expt))

ggplot(michelson, aes(Expt, Speed)) +
  geom_boxplot()

ggplot(michelson, aes(Expt, Speed)) +
  geom_dotplot(binwidth = 10, binaxis = "y", stackdir = "center")

ggplot(michelson, aes(Expt, Speed)) +
  geom_point()

ggplot(michelson, aes(Expt, Speed)) +
  geom_jitter(width = .25)

# The liner Titanic sank on its maiden voyage in 1912 with great loss of life.
# The dataset is provided in R as a table. Convert this table into a data frame
# using data.frame(Titanic).

# (a) What plot would you draw for showing the distribution of all the values
# together? What conclusions would you draw?

# (b) Draw a graphic to show the number sailing in each class. What order of
# variable categories did you choose and why? Are you surprised by the different
# class sizes?

# (c) Draw graphics for the other three categorical variables. How good do you
# think these data are? Why are there not more detailed data on the ages of
# those sailing? Even if the age variable information (young and old) was
# accurate, is this variable likely to be very useful in any modelling?

?Titanic
class(Titanic)
str(Titanic)
str(as.data.frame(Titanic))
View(Titanic)

p16 <- ggplot(as.data.frame(Titanic), aes(Class, weight = Freq)) +
  geom_bar()

p17 <- ggplot(as.data.frame(Titanic), aes(Sex, weight = Freq)) +
  geom_bar()

p18 <- ggplot(as.data.frame(Titanic), aes(Age, weight = Freq)) +
  geom_bar()

p19 <- ggplot(as.data.frame(Titanic), aes(Survived, weight = Freq)) +
  geom_bar()

p16 + p17 + p18 + p19 + plot_layout(nrow = 1, widths = c(2, 1, 1, 1)) &
  scale_y_continuous(limits = c(0, 2100))

p16

ggplot(as.data.frame(Titanic), aes(y = "", fill = Class, weight = Freq)) +
  geom_bar(width = 1, position = position_stack(reverse = TRUE))

# The dataset swiss contains a standardized fertility measure and various
# socioeconomic indicators for each of 47 French-speaking provinces of
# Switzerland in about 1888.

# (a) What plot would you draw for showing the distribution of all the values
# together? What conclusions would you draw?

# (b) Draw graphics for each variable. What can you conclude from the
# distributions concerning their form and possible outliers?

# (c) Draw a scatterplot of Fertility against % Catholic. Which kind of
# areas have the lowest fertility rates?

# (d) What sort of relationship is there between the variables Education and
# Agriculture?

?swiss
str(swiss)
View(swiss)

names4 <- names(swiss)

group6 <- map(
  names4,
  ~plot3(data = swiss, var1 = .x)
)

wrap_plots(group6, nrow = 1) &
  scale_y_continuous(limits = c(0, 100))

ggplot(swiss, aes(Fertility, Catholic)) +
  geom_point()

library(ggrepel)

ggplot(swiss, aes(Fertility, Catholic, label = rownames(swiss))) +
  geom_text_repel() +
  geom_point(aes(color = Fertility < 50)) +
  scale_color_manual(values = c("black", "firebrick"), guide = "none")

ggplot(swiss, aes(Education, Agriculture)) +
  geom_point()

# cor(swiss$Education, swiss$Agriculture)

# The dataset painters in package MASS contains assessments of 54 classical
# painters on four characteristics: composition, drawing, colour, and
# expression. The scores are due to the eighteenth century art critic de Piles.

# (a) What plot would you draw for showing the distribution of all the values
# together? What conclusions would you draw?

# (b) Draw a display to compare the distributions of the four assessments. Is it
# necessary to scale the variables first? What information might you lose, if
# you did? What comments would you make on the distributions individually
# and as a set?

# (c) What would you expect the association between the scores for drawing and
# those for colour to be? Draw a scatterplot and discuss what the display
# shows in relation to your expectations.

?painters
str(painters)
View(painters)

p30 <- ggplot(painters, aes(School)) +
  geom_bar()

plot5 <- function(data, var1) {
  data |>
    ggplot(aes(x = !!sym(var1))) +
    geom_bar()
}

plot5(painters, "Composition")

group7 <- map(
  c("Composition", "Drawing", "Colour", "Expression"),
  ~plot5(data = painters, var1 = .x)
)

group8 <- wrap_plots(group7, ncol = 1) &
  lims(x = c(0, 20), y = c(0, 16))

group8 | p30

p31 <- ggplot(painters, aes(Composition)) +
  geom_boxplot()

p32 <- ggplot(painters, aes(Drawing)) +
  geom_boxplot()

p33 <- ggplot(painters, aes(Colour)) +
  geom_boxplot()

p34 <- ggplot(painters, aes(Expression)) +
  geom_boxplot()

p31 + p32 + p33 + p34 + plot_layout(ncol = 1) &
  scale_x_continuous(limits = c(0, 20))

library(tibble)

# has_rownames(painters)

# https://r4ds.hadley.nz/data-tidy.html

painters_longer <- rownames_to_column(painters, var = "Painter") |>
  pivot_longer(
    c(`Composition`, `Drawing`, `Colour`, `Expression`),
    names_to = "Characteristic",
    values_to = "Assessment"
  )

ggplot(painters_longer, aes(Assessment)) +
  geom_bar()

ggplot(painters, aes(Drawing, Colour)) +
  geom_point()

# cor(painters$Drawing, painters$Colour)

# The dataset faithful contains data on the time between eruptions and the
# duration of the eruption for the Old Faithful geyser in Yellowstone National
# Park, USA.

# (a) Draw histograms of the variable eruptions using the functions hist
# and ggplot (from the package ggplot2). Which histogram do you prefer
# and why? ggplot produces a warning, suggesting you choose your own
# binwidth. What binwidth would you choose to convey all the information
# you want to convey in a clear manner? Would a boxplot be a good alternative
# here?

# (b) Draw a scatterplot of the two variables using either plot or ggplot. How
# would you summarise the information in the plot?

?faithful
str(faithful)
View(faithful)

ggplot(faithful, aes(eruptions)) +
  geom_histogram()

ggplot(faithful, aes(eruptions)) +
  geom_histogram(binwidth = .05)

ggplot(faithful, aes(eruptions)) +
  geom_boxplot()

ggplot(faithful, aes(eruptions, waiting)) +
  geom_point()

# Van Dyck, Charles I, King of England, from Three Angles belongs to the Royal
# Collection in Windsor Castle. What is gained from having more than one view
# of the King?

# https://en.wikipedia.org/wiki/Charles_I_in_Three_Positions#/media/File:Sir_Anthony_Van_Dyck_-_Charles_I_(1600-49)_-_Google_Art_Project.jpg

# https://wilkelab.org/SDS375/slides/getting-things-in-order.html

library(nycflights13)
library(forcats)

flight_data <- flights %>%
  left_join(airlines) %>%
  select(name, carrier, flight, year, month, day, origin, dest)

flight_data %>%
  ggplot(aes(y = name)) +
  geom_bar()

flight_data %>%
  mutate(
    name = fct_lump_n(fct_infreq(name), 7),
    highlight = fct_other(name, keep = "Other", other_level = "Named")
  ) %>%
  ggplot() +
  aes(y = fct_rev(name), fill = highlight) +
  geom_bar() +
  scale_x_continuous(name = "Number of flights") +
  scale_y_discrete(name = NULL) +
  scale_fill_manual(
    values = c(Named = "gray50", Other = "#98545F"),
    guide = "none"
  ) +
  theme(axis.text.y = element_text(hjust = 0))

data <- data.frame(
  grp = LETTERS[1:15],
  value = c(265, 254, 233, 17, 16, 14, 10, 7, 5.5, 5.5, 4, 3, 2, 1.5, 1.2)
)

data |>
  mutate(grp = ifelse(value > 20, grp, "other")) |>
  group_by(grp) |>
  summarize(value = sum(value)) |>
  ggplot(aes(x = grp, y = value, fill = grp == "other")) +
  geom_col() +
  scale_fill_manual(values = c("#28A87D", "grey65"), guide = "none")

library(palmerpenguins)

penguins |>
  count(island, species, .drop = FALSE) |>
  ggplot(aes(x = island, fill = species, weights = n)) +
  geom_bar(color = "black", position = "dodge")

library(readr)
library(lubridate)

bikes <- read_csv("data/london-bikes.csv", col_types = "Dcfffilllddddc")

ggplot(bikes, aes(x = month(date, label = TRUE), y = temp)) +
  geom_boxplot()

Sys.getlocale()
(temp <- Sys.getlocale("LC_TIME"))
Sys.setlocale("LC_TIME", "C")

ggplot(bikes, aes(x = month(date, label = TRUE), y = temp)) +
  geom_boxplot()

Sys.setlocale("LC_TIME", temp)

sessionInfo()

library(lintr)
lint("L00.R")
