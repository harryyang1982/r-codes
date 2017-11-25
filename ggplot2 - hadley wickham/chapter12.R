library(ggplot2)

# 12.2 Single Components

bestfit <- geom_smooth(
  method = "lm",
  se = F,
  color = alpha("steelblue", 0.5),
  size = 2
)

ggplot(mpg, aes(cty, hwy)) +
  geom_point() +
  bestfit

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  bestfit

geom_lm <- function(formula = y~x, color = alpha("steelblue", 0.5),
                    size = 2, ...) {
  geom_smooth(formula = formula, se = F, method = "lm", color = color, size = size, ...)
}

ggplot(mpg, aes(displ, 1 / hwy)) +
  geom_point() +
  geom_lm()

ggplot(mpg, aes(displ, 1 / hwy)) +
  geom_point() +
  geom_lm(y ~ poly(x, 2), size = 1, color = "red")

# 12.2.1 Exercise

#1
pink_hist <- geom_histogram(
  bins = 100,
  fill = "pink",
)

ggplot(mpg, aes(displ)) +
  pink_hist

#2
blues_scale <-
  scale_color_brewer("Blues")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color=drv)) +
  blues_scale

#4
scale_color_wesanderson <- 
  scale_color_manual(values = wes_palette("GrandBudapest"))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color=drv)) +
  scale_color_wesanderson

#12.3 Multiple Components

geom_mean <- function() {
  list(
    stat_summary(fun.y = "mean", geom = "bar", fill = "grey70"),
    stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.4)
  )
}

ggplot(mpg, aes(class, cty)) + geom_mean()
ggplot(mpg, aes(drv, cty)) + geom_mean()

geom_mean <- function(se = T) {
  list(
    stat_summary(fun.y = "mean", geom = "bar", fill = "grey70"),
    if (se)
      stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.4)
  )
}

ggplot(mpg, aes(drv, cty)) + geom_mean()
ggplot(mpg, aes(drv, cty)) + geom_mean(se = F)

# 12.3.2 Annotation

borders <- function(database = "world", regions = ".", fill = NA,
                    color = "grey50", ...) {
  df <- map_data(database, regions)
  geom_polygon(
    aes_(~lat, ~long, group = ~group),
    data = df, fill = fill, color = color, ...,
    inherit.aes = F, show.legend = F
  )
}

geom_mean <- function(..., bar.params = list(), errorbar.params = list()) {
  params <- list(...)
  bar.params <- modifyList(params, bar.params)
  errorbar.params <- modifyList(params, errorbar.params)
  
  bar <- do.call("stat_summary", modifyList(
    list(fun.y = "mean", geom = "bar", fill = "grey70"),
    bar.params)
  )
  errorbar <- do.call("stat_summary", modifyList(
    list(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.4),
    errorbar.params)
  )
  list(bar, errorbar)
}

ggplot(mpg, aes(class, cty)) +
  geom_mean(color = "steelblue",errorbar.params = list(width = 0.5, size = 1))


ggplot(mpg, aes(class, cty)) +
  geom_mean(
    bar.params = list(fill = "steelblue"),
    errorbar.params = list(color = "blue")
  )

#12.4 Plot Functions

piechart <- function(data, mapping) {
  ggplot(data, mapping) +
    geom_bar(width = 1) +
    coord_polar(theta = "y") +
    xlab(NULL) + ylab(NULL)
}

piechart(mpg, aes(factor(1), fill = class))

pcp_data <- function(df) {
  is_numeric <- vapply(df, is.numeric, logical(1))
  
  # Rescale numeric columns
  rescale01 <- function(x) {
    rng <- range(x, na.rm = T)
    (x - rng[1]) / (rng[2] - rng[1])
  }
  df[is_numeric] <- lapply(df[is_numeric], rescale01)
  
  # Add row identifier
  df$.row <- rownames(df)
  
  # Treat numerics as value (aka measure) variables
  # gather_ is the standard-evaluation version of gather, and
  # is usually easier to program with.
  tidyr::gather_(df, "variable", "value", names(df)[is_numeric])
}

pcp <- function(df, ...) {
  df <- pcp_data(df)
  ggplot(df, aes(variable, value, group = .row)) + geom_line(...)
}

pcp(mpg)
pcp(mpg, aes(color = drv))

# 12.4.1 Indirectly Referring to Variables

x_var <- "displ"
aes(x_var)

aes_(quote(displ))
aes_(as.name(x_var))
aes_(parse(text = x_var)[[1]])
f <- function(x_var) {
  aes_(substitute(x_var))
}
f(displ)

aes_(~displ)

piechart1 <- function(data, var, ...) {
  piechart(data, aes_(~factor(1), fill = as.name(var)))
}
piechart1(mpg, "class") + theme(legend.position = "none")

piechart2 <- function(data, var, ...) {
  piechart(data, aes_(~factor(1), fill = var))
}
piechart2(mpg, ~class) + theme(legend.position = "none")

piechart3 <- function(data, var, ...) {
  piechart(data, aes_(~factor(1), fill = substitute(var)))
}
piechart3(mpg, class) + theme(legend.position = "none")

# 12.4.2 The Plot Environment

f <- function() {
  n <- 10
  geom_line(aes(x / n))
}
df <- data.frame(x = 1:3, y = 1:3)
ggplot(df, aes(x, y)) + f()

f <- function() {
  color <- "blue"
  geom_line(color = color)
}
ggplot(df, aes(x, y)) + f()

f <- function() {
  levs <- c("2seater", "compact", "midsize", "minivan", "pickup", "subcompact", "suv")
  piechart3(mpg, factor(class, levels = levs))
}
f()

# 12.5 Functional Programming

geoms <- list(
  geom_point(),
  geom_boxplot(aes(group = cut_width(displ, 1))),
  list(geom_point(), geom_smooth())
)

p <- ggplot(mpg, aes(displ, hwy))
lapply(geoms, function(g) p + g)

# 12.5.1 Exercises

#1
plots <- list(
  ggplot(mpg, aes(displ, hwy)),
  ggplot(diamonds, aes(carat, price)),
  ggplot(faithfuld, aes(waiting, eruptions, size = density))
)

k <- geom_point()

lapply(plots, function(g) g + k)

#2
mystery <- function(...) {
  Reduce(`+`, list(...), accumulate = T)
}

mystery(
  ggplot(mpg, aes(displ, hwy)) + geom_point(),
  geom_smooth(),
  xlab(NULL), ylab(NULL)
)
