library(tidyverse)

diamonds

diamonds <- rename(diamonds, colour = color)

# select ------------------------------------------------------------------
# The 4 C's of Diamonds
select(diamonds, carat, cut, colour, clarity, price)
select(diamonds, carat:clarity, price)
select(diamonds, 1:4, price)
select(diamonds, starts_with('c'), price)

# Check the docs
?select

# mutate ------------------------------------------------------------------
# columns are vectorised
x <- c(1, 2, 3)
x*3 + x

mutate(diamonds, price = price * 1.3)
mutate(diamonds, price_aud = price * 1.3)
mutate(diamonds, ppc = price/carat)
mutate(diamonds, colour = str_to_lower(colour))

# filter ------------------------------------------------------------------
# What are the different levels?
distinct(diamonds, colour)$colour
filter(diamonds, colour <= 'G')

filter(diamonds, carat > 0.24, cut == 'Good')
filter(diamonds, colour == 'I' | colour == 'J')
filter(diamonds, colour %in% c('I', 'J'))

# arrange -----------------------------------------------------------------
arrange(diamonds, cut)
arrange(diamonds, cut, colour)
arrange(diamonds, desc(cut))

# summarise ---------------------------------------------------------------
# This is an important one. Its how we derive insights from a dataset
summarise(diamonds, m_carat = mean(carat))

diamonds %>%
  group_by(colour) %>%
  summarise(mean_price = mean(price))

# Really weird! Price increases as colour get worse?? 
# We will explore this further. Think about what could be causing this

# task 1 ------------------------------------------------------------------
excellent_diamonds <- diamonds %>% 
  filter(cut >= 'Very Good') %>%  # Only consider Very Good, Premium, Ideal Cut
  filter(carat >= 0.3) %>%
  mutate(ppc = price/(100*carat)) %>%
  select(carat, price, ppc, colour = colour, clarity)


# pivoting example --------------------------------------------------------
relig_income %>% 
  pivot_longer(-religion, names_to = 'income', values_to = 'count') %>%
  pivot_wider(names_from = 'income', values_from = 'count')

# task 2 ------------------------------------------------------------------
carat_lower_bounds <- c(0.01, 0.04, 0.08, 0.15, 0.18, 
                        0.23, 0.3, 0.4, 0.5, 0.7, 
                        0.9, 1, 1.5, 2, 3, 4, 5)

rapaport_report <- excellent_diamonds %>%
  mutate(size_interval = cut(carat,
                             carat_lower_bounds, 
                             include.lowest = TRUE, 
                             right = FALSE)) %>%
  group_by(size_interval, colour, clarity) %>%
  summarise(ppc = median(ppc), .groups = 'drop')

# Single report
rapaport_report %>%
  filter(size_interval == '[1,1.5)') %>%
  select(-size_interval) %>%
  arrange(desc(clarity)) %>%
  pivot_wider(names_from = clarity, values_from = ppc)

# All reports
rapaport_report %>%
  group_split(size_interval) %>%
  lapply(function(df) {
    df %>%
      select(-size_interval) %>%
      arrange(desc(clarity)) %>%
      pivot_wider(names_from = clarity, values_from = ppc)
  })

# ggplot ------------------------------------------------------------------
# Impact of Colour
diamonds %>%
  group_by(colour) %>%
  summarise(mean_price = mean(price)) %>%
  ggplot(aes(colour, mean_price)) +
  geom_col()

# Large spread. Low Confidence
diamonds %>%
  ggplot(aes(x = colour, y = price)) +
  geom_boxplot() 

# What else is impacting price?
diamonds %>%
  group_by(colour) %>%
  summarise(median_carats = median(carat))

# More good coloured diamonds with lower carats. Those are influencing the price
# This tells me that carats have a much bigger impact on price than colour

diamonds %>%
  filter(carat > 0.9, carat < 1.1) %>%
  group_by(colour) %>%
  summarise(mean_price = mean(price)) %>%
  ggplot(aes(colour, mean_price)) +
  geom_col()

# Another way to visualise this
diamonds %>%
  filter(carat < 2.5) %>%
  ggplot(aes(x = carat, fill = colour)) + 
  geom_histogram(bins = 5)

# Prettify
diamonds %>%
  filter(carat < 2.5) %>%
  ggplot(aes(x = carat, fill = colour)) + 
  geom_histogram(bins = 5, colour = 'black') +
  theme_bw() + 
  labs(title = 'Distribution of Diamonds by Carat and Colour',
       x = 'Carats',
       y = 'Number of Diamonds',
       fill = 'Colour') +
  theme(text = element_text(size = 25))

# Carats seem to be the main driver of price
# Uneven distribution. Clustered at round numbers
diamonds %>%
  slice_sample(n = 5000) %>%
  ggplot(aes(x = carat, y = price)) +
  geom_point() 

# task 3 ------------------------------------------------------------------
rapaport_report %>%
  filter(size_interval == '[1,1.5)') %>%
  ggplot(aes(x = reorder(clarity, desc(clarity)), 
             y = reorder(colour, desc(colour)),
             fill = ppc)) +
  geom_tile() +
  scale_x_discrete(position = "top") +
  labs(x = 'Clarity', y = 'Colour', fill = 'PPC - $') + 
  theme_bw() +
  scale_fill_gradientn(colours = c('red', 'yellow', 'green'))

rapaport_report %>%
  ggplot(aes(x = reorder(clarity, desc(clarity)), 
             y = reorder(colour, desc(colour)),
             fill = ppc)) +
  geom_tile() +
  scale_x_discrete(position = "top") +
  labs(x = 'Clarity', y = 'Colour', fill = 'PPC - $') + 
  theme_bw() +
  scale_fill_gradientn(colours = c('red', 'yellow', 'green')) +
  facet_wrap(~size_interval)


