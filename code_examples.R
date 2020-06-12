library(tidyverse)

diamonds <- rename(diamonds, colour = color)

# select ------------------------------------------------------------------
# The 4 C's of Diamonds
select(diamonds, carat, cut, colour, clarity, price)
select(diamonds, carat:clarity, price)
select(diamonds, 1:4, price)
select(diamonds, starts_with('c'), price)

# mutate ------------------------------------------------------------------
mutate(diamonds, ppc = price/carat)
mutate(diamonds, colour = str_to_lower(colour))

# filter ------------------------------------------------------------------
# What are the different levels?
distinct(diamonds, colour)$colour

filter(diamonds, carat > 0.24, cut == 'Good')
filter(diamonds, colour %in% c('I', 'J'))

# arrange -----------------------------------------------------------------
arrange(diamonds, cut, colour)
arrange(diamonds, desc(cut), colour)

# summarise ---------------------------------------------------------------
summarise(diamonds, m_carat = mean(carat))

diamonds %>%
  group_by(colour) %>%
  summarise(mean_price = mean(price))


# ggplot ------------------------------------------------------------------
# Problems with simple plotting

# Uneven distribution. Clustered at round numbers
diamonds %>%
  slice_sample(n = 5000) %>%
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth()

diamonds %>%
  mutate(ppc = price/carat) %>%
  ggplot(aes(x = carat, y = ppc)) +
  geom_smooth()

ggplot(data = diamonds, mapping = aes(x = carat)) + 
  geom_histogram(bins = 10)

ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar()

diamonds %>%
  mutate(clarity = reorder(clarity, desc(clarity))) %>%
  ggplot(mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(colour = 'black') +
  theme_bw() + 
  labs(title = 'Distribution of Diamonds by Cut',
       # subtitle = 'with breakdown of clarity',
       x = 'Cut',
       y = 'Number of Diamonds',
       fill = 'Clarity') +
  theme(text = element_text(size = 25))

# Too many variables influencing price 
diamonds %>%
  slice_sample(n = 5000) %>%
  ggplot(aes(x = cut, y = price)) +
  geom_boxplot() 

# diamonds %>%
#   filter(cut >= 'Good') %>%
#   mutate(ppc = price/carat) %>%
#   group_by(clarity) %>%
#   summarise(median_ppc = median(ppc), .groups = 'drop') %>%
#   mutate(clarity = reorder(clarity, desc(clarity))) %>%
# ggplot(aes(clarity, median_ppc)) +
#   geom_col()

# Rapaport Report ---------------------------------------------------------
carat_lower_bounds <- c(0.01, 0.04, 0.08, 0.15, 0.18, 
                        0.23, 0.3, 0.4, 0.5, 0.7, 
                        0.9, 1, 1.5, 2, 3, 4, 5)

excellent_diamonds <- diamonds %>% 
  filter(cut >= 'Very Good') %>%  # Only consider Very Good, Premium, Ideal Cut
  filter(carat >= 0.3) %>%
  mutate(ppc = price/(100*carat)) %>%
  select(carat, price, ppc, colour = color, clarity)

rapaport_report <- excellent_diamonds %>%
  mutate(size_interval = cut(carat,
                             carat_lower_bounds, 
                             include.lowest = TRUE, 
                             right = FALSE)) %>%
  group_by(size_interval, colour, clarity) %>%
  summarise(ppc = median(ppc), .groups = 'drop')

rapaport_report %>%
  filter(size_interval == '[1,1.5)') %>%
  select(-size_interval) %>%
  arrange(desc(clarity)) %>%
  pivot_wider(names_from = clarity, values_from = ppc)
# pivot_longer(-colour, names_to = 'clarity', values_to = 'price_per_carat')

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


