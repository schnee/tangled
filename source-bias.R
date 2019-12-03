library(urltools)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(ggrepel)

make_bias_chart <- function(tangled, afm_bias, afm_articles){
afm_bias <- afm_bias %>% 
  left_join(afm_articles, by=c("Source" = "Source")) %>%
  rename(Bias = Bias.x,
         Quality = Quality.x) %>%
  select(-Bias.y, -Quality.y) %>% # these are the article scores
  group_by(Source) %>%
  top_n(1, Url) # just get one score per Source - we now have an example Url

bias <- afm_bias %>% pull(Url)%>% 
  url_parse() %>% 
  pull(domain) %>% 
  suffix_extract()  %>%
  mutate(subdomain = if_else(is.na(subdomain), "", subdomain)) %>%
  mutate(domain = if_else(subdomain == "abcnews", "abcnews", domain)) %>%
  bind_cols(afm_bias, .) 

# biased %>% filter(!is.na(n)) %>% 
#   mutate(bias_class = cut(m_bias, breaks = seq(from=-42, to=42, by=12))) %>%
#   arrange(desc(n))

color.background = "#fffbf0ff"
color.text = "#22211d"
the_bias_breaks <- seq(from=-42, to=42, by=14)
the_bias_labels <- c("Far Left", "Left", "Center Left",
                     "Center",
                     "Center Right", "Right", "Far Right")

plot_df <- tangled %>% pull(source) %>% url_parse() %>% pull(domain)  %>% 
  suffix_extract() %>% 
  mutate(subdomain = if_else(is.na(subdomain), "", subdomain)) %>%
  mutate(domain = if_else(subdomain == "abcnews", "abcnews", domain)) %>% 
  right_join(bias, by=c("domain" = "domain"))

# Label the top 25 sources
label_df <- plot_df %>% group_by(domain) %>%
  tally() %>% 
  top_n(25, n) %>% left_join(bias, by = c("domain" = "domain"))

plot_df %>%
  ggplot(aes(x=Bias, y=Quality)) + 
  geom_count(aes(fill = Bias), alpha=.75, shape=21, color = "black", na.rm=TRUE) + 
  scale_fill_gradient2("Bias", high = muted("red"), mid = "white",
                       low = muted("blue"), breaks = the_bias_breaks,
                       labels = the_bias_labels) +
  scale_size("Number of Articles")+
  expand_limits(x=c(-42,42)) +
  scale_x_continuous("Bias", breaks = the_bias_breaks, 
                     labels = the_bias_labels)+
  theme_few() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Format background colors
  theme(panel.background = element_rect(fill = color.background, 
                                        color = color.background)) +
  theme(plot.background  = element_rect(fill = color.background,
                                        color = color.background)) +
  theme(panel.border     = element_rect(color = color.background)) +
  theme(strip.background = element_rect(fill = color.background, 
                                        color = color.background)) +
  theme(legend.background = element_rect(fill = color.background, 
                                         color = color.background)) +
  geom_vline(xintercept = the_bias_breaks, color = "#555555", linetype=3) +
  labs(
    title = "The Tangled Web Bias Chart",
    caption = "h/t Ad Fontes Media",
    xlab = "Bias",
    ylab = "Quality"
  ) +
  geom_label_repel(data = label_df, aes(x=Bias, y=Quality, label = domain)) 
}


tangled <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSosbIjCD2KyWJCm712HsEHCkSOdR75Gba5DbobZxlgNSeHjNutef7KkNHRiPU861sA10RfJwyQujuK/pub?gid=0&single=true&output=csv")

afm_bias <- read_csv("./data/Overall-Source-Ratings-October-2019.csv") %>%
  group_by(Source) %>% top_n(1,-abs(Bias)) %>%
  ungroup() 

afm_articles <- read_csv("./data/Interactive-Media-Bias-Chart-Ad-Fontes-Media.csv") %>%
  filter(grepl("^http", .$Url)) # only the things that start with 'http"

make_bias_chart(tangled, afm_bias, afm_articles)
         