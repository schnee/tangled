library(urltools)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

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

tangled %>% pull(source) %>% url_parse() %>% pull(domain)  %>% 
  suffix_extract() %>% 
  mutate(subdomain = if_else(is.na(subdomain), "", subdomain)) %>%
  mutate(domain = if_else(subdomain == "abcnews", "abcnews", domain)) %>%
  group_by(domain) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  right_join(bias, by=c("domain" = "domain")) %>%
  ggplot(aes(x=Bias, y=Quality)) + 
  geom_point(aes(size=sqrt(n), fill = Bias), shape=21, color = "black", na.rm=TRUE) + 
  scale_fill_gradient2("Bias", high = muted("red"), mid = "white",
                       low = muted("blue")) +
  scale_size_area("Number of Articles", breaks = c(2,4,6,8), 
                  labels = c(4,16,36,64))+
  expand_limits(x=c(-42,42)) +
  scale_x_continuous("Bias", breaks = the_bias_breaks, 
                     labels = c("Far Left", "Left", "Center Left",
                                "Center",
                                "Center Right", "Right", "Far Right"))+
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
  geom_vline(xintercept = the_bias_breaks, color = "#555555", linetype=2) +
  labs(
    title = "The Tangled Web Bias Chart",
    caption = "h/t Ad Fontes Media",
    xlab = "Bias",
    ylab = "Quality"
  )
}

afm_bias <- read_csv("./data/Overall-Source-Ratings-October-2019.csv")
afm_articles <- read_csv("./data/Interactive-Media-Bias-Chart-Ad-Fontes-Media.csv")

make_bias_chart(tangled, afm_bias, afm_articles)
         