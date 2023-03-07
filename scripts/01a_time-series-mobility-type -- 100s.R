#-------------------------------------------------------------------------------
# Time series - calls by date of posting, floored to week
#-------------------------------------------------------------------------------


# Prep data and vars -----------------------------------------------------------

df <- clean

# Make sure authored is in date format & floor to months

df$authored <- as.Date(df$authored, "%Y-%m-%d")
df$authored <- floor_date(df$authored, unit="month")

# Filter out NAs

df <- df %>% filter(!is.na(mobility_type))

# Do overall monthly counts, merge & calculate percentages

df2 <- df %>% group_by(authored) %>% count(name="total")
df <- df %>% group_by(authored, mobility_type) %>% count(name="n")  
df <- merge(df, df2, by="authored")  

df <- df %>% mutate(per = round(100/total * n, digits = 1))
# df$per <- paste0(df$per, "%")


# Draw the plot
plot <- ggplot(df, aes(x=authored, y=per)) + 
  geom_col(aes(fill=mobility_type)) + 
  scale_x_date(date_labels = "%b %Y",
               limits = as.Date(c('2021-01-01','2022-12-01'))) +
  labs(title = "Percentage of calls by mobility type") +
  scale_y_continuous(breaks = pretty_breaks()) +
  ylab("") + 
  xlab("") +
  theme(text=element_text(family="Peclet"))

plot
