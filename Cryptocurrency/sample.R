suppressMessages(library(tidyverse))
suppressMessages(library(readxl))
suppressMessages(library(scales))
suppressMessages(library(gganimate))
options(scipen = 999)

path <- "./Cryptocurrency dataset.xlsx"

list <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = path)
head(list)
crypto <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map_df(~ read_excel(path = path, sheet = .x, range = 'A1:G90'), .id = 'sheet')

head(crypto)
names(crypto) <- c('name','date','open','high','low','close','volume','cap')
head(crypto$date)
print(as.Date(crypto$date,format = "%b %d, %Y"))
crypto$date <- as.Date( str(crypto$date), format = "%b  %d, %Y")
crypto$open <- as.numeric(gsub(pattern = '[$,]', replacement = '', crypto$open))
crypto$high <- as.numeric(gsub(pattern = '[$,]', replacement = '', crypto$high))
crypto$low <- as.numeric(gsub(pattern = '[$,]', replacement = '', crypto$low))
crypto$close <- as.numeric(gsub(pattern = '[$,]', replacement = '', crypto$close))
crypto$volume <- as.numeric(gsub(pattern = '[$,]', replacement = '', crypto$volume))
crypto$cap <- as.numeric(gsub(pattern = '[$,]', replacement = '', crypto$cap))


str(crypto)

crypto %>%
  mutate(percent = cap / sum(cap)) %>%
  group_by(name) %>%
  ggplot(aes(date, percent, group = name, color = name))+
  geom_line(lwd = 1) +
  scale_x_date(date_breaks = '1 weeks')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 14),
        legend.title = element_blank())+
  labs(x = '', y = 'valor de mercado')
