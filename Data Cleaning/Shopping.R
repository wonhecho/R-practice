library(dplyr)
library(tictoc)

#데이터를 불러옵니다.
tic("loading data")
sales_data = fread("sales_train.csv")
item_data = fread("items.csv")
test_data = fread("test.csv")
shop_data = freamd("shops.csv")
toc()

# dplyr::glimpse 데이터를 확인합니다

glimpse(sale_data)
glipse(item_data)
glimpse(test_data)
glimpse(shop_data)

# merge, cbind rbind 종류가 있는데 merge를 중점적으로 이용해봅니다.

#shop_data와 test_data가 공통적으로 가지고 있는 칼럼은 shop_id
#all.x는 True와 False를 지정해줄 수 있으며, T일 때는 NA 값을 살려주고, F일 경우에는 NA값을 없앰
shop_item_data = merge(shop_data,test_data(c[,"shop_id","item_id"],by="shop_id",all.x=T

#이로 도출될 수 있는 정보는 가게에서 어떤 제품을 파는지 각자의 item_id를 추출할 수 있음
#아이템 이름에 대해서는 나타나 있는 것이 없기 때문에 아이템의 카테고리를 확인
shop_item_data = merge(shop_item_data,item_data[,c("item_id","item_category_id")],by="item_id",all.x=F)

#이렇게 되면, item_id를 기준으로 sorting 되므로 order을 통해서 다시 shop_id를 기준으로 만들어준다

shop_item_data = <- shop_item_data[c(order(shop_item_data$shop_id)),]

#결과적으로 어떤 상점에서 어떤 제품을 판매하는지와 이 제품이 어떤 카테고리에 들어가는지를 확인할 수 있다.
#여기에 코드가 잘 들어갈 수 있도록 년, 월, 일, 주 를 칼럼화 하여 나눠준다.

shop_item_data$date = as.Date(sales_data$date,"%d.%m.%Y")
#EX)
shop_item_data$year = year(shop_item_data$date)
shop_item_data$year = as.factor(shop_item_data$year)

shop_item_data$month = month(shop_itme_data$date)
shop_item_data$month = as.factor(shop_item_data$month)
                                           
shop_item_data$day = day(shop_item_data$date)
shop_item_data$day = as.factor(shop_item_data$date)
                                           
shop_item_data$weekdays = weekdays(shop_item_data$date)
shop_item_data$seekdays = as.factor(shop_item_data$weekdays)
                                           
# 카테고리도 요소화
shop_item_data$itme_category_id = as.factor(shop_item_data$item_category_id)

# data summarise
sales_showwise = shop_item_data %>%
  select(shop_id,item_category_id) %>%
  group_by(shop_id) %>%                                        
  summarise(item_category_id)
                                           
# data 그래프화                                           
ggplot(data =  sales_shopwise, 
       mapping = aes(x = reorder(shop_id, item_category_id), 
                     y = item_category_id, 
                     fill = factor(shop_id))) +
  geom_histogram(stat = "identity", color = "yellow") +
  # coord_flip() +
  xlab("Shop ID") + ylab("Sales category")+
  # geom_label(stat = "identity",position = position_dodge(width = 1),hjust = "center", aes(label = item_cnt_day)) +
  ggtitle(label = "Shop wise sales") +
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
    # Change plot and panel background
    plot.background=element_rect(fill = "black"),
    panel.background = element_rect(fill = 'black'),
    # Change legend 
    # legend.position = c(0.6, 0.07),
    # legend.direction = "horizontal",
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(color = "gray", fill = "black"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    # align title to top center, top ledt is by default.
    plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
    # axis ticks to bold black
    axis.text=element_text(colour = "yellow",face = "bold"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white")
  ) 
# shop에 어떤 제품들이 있는지를 확인할 수 있는                                            
 items_in_shop = sales_data %>%
  select(shop_id,item_id) %>%
  group_by(shop_id) %>%
  summarise(item_id = n_distinct(item_id))

ggplot(data = items_in_shop,
       mapping = aes(x=reorder(shop_id,item_id),
                     y=item_id,
                     fill = factor(shop_id)))+
  geom_histogram(stat = "identity",color="yellow")+
  xlab(" Shop ID")+ ylab("Items in shop")+
  ggtitle("Most Items in Shops")+
  coord_flip()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
    # Change plot and panel background
    plot.background=element_rect(fill = "black"),
    panel.background = element_rect(fill = 'black'),
    # Change legend 
    # legend.position = c(0.6, 0.07),
    # legend.direction = "horizontal",
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(color = "gray", fill = "black"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    # align title to top center, top ledt is by default.
    plot.title = element_text(color = "white", hjust = 0.5, face = "bold"),
    # axis ticks to bold black
    axis.text=element_text(colour = "yellow",face = "bold"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white")
  )                                          
                                           








