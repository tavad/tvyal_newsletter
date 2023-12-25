# https://drmowinckels.io/blog/2018/christmas-tree-in-ggplot/

cone = data.frame(x = 1:9,
                  y = c(1:5,4:1)) %>% 
  na.omit() %>% 
  arrange(x)

cone %>% 
  ggplot(aes(x=x, y=y)) +
  geom_polygon(fill="#213c18")

fancy = cone %>% 
  mutate(xoff = ifelse(x<5, x+.4, ifelse(x>5, x-.4, NA))) %>% 
  gather(del, x, contains("x")) %>% 
  mutate(y = ifelse(del=="xoff", y-.1,y)) %>% 
  filter(y>=1) %>% 
  na.omit() %>% 
  select(-del) %>% 
  arrange(y)

cone %>% 
  ggplot(aes(x=x, y=y)) +
  geom_polygon(fill="#213c18") +
  geom_polygon(data=fancy, fill = "#668c6f")

library(gganimate, quietly = T)

bauble_colours = c("#e5d08f", "#e3d4b6",
                   "#cacbce", "#9c9c9c", "#e1dfdf",
                   "#c94c4c", "#8d1414")

baubles <- 
  cone %>% 
  
  # Group by y, nest and make up some random values for x.
  group_by(y) %>% 
  nest() %>% 
  mutate(data =  map(data, ~tibble(x=seq(min(.$x), max(.$x), by=.1)))) %>% 
  unnest() %>% 
  
  # Group by x, nest and make up some random values for y.
  group_by(x) %>% 
  nest() %>% 
  mutate(data =  map(data, ~data.frame(y=seq(min(.$y), max(.$y), by=.1)))) %>% 
  ungroup() |> 
  # slice_sample(n = 10)
  unnest() %>% 
  ungroup() %>% 
  
  # Give baubles random shapes, sizes and two different colours.
  mutate(col1 = sample(bauble_colours, nrow(.), replace = T),
         col2 = sample(bauble_colours, nrow(.), replace = T),
         shp = sample(1:7, nrow(.), replace = T),
         sz = sample(seq(.5,2,by=.1), nrow(.), replace = T),
         time = sample(seq(.5,1,by=.01), nrow(.), replace = T)
  ) %>%
  rownames_to_column() %>% 
  
  # Grab only 60 baubles
  sample_n(60) %>% 
  
  # Gather the colours into a single column
  gather(dd, cols, contains("col")) %>% 
  mutate(alph = ifelse(dd == "col1", .8, 1))

cone %>% 
  ggplot(aes(x=x, y=y)) +
  geom_polygon(fill="#213c18") +
  geom_polygon(data=fancy, fill = "#668c6f") +
  geom_point(data = baubles, aes(colour=I(cols), fill=I(cols), 
                                 shape = factor(shp),size=sz), show.legend = F) + 
  scale_shape_manual(values = c(20:25,8))

############# add a topper




# snow

snow = data.frame(x = sample(seq(1, max(cone$x)+1, by=.01), 100, replace = F),
                  y = sample(seq(1, max(cone$y)+1, by=.01), 100, replace = F)) %>% 
  group_by(x) %>% 
  nest() %>% 
  mutate(data =  map(data, 
                     ~data.frame(y=seq(.$y, .$y-sample(seq(.5,1,by=.01),1), length.out = 100)) %>% 
                       mutate(time = sample(seq(0.5,.9, .01), 1)) %>% 
                       mutate(time = seq(unique(time), unique(time)+.02, length.out = nrow(.)))
  )) %>% 
  unnest() 



cone %>% 
  ggplot(aes(x=x, y=y)) +
  
  # Snow
  geom_jitter(data=snow, aes(group=x), colour="white", shape=8, size=1) +
  
  # Cone
  geom_polygon(fill="#213c18") +
  
  # Fancy
  geom_polygon(data=fancy, fill = "#668c6f") +
  
  # Baubles
  geom_point(data = baubles %>% select(-time), show.legend = F, alpha = .7,
             aes(colour=I(cols), fill=I(cols),
                 shape = factor(shp),size=sz, group=rowname)) +
  
  # animated baubles!
  geom_point(data = baubles, show.legend = F,
             aes(colour=I(cols), fill=I(cols), alpha=I(alph),
                 shape = factor(shp),size=sz, group=rowname)) +
  
  # Topper
  geom_point(data=data.frame(x=5, y=5), colour="#e5d08f", size=15, shape=8) +
  scale_shape_manual(values = c(20:25,8)) +
  
  # remove axes etc., and make background black
  theme_void() + 
  theme(plot.background = element_rect(fill="black"), title = element_text(colour="white")) +
  
  # Animate
  transition_time(time) + 
  ease_aes('sine-in-out') 






