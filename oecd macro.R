library(eurostat)
library(OECD)
library(sf)
library(tmap)
library(ggpmisc)
library(jtools)
library(scales)
library(plotly)
library(raster)
library(stars)
library(tidyverse)
library(lubridate)
library(tsibble)
library(tempdisagg)
library(slider)
library(patchwork)

# OECD data -------------------
# more countries and more regions, more dates, 
# may be at the price of relevance of data

# peek the data
datasets <- OECD::get_datasets()

struct <- OECD::get_data_structure("EO108_INTERNET")
struct$VAR_DESC
reg_names <- struct$REG_ID 
struct$LOCATION
struct$OBS_VALUE
struct$VARIABLE %>% view


query_A <- "EA17+GBR+USA.GNFLQ+CBGDPR+ITISK+POP.A"
query_Q <- "EA17+GBR+USA.UNR+GNFLQ+PCORE+PCOREH+CBGDPR+ITISK+GDP+GDPV+GDPVD+GGFLMQ.Q"

oecd_a <- get_dataset(dataset="EO109_INTERNET", query_A) 
oecd_q <- get_dataset(dataset="EO109_INTERNET", query_Q) 

oecd1 <- oecd_a %>%
  select(-UNIT,  -TIME_FORMAT, -FREQUENCY) %>%
  mutate(year=as.numeric(obsTime), pays=LOCATION) %>% 
  pivot_wider(names_from=c(VARIABLE), values_from=obsValue) %>% 
  as_tsibble(key=pays, index=year) %>% 
  filter(year>2000) %>% 
  group_by_key() %>% 
  mutate( dette = ifelse(is.na(GNFLQ),GGFLMQ,GNFLQ),
          dette = dette-dette[which(year==2007)])

pop.a <- map(oecd1 %>% as_tibble() %>% select(pays, POP, year) %>% pivot_wider(names_from = pays, values_from = POP) %>% select(-year),
             ~ts(.x, start=min(oecd1$year), end=max(oecd1$year)))
pop.q <- map(pop.a, ~predict(td(.x ~ 1, to="quarterly", method = "denton-cholette", conversion = "average")))
pop.q <- do.call(cbind,pop.q) %>% as_tsibble() %>% rename(POP=value)

oecd2 <- oecd_q %>%
  filter(FREQUENCY=="Q") %>% 
  select(-UNIT, -REFERENCEPERIOD, -TIME_FORMAT, -FREQUENCY) %>%
  rename(trim=obsTime, pays=LOCATION) %>% 
  mutate(date = lubridate::yq(trim)) %>% 
  pivot_wider(names_from=c(VARIABLE), values_from=obsValue) %>% 
  mutate(PCORE=ifelse(is.na(PCORE), PCOREH, PCORE)) %>% 
  group_by(pays) %>% 
  arrange(pays,date) %>% 
  mutate(core_inflation = (PCORE/lag(PCORE, 4)-1)) %>% 
  select(-PCOREH) %>% 
  left_join(pop.q, by=c("pays"="key", "date"="index")) %>% 
  drop_na(date) %>% 
  as_tsibble(key=pays) %>%
  group_by_key() %>% 
  mutate( gdp_pc = GDPV/POP/(GDPV[which(trim=="2007-Q1")]/POP[which(trim=="2007-Q1")]),
          inv = ITISK/GDP/(ITISK[which(trim=="2007-Q1")]/GDP[which(trim=="2007-Q1")])) %>% 
  mutate(ca_ma = slide_dbl(CBGDPR, mean, .before=3),
         inv= slide_dbl(inv, mean, .before=3))

so <-  "Source OECD EO#109"
colors <- c(EA17="darkorange2", USA="skyblue", GBR="olivedrab3")

crises <- list(annotate("rect", xmin=yq("2008 Q1"), xmax=yq("2011 Q2"), ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray"),
               annotate("rect", xmin=yq("2011 Q2"), xmax=yq("2013 Q4"), ymin=-Inf, ymax=Inf, alpha=0.1, fill="gray"),
               annotate("rect", xmin=yq("2020 Q2"), xmax=yq("2021 Q4"), ymin=-Inf, ymax=Inf, alpha=0.2, fill="thistle"),
               xlab(""),ylab(""), theme_minimal(), theme(text=element_text(size=5), plot.title = element_text(size=7)),
               scale_color_manual(values = colors))

crises.a <- list(annotate("rect", xmin=2008, xmax=2011, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray"),
               annotate("rect", xmin=2011, xmax=2013, ymin=-Inf, ymax=Inf, alpha=0.1, fill="gray"),
               annotate("rect", xmin=2020, xmax=2021, ymin=-Inf, ymax=Inf, alpha=0.2, fill="thistle"),
               xlab(""),ylab(""), theme_minimal(), theme(text=element_text(size=5), plot.title = element_text(size=7)),
               scale_color_manual(values = colors))

unr <- ggplot(oecd2 %>% filter(between(date,yq("2007-Q1"),yq("2021-Q4"))),aes(x=date, y=UNR, col=pays))+
  geom_line(lwd=0.5)+geom_point(size=0.1)+
  crises+
  labs(title="Taux de chômage", caption=so)

pib <- ggplot(oecd2 %>% filter(between(date,yq("2007-Q1"),yq("2021-Q4"))),aes(x=date, y=gdp_pc, col=pays))+
  geom_path(lwd=0.5)+geom_point(size=0.1)+
  crises+
  labs(title="PIB par habitant (2007=1)", caption=so)

inf <- ggplot(oecd2 %>% filter(between(date,yq("2007-Q1"),yq("2021-Q4"))),aes(x=date, y=core_inflation, col=pays))+
  geom_line(lwd=0.5)+geom_point(size=0.1)+
  theme_minimal()+
  crises+labs(title="Inflation sous-jacente", caption=str_c(so," glissement annuel"))

ca <- ggplot(oecd2 %>% filter(between(date,yq("2007-Q1"),yq("2021-Q4"))),aes(x=date, y=ca_ma, col=pays))+
  geom_line(lwd=0.5)+geom_point(size=0.1)+
  theme_minimal()+
  crises+labs(title="Balance courante", caption=str_c(so," Moyenne mobile sur 4 trimestres"))

inv <- ggplot(oecd2 %>% filter(between(date,yq("2007-Q1"),yq("2021-Q4"))),aes(x=date, y=inv, col=pays))+
  geom_line(lwd=0.5)+geom_point(size=0.1)+
  theme_minimal()+
  crises+labs(title = "Investissement/PIB (2007=1)", caption=str_c(so," Moyenne mobile sur 4 trimestres"))

dette <- ggplot(oecd1 %>% filter(between(year,2007,2021)), aes(x=year, y=dette, col=pays))+
  geom_line(lwd=0.5)+geom_point(size=0.1)+
  theme_minimal()+
  crises.a+labs(title="Dette publique (2007=0", caption=str_c(so, " Dette au sens de Maastricht pour EA et GBR"))

gg <- (pib|unr)/(inf|inv)/(ca|dette)
ggsave("macro eo109.svg", plot=gg, width=24, height=16, units="cm")
