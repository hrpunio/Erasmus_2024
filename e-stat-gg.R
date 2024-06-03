## Szablon do różnych wykresów
## Pakiety
library("ggplot2") ## funkcje rysujące
library("ggpubr") ## łącznie wykresów w jeden rysunek
library("dplyr") ## redukcja danych filter/selecy
library("tidyr") ## funkcje transformacji danych
library("scales") ## definiowanie skal
library("ggthemes") ## wyjaśnione/dołączone niżej

## Użyteczne funkcje
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

## Dane z CSV
o <- read.csv(file='!!!!',sep=';',header=T)

## Filtrowanie wierszy
o <- o %>% filter (kraj == 'POL' & plec != 'BTSX')

## Wykres liniowy/punktowy
## czas jako czynnik
ggplot(o, aes(x= as.factor(rok), 
        group=as.factor(plec), color=as.factor(plec), y=nadwaga )) +
  geom_line(size=.5 ) +
  geom_point(size=2.5, alpha=.3) +
  xlab(label="") +
  scale_color_discrete(name="Płeć", labels =c("M", 'F'), breaks=c('MLE', 'FMLE'))
  scale_x_discrete( breaks = every_nth(n = 10)) +
  ylab(label="tys") +
  ggtitle("Nadwaga w PL")

## czas jako czas
ggplot(o, aes(x= as.Date(as.character(rok), "%Y"),
        group=as.factor(plec), color=as.factor(plec), y=nadwaga )) +
  geom_line(size=.5 ) + geom_point(size=2.5, alpha=.3) +
  ylab(label="tys") + xlab(label="rok") +
  ## %y to rok w notacji dwucyfrowej np. 21
  scale_x_date( breaks = "4 years", labels = date_format("%y")) +
  scale_color_discrete(name="Płeć", labels =c("M", 'F'),
                       breaks=c('MLE', 'FMLE')) +
  ggtitle("Nadwaga w PL")

## facety / %in%
o <- o %>% filter (plec != 'BTSX' & kraj %in% countries) %>% as.data.frame

ggplot(ox, aes(x= as.factor(rok), 
          group=as.factor(plec), color=as.factor(plec), y=nadwaga )) +
  geom_line(size=.5, alpha=.6 ) +
  geom_point(size=.5, alpha=.3) +
  ylab(label="tys") +
  facet_wrap( ~kraj, scales = "fixed") +
  ggtitle("Nadwaga w PL", subtitle=sprintf(""))

## Wykres punktowo-słupkowy
## Dwa wykresy punktowe dla pierwszego i ostatniego roku. Do tego
## wykres słupkowy przedstawiający różnicę

obesity$yr <- as.numeric(as.character(obesity$rok))

obesity_diff <- obesity %>%
  filter (plec == 'BTSX' & yr > 1979 ) %>%
  group_by(kraj) %>%
  summarise(fst= nadwaga[which.min(yr)], 
            lst = nadwaga[which.max(yr)],
            diff = nadwaga[which.max(yr)] - nadwaga[which.min(yr)])
  
obesity_diff %>%
  ggplot(aes(x = reorder(kraj, lst ))) +
  geom_point(aes(y = lst, color="lst"), size=1 ) +
  geom_point(aes(y = fst, color="fst"), size=1 ) +
  geom_bar(aes(y = diff, fill='diff'), stat="identity", alpha=.25 ) +
  xlab(label="kraj") +
  ylab(label="obesity") +
  ggtitle("Nadwaga na świecie") +
  ##
  scale_color_manual(name="", labels =c("2016", "1980"),
                     values = c(lst="red", fst="blue" ) ) +
  scale_fill_manual(name="", values = c( diff="green" ) ) +
  #
  theme(axis.text = element_text(size = 4)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

## Wykres pudełkowy

Dla wybranych okresów wykresy pudełkowe

lata <- c ('1980', '1990', '2000', '2016')
oz <- obesity %>% filter ((rok %in% lata ) & plec == 'BTSX')

pow <- ggplot(oz, aes(x=rok, y=nadwaga, fill=rok)) + 
  geom_boxplot() + 
  ylab("#") + 
  xlab('')


## Wyróżnianie

o$dummyGrp <- "0"
o <- o  %>% mutate( dummyGrp=case_when(Area=="Poland" ~ "1", TRUE ~ "0"))

theme(legend.position="none") +
scale_color_manual( values = c( "1" = "#F8766D", "0" = "#00BFC4" ), guide = FALSE ) +

## 
d <- d %>%
   mutate(newc = replace(newc, which( newc < 0), NA),
     newc = replace(newd, which( newd < 0), NA),
    )  %>%
## Filtruj wg daty
d <- d %>% filter(as.Date(date, format="%Y-%m-%d") > fd)

