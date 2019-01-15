########################
### Sooahn Shin
## SNU PolMeth Boot Camp
## [Day 3] Tidy data: data preprocess & visualization
## Part 3. Case study

# specify the working directory
setwd("~/Google Drive/Sooahn/2018-겨울/방법론 캠프/R/")
library(tidyverse)

### Senate 114 W-NOMINATE
num <- "114"
votes <- read.csv(paste("voteview/S", num, "_votes.csv", sep=""))
votes <- votes %>% select(rollnumber,icpsr,cast_code)
legis <- read.csv(paste("voteview/S", num, "_members.csv", sep=""))
legis <- legis[legis$chamber=="Senate",]
rcvs <- read.csv(paste("voteview/S", num, "_rollcalls.csv", sep=""))
rcvs <- rcvs %>% select(rollnumber,date,session,bill_number,vote_result,yea_count,nay_count)

n <- nrow(legis)
m <- length(unique(votes$rollnumber))
head(votes)

Y <- votes %>%
  spread(rollnumber, cast_code)

legis.data <- legis %>%
  select(icpsr, bioname, state_abbrev, party_code)
legis.data <- Y %>%
  left_join(legis.data) %>%
  select(icpsr, bioname, state_abbrev, party_code)
Y <- Y %>%
  .[,-1] %>%
  replace(is.na(.), 10)

result <- as.data.frame(Y) %>%
  gather(key, value) %>%
  group_by(key, value) %>%
  tally %>%
  spread(value, n, fill = 0)
result <- result %>%
  mutate(lop=min(`1`, `6`)/sum(`1`, `6`))
result
qplot(result$lop)

library(wnominate)

rc114 <- rollcall(Y, 
                  yea=c(1,2,3), 
                  nay=c(4,5,6),
                  missing = c(7,8,9,10),
                  notInLegis=c(0),
                  legis.names=legis.data$bioname,
                  legis.data=legis.data,
                  desc="senate 114")

wn114 <- wnominate(rc114, ubeta=15, uweights=0.5, dims=2, minvotes=20,
                   lop=0.025,trials=3, polarity=c(1,2), verbose=T)

wn.legis <- wn114$legislators 
wn.legis$party_code <- as.factor(wn.legis$party_code)
cols <- c("blue","red","grey")

get_circle_coords <- function(r = 1, ...) {
  data_frame(theta = seq(0, 2 * pi, ...),
             x     = cos(theta) * r,
             y     = sin(theta) * r)
}

circ <- get_circle_coords(length.out = 200)

wn.legis %>%
  ggplot() %>%
  + geom_path(data = circ, aes(x=x,y=y),color = "grey") %>%
  + geom_point(aes(x = coord1D, y = coord2D, col = party_code, shape = party_code)) %>%
  + scale_colour_manual(values = cols) %>%
  + coord_fixed(xlim=c(-1, 1),ylim=c(-1, 1)) %>%
  + theme_bw() %>% 
  + theme(legend.position="none") %>%
  + labs(x="Dimension 1",y="Dimension 2",title="W-NOMINATE Estimation (S114)")

### Tips!!!
library(plotly)

p <- plot_ly(
  wn.legis, x = ~coord1D, y = ~coord2D,
  text = ~paste("Bioname: ", bioname, '\nState:', state_abbrev),
  color = ~party_code, colors = cols, opacity = 0.7
)
p

# # Create a shareable link to your chart
# # Set up API credentials: https://plot.ly/r/getting-started
# chart_link = api_create(p, filename="scatter-senate114")
# chart_link