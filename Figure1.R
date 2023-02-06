setwd("/media/MeinzerStudy/gendercitation_v5") # change to your path
library(dplyr)
library(ggplot2)
library(patchwork)

# load data.frame, ref_proportions & has_citations from Step11
load("df11_additional_plots.RData")

## Paper published per year

paper.per.year <- article.data %>%
  count(PY)

p1 <- ggplot(paper.per.year, aes(x = PY, y = n)) +
  geom_rect(
    data = NULL, aes(xmin = 2010, xmax = 2020, ymin = -Inf, ymax = Inf),
    fill = "grey", alpha = .2
  ) +
  geom_line() +
  xlab("") +
  ylab("Number of papers\npublished per year") +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
  theme_classic() +
  theme(text = element_text(size = 20)) # top, right, bottom, left

p1

# Note that this produces slightly higher numbers of papers
# than number included in the analyses, since papers which were
# not cited are still included.

print(paste0(
  "There were ", sum(paper.per.year$n),
  " paper in the whole dataset. The analyzed",
  " time frame included a total of ",
  sum(paper.per.year$n[
    paper.per.year$PY %in% c(2010:2020)
  ]), "."
))


## Paper cited by year

citations.per.year <- data.frame(
  PY = article.data$PY,
  N = ref_proportions[, 13]
)

citations.per.year <- citations.per.year %>%
  group_by(PY) %>%
  summarise(Frequency = sum(N))

print(paste0(
  "There were ", sum(citations.per.year$Frequency),
  " citations in the whole dataset. The analyzed",
  " time frame included a total of ",
  sum(citations.per.year$Frequency[
    citations.per.year$PY %in% c(2010:2020)
  ]), "."
))

p2 <- ggplot(citations.per.year, aes(x = PY, y = Frequency)) +
  geom_rect(
    data = NULL, aes(xmin = 2010, xmax = 2020, ymin = -Inf, ymax = Inf),
    fill = "grey", alpha = .2
  ) +
  geom_line() +
  xlab("Years") +
  ylab("Number of papers\ncited per year") +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
  theme_classic() +
  theme(text = element_text(size = 20))

p2

p.comb <- p1 / p2 + plot_annotation(
  tag_levels = "A",
  tag_suffix = "\n"
)

p.comb
