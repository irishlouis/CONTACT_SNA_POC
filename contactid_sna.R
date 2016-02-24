library(jsonlite)      # read in the JSON data from the API
library(dplyr)         # data munging
library(igraph)        # work with graphs in R
library(devtools)
# install_github("briatte/ggnetwork")
library(ggnetwork)     # devtools::install_github("briatte/ggnetwork")
# install.packages("intergraph")
library(intergraph)    # ggnetwork needs this to wield igraph things
library(ggrepel)       # fancy, non-ovelapping labels
# install.packages("svgPanZoom")
library(svgPanZoom)    # zoom, zoom
# source("https://bioconductor.org/biocLite.R")
# biocLite()
# biocLite("SVGAnnotation")
library(SVGAnnotation) # to help svgPanZoom; it's a bioconductor package
library(DT)            # pretty tables

if (!file.exists("data.rda")) {
  df <- read.csv("contact_loc_study.csv", stringsAsFactors = FALSE)
  save(df, file="data.rda")
}

load("data.rda")
head(df)
# tidy data
df2 <- df %>% 
  filter(STUDY_ID != "") %>%
  filter(THERAPEUTIC_AREA_NM == "Cardiovascular") %>%  # filter(THERAPEUTIC_AREA_NM == "Oncology") %>%
  select(LOCATION_ID, PI_CONTACT_ID) %>% 
  mutate(PI_CONTACT_ID = as.numeric(PI_CONTACT_ID)) %>%
  distinct

df3 <- left_join(df2, 
                 df2, 
                 by = "LOCATION_ID") %>% 
  filter(PI_CONTACT_ID.x < PI_CONTACT_ID.y) %>% 
  select(-LOCATION_ID)

# make igraph object
gr <- graph_from_data_frame(df3, 
                            directed=FALSE)

# look at the degree centrality so we can properly size the nodes for the final vis
V(gr)$size <- centralization.degree(gr)$res

# # create DT 
 datatable(data.frame(contact_id=V(gr)$name, centrality_degree=V(gr)$size) %>% 
             arrange(desc(centrality_degree)))

# there are a large number of redundant edges. 
# combine them by simplifying the graph and stroring the sum of the edge connections 

rm(df)
rm(df2)
rm(df3)

E(gr)$weight <- 1
g <- simplify(gr, edge.attr.comb="sum")
rm(gr)

save(g, file = "graph.RDA")
load("graph.RDA")

set.seed(1234)

# set plot layout
dat <- ggnetwork(g, layout="fruchtermanreingold", arrow.gap=0, cell.jitter=0)
save(dat, file = "graph.data.RDA")
load("graph.data.RDA")

# make graph into ggplot2 (using ggnetwork). geom_edges + geom_nodes 
# add a "repelling label" to the nodes with higher centrality 
# pass the ggplot object to svgPlot and svgPanZoom to make it easier to generate a huge graph but still make it explorable

gg <- ggplot() +
  geom_edges(data=dat, 
             aes(x=x, y=y, xend=xend, yend=yend),
             color="grey50", curvature=0.1, size=0.15, alpha=0.25) +
  geom_nodes(data=dat,
             aes(x=x, y=y, xend=xend, yend=yend, size=sqrt(size)),
             alpha=1/3) +
  geom_label_repel(data=unique(dat[dat$size>5,c(1,2,5)]),
                   aes(x=x, y=y, label=vertex.names), 
                   fontface = "bold",
                   size=2, 
                   color="red",
                   segment.color = 'red') +
  theme_blank() +
  theme(legend.position="none") +
  labs(title = "Socal Network of PI's linked by Location_ID") +
  theme(plot.title = element_text(size = 20, face = "bold"))
gg
pdf("Cardiovascular SNA Graph 30x18.pdf", width=30/2.54, height=18/2.54)
plot(gg)
dev.off()

gg
