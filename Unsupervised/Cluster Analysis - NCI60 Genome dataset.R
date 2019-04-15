#Genome data clustering

library(ISLR)

nci_labels = NCI60$labs
nci_data = NCI60$data

table(nci_labels)
scaled_data = scale(nci_data)

dist = dist(scaled_data)
hc_single = hclust(dist, method = "single")
hc_complete = hclust(dist, method = "complete")
hc_average = hclust(dist, method = "average")

library(gridExtra)
library(ggdendro)
plot_complete_nci = ggdendrogram(hc_single, rotate = FALSE, size = 2) + labs(title = "NCI: Complete Linkage") 
plot_average_nci = ggdendrogram(hc_complete, rotate = FALSE, size = 2) + labs(title = "NCI: Average Linkage")
plot_single_nci = ggdendrogram(hc_average, rotate = FALSE, size = 2) + labs(title = "NCI: Single Linkage")

grid.arrange(plot_complete_nci, plot_average_nci, plot_single_nci)

cluster.complete = cutree(hc_complete, 4)

table(nci_labels, cluster.complete)
