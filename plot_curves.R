#!/usr/local/bin/R

library(tidyverse)
library(tools)
library(readxl)
library(ggplot2)
library(repr)
library(RColorBrewer)


amp_curves <- function(layout, spreadsheet){
    
    ###

    sc <- structure(list(),class = "amplification_curves");

    sc$dilution_layout <- read.csv(layout);
    colnames(sc$dilution_layout) <- c("Well Position","Sample","Replicate");
    
    sc$amplification_data = read_excel(spreadsheet, sheet = "Amplification Data", skip = 42);
    sc$software_results = read_excel(spreadsheet, sheet = 'Results', skip = 42); 
    
    sc$amplification_data <- merge(x = sc$amplification_data, sc$dilution_layout, by = "Well Position", all.x = TRUE)

    colnames(sc$amplification_data) <- make.names(colnames(sc$amplification_data));
    
    sc$amplification_data <- na.omit(sc$amplification_data)
    
    return(sc)
    
}

curve_plot <- function(standard_curve, samples = NULL, replicates = NULL, palette = NULL, colors = NULL, title = NULL){
    
    options(repr.plot.width=20, repr.plot.height=8)
    
    if (class(samples) == "NULL"){
        stop("Please specify which sample to plot")
    } else {
        standard_curve$amplification_data = standard_curve$amplification_data[standard_curve$amplification_data$Sample == samples,]
    }
    
    if (class(replicates) != "NULL"){
        standard_curve$amplification_data = standard_curve$amplification_data[standard_curve$amplification_data$Replicate == replicates,]
    }

    if (length(unique(standard_curve$amplification_data$Sample))==1){
        plots <- ggplot(data = standard_curve$amplification_data, aes(x = Cycle, y = Delta.Rn, group = Well.Position, color = Replicate))
    } else {
        plots <- ggplot(data = standard_curve$amplification_data, aes(x = Cycle, y = Delta.Rn, group = Well.Position, color = Sample))
    }
    
    if (class(palette) != "NULL"){
        plots <- plots + scale_color_brewer(palette="Blues")
    }
    if (class(colors) != "NULL"){
        plots <- plots + scale_color_manual(values = colors)
    }
    
    if (class(title) != "NULL"){
        plots <- plots + ggtitle(title)
    }

    plots <- plots + theme(text = element_text(size = 30))+ geom_line(linewidth = 2) + geom_point()
    
}
