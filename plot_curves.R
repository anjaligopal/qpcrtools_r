#!/usr/local/bin/R

library(tidyverse)
library(tools)
library(readxl)
library(ggplot2)
library(repr)
library(RColorBrewer)


amp_curves <- function(layout, spreadsheet, skip_rows = 42){
    
    ###

    sc <- structure(list(),class = "amplification_curves");
    
    if (class(layout)=="data.frame"){
        sc$dilution_layout <- layout;
    } else {
        sc$dilution_layout <- read.csv(layout);        
    }
    


    colnames(sc$dilution_layout) <- c("Well Position","Sample","Replicate");
    
    sc$amplification_data = read_excel(spreadsheet, sheet = "Amplification Data", skip = skip_rows);
    
    if (colnames(sc$amplification_data)[1] != "Well"){
        stop("Please check the position of the starting row for the Results sheet")
    }
    
    sc$software_results = read_excel(spreadsheet, sheet = 'Results', skip = skip_rows); 
    
    sc$amplification_data <- merge(x = sc$amplification_data, sc$dilution_layout, by = "Well Position", all.x = TRUE);

    colnames(sc$amplification_data) <- make.names(colnames(sc$amplification_data));
    
    sc$amplification_data <- na.omit(sc$amplification_data);
    
    return(sc)
    
}

curve_plot <- function(standard_curve, samples = NULL, replicates = NULL, 
                       palette = NULL, colors = NULL, title = NULL, 
                       text_size = 20, plot_width = 12, plot_height = 12, 
                       linewidth = 2, legend_position = "left",
                        plot_type = "Delta.Rn"){
    
    options(repr.plot.width= plot_width, repr.plot.height= plot_height)
    
    if (class(samples) == "NULL"){
        stop("Please specify which sample to plot")
    } else {
        standard_curve$amplification_data = standard_curve$amplification_data[standard_curve$amplification_data$Sample == samples,]
    }
    
    if (class(replicates) != "NULL"){
        standard_curve$amplification_data = standard_curve$amplification_data[standard_curve$amplification_data$Replicate == replicates,]
    }

    if (plot_type == "Delta.Rn"){
        if (length(unique(standard_curve$amplification_data$Sample))==1){
            plots <- ggplot(data = standard_curve$amplification_data, aes(x = Cycle, y = Delta.Rn, group = Well.Position, color = Replicate))
        } else {
            plots <- ggplot(data = standard_curve$amplification_data, aes(x = Cycle, y = Delta.Rn, group = Well.Position, color = Sample))
        }
        
    } else if (plot_type == "Rn"){
        if (length(unique(standard_curve$amplification_data$Sample))==1){
            plots <- ggplot(data = standard_curve$amplification_data, aes(x = Cycle, y = Rn, group = Well.Position, color = Replicate))
        } else {
            plots <- ggplot(data = standard_curve$amplification_data, aes(x = Cycle, y = Rn, group = Well.Position, color = Sample))
        }
                
        
        
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

    plots <- plots + theme_bw() + theme(text = element_text(size = text_size))+ geom_line(linewidth = linewidth)
    
    plots <- plots + theme(panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.line = element_line(colour = "black"))
    
    plots <- plots + theme(legend.position = legend_position)

}
