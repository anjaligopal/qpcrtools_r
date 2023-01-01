#!/usr/local/bin/R

library(tidyverse)
library(tools)
library(readxl)

import_plate <- function(file, save_tabular = TRUE, sheet = 1){
    
    # Imports a plate either from a csv, xls, or xlsx
    # file and converts into a tabular plate format.

    # Input: plate file, either csv or excel file. 
    #        See sample for more information.

    # Output: plate layout in tabular format (pandas)
    

    # Checking the file extension
    file_extension = file_ext(file)
    
    # Reading file depending on whether it's excel or csv
    if (file_extension == "xls" | file_extension == "xlsx"){
        plate <- read_excel(file, sheet = sheet)
    } else {
        plate <- read.csv(file)
    }

    plate <- plate[,2:ncol(plate)];

    # Creating a list of plate indices
    plate_rows = c('A','B','C','D','E','F','G','H')
    plate_columns = 1:12

    plate_indices <- list()

    for (row in plate_rows) {
        for (column in plate_columns) {
            index <- paste(row,column,sep="")
            plate_indices <- append(plate_indices,index)
        }
    }
        
    # Reading and re-shpaing plate file
    plate <- unlist(t(plate), use.names = FALSE)

    # Splitting plate entries by \n
    # We have to check for entries that are null
    
    sample_name <- list();
    replicate_name <- list();

    for (entry in plate) {

        # Using grep to search for a variety of delimiters
        if (length(grep("\r\n|\n|\r|\r\n", entry)) > 0) {
            entry <- gsub("\r\n|\n|\r|\r\n", "\n", entry)

        } else {
            entry <- paste(entry, "", sep = "\n")
        }
 
        # Splitting according to delimeter
        entry_split <- unlist(strsplit(entry, "\n"))

        # Assigning entries
        sample_name <- append(sample_name, entry_split[1])
        replicate_name <- append(replicate_name, entry_split[2])

    }
            
    # Concatenating rows
    plate_tabular <- data.frame(cbind(plate_indices,sample_name,replicate_name))
    colnames(plate_tabular) <- c('Well','Sample','Replicate')

    # Getting rid of NAs
    plate_tabular <- replace(plate_tabular, is.na(plate_tabular),"")

    # Fixing object type issues
    plate_tabular <- apply(plate_tabular,2,as.character) 

    if (save_tabular == TRUE){
        
        # Getting the filename
        tabular_filename <- paste(file_path_sans_ext(file),"_",sheet,"_tabular.csv",sep="")
        write.csv(plate_tabular,file=tabular_filename, row.names=FALSE, quote=FALSE)
    }

    return(data.frame(plate_tabular))
}

create_plate <- function(plate_layout, output_file="example_data/plate_layout.txt", 
    reporter = 'FAM', quencher = 'NFQ-MGB', task = 'UNKNOWN', 
    sample_color = '"RGB(0,139,69)"', target_color = '"RGB(0,139,69)"', 
    header_file = 'example_data/plate_layout_header.txt'){

    
    # Takes a tabular plate layout (pandas data frame) format as input 
    # and outputs a qPCR plate layout file.
    

    sample_columns <- c('Well','Well Position','Sample Name','Sample Color','Biogroup Name','Biogroup Color','Target Name','Target Color','Task','Reporter','Quencher','Quantity','Comments')
    plate_setup <- data.frame(matrix(,nrow=96,ncol=length(sample_columns)))
    colnames(plate_setup) <- sample_columns;

    plate_layout <- data.frame(plate_layout)
    
    plate_setup$Well <- 1:96
    plate_setup$'Well Position' <- plate_layout$Well
    plate_setup$'Sample Name' <- plate_layout$Sample
    plate_setup$'Target Name' <- plate_layout$Replicate


    quencher_array <- list()
    reporter_array <- list()
    task_array <- list()
    sample_color_array <- list()
    target_color_array <- list()


    for (sample in unlist(plate_setup['Sample Name'])){
        if (sample == ''){
            quencher_array <- append(quencher_array,'');
            reporter_array <- append(reporter_array,'');
            task_array <- append(task_array,'');
            sample_color_array <- append(sample_color_array,'');
            target_color_array <- append(target_color_array,'');
        }    
        else {
            quencher_array <- append(quencher_array,quencher)
            reporter_array <- append(reporter_array,reporter)
            task_array <- append(task_array,task)
            sample_color_array <- append(sample_color_array,sample_color)
            target_color_array <- append(target_color_array,target_color)
        }
    }
    plate_setup$'Sample Color' <- sample_color_array; 
    plate_setup$'Target Color' <- target_color_array; 
    plate_setup$'Task' <- task_array;
    plate_setup$'Reporter' <- reporter_array;
    plate_setup$'Quencher' <- quencher_array;

    # Getting rid of NAs
    plate_setup <- replace(plate_setup, is.na(plate_setup),"")

    # Fixing object type issues
    plate_setup <- apply(plate_setup,2,as.character) 

    #Grabbing and writing header file
    header_file <- readLines('example_data/plate_layout_header.txt')
    writeLines(header_file, output_file)
    
    # Writing to output
    write.table(plate_setup,file=output_file, append = TRUE, sep = "\t", row.names=FALSE, quote=FALSE)
    return(plate_setup)
}