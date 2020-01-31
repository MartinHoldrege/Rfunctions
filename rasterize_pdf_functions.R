
# Martin Holdrege

# script started 1/3/20

# purpose is to have a function that can tak a pdf and then convert it to a 
# rasterized (just images) pdf, so it is easier to view 
# (useful for figs with 100k points)

# some files, such as this one don't work:
# pdf = "figures/deltaT/Clark_229hrly_cor-deltaT_by-sensor_vs_date_v2.pdf"



# create png files from pdf pages -----------------------------------------

# doesn't work with some (semi corrupted?) pdfs

pdf2png <- function(pdf) {
    # args:
    #   pdf--path to pdf file
    # returns:
    #   creates png files of each page of the pdf, saved in working directory

    stopifnot(
        is.character(pdf)
    )
    
    # Magick requires ghostscript to be installed to render the PDF
    manual <- magick::image_read(pdf, density = "200x200", strip = TRUE)
    n <- length(manual)
    pngFiles <- paste0("temp_png", 1:n, ".png")
    
    for (i in 1:n) {
        magick::image_write(manual[i], path = pngFiles[i], format = "png") 
    }
    info <- magick::image_info(manual[1])
    # convering height to inches (based on set 200x200 pixel value above)
    out <- list("names" = pngFiles,
                "width" = (info$width/200),
                "height" = (info$height/200)
                )
    out
}


# merge png files into pdf ------------------------------------------------


merge.png.pdf <- function(pdfFile, pngFiles, deletePngFiles = FALSE,
                          height = 7, width = 7) {
    # function from https://jonkimanalyze.wordpress.com/2014/07/24/r-compile-png-files-into-pdf/ 
    # and modified slightly
    
    #### Package Install ####
    pngPackageExists <- require ("png")
    
    if ( !pngPackageExists ) {
        install.packages ("png")
        library ("png")
        
    }
    #########################
    
    pdf(pdfFile, width = width,
        height = height)
    
    n <- length(pngFiles)
    
    for( i in 1:n) {
        
        pngFile <- pngFiles[i]
        
        pngRaster <- readPNG(pngFile)
        
        grid::grid.raster(pngRaster)
        
        if (i < n) plot.new()
        
    }
    
    dev.off()
    
    if (deletePngFiles) {
        
        unlink(pngFiles)
    }
    
}


# convert pdf to rasterized pdf -------------------------------------------

rasterize_pdf <- function(pdf_in, pdf_out = NULL, deletePngFiles = TRUE) {
    # args:
    #   pdf_in --path to pdf
    #   pdf_out --destination of new pdf (if NULL pdf_in is replaced)
    #   deletingPngFiles--delete the png files created in the process?
    # returns:
    #   pdf
    stopifnot(
        is.character(pdf_in),
        is.character(pdf_out) | is.null(pdf_out)
    )

    png_info <- pdf2png(pdf = pdf_in)

    # if not changing name
    if (is.null(pdf_out)) {
        pdf_out <- pdf_in
        # delete original pdf so can be replaced
        unlink(pdf_in) 
    }
    
    merge.png.pdf(pdfFile = pdf_out,
                  pngFiles = png_info$names,
                  height = png_info$height,
                  width = png_info$width,
                  deletePngFiles = deletePngFiles)
}

# pdf = "figures/deltaT/Clark_229hrly_cor-deltaT_by-sensor_vs_date_v2_comp.pdf"
# rasterize_pdf(pdf,
#               "test7.pdf")

