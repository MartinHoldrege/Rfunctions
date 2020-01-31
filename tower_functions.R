
# martin holdrege 

# script started 11/21/19

# functions used for processing and working with tower data 
# (e.g. NVI, PRI, IR)


# renaming ----------------------------------------------------------------

rename_twr <- function(df) {
    # args:
    #   df--data frame of twr files read in with readxl with first row skipped
    # returns:
    #   renamed data frame
    stopifnot(
        is.data.frame(df),
        ncol(df) == 15
    )
    date_name <- names(df)[1]
    df <- dplyr::rename(df,date.time = date_name,
                hem_532="SRS-Pi PRI Hemispherical..2",
                hem_570= "SRS-Pi PRI Hemispherical..3",
                hem_PRI= "SRS-Pi PRI Hemispherical..4",
                hem_630= "SRS-Ni NDVI Hemispherical..5",
                hem_800 = "SRS-Ni NDVI Hemispherical..6", 
                hem_NDVI= "SRS-Ni NDVI Hemispherical..7",
                field_532= "SRS-Pr PRI Field Stop..8" , 
                field_570= "SRS-Pr PRI Field Stop..9",
                field_PRI= "SRS-Pr PRI Field Stop..10", 
                field_630= "SRS-Nr NDVI Field Stop..11", 
                field_800= "SRS-Nr NDVI Field Stop..12", 
                field_NDVI= "SRS-Nr NDVI Field Stop..13", 
                IR_target= "SI-421 Infrared Radiometer..14", 
                IR_body= "SI-421 Infrared Radiometer..15")
    df
    
}
