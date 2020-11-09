#' Prepare data for assimilation with useful bare-bones script
#'
#' @param meta_table_path character, csv with some pre-defined headers, including from automated additions from previous runs of this function
#' @param data_processed_path character, path to `data_processed` folder (i.e. where cleaned data should live)
#' @param verbose logical, display informative messages and user prompts to proceed?
#' @param oid
#'
#' @return
#' @export
#'
prep_data <- function(meta_table_path,
                      data_processed_path,
                      verbose = TRUE,
                      oid = NULL){


    # check meta_table_path is csv
    if(!inherits(meta_table_path, "character") ||
        fs::path_ext(meta_table_path) != "csv"){
        stop("Please provide the meta table as a path (character) to a csv file")
    }

    # check if output path is character
    if(!inherits(data_processed_path, "character")){
        stop("Please provide the output directory as a path (character)")
    }



    # read file and grab id columns
    meta <- read.csv(file = meta_table_path,
                     header = TRUE,
                     fileEncoding="UTF-8-BOM")
    colnames(meta) <- tolower(colnames(meta))

    id_cols_idx <- grep(pattern = "id[_].*",
                         x = colnames(meta),
                         perl = TRUE)
    id_cols <- colnames(meta)[id_cols_idx]

    # check if any prepping has been done,
    # and if not, add new prep columns to meta
    if(!any(grepl(pattern = "prep[_].*",
                 x = colnames(meta),
                 perl = TRUE))){

        meta <- base::transform(meta,
                  prep_script_path	= NA,
                  prep_file_path = NA,
                  prep_done = NA
        )
    }

    # identify which sample to start with,
    # if OID is specified, then use that index
    if(!is.null(oid)){

        if(nrow(meta) < oid){
            stop("Please provide an OID found in the table.")
        }

        if(any(diff(meta$oid) != 1)){
            stop("Check the your meta table - OIDs are out of order")
        }

        start_index <- oid


    } else {
        start_index <- min(which(is.na(meta$prep_done)))
    }


    # give informative message if verbose
    if(verbose){

        message("The following data set will be processed:")
        print(meta[start_index, c("oid", id_cols, "param")])
        # message(meta[start_index, c("oid", id_cols, "param")])

        cat("\n")
        message("This will create:")
        message("  - parameter parent folder,")
        message("  - ID sub-folders,")
        message("  - prepping script\n")

        user_input <- readline(sprintf("Continue with processing of OID %i? - Y/N:\n", start_index))
        if(user_input %nin% c("Y", "y", "T", "TRUE")){
            stop("Aborted processing.")
        }

    }


    # check for and create directory in data_processed_path





    return(list(id_cols, meta))



}
