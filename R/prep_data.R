#' Prepare data for assimilation with useful bare-bones script
#'
#' Drops last id column for nesting level.
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
                      overwrite = FALSE,
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
                     fileEncoding="UTF-8-BOM",
                     stringsAsFactors = FALSE)
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
                  prep_done = NA,
                  clean_path = NA,
                  clean_done = NA,

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



    current_param <- meta$param[start_index]



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
        message("And modify the meta table at meta_table_path\n")

        user_input <- readline(sprintf("Continue with processing of OID %i? - Y/N:\n", start_index))
        if(user_input %nin% c("Y", "y", "T", "TRUE")){
            stop("Aborted processing.")
        }

    }


    # check for and create directory in data_processed_path
    # make out file_path


    if(length(id_cols) > 1){
        folder_id_cols <- id_cols[-length(id_cols)]
    }   else {
        folder_id_cols <- id_cols
    }

    out_dir_path <- fs::path(data_processed_path,
                             current_param,
                             paste0(meta[start_index, folder_id_cols], collapse = "/"))


    base_file_name <- paste0(
        c(current_param,
          paste0(meta[start_index, id_cols],
                 collapse = "_")),
        collapse = "_")

    print(out_dir_path)
    print(base_file_name)
    #
    if(!fs::dir_exists(out_dir_path)){
        fs::dir_create(out_dir_path, recurse = TRUE)
        message(sprintf("Creating folder(s): (%s)", out_dir_path))
    } else {
        message(sprintf("Output path already exists (%s)", out_dir_path))
    }


    # Make Script template
    path_prep_script <- fs::path(out_dir_path, base_file_name, ext = "R")


    # is file a excel or MS?

    current_file <- meta$file_raw[start_index]

    # manage read in for template
    if(fs::path_ext(current_file) %in% c("csv", "txt", "dat")){
        excel <- FALSE
        text <- TRUE
    } else if(fs::path_ext(current_file) %in% c("xls", "xlsx")){
        excel <- TRUE
        text <- FALSE
    }

    templating_expr <- expression( usethis::use_template(template = "prep_script.R",
                                                         save_as = path_prep_script,
                                                         data = list(param = current_param,
                                                                     raw_file = current_file,
                                                                     save_dir = out_dir_path,
                                                                     base_file_name = base_file_name,
                                                                     current_index = start_index,
                                                                     date_time = Sys.Date(),
                                                                     excel_file = excel,
                                                                     text_file = text,
                                                                     meta_table_path = meta_table_path),
                                                         package = "ROAR",
                                                         open = TRUE))


    # create if doesn't exist
    # if exists, don't do anything - unless in verbose mode, where choice is given.
    if(!fs::file_exists(path_prep_script) | overwrite){

        if(overwrite){
            message("Prepping script already exists. Overwriting due to overwrite = TRUE")
        }

        eval(templating_expr)

    } else {

        if(verbose){
            user_input <- readline(sprintf("Processing script for this OID (%i?) exists. Overwrite? - Y/N:\n", start_index))
            if(user_input %nin% c("Y", "y", "T", "TRUE")){
                stop("Aborted processing.")
            } else {

                eval(templating_expr)
            }

        } else {
            stop(sprintf("Processing script for this OID (= %i) exists. Aborting", start_index))

        }

    }


    meta[start_index, "prep_script_path"] <- path_prep_script

    write.csv(x = meta,
              file = meta_table_path,
              row.names = FALSE)


    # return(list(id_cols, meta))



}
