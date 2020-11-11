#' Prepare data for assimilation with useful bare-bones script
#'
#' Drops last id column for nesting level.
#'
#' @param meta_table_path character, csv with some pre-defined headers, including from automated additions from previous runs of this function
#' @param data_processed_path character, path to `data_processed` folder (i.e. where cleaned data should live)
#' @param verbose logical, display informative messages and user prompts to proceed?
#' @param oid numeric, length one, row in meta table
#' @param overwrite should existing scripts be overwritten?
#' @param template .. not yet implemented, custom template
#' @param data_list .. not yet implemented, custom data list (referring to meta$column[oid])
#'
#' @return
#' @export
#'
prep_data <- function(meta_table_path,
                      data_processed_path,
                      verbose = TRUE,
                      overwrite = FALSE,
                      oid = NULL,
                      template = NULL,
                      data_list = NULL){


    # check meta_table_path is csv
    if(!inherits(meta_table_path, "character") ||
        fs::path_ext(meta_table_path) != "csv"){
        usethis::ui_stop("Please provide the meta table as a path (character) to a csv file")
    }

    # check if output path is character
    if(!inherits(data_processed_path, "character")){
        usethis::ui_stop("Please provide the output directory as a path (character)")
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
    if(!any(grepl(pattern = "(^prep[_].*)|(^clean.*)",
                 x = colnames(meta),
                 perl = TRUE))){

        usethis::ui_info("Adding columns to meta table.")

        meta <- base::transform(meta,
                  prep_script_path	= NA,
                  prep_file_path = NA,
                  prep_done = NA,
                  clean_file_path = NA,
                  clean_done = NA

        )
    }

    # identify which sample to start with,
    # if OID is specified, then use that index
    if(!is.null(oid)){

        if(nrow(meta) < oid){
            usethis::ui_stop("Please provide an OID found in the table.")
        }

        if(any(diff(meta$oid) != 1)){
            usethis::ui_stop("Check the your meta table - OIDs are out of order")
        }

        start_index <- oid


    } else {

        process_columns <- c(
            "prep_script_path",
            "prep_file_path",
            "prep_done",
            "clean_file_path",
            "clean_done")



        # prep_script_path	prep_file_path	prep_done	clean_path	clean_done


        if(all(complete.cases(meta[ , process_columns]))){

            usethis::ui_stop("All data sets have been processed. Set oid to a specific value to re-process")

        }

        start_index <- which(complete.cases(meta[ , process_columns]))



        if(length(start_index) > 0){

            start_index <- start_index + 1
        } else {

            start_index <- 1
        }


        # start_index <- min(which(is.na(meta$prep_done)))
    }



    current_param <- meta$param[start_index]



    # give informative message if verbose


    # check for and create directory in data_processed_path
    # make out out_dir_path


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


    usethis::ui_todo(sprintf("Checking processing for: %s", base_file_name))


    if(verbose && is.na(meta[start_index, "prep_script_path"])){

        usethis::ui_todo("The following data set will be processed:")
        # message(meta[start_index, c("oid", id_cols, "param")])

        cat("\n")
        usethis::ui_info("If necessary, this will create:")
        usethis::ui_line("  - parameter parent folder,")
        usethis::ui_line("  - ID sub-folders,")
        usethis::ui_line("  - prepping script")

        user_input <- readline(sprintf("Continue with processing of OID %i? - Y/N:\n", start_index))
        if(user_input %nin% c("Y", "y", "T", "TRUE")){
            usethis::ui_stop("Aborted processing.")
        }

    }





    if(!fs::dir_exists(out_dir_path)){
        fs::dir_create(out_dir_path, recurse = TRUE)
        usethis::ui_todo(sprintf("Creating folder(s): (%s)", out_dir_path))
    } else {
        usethis::ui_info(sprintf("Output path already exists (%s)", out_dir_path))
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


    if(is.null(template) && is.null(data_list)){

        templating_expr <- expression(
            usethis::use_template(
                template = "prep_script.R",
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
    } else {

        templating_expr <- expression({
            writelines(
                text = whisker::whisker.render(
                    template = template,
                    data = data_list),
                con = path_prep_script)
            usethis::edit_file(path_prep_script)
        })
    }




    # create if doesn't exist
    # if exists, don't do anything - unless in verbose mode, where choice is given.
    if(!fs::file_exists(path_prep_script) | overwrite){

        if(overwrite){
            usethis::ui_todo("Prepping script already exists. Overwriting due to overwrite = TRUE")
        }

        eval(templating_expr)


    } else {

        if(verbose){
            user_input <- readline(sprintf("Processing script for this OID (%i?) exists. Overwrite? - Y/N:\n", start_index))
            if(user_input %nin% c("Y", "y", "T", "TRUE")){
                usethis::ui_stop("Aborted processing.")
            } else {
                eval(templating_expr)

            }

        # not verbose
        } else {
            if(fs::file_exists(path_prep_script) &&
               (is.na(meta[start_index, "prep_done"]) || is.na(meta[start_index, "clean_done"]) ) ){

                usethis::ui_todo("Opening prep script to continue processing.")
                usethis::edit_file(path = path_prep_script)
            } else {


                usethis::ui_info(sprintf("Processing script for this OID (= %i) exists and is completed. ", start_index))
                user_input <- readline(sprintf("Overwrite or Continue working in the processing script? - O / C / eXit?:\n", start_index))
                if(user_input %in% c("C", "c")){

                    usethis::ui_todo("Continuing work in prep script.")
                    usethis::edit_file(path = path_prep_script)

                } else if(user_input %in% c("O", "o")){

                    usethis::ui_todo("Overwriting existing script")
                    eval(templating_expr)

                } else {

                    usethis::ui_stop("Aborted processing.")

                }
            }

        }

    }



    meta[start_index, "prep_script_path"] <- path_prep_script

    write.csv(x = meta,
              file = meta_table_path,
              row.names = FALSE)


    # return(list(id_cols, meta))



}
