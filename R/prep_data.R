#' Prepare data assimilation with templated scripts
#'
#' `prep_data()` cycles through a meta table (created programmatically or manually) and generates a convenient database-like
#' folder structure and processing scripts based on templates for each dataset entry in the meta table.
#'
#' @param meta_table_path character, txt (tab delimited) with some pre-defined headers, including from automated additions from previous runs of this function
#' @param data_processed_path character, path to `data_processed` folder (i.e. where cleaned data should live)
#' @param verbose logical, display informative messages and user prompts to proceed?
#' @param oid numeric, length one, row in meta table
#' @param overwrite should existing scripts be overwritten?
#' @param template character, path to custom template
#' @param data_list named list, where names correspond to whisker tokens in template, and values refer to columns in meta table (i.e.  `meta$column[oid]`)
#'
#' @details
#' The meta table must contain one row per dataset that will be assimilated.
#' It must have at least the following columns, which are used to generate database-like folder structures,
#' processing scripts from templates, and to populate these with information from the meta table:
#'
#'  + **oid**: original meta table entry id
#'  + **param**: the measured parameter stored in the file
#'  + **id_ ..**: identifier column(s) prefixed with `id_`.
#'  + **file_raw**: absolute or project/working directory-relative file path to data set
#'
#'  These will be used to generate a folder structure of the form:
#'
#'  + `param/id_[1]/id_[2]/.../id_[n-1]`
#'
#'  and a processing script within that folder of the form:
#'
#'  + `param_id_[1]..id_[n].R`
#'
#'  as well as intermediate files/outputs from the populated template script with similar naming conventions.
#'
#' The meta table can include as much detail and information as is desired, and entries can be accessed
#' through token-value combinations in `data_list` in a custom `template`.
#' The internal template(s) all have access to fixed set of these token-value combinations (see further below),
#' which are structured following [`mustache` syntax](http://mustache.github.io/) through [whisker::whisker.render()].
#'
#'
#' Tokens that are **always** available and included in the package-internal templates include:
#' + **param**: parameter corresponding to OID in meta table currently processed,
#' + **raw_file**: path to raw file corresponding to OID in meta table currently processed,
#' + **save_dir**: path to (new) directory where all outputs from OID in meta table will be saved,
#' + **base_file_name**: object and file names in processing script (made from template), based on OID in meta table currently processed,
#' + **current_index**: OID that is currently processed,
#' + **date_time**: Sys.Date(),
#' + **excel_file**: logical flag, is raw file MS Excel-based?,
#' + **text_file**: logical flag, is raw file text based?
#' + **meta_table_path**: path to meta table
#'
#' To use a custom template with only **internal** tokens, supply an empty list: `data_list = list()`.
#'
#' If there are multiple values or columns in the data, that need to be represented in the meta table,
#' split the column names by forward dashes (`/`).
#' For example, if a wide table needs to be split into long format, but a unique observation is defined through
#' multiple columns, (e.g. site, plot, tree), these values can be listed as `site/plot/tree` in a aptly named meta table column for
#' later use in a template.
#'
#' @return Nothing.
#' @export
#'
prep_data <- function(meta_table_path,
                      data_processed_path,
                      verbose = TRUE,
                      overwrite = FALSE,
                      oid = NULL,
                      template = NULL,
                      data_list = NULL){


    # check meta_table_path is txt
    if(!inherits(meta_table_path, "character") ||
        fs::path_ext(meta_table_path) != "txt"){
        usethis::ui_stop("Please provide the meta table as a path (character) to a txt file (tab delimited)")
    }

    # check if output path is character
    if(!inherits(data_processed_path, "character")){
        usethis::ui_stop("Please provide the output directory as a path (character)")
    }



    # read file and grab id columns
    meta <- read.table(file = meta_table_path,
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

        start_index <- which(!complete.cases(meta[ , process_columns]))



        if(length(start_index) > 0){

            start_index <- min(start_index)
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

        # usethis::ui_todo("The following data set will be processed:")
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

    # set up token lits for whisker templating
    data_list_internal <-  list(param = current_param,
                                raw_file = current_file,
                                save_dir = out_dir_path,
                                base_file_name = base_file_name,
                                current_index = start_index,
                                date_time = Sys.Date(),
                                excel_file = excel,
                                text_file = text,
                                meta_table_path = meta_table_path)

    if(is.null(template) && is.null(data_list)){

        templating_expr <- expression(
            usethis::use_template(
                template = "prep_script.R",
                save_as = path_prep_script,
                data = data_list_internal,
                package = "ROAR",
                open = TRUE))
    } else {


        # make list with custom and internal values
        # append flag list for external
        data_list <- c(lapply(data_list,
                            function(x) meta[start_index, x]),
                       data_list_internal)
        data_list <- c(data_list,
                       make_flag_list(data_list))



        templating_expr <- expression({

            template_content <- strsplit(
                whisker::whisker.render(
                    usethis:::read_utf8(
                        template),
                    data = data_list),
                "\n")[[1]]
            usethis::write_over(path = path_prep_script,
                                lines = template_content,
                                quiet = TRUE)
            usethis::edit_file(path_prep_script)
        })
    }




    # create if doesn't exist
    # if exists, don't do anything - unless in verbose mode, where choice is given.
    if(!fs::file_exists(path_prep_script)){

        if(overwrite){
            usethis::ui_todo("Prepping script already exists. Overwriting due to overwrite = TRUE")
        } else if(!overwrite){
            usethis::ui_stop("Prep script already exists - set overwrite = TRUE, or manually delete the script.")
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

    write.table(x = meta,
              file = meta_table_path,
              row.names = FALSE)


    # return(list(id_cols, meta))



}
