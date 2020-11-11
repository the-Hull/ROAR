# utils

`%nin%` <- Negate(`%in%`)



#' Wrapper for updating a meta table
#'
#' @param meta_table_path character, path to meta table (assumed relative to project dir, but can be absolute)
#' @param idx numeric, length 1, giving row index of the meta table corresponding to dataset being processed
#' @param update_list named list, length 1, where name equals the target column in meta table, and value is the adjustment
#' @param ... reserved for including automated git commits to track changes to meta table
#'
#' @return nothing
#' @export
#'
update_meta <- function(meta_table_path, idx, update_list, ...){

    # check if meta exists
    if(!fs::file_exists(meta_table_path)){
        usethis::ui_stop("Meta table does not found in the supplied path")
    }

    # check if idx is numeric and length 1
    if(length(idx) != 1 | !is.numeric(idx)){
        usethis::ui_stop("The meta table index must be a numeric vector of length 1.")
    }

    # read meta and adjust colnames
    meta <- read.csv(file = meta_table_path,
                     header = TRUE,
                     fileEncoding="UTF-8-BOM",
                     stringsAsFactors = FALSE)
    colnames(meta) <- tolower(colnames(meta))


    # check if meta has any process columns
    process_columns <- c("prep_script_path",
                         "prep_file_path",
                         "prep_done",
                         "clean_file_path",
                         "clean_done")





    if(!all(process_columns %in% colnames(meta) )){
        usethis::ui_stop("ROAR process columns not found in meta table")
    }

    # check update_list is length=1 and named
    if(length(update_list) != 1 | is.null(names(update_list))){
        usethis::ui_stop("update_list must be a length 1 and named list. Please change it accordingly.")

    }

    # update meta using name of list, and value of list
    meta[idx, names(update_list)] <- update_list[[1]]

    # write meta back to meta_path
    write.csv(x = meta,
              file = meta_table_path,
              row.names = FALSE)


}





check_meta_db <- function(meta_path){


    # grab process colnames

    # for each row, check file and script paths exist. Notify if anything is out of sync, and offer to launch
    # prep_data() with that oid


}
