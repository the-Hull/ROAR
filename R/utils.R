# utils
#' Negated matching
#' @export
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
        usethis::ui_stop("Meta table not found in the supplied path")
    }

    # check if idx is numeric and length 1
    if(length(idx) != 1 | !is.numeric(idx)){
        usethis::ui_stop("The meta table index must be a numeric vector of length 1.")
    }

    # read meta and adjust colnames
    meta <- read.table(file = meta_table_path,
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
    write.table(x = meta,
              file = meta_table_path,
              row.names = FALSE,
              sep="\t")

}





#' Check if all generated files exist
#'
#' @param meta
#'
#' @return
#' @export
#'
check_meta_paths <- function(meta){

    # check for "path" columns
    path_cols <- colnames(meta)[grepl("path", x = colnames(meta))]
    if(length(path_cols) == 0){

        usethis::ui_stop("No processing has been done yet.")
    }

    meta_path_only <- meta[ ,path_cols]



    not_na_paths <- !apply(meta_path_only, MARGIN = c(2), is.na)
    existing_files <- apply(meta_path_only, MARGIN = c(2), fs::file_exists)


    meta_path_only[not_na_paths & existing_files] <- "confirmed"
    meta_path_only[not_na_paths & !existing_files] <- "issue"
    meta_path_only[!not_na_paths & !existing_files] <- NA

    assessment <- cbind(oid = meta$oid, meta_path_only)

    return(assessment)

}


#' Print internal template to console
#'
#' @param template_name character, name of internal tempalte (currently only `prep_script.R` )
#'
#' @return all available internal templates
#' @export
#'
view_template <- function(template_name = "prep_script.R"){

    templates <- fs::path_file(fs::dir_ls(fs::path_package("ROAR", "templates")))

    if(template_name %nin% templates){
        usethis::ui_stop(sprintf("The template is misspelled or does not exist yet. \n
                         Current ROAR templates are: %s", templates))
    }


    usethis::ui_code_block(readLines(fs::path_package("ROAR", "templates", template_name)))

    return(templates)

}


#' Split up strings in meta table
#'
#' For multi-column data adjustments/details, e.g. definition of time stamp columns, a meta string
#' can be provided in a single column of the meta table (e.g. `time_stamp_columns`), preventing the
#' meta table from cluttering.
#' This is can be leveraged for in templates to construct objects from meta strings.
#' A meta string can be set to either `NA`, a single value (`"string"`) or a multi-column string
#' (`"string1/string2/string3/string with space4"`).
#' This function splits these strings, and allows e.g. pasting multiple columns together.
#'
#' @param string
#'
#' @return character, (split) `string`
#' @export
parse_meta_strings <- function(string){

    if(!is.character(string)){
        usethis::ui_stop("Please provide string as character")
    }

    string <- unlist(strsplit(string, split = "/"))

    # clean formatting
    string <- gsub(pattern = "(^[ ]*|[ ]*$)", replacement = "",x = string, perl = TRUE)
    return(string)

}



#' Convenience function to create a universal time stamp
#'
#' Grab one or more time-defining columns and create a universal time stamp,
#' in character format, ideally as "YYYY-mm-dd HH:MM:SS)
#' This stamp can be re-read in subsequent steps, and the time zone adjusted as necessary.
#'
#' @param x data frame which contains `timestamp_cols`
#' @param timestamp_cols character, column(s) defining time stamp and must be in `x`
#'
#' @return x, with additional column `timestamp_roar`
#' @export
make_timestamp <- function(x, timestamp_cols){

    if(is.na(timestamp_cols)){
        return(x)
    } else if(!is.character(timestamp_cols)){
    # check that timestamps are provided in char
        usethis::ui_stop("Please provide timestamp_cols as character.")
    }



    # check if cols exist in df
    if(!all(timestamp_cols %in% colnames(x))){
        usethis::ui_stop("Time stamp column(s) are not (all) found in the data frame")
    }

    if(length(timestamp_cols)  == 1){
        x$timestamp_roar <-  as.character(x[ , timestamp_cols, drop = TRUE])
        message("length 1")
    } else {
        message("length > 1")
        x$timestamp_roar <- apply(x[ ,timestamp_cols],
                                  MARGIN = 1,
                                  function(x) paste(as.character(x), sep = "-"))
    }


    return(x)



}

# https://stackoverflow.com/a/48798064
# https://creativecommons.org/licenses/by-sa/3.0/
#
#

#' Skip read
#'
#' This function reads a `.csv` while skipping specified rows (e.g. meta table included beneath headers).
#' Note, that `stringsAsFactors = FALSE` is assumed.
#'
#' @param skip_internal numeric, vector of rows to skip **below** headers. These row numbers correspond to
#' the raw input, i.e. if headers are in row one, and additional information in row 2, `skip_internal = 2`.
#' @param sep character, delimiter value for tables (e.g. `,` `:`, `\t`)
#' @param ... additional arguments passed to `read_delim()`.
#'
#' @return data.frame read from `path`
#' @export
#'
readskip_delim <-  function(path, sep = ",", ..., skip_internal = NA){

    if(!rlang::inherits_any(path, c("character", "fs_path"))){
        usethis::ui_stop("Provide path to read in file as character / fs path")
    }

    if (!is.na(skip_internal)) {

        # tmp <-  textConnection(readLines(path)[-skip_internal])
        tmp <-  readLines(path)[-skip_internal]


        tmpFile = tempfile()
        on.exit(unlink(tmpFile))
        writeLines(tmp,tmpFile)
        # file = tmpFile

        path <- tmpFile
    }
    dframe <- readr::read_delim(path, delim = sep, ...)
    return(dframe)
}


#' Create list of conditional flags
#'
#' This function identifies whether an item in a named list is `NA` or not,
#' and creates a named list with respective `TRUE` / `FALSE` values.
#' This list can be appended to an existing whisker tokens list, and used in templates for conditional
#' generation of text snippets and code chunks.
#' The names in the generated list are of the form `previousname_roar_flag`
#'
#' @param tokenlist, named list with at least 1 item
#'
#' @return list, with one item per `tokenlist` item
#' @export
#'
make_flag_list <- function(tokenlist){

    if(length(tokenlist) < 1){
        usethis::ui_stop("Please provide 1-row data.frame")
    }

    logicals <- lapply(tokenlist, function(x) !is.na(x))
    logicals <- setNames(logicals, paste0(names(tokenlist), "_roar_flag"))
    return(logicals)

}
