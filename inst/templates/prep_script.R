# META -------------------------------------------------------------------
# ROAR for
# parameter: {{param}}
# sample_file_id: {{base_file_name}}
# Created on {{date_time}}
# Author:

# START ------------------------------------------------------------------------

current_index <- {{current_index}}
meta_table_path <- "{{meta_table_path}}"

# Read data ---------------------------------------------------------------


## parameters for reading


skip <- 0
{{#excel_file}}

{{base_file_name}} <- janitor::clean_names(readxl::read_excel(path = "{{raw_file}}",
                                         skip = skip))
{{/excel_file}}
{{#text_file}}
header <- TRUE
stringsAsFactors <- FALSE
seperator <- ","

{{base_file_name}} <- janitor::clean_names(utils::read_csv(path = "{{raw_file}}",
                                             skip = skip,
                                      header = header,
                                      stringsAsFactors = stringsAsFactors,
                                      sep = seperator))

{{/text_file}}




## add additional cleaning steps, where necessary

# Reshape to long format --------------------------------------------------

# this section will always need manual adjustments
# modify / add as necessary

identifier_columns <- c("timestamp")
# identifier_columns <- c("timestamp", "sample")
# identifier_columns <- c("timestamp", "sample_id")

{{base_file_name}} <- tidyr::pivot_longer(data = {{base_file_name}},
                                          cols = !tidyselect::all_of(identifier_columns),
                                          names_to = "sensor",
                                          values_to = "value")
# check all columns have appropriate values
str({{base_file_name}})



# Save PREP data ---------------------------------------------------------------

# save to {{save_dir}}

path_save_prep <- fs::path("{{save_dir}}",
                           paste("{{base_file_name}}", "prep", sep = "_"),
                                ext = "Rds")

saveRDS(object = {{base_file_name}},
        file = path_save_prep)

# update the meta table

ROAR::update_meta(meta_table_path = meta_table_path,
            idx = current_index,
            update_list = list(prep_file_path = path_save_prep))




# Update meta table (PREP) -------------------------------------------------------

ROAR::update_meta(meta_table_path = meta_table_path,
            idx = current_index,
            update_list = list(prep_done = TRUE))


# Run datacleanr (CLEAN)----------------------------------------------------------

datacleanr::dcr_app(path_save_prep)




# copy CLEAN to data_processed ---------------------------------------------------


# Update meta table (CLEAN) -------------------------------------------------------

clean_file_path <- fs::path("{{save_dir}}",
                            paste("{{base_file_name}}", "prep_cleaned", sep = "_"),
                            ext = "Rds")

ROAR::update_meta(meta_table_path = meta_table_path,
            idx = current_index,
            update_list = list(clean_file_path = clean_file_path))

ROAR::update_meta(meta_table_path = meta_table_path,
            idx = current_index,
            update_list = list(clean_done = TRUE))



