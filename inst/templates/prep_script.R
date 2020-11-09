# META -------------------------------------------------------------------
# ROAR for
# parameter: {{param}}
# sample_file_id: {{base_file_name}}
# Created on {{date_time}}
# Author:
# START ------------------------------------------------------------------------




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

{{base_file_name}} <- janitor::clean_names(utils::read_csv(path = "{{raw_file}}",
                                             skip = skip,
                                      header = header,
                                      stringsAsFactors = stringsAsFactors))

{{/text_file}}

## add additional cleaning steps, where necessary

# Reshape to long format --------------------------------------------------

identifier_columns <- c("timestamp")
# identifier_columns <- c("timestamp", "sample")
# identifier_columns <- c("timestamp", "sample_id")

{{base_file_name}} <- tidyr::pivot_longer(data = {{base_file_name}},
                                          cols = !identifier_columns,
                                          names_to = "sensor",
                                          values_to = "value")


# Save PREP data ---------------------------------------------------------------

# save to {{save_dir}}

meta <- read.csv(file = "{{meta_table_path}}",
                 header = TRUE,
                 fileEncoding="UTF-8-BOM",
                 stringsAsFactors = FALSE)
colnames(meta) <- tolower(colnames(meta))

path_save_prep <- fs::path("{{save_dir}}",
                           paste("{{base_file_name}}", "prep", sep = "_"),
                                ext = "RDS")


meta[{{current_index}}, "prep_file_path"] <- path_save_prep

write.csv(x = meta,
          file = "{{meta_table_path}}",
          row.names = FALSE)



# Update meta table (PREP) -------------------------------------------------------

meta <- read.csv(file = meta_table_path,
                 header = TRUE,
                 fileEncoding="UTF-8-BOM",
                 stringsAsFactors = FALSE)
colnames(meta) <- tolower(colnames(meta))

meta[{{current_index}}, "prep_done"] <- TRUE


# Run datacleanr (CLEAN)----------------------------------------------------------




# copy CLEAN to data_processed -----------------------------------



# Update meta table (CLEAN) -------------------------------------------------------

meta <- read.csv(file = meta_table_path,
                 header = TRUE,
                 fileEncoding="UTF-8-BOM",
                 stringsAsFactors = FALSE)
colnames(meta) <- tolower(colnames(meta))

meta[{{current_index}}, "clean_done"] <- TRUE

