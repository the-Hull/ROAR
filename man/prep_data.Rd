% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_data.R
\name{prep_data}
\alias{prep_data}
\title{Prepare data assimilation with templated scripts}
\usage{
prep_data(
  meta_table_path,
  data_processed_path,
  verbose = TRUE,
  overwrite = FALSE,
  oid = NULL,
  template = NULL,
  data_list = NULL
)
}
\arguments{
\item{meta_table_path}{character, txt (tab delimited) with some pre-defined headers, including from automated additions from previous runs of this function}

\item{data_processed_path}{character, path to \code{data_processed} folder (i.e. where cleaned data should live)}

\item{verbose}{logical, display informative messages and user prompts to proceed?}

\item{overwrite}{should existing scripts be overwritten?}

\item{oid}{numeric, length one, row in meta table}

\item{template}{character, path to custom template}

\item{data_list}{named list, where names correspond to whisker tokens in template, and values refer to columns in meta table (i.e.  \code{meta$column[oid]})}
}
\value{
Nothing.
}
\description{
\code{prep_data()} cycles through a meta table (created programmatically or manually) and generates a convenient database-like
folder structure and processing scripts based on templates for each dataset entry in the meta table.
}
\details{
The meta table must contain one row per dataset that will be assimilated.
It must have at least the following columns, which are used to generate database-like folder structures,
processing scripts from templates, and to populate these with information from the meta table:
\itemize{
\item \strong{oid}: original meta table entry id
\item \strong{param}: the measured parameter stored in the file
\item \strong{id_ ..}: identifier column(s) prefixed with \code{id_}.
\item \strong{file_raw}: absolute or project/working directory-relative file path to data set
}

These will be used to generate a folder structure of the form:
\itemize{
\item \code{param/id_[1]/id_[2]/.../id_[n-1]}
}

and a processing script within that folder of the form:
\itemize{
\item \verb{param_id_[1]..id_[n].R}
}

as well as intermediate files/outputs from the populated template script with similar naming conventions.

The meta table can include as much detail and information as is desired, and entries can be accessed
through token-value combinations in \code{data_list} in a custom \code{template}.
The internal template(s) all have access to fixed set of these token-value combinations (see further below),
which are structured following \href{http://mustache.github.io/}{\code{mustache} syntax} through \code{\link[whisker:whisker.render]{whisker::whisker.render()}}.

Tokens that are \strong{always} available and included in the package-internal templates include:
\itemize{
\item \strong{param}: parameter corresponding to OID in meta table currently processed,
\item \strong{raw_file}: path to raw file corresponding to OID in meta table currently processed,
\item \strong{save_dir}: path to (new) directory where all outputs from OID in meta table will be saved,
\item \strong{base_file_name}: object and file names in processing script (made from template), based on OID in meta table currently processed,
\item \strong{current_index}: OID that is currently processed,
\item \strong{date_time}: Sys.Date(),
\item \strong{excel_file}: logical flag, is raw file MS Excel-based?,
\item \strong{text_file}: logical flag, is raw file text based?
\item \strong{meta_table_path}: path to meta table
}

To use a custom template with only \strong{internal} tokens, supply an empty list: \code{data_list = list()}.

If there are multiple values or columns in the data, that need to be represented in the meta table,
split the column names by forward dashes (\code{/}).
For example, if a wide table needs to be split into long format, but a unique observation is defined through
multiple columns, (e.g. site, plot, tree), these values can be listed as \code{site/plot/tree} in a aptly named meta table column for
later use in a template.
}
