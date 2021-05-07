# datatable function

#data fram link = https://datatables.net/extensions/buttons/examples/initialisation/export.html
datatable2 <- function(x, vars = NULL, opts = NULL, ...) {
  names_x <- names(x)
  if (is.null(vars)) stop("'vars' must be specified!")
  pos <- match(vars, names_x)
  if (any(map_chr(x[, pos], typeof) == "list")) {
    stop("list columns are not supported in datatable2()")
  }

  pos <- pos[pos <= ncol(x)] + 1
  rownames(x) <- NULL
  if (nrow(x) > 0) x <- cbind(" " = "&#9658;", x)

  # options
  opts <- c(
    opts,
    list(
      columnDefs = list(
        list(visible = FALSE, targets = c(0, pos)),
        list(orderable = FALSE, className = "details-control", targets = 1),
        list(className = "dt-left", targets = 1:3),
        list(className = "dt-right", targets = 4:ncol(x))
      )
    )
  )

  datatable(
    x,
    ...,
    escape = -2,
    options = opts,
    callback = JS(.callback2())
  )
}

# first callback
.callback2 <- function() {
  "table.column(1).nodes().to$().css({cursor: 'pointer'});
  var format = function(d) {
    text = '<div><table >' + 
                      '<tr>' +\n  
                          '<td>' + 'Employee ID:' + '</td>' +\n
                          '<td>' + d[8] + '</td>' +\n
                      '</tr>' +  
                      '<tr>' +\n  
                          '<td>' + 'Age:' + '</td>' +\n 
                          '<td>' + d[5] + '</td>' +\n
                      '</tr>' +  
                      '<tr>' +\n  
                          '<td>' + 'Start date:' + '</td>' +\n 
                          '<td>' + d[6] + '</td>' +\n
                      '</tr>' + '</table></div>'
      return text;};
   table.on('click', 'td.details-control', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if (row.child.isShown()) {
      row.child.hide();
      td.html('&#9658;');
    } else {
      row.child(format(row.data())).show();
      td.html('&#9660;');
    }
  });"
  
  
}





# main code
library(tidyverse)
library(DT)
x <- readr::read_csv("datatable.csv")
x[["Employee ID"]] <- round(runif(nrow(x)) * 10000)

datatable2(
  x = x,
  vars = c("Employee ID", "Age", "Start date"),
  opts = list(pageLength = 5)
)

# "\n  
# var format = function(d) {
# \n    text = '<div><table >' + \n 
#                       '<tr>' +\n  
#                           '<td>' + 'Employee ID:' + '</td>' +\n
#                           '<td>' + d[8] + '</td>' +\n
#                       '</tr>' +  
#                       '<tr>' +\n  
#                           '<td>' + 'Age:' + '</td>' +\n 
#                           '<td>' + d[5] + '</td>' +\n
#                       '</tr>' +  
#                       '<tr>' +\n  
#                           '<td>' + 'Start date:' + '</td>' +\n 
#                           '<td>' + d[6] + '</td>' +\n
#                       '</tr>' + '</table></div>'\n 
#         return text;};"
