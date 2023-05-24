openFiles_fun <- function(curr_year){
  shell.exec(
    paste0(
      "Input\\Background Data\\Big Sheets\\",
      curr_year-1,
      "WillametteBigSheet.xlsx"
    )
  )
  
  shell.exec(
    paste0(
      "Input\\Background Data\\Stoplight Charts\\",
      curr_year,
      "noaaStoplightChart.xlsx"
    )
  )
  
  shell.exec(
    paste0(
      "Input\\Background Data\\Willamette Falls Counts\\",
      curr_year,
      "WillametteFallsCounts.xlsx"
    )
  )
}