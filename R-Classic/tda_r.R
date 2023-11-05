library(readxl)
library(tidyr)
library(readxl)
library(dplyr)
library(stringr)

local_df <- function(directory_path) {
  final_data <- tibble()
  
  # 파일 목록
  file_list <- list.files(directory_path, full.names = TRUE)
  
  # 파일 개수만큼 반복
  for (file_path in file_list) {
    # 파일 읽기 및 처리
    suppressMessages( # silence new names messages
      data <- read_excel(file_path)
    )
    data <- data %>% slice(3:n())
    colnames(data) <- data[1, ]
    data <- data[-1, c(-1, -3, -4)] # 관련 없는 행, 열 제거
    data <- data %>% mutate(행정기관 = sapply(str_split(행정기관, ' '), '[', 3))
    
    # 파일 경로 슬래시로 분할
    file_parts <- unlist(str_split(file_path, "/"))
    
    # 지역이름 추출
    directory_name <- file_parts[length(file_parts) - 1]
    
    # 년도추출
    year_code <- substr(file_parts[which(file_parts == directory_name) + 1], 1, 4)
    data$year <- as.integer(year_code)
    
    # 데이터를 final_data에 추가
    final_data <- bind_rows(final_data, data)
  }
  
  return(final_data)
}

busan_data <- local_df("../Population-raw/서울")
