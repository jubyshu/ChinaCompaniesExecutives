library(data.table)
library(rvest)

# 深交所上市公司
sz_companies <- readxl::read_xlsx("~/downloads/sz_companies.xlsx")
setDT(sz_companies)
sz_companies <- sz_companies[, .(A股代码, A股简称)][!is.na(A股代码)]
setnames(sz_companies, names(sz_companies), c("code", "company"))
sz_companies[, company := gsub(" ", "", company)]

# 上交所上市公司
sh_url <- "https://resource.emagecompany.com/publiccompanies/listedcompanies_shanghai.html"
sh_companies <- sh_url %>% 
  read_html() %>% 
  html_nodes("div#eWebEditor_Temp_Excel_Sheet_Div table") %>% 
  html_table(fill = TRUE) %>% 
  as.data.table() %>% 
  .[-c(1:2)]
setnames(sh_companies, names(sh_companies), c("code", "company"))

# A股公司代码
company <- rbind(sz_companies, sh_companies)
company_code <- company$code

# 创建函数，从凤凰财经抓取数据
# member <- c("ggcy", "dshcy", "jshcy")
get_data <- function(member, code) {
  ifeng_url <- "http://app.finance.ifeng.com/data/stock/"
  all_url <- lapply(code, function(x) paste0(ifeng_url, member, ".php?symbol=", x))
  
  data <- all_url %>% 
    lapply(. %>% 
      read_html() %>% 
      html_nodes(".contentR table") %>% 
      html_table(fill = TRUE)
  )
  
  return(data)
}

# 创建函数，清理表格
tidy_data <- function(data){
  result <- c()
  for (i in 1:length(data)) {
    if (length(data[[i]]) != 0) {
      for (j in 1:length(data[[i]])) {
        dt <- data[[i]][[j]] %>% as.data.table()
        
        dt1 <- dt[, .(X1, X2)][, code := names(data)[i]]
        setcolorder(dt1, c(3, 1:2))
        dt11 <- dcast(dt1, code ~ X1, value.var = "X2")
        
        dt2 <- dt[, .(X3, X4)][, code := names(data)[i]][-6]
        setcolorder(dt2, c(3, 1:2))
        dt22 <- dcast(dt2, code ~ X3, value.var = "X4")
        
        dt3 <- cbind(dt11, dt22[, !"code"]) %>% list()
        
        result <- c(result, dt3)
      }
    }
  }
  result_dt <- rbindlist(result)
  return(result_dt)
}

# 高管数据
ggcy <- get_data("ggcy", company_code)
names(ggcy) <- company_code
gg <- tidy_data(ggcy)

# 董事会数据
dshcy <- get_data("dshcy", company_code)
names(dshcy) <- company_code
dsh <- tidy_data(dshcy)

# 监事会数据
jshcy <- get_data("jshcy", company_code)
names(jshcy) <- company_code
jsh <- tidy_data(jshcy)

# 合并数据
setnames(gg, "高管职务", "职务")
gg[, 机构 := "高管"]

dsh[, 机构 := "董事会"]

jsh[, 机构 := "监事会"]

final_data <- rbindlist(list(gg, dsh, jsh))

final_data[company, on = "code", 公司 := i.company]

setnames(final_data, "code", "股票代码")

col_names <- c("股票代码", "公司", "姓名", "机构", "职务", "性别", "出生年份", 
               "政治面貌", "毕业院校", "最高学历", "职称和所得证书", "任期起", 
               "任期止", "工作经历")
setcolorder(final_data, col_names)

final_data <- final_data[order(股票代码)]

new_names <- c("stock_code", "company", "name", "institute", "duty", "gender", 
               "born", "party", "graduate_college", "highest_degree", "title", 
               "service_from", "service_to", "cv")
setnames(exe, names(exe), new_names)

# fwrite(final_data, "companies_executives.csv")
