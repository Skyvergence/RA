library("xml2")
library("rvest")
library("httr")
library("stringr")
library("tidyverse")
options(stringAsFactor = F)
Sys.setlocale("LC_ALL","Chinese")
Sys.setlocale(category = "LC_ALL", locale = "cht")

url <- "http://ce.cn/ztpd/xwzt/zyzsjgrwk/"
doc <- read_html(url)

# 1. 党中央各部门 (16)---------------------------------------------------------------
first_df <- data.frame(
    institution <- character(0),
    department <- character(0),
    link <- character(0)
)

for(i in 1:16){
    tempdf <- data.frame(
        institution <- character(0),
        department <- character(0),
        link <- character(0)
    )
    
    # institution
    institutionxpath <- "/html/body/div[4]/div[1]/h3/text()"
    findinstitution <- xml_find_all(doc, institutionxpath)
    institution <- xml_text(findinstitution)
    
    # department
    departmentxpath <- paste0("/html/body/div[4]/div[2]/ul/li[", i,"]")
    finddepartment <- xml_find_all(doc, departmentxpath)
    department <- xml_text(finddepartment)
    
    # department url
    hrefxpath <- paste0(" /html/body/div[4]/div[2]/ul/li[", i,"]/a")
    findall <- xml_find_all(doc, hrefxpath)
    href <- as.character(xml_attr(findall, "href"))
    if (length(href) == 0){
        href <- NA
    }
    
    
    tempdf <- data.frame(institution = institution,
                         department = department,
                         link = href)

    first_df <- rbind(first_df, tempdf)
}

first_person_df <- data.frame(
    department <- character(0),
    name <- character(0),
    link <- character(0)
)

for(i in 1:length(first_df[,3])){
    if (is.na(first_df[i,3]) == T){
        next
    } 
    if (substr(first_df[i,3], 1,17) != "http://www.ce.cn/"){
        next
    }
    
    for(j in 1:10){
        for (k in 1:15){
            print(paste0("(",i,",",j,",",k,")"))
            tempdf <- data.frame(
                department = character(0),
                name = character(0),
                link = character(0))
            
            url_department <- as.character(first_df[i,3])
            doc_department <- read_html(url_department, encoding = "GB18030")
            
            # department
            department <- first_df[i,2]
            
            
            # name
            hrefxpath_name <- paste0("/html/body/div[1]/div[2]/div[1]/div[2]/ul[", j,"]/li[", k,"]/p/a")
            findall_name <- xml_find_all(doc_department, hrefxpath_name)
            name <- xml_text(findall_name)
            
            # url
            hrefxpath_department <- paste0("/html/body/div[1]/div[2]/div[1]/div[2]/ul[", j,"]/li[", k,"]/p/a")
            findall_ministry <- xml_find_all(doc_department, hrefxpath_department)
            href <- xml_attr(findall_ministry, "href")
            
            
            
            if (length(href) == 0){
                next
            }
            print("check")
            
            tempdf <- data.frame(department = department,
                                 name = name,
                                 link = href)
            
            
            first_person_df <- rbind(first_person_df, tempdf)
        }
    }
}




# 2. 党中央直属事业单位 (10)------------------------------------------------------------
second_df <- data.frame(
    institution <- character(0),
    department <- character(0),
    link <- character(0)
)

for(i in 1:10){
    tempdf <- data.frame(
        institution <- character(0),
        department <- character(0),
        link <- character(0)
    )
    
    # institution
    institutionxpath <- "/html/body/div[5]/div[1]/h3/text()"
    findinstitution <- xml_find_all(doc, institutionxpath)
    institution <- xml_text(findinstitution)
    
    # department
    departmentxpath <- paste0("/html/body/div[5]/div[2]/ul/li[", i,"]")
    finddepartment <- xml_find_all(doc, departmentxpath)
    department <- xml_text(finddepartment)

    # department url
    hrefxpath <- paste0(" /html/body/div[5]/div[2]/ul/li[", i,"]/a")
    findall <- xml_find_all(doc, hrefxpath)
    href <- as.character(xml_attr(findall, "href"))
    if (length(href) == 0){
        href <- NA
    }
    
    
    tempdf <- data.frame(institution = institution,
                         department = department,
                         link = href)
    
    second_df <- rbind(second_df, tempdf)
}

second_person_df <- data.frame(
    department <- character(0),
    name <- character(0),
    link <- character(0)
)

for(i in 1:length(second_df[,3])){
    if (is.na(second_df[i,3]) == T){
        next
    } 
    if (substr(second_df[i,3], 1,17) != "http://www.ce.cn/"){
        next
    }
    
    for(j in 1:10){
        for (k in 1:15){
            print(paste0("(",i,",",j,",",k,")"))
            tempdf <- data.frame(
                department = character(0),
                name = character(0),
                link = character(0))
            
            url_department <- as.character(second_df[i,3])
            doc_department <- read_html(url_department, encoding = "GB18030")
            
            # department
            department <- second_df[i,2]
            
            
            # name
            hrefxpath_name <- paste0("/html/body/div[1]/div[2]/div[1]/div[2]/ul[", j,"]/li[", k,"]/p/a")
            findall_name <- xml_find_all(doc_department, hrefxpath_name)
            name <- xml_text(findall_name)
            
            # url
            hrefxpath_department <- paste0("/html/body/div[1]/div[2]/div[1]/div[2]/ul[", j,"]/li[", k,"]/p/a")
            findall_ministry <- xml_find_all(doc_department, hrefxpath_department)
            href <- xml_attr(findall_ministry, "href")
            
            
            
            if (length(href) == 0){
                next
            }
            print("check")
            
            tempdf <- data.frame(department = department,
                                 name = name,
                                 link = href)
            
            
            second_person_df <- rbind(second_person_df, tempdf)
        }
    }
}



# Preparation -------------------------------------------------------------
departmentlist <- rbind(first_df, second_df)

namelist <- rbind(first_person_df, second_person_df)

departmentlist$institution <- as.character(departmentlist$institution)
departmentlist$department <- as.character(departmentlist$department)

namelist$department <- as.character(namelist$department)
namelist$name <- as.character(namelist$name)

temp_df <- data.frame(index <- 1:length(namelist$name))
namelist <- cbind(temp_df, namelist)
colnames(namelist)[1] <- "index"

bio_df <- data.frame(
    index = character(0),
    department = character(0),
    name = character(0),
    bio = character(0)
)

for (i in 1:length(namelist$link)){
    tryCatch({
        bio = character(0)
        tempdf <- data.frame(
            index = character(0),
            department = character(0),
            name = character(0),
            bio = character(0))
        
        url_name <- as.character(namelist[i,4])
        tryCatch({
        }, error=function(e){})
        doc_name <- read_html(url_name, encoding = "GB18030")
        
        # index?
        index <- i
        
        # name
        name <- namelist[i,3]
        
        # department
        department <- namelist[i,2]
        
        
        # bio
        hrefxpath_bio <- "//*[@id='articleText']"
        findall_bio <- xml_find_all(doc_name, hrefxpath_bio)
        bio <- xml_text(findall_bio)
        
        tempdf <- data.frame(
            index = index,
            department = department,
            name = name,
            bio = bio)
        if (ncol(tempdf) == ncol(bio_df)){
            tempdf$department <- as.character(tempdf$department)
            tempdf$name <- as.character(tempdf$name)
            tempdf$bio <- as.character(tempdf$bio)
            
            write.csv(tempdf, file = paste0(name, ".csv"))
        }
    }, error=function(e){cat("ERROR",conditionMessage(e), "\n")})

}

write.csv(bio_df, file = "bio_df2.csv")
write.csv(namelist, file = "namelist.csv")