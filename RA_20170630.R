library("xml2")
library("rvest")
library("httr")
library("stringr")
library("tidyverse")
options(stringAsFactor = F)
Sys.setlocale("LC_ALL","Chinese")
Sys.setlocale(category = "LC_ALL", locale = "cht")

url <- "http://www.ce.cn/ztpd/xwzt/rwk/"
doc <- read_html(url)


# 1. 国务院组成部门（25个） ------------------------------------------------------------
first_df <- data.frame(
    institution <- character(0),
    department <- character(0),
    link <- character(0)
)

for(i in 1:25){
    # institution
    institutionxpath <- "/html/body/div[2]/div[2]/div[2]/div[4]/h3"
    findinstitution <- xml_find_all(doc, institutionxpath)
    institution <- xml_text(findinstitution)
    
    # department
    departmentxpath <- paste0("/html/body/div[2]/div[2]/div[2]/div[5]/div[", i,"]/a")
    finddepartment <- xml_find_all(doc, departmentxpath)
    department <- xml_text(finddepartment)
    
    
    # department url
    hrefxpath <- paste0("/html/body/div[2]/div[2]/div[2]/div[5]/div[", i,"]/a")
    findall <- xml_find_all(doc, hrefxpath)
    href <- as.character(xml_attr(findall, "href"))
    
    
    tempdf <- data.frame(institution = institution,
                         department = department,
                         link = href)
    
    
    first_df <- rbind(first_df, tempdf)
}


first_df <- first_df[-2,]

first_person_df <- data.frame(
    department <- character(0),
    name <- character(0),
    link <- character(0)
)

for(i in 1:length(first_df[,3])){
    print(i)
    for(j in 1:10){
        for (k in 1:15){
            
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
            
            
            print(paste0("(",j,",",k,")"))
            if (length(href) == 0){
                next
            }
            
            tempdf <- data.frame(department = department,
                                 name = name,
                                 link = href)
            
            
            first_person_df <- rbind(first_person_df, tempdf)
        }
    }
}




# 2. 国务院直属机构（16个） ---------------------------------------------------------
second_df <- data.frame(
    institution <- character(0),
    department <- character(0),
    link <- character(0)
)


for(i in 1:16){
    # institution
    institutionxpath <- "/html/body/div[2]/div[2]/div[2]/div[8]/h3"
    findinstitution <- xml_find_all(doc, institutionxpath)
    institution <- xml_text(findinstitution)
    
    # department
    departmentxpath <- paste0("/html/body/div[2]/div[2]/div[2]/div[9]/div[", i,"]/a")
    finddepartment <- xml_find_all(doc, departmentxpath)
    department <- xml_text(finddepartment)
    
    
    # department url
    hrefxpath <- paste0("/html/body/div[2]/div[2]/div[2]/div[9]/div[", i,"]/a")
    findall <- xml_find_all(doc, hrefxpath)
    href <- as.character(xml_attr(findall, "href"))
    
    
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
    print(i)
    for(j in 1:10){
        for (k in 1:15){
            
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
            
            
            print(paste0("(",j,",",k,")"))
            if (length(href) == 0){
                next
            }
            
            tempdf <- data.frame(department = department,
                                 name = name,
                                 link = href)
            
            
            second_person_df <- rbind(second_person_df, tempdf)
            
        }
    }
}






# 3. 国务院直属特设机构 ------------------------------------------------------------
third_df <- data.frame(
    institution <- character(0),
    department <- character(0),
    link <- character(0)
)

for(i in 1){
    # institution
    institutionxpath <- "/html/body/div[2]/div[2]/div[2]/div[6]/h3"
    findinstitution <- xml_find_all(doc, institutionxpath)
    institution <- xml_text(findinstitution)
    
    # department
    departmentxpath <- paste0("/html/body/div[2]/div[2]/div[2]/div[7]/div[", i,"]/a")
    finddepartment <- xml_find_all(doc, departmentxpath)
    department <- xml_text(finddepartment)
    
    
    # department url
    hrefxpath <- paste0("/html/body/div[2]/div[2]/div[2]/div[7]/div[", i,"]/a")
    findall <- xml_find_all(doc, hrefxpath)
    href <- as.character(xml_attr(findall, "href"))
    
    
    tempdf <- data.frame(institution = institution,
                         department = department,
                         link = href)
    
    
    third_df <- rbind(third_df, tempdf)
}


third_person_df <- data.frame(
    department <- character(0),
    name <- character(0),
    link <- character(0)
)


for(i in 1:length(third_df[,3])){
    print(i)
    for(j in 1:10){
        for (k in 1:15){
            
            tempdf <- data.frame(
                department = character(0),
                name = character(0),
                link = character(0))
            
            url_department <- as.character(third_df[i,3])
            doc_department <- read_html(url_department, encoding = "GB18030")
            
            # department
            department <- third_df[i,2]
            
            
            # name
            hrefxpath_name <- paste0("/html/body/div[1]/div[2]/div[1]/div[2]/ul[", j,"]/li[", k,"]/p/a")
            findall_name <- xml_find_all(doc_department, hrefxpath_name)
            name <- xml_text(findall_name)
            
            
            # url
            hrefxpath_department <- paste0("/html/body/div[1]/div[2]/div[1]/div[2]/ul[", j,"]/li[", k,"]/p/a")
            findall_ministry <- xml_find_all(doc_department, hrefxpath_department)
            href <- xml_attr(findall_ministry, "href")
            
            
            print(paste0("(",j,",",k,")"))
            if (length(href) == 0){
                next
            }
            
            tempdf <- data.frame(department = department,
                                 name = name,
                                 link = href)
            
            
            third_person_df <- rbind(third_person_df, tempdf)
            
        }
    }
}



# 4. 国务院办事机构（4个） ----------------------------------------------------------
forth_df <- data.frame(
    institution <- character(0),
    department <- character(0),
    link <- character(0)
)

for(i in 1:4){
    # institution
    institutionxpath <- "/html/body/div[2]/div[2]/div[2]/div[10]/h3"
    findinstitution <- xml_find_all(doc, institutionxpath)
    institution <- xml_text(findinstitution)
    
    # department
    departmentxpath <- paste0("/html/body/div[2]/div[2]/div[2]/div[11]/div[", i,"]/a")
    finddepartment <- xml_find_all(doc, departmentxpath)
    department <- xml_text(finddepartment)
    
    
    # department url
    hrefxpath <- paste0("/html/body/div[2]/div[2]/div[2]/div[11]/div[", i,"]/a")
    findall <- xml_find_all(doc, hrefxpath)
    href <- as.character(xml_attr(findall, "href"))
    
    
    tempdf <- data.frame(institution = institution,
                         department = department,
                         link = href)
    
    
    forth_df <- rbind(forth_df, tempdf)
}



forth_person_df <- data.frame(
    department <- character(0),
    name <- character(0),
    link <- character(0)
)



for(i in 1:length(forth_df[,3])){
    print(i)
    for(j in 1:10){
        for (k in 1:15){
            
            tempdf <- data.frame(
                department = character(0),
                name = character(0),
                link = character(0))
            
            url_department <- as.character(forth_df[i,3])
            doc_department <- read_html(url_department, encoding = "GB18030")
            
            # department
            department <- forth_df[i,2]
            
            
            # name
            hrefxpath_name <- paste0("/html/body/div[1]/div[2]/div[1]/div[2]/ul[", j,"]/li[", k,"]/p/a")
            findall_name <- xml_find_all(doc_department, hrefxpath_name)
            name <- xml_text(findall_name)
            
            
            # url
            hrefxpath_department <- paste0("/html/body/div[1]/div[2]/div[1]/div[2]/ul[", j,"]/li[", k,"]/p/a")
            findall_ministry <- xml_find_all(doc_department, hrefxpath_department)
            href <- xml_attr(findall_ministry, "href")
            
            
            print(paste0("(",j,",",k,")"))
            if (length(href) == 0){
                next
            }
            
            tempdf <- data.frame(department = department,
                                 name = name,
                                 link = href)
            
            
            forth_person_df <- rbind(forth_person_df, tempdf)
            
        }
    }
}


# 5. 国务院直属事业单位（13个）-------------------------------------------------------
# 85 人
fifth_df <- data.frame(
    institution <- character(0),
    department <- character(0),
    link <- character(0)
)

for(i in 1:13){
    # institution
    institutionxpath <- "/html/body/div[2]/div[2]/div[2]/div[12]/h3"
    findinstitution <- xml_find_all(doc, institutionxpath)
    institution <- xml_text(findinstitution)
    
    # department
    departmentxpath <- paste0("/html/body/div[2]/div[2]/div[2]/div[13]/div[", i,"]/a")
    finddepartment <- xml_find_all(doc, departmentxpath)
    department <- xml_text(finddepartment)
    
    
    # department url
    hrefxpath <- paste0("/html/body/div[2]/div[2]/div[2]/div[13]/div[", i,"]/a")
    findall <- xml_find_all(doc, hrefxpath)
    href <- as.character(xml_attr(findall, "href"))
    
    
    tempdf <- data.frame(institution = institution,
                         department = department,
                         link = href)
    
    
    fifth_df <- rbind(fifth_df, tempdf)
}



fifth_person_df <- data.frame(
    department <- character(0),
    name <- character(0),
    link <- character(0)
)

for(i in 1:length(fifth_df[,3])){
    print(i)
    for(j in 1:10){
        for (k in 1:15){
            
            tempdf <- data.frame(
                department = character(0),
                name = character(0),
                link = character(0))
            
            url_department <- as.character(fifth_df[i,3])
            doc_department <- read_html(url_department, encoding = "GB18030")
            
            # department
            department <- fifth_df[i,2]
            
            
            # name
            hrefxpath_name <- paste0("/html/body/div[1]/div[2]/div[1]/div[2]/ul[", j,"]/li[", k,"]/p/a")
            findall_name <- xml_find_all(doc_department, hrefxpath_name)
            name <- xml_text(findall_name)
            
            
            # url
            hrefxpath_department <- paste0("/html/body/div[1]/div[2]/div[1]/div[2]/ul[", j,"]/li[", k,"]/p/a")
            findall_ministry <- xml_find_all(doc_department, hrefxpath_department)
            href <- xml_attr(findall_ministry, "href")
            
            
            print(paste0("(",j,",",k,")"))
            if (length(href) == 0){
                next
            }
            
            tempdf <- data.frame(department = department,
                                 name = name,
                                 link = href)
            
            
            fifth_person_df <- rbind(fifth_person_df, tempdf)
        }
    }
}




# 6. 国务院部委管理的国家局（16个） -----------------------------------------------------
sixth_df <- data.frame(
    institution <- character(0),
    department <- character(0),
    link <- character(0)
)

for(i in 1:16){
    # institution
    institutionxpath <- "/html/body/div[2]/div[2]/div[2]/div[14]/h3"
    findinstitution <- xml_find_all(doc, institutionxpath)
    institution <- xml_text(findinstitution)
    
    # department
    departmentxpath <- paste0("/html/body/div[2]/div[2]/div[2]/div[15]/div[", i,"]/a")
    finddepartment <- xml_find_all(doc, departmentxpath)
    department <- xml_text(finddepartment)
    
    
    # department url
    hrefxpath <- paste0("/html/body/div[2]/div[2]/div[2]/div[15]/div[", i,"]/a")
    findall <- xml_find_all(doc, hrefxpath)
    href <- as.character(xml_attr(findall, "href"))
    
    
    tempdf <- data.frame(institution = institution,
                         department = department,
                         link = href)
    
    
    sixth_df <- rbind(sixth_df, tempdf)
}



sixth_person_df <- data.frame(
    department <- character(0),
    name <- character(0),
    link <- character(0)
)

for(i in 1:length(sixth_df[,3])){
    print(i)
    for(j in 1:10){
        for (k in 1:15){
            
            tempdf <- data.frame(
                department = character(0),
                name = character(0),
                link = character(0))
            
            url_department <- as.character(sixth_df[i,3])
            doc_department <- read_html(url_department, encoding = "GB18030")
            
            # department
            department <- sixth_df[i,2]
            
            
            # name
            hrefxpath_name <- paste0("/html/body/div[1]/div[2]/div[1]/div[2]/ul[", j,"]/li[", k,"]/p/a")
            findall_name <- xml_find_all(doc_department, hrefxpath_name)
            name <- xml_text(findall_name)
            
            
            # url
            hrefxpath_department <- paste0("/html/body/div[1]/div[2]/div[1]/div[2]/ul[", j,"]/li[", k,"]/p/a")
            findall_ministry <- xml_find_all(doc_department, hrefxpath_department)
            href <- xml_attr(findall_ministry, "href")
            
            
            print(paste0("(",j,",",k,")"))
            if (length(href) == 0){
                next
            }
            
            tempdf <- data.frame(department = department,
                                 name = name,
                                 link = href)
            
            
            sixth_person_df <- rbind(sixth_person_df, tempdf)
        }
    }
}



departmentlist <- rbind(first_df, second_df, third_df, forth_df, fifth_df, sixth_df)
View(departmentlist)

namelist <- rbind(first_person_df, second_person_df,
                  third_person_df, forth_person_df,
                  fifth_person_df, sixth_person_df)
View(namelist)

write.csv(departmentlist, file = "departmentlist.csv")
write.csv(namelist, file = "namelist.csv")

departmentlist$institution <- as.character(departmentlist$institution)
departmentlist$department <- as.character(departmentlist$department)

namelist$department <- as.character(namelist$department)
namelist$name <- as.character(namelist$name)



# Prepare -----------------------------------------------------------------
temp_df <- data.frame(index <- 1:538)
namelist <- cbind(temp_df, namelist)
colnames(namelist)[1] <- "index"

no_data <- c(259, 262, 286, 292, 302, 304, 326, 327, 364, 365, 410, 426, 466, 467, 475,
             482, 483, 484, 515, 516, 517, 518, 519)


list <- c(1:length(namelist[,3]))
list <- subset(list, !(list %in% no_data))

list3 <- c(1:length(namelist[,3]))
list3 <- subset(list3, !(list %in% no_data))


# Test individual scrape --------------------------------------------------
bio_df <- data.frame(
    index = character(0),
    department = character(0),
    name = character(0),
    bio = character(0)
)


for (i in 1:10){
    bio = character(0)
    tempdf <- data.frame(
        index = character(0),
        department = character(0),
        name = character(0),
        bio = character(0))
    
    url_name <- as.character(namelist[i,4])
    doc_name <- read_html(url_name, encoding = "GB18030")
    
    # index?
    index <- i
    
    # name
    name <- namelist[i,3]
    
    # department
    department <- namelist[i,2]
    
    
    content <- c()
    # bio
    for (j in 1:50){
        hrefxpath_bio <- paste0("//*[@id='articleText']/p[", j, "]")
        findall_bio <- xml_find_all(doc_name, hrefxpath_bio)
        temp <- xml_text(findall_bio)
        if (length(temp) != 0){
            content <- c(content, temp)
        }
    }
    
    for (k in 1:length(content)){
        bio <- paste0(bio, content[k], sep = ";") 
    }
    
    
    if (length(bio) == 0){
        error <- c(error, i)
        tempdf <- data.frame(
            index = index,
            department = department,
            name = name,
            bio = "error")
        next
    }
    
    
    tempdf <- data.frame(
        index = index,
        department = department,
        name = name,
        bio = bio)
    
    tempdf$department <- as.character(tempdf$department)
    tempdf$name <- as.character(tempdf$name)
    tempdf$bio <- as.character(tempdf$bio)
    
    write.csv(tempdf, file = paste0(name, ".csv"))
    
    print(i)
    
}



bio_df$department <- as.character(bio_df$department)
bio_df$name <- as.character(bio_df$name)
bio_df$bio <- as.character(bio_df$bio)




bio_df2 <- data.frame(
    index = character(0),
    department = character(0),
    name = character(0),
    bio = character(0)
)


for (i in list){
    bio = character(0)
    tempdf <- data.frame(
        index = character(0),
        department = character(0),
        name = character(0),
        bio = character(0))
    
    url_name <- as.character(namelist[i,4])
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
    
    if (ncol(tempdf) == ncol(bio_df2)){
        tempdf$department <- as.character(tempdf$department)
        tempdf$name <- as.character(tempdf$name)
        tempdf$bio <- as.character(tempdf$bio)
        
        write.csv(tempdf, file = paste0(name, ".csv"))
    }
    
}

bio_df2$department <- as.character(bio_df2$department)
bio_df2$name <- as.character(bio_df2$name)
bio_df2$bio <- as.character(bio_df2$bio)



