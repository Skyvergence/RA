library("xml2")
library("rvest")
library("httr")
library("stringr")
library("tidyverse")
options(stringAsFactor = F)
Sys.setlocale("LC_ALL","Chinese")
Sys.setlocale(category = "LC_ALL", locale = "cht")

url <- "http://district.ce.cn/zt/rwk/index.shtml"
doc <- read_html(url, encoding = "GB18030")

provincelist <- data.frame(
    province <- character(0),
    link <- character(0)
)

for (i in 1:33){
    tempdf <- data.frame(
        province <- character(0),
        link <- character(0)
    )


    # Province
    provincexpath <- paste0("/html/body/div[contains(@class, 'main')]
                            /div[contains(@class, 'left')]
                            /div[contains(@class, 'left1')]
                            /div[", i, "]/a/font/b")
    findprovince <- xml_find_all(doc, provincexpath)
    province <- xml_text(findprovince)
    
    # Province Url
    hrefxpath <- paste0("/html/body/div[contains(@class, 'main')]
                            /div[contains(@class, 'left')]
                            /div[contains(@class, 'left1')]
                            /div[", i, "]/a")
    findall <- xml_find_all(doc, hrefxpath)
    href <- as.character(xml_attr(findall, "href"))
    if (length(href) == 0){
        href <- NA
    }
    
    tempdf <- data.frame(province = province,
                         link = href)
    
    provincelist <- rbind(provincelist, tempdf)
}


namelist <- data.frame(
    province <- character(0),
    name <- character(0),
    position <- character(0),
    link <- character(0)
)
# 1:5, 7, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 23, 24, 25, 26, 27, 28, 29, 31
for(i in c(25, 26, 27, 28, 29, 31)){
    if (is.na(provincelist[i,2]) == T){
        next
    } 
    
    for(j in 1:15){
        tempdf <- data.frame(
            province = character(0),
            name = character(0),
            position = character(0),
            link = character(0))
        
        url_province <- as.character(provincelist[i,2])
        doc_province <- read_html(url_province, encoding = "GB18030")
        
        # province
        province <- provincelist[i,1]
        

        # name
        hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                 /div[contains(@class, 'left2')]
                                 /div/div/div/table/tbody/tr[", j, "]/td[2]")
        findall_name <- xml_find_all(doc_province, hrefxpath_name)
        name <- xml_text(findall_name)[1]
        
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                 /div[contains(@class, 'left2')]
                                 /div/div/table/tbody/tr[", j, "]/td[1]/strong/a/font")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                 /div[contains(@class, 'left2')]
                                 /div/div/table/tbody/tr[", j, "]/td[1]/a/font/strong")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                 /div[contains(@class, 'left2')]
                                 /div/div/table/tbody/tr[", j, "]/td[1]/p/b/span/span/strong/a/font")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                 /div[contains(@class, 'left2')]
                                 /div/div/table/tbody/tr[", j, "]/td[1]/p/b/span/span/a/font")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                 /div[contains(@class, 'left2')]
                                 /div/div/table/tbody/tr[", j, "]/td[1]/span/strong/a/font")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                 /div[contains(@class, 'left2')]
                                 /div/div/table/tbody/tr[", j, "]/td[1]/font/a/font/strong")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                 /div[contains(@class, 'left2')]
                                 /div/div/table/tbody/tr[", j, "]/td[1]/p/a/font/strong")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                     /div[contains(@class, 'left')]
                                     /div[contains(@class, 'left2')]
                                     /div/div/table/tbody/tr[", j, "]/td[1]/p/span/strong/a/font")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        
        
        
        # position
        hrefxpath_position <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                 /div[contains(@class, 'left2')]
                                 /div/div/div/table/tbody/tr[", j, "]/td[3]")
        findall_position <- xml_find_all(doc_province, hrefxpath_position)
        position <- xml_text(findall_position)[1]
        

        if (is.na(position)){
            hrefxpath_position <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                 /div[contains(@class, 'left2')]
                                 /div/div/table/tbody/tr[", j, "]/td[2]/p/span/font")
            findall_position <- xml_find_all(doc_province, hrefxpath_position)
            position <- xml_text(findall_position)[1]
        }
        if (is.na(position)){
            hrefxpath_position <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                 /div[contains(@class, 'left2')]
                                 /div/div/table/tbody/tr[", j, "]/td[2]/font")
            findall_position <- xml_find_all(doc_province, hrefxpath_position)
            position <- xml_text(findall_position)[1]
        }
        
        if (is.na(position)){
            hrefxpath_position <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                 /div[contains(@class, 'left2')]
                                 /div/div/table/tbody/tr[", j, "]/td[2]/span/font")
            findall_position <- xml_find_all(doc_province, hrefxpath_position)
            position <- xml_text(findall_position)[1]
        }
        
        # url
        hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                 /div[contains(@class, 'left2')]
                                 /div/div/div/table/tbody/tr[", j, "]/td[2]/a")
        findall_url <- xml_find_all(doc_province, hrefxpath_url)
        link <- xml_attr(findall_url, "href")[1]
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                    /div[contains(@class, 'left2')]
                                    /div/div/div/table/tbody/tr[", j, "]/td[2]/div/span/span/span/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                    /div[contains(@class, 'left2')]
                                    /div/div/div/table/tbody/tr[", j, "]/td[2]/span/span/span/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                    /div[contains(@class, 'left')]
                                    /div[contains(@class, 'left2')]
                                    /div[contains(@class, 'ren2')]
                                    /div[contains(@class, 'TRS_Editor')]/table/tbody/tr[", j, "]/td[1]/strong/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                    /div[contains(@class, 'left')]
                                    /div[contains(@class, 'left2')]
                                    /div[contains(@class, 'ren2')]
                                    /div[contains(@class, 'TRS_Editor')]/table/tbody/tr[", j, "]/td[1]/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }

        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                    /div[contains(@class, 'left')]
                                    /div[contains(@class, 'left2')]
                                    /div
                                    /div
                                    /div
                                    /table/tbody/tr[", j, "]/td[2]/strong/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                    /div[contains(@class, 'left')]
                                    /div[contains(@class, 'left2')]
                                    /div/div/table/tbody/tr[", j, "]/td[1]/p/b/span/span/strong/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                    /div[contains(@class, 'left')]
                                    /div[contains(@class, 'left2')]
                                    /div/div/table/tbody/tr[", j, "]/td[1]/p/b/span/span/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                    /div[contains(@class, 'left')]
                                    /div[contains(@class, 'left2')]
                                    /div/div/table/tbody/tr[", j, "]/td[1]/span/strong/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                    /div[contains(@class, 'left')]
                                    /div[contains(@class, 'left2')]
                                    /div/div/table/tbody/tr[", j, "]/td[1]/font/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                    /div[contains(@class, 'left')]
                                    /div[contains(@class, 'left2')]
                                    /div/div/table/tbody/tr[", j, "]/td[1]/p/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                    /div[contains(@class, 'left')]
                                    /div[contains(@class, 'left2')]
                                    /div/div/div/table/tbody/tr[", j, "]/td[2]/font/font/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                    /div[contains(@class, 'left')]
                                    /div[contains(@class, 'left2')]
                                    /div/div/table/tbody/tr[", j, "]/td[1]/p/span/strong/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                    /div[contains(@class, 'left')]
                                    /div[contains(@class, 'left2')]
                                    /div/div/div/table/tbody/tr[", j, "]/td[2]/span/span/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                    /div[contains(@class, 'left')]
                                    /div[contains(@class, 'left2')]
                                    /div/div/div/table/tbody/tr[", j, "]/td[2]/span/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        
        print(link)
        if (length(link) == 0){
            link <- NA
        }
        
        tempdf <- data.frame(
            province <- province,
            name <- name,
            position <- position,
            link <- link)
        
        namelist <- rbind(namelist, tempdf)
    }
}

# 6, 8, 9, 10, 11
namelist2 <- data.frame(
    province <- character(0),
    name <- character(0),
    position <- character(0),
    link <- character(0)
)

for(i in c(6, 8, 9, 10, 11)){
    if (is.na(provincelist[i,2]) == T){
        next
    } 
    
    for(j in 1:15){
        tempdf <- data.frame(
            province = character(0),
            name = character(0),
            position = character(0),
            link = character(0))
        
        url_province <- as.character(provincelist[i,2])
        doc_province <- read_html(url_province, encoding = "GB18030")
        
        # province
        province <- provincelist[i,1]
        
        
        # name
        hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                 /div[contains(@class, 'left2')]
                                 /div/div/div/table/tbody/tr[", j, "]
                                 /td[1]/strong/a/font")
        findall_name <- xml_find_all(doc_province, hrefxpath_name)
        name <- xml_text(findall_name)[1]
        
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                     /div[contains(@class, 'left2')]
                                     /div/div/div/table/tbody/tr[", j, "]
                                     /td[1]/a/font/strong")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                     /div[contains(@class, 'left2')]
                                     /div/div/div/table/tbody/tr[", j, "]
                                     /td[1]/p/span/a/b/span/span/font")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                     /div[contains(@class, 'left2')]
                                     /div/div/div/table/tbody/tr[", j, "]
                                     /td[1]/p/span/a/font/strong")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                     /div[contains(@class, 'left2')]
                                     /div/div/div/table/tbody/tr[", j, "]
                                     /td[1]/p/span/a/font/font/font/b/span/span")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                     /div[contains(@class, 'left2')]
                                     /div/div/div/table/tbody/tr[", j, "]
                                     /td[1]/p/b/span/a/span/span/font")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                     /div[contains(@class, 'left2')]
                                     /div/div/div/table/tbody/tr[", j, "]
                                     /td[1]/a/font")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                     /div[contains(@class, 'left2')]
                                     /div/div/div/table/tbody/tr[", j, "]
                                     /td[1]/p/span/a/span/span/strong/font")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                     /div[contains(@class, 'left')]
                                     /div[contains(@class, 'left2')]
                                     /div/div/table/tbody/tr[", j, "]
                                     /td[1]/p/b/span/a/font")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        
        # position
        hrefxpath_position <- paste0("/html/body/div[contains(@class, 'main')]
                                     /div[contains(@class, 'left')]
                                     /div[contains(@class, 'left2')]
                                     /div/div/div/table/tbody/tr[", j, "]
                                     /td[2]/p/span/font")
        findall_position <- xml_find_all(doc_province, hrefxpath_position)
        position <- xml_text(findall_position)[1]
        if (is.na(position)){
            hrefxpath_position <- paste0("/html/body/div[contains(@class, 'main')]
                                     /div[contains(@class, 'left')]
                                         /div[contains(@class, 'left2')]
                                         /div/div/div/table/tbody/tr[", j, "]
                                         /td[2]")
            findall_position <- xml_find_all(doc_province, hrefxpath_position)
            position <- xml_text(findall_position)[1]
        }
        if (is.na(position)){
            hrefxpath_position <- paste0("/html/body/div[contains(@class, 'main')]
                                     /div[contains(@class, 'left')]
                                         /div[contains(@class, 'left2')]
                                         /div/div/div/table/tbody/tr[", j, "]
                                         /td[2]/p/span/font")
            findall_position <- xml_find_all(doc_province, hrefxpath_position)
            position <- xml_text(findall_position)[1]
        }
        if (is.na(position)){
            hrefxpath_position <- paste0("/html/body/div[contains(@class, 'main')]
                                     /div[contains(@class, 'left')]
                                         /div[contains(@class, 'left2')]
                                         /div/div/table/tbody/tr[", j, "]
                                         /td[2]/p/span/font")
            findall_position <- xml_find_all(doc_province, hrefxpath_position)
            position <- xml_text(findall_position)[1]
        }
        

        
        # url
        hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                /div[contains(@class, 'left')]
                                /div[contains(@class, 'left2')]
                                /div/div/div/table/tbody/tr[", j, "]/td[1]/strong/a")
        findall_url <- xml_find_all(doc_province, hrefxpath_url)
        link <- xml_attr(findall_url, "href")[1]
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                /div[contains(@class, 'left')]
                                /div[contains(@class, 'left2')]
                                /div/div/div/table/tbody/tr[", j, "]/td[1]/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                /div[contains(@class, 'left')]
                                /div[contains(@class, 'left2')]
                                /div/div/div/table/tbody/tr[", j, "]/td[1]/p/span/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                /div[contains(@class, 'left')]
                                /div[contains(@class, 'left2')]
                                /div/div/div/table/tbody/tr[", j, "]/td[1]/p/b/span/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                /div[contains(@class, 'left')]
                                /div[contains(@class, 'left2')]
                                /div/div/table/tbody/tr[", j, "]/td[1]/p/b/span/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        
        
        print(link)
        if (length(link) == 0){
            link <- NA
        }
        
        tempdf <- data.frame(
            province <- province,
            name <- name,
            position <- position,
            link <- link)
        
        namelist2 <- rbind(namelist2, tempdf)
    }
}


# 22
namelist3 <- data.frame(
    province <- character(0),
    name <- character(0),
    position <- character(0),
    link <- character(0)
)

for(i in 22:22){
    if (is.na(provincelist[i,2]) == T){
        next
    } 
    
    for(j in 1:15){
        tempdf <- data.frame(
            province = character(0),
            name = character(0),
            position = character(0),
            link = character(0))
        
        url_province <- as.character(provincelist[i,2])
        doc_province <- read_html(url_province, encoding = "GB18030")
        
        # province
        province <- provincelist[i,1]
        
        
        # name
        hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                 /div[contains(@class, 'left2')]
                                 /div/div/table/tbody/tr[", j, "]
                                 /td[1]/p/b/span/a/font")
        findall_name <- xml_find_all(doc_province, hrefxpath_name)
        name <- xml_text(findall_name)[1]
        
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                     /div[contains(@class, 'left')]
                                     /div[contains(@class, 'left2')]
                                     /div/div/table/tbody/tr[", j, "]
                                     /td[1]/span/span/span/strong/a/font")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                     /div[contains(@class, 'left')]
                                     /div[contains(@class, 'left2')]
                                     /div/div/table/tbody/tr[", j, "]
                                     /td[1]/a/font/strong")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        

        
        # position
        hrefxpath_position <- paste0("/html/body/div[contains(@class, 'main')]
                                     /div[contains(@class, 'left')]
                                     /div[contains(@class, 'left2')]
                                     /div/div/table/tbody/tr[", j, "]
                                     /td[2]/p/span/font")
        findall_position <- xml_find_all(doc_province, hrefxpath_position)
        position <- xml_text(findall_position)[1]
        if (is.na(position)){
            hrefxpath_position <- paste0("/html/body/div[contains(@class, 'main')]
                                         /div[contains(@class, 'left')]
                                         /div[contains(@class, 'left2')]
                                         /div/div/div/table/tbody/tr[", j, "]
                                         /td[2]/p/span/font/font/font/font/font/font/font/font/font/font/font/font/font/font/span")
            findall_position <- xml_find_all(doc_province, hrefxpath_position)
            position <- xml_text(findall_position)[1]
        }
        if (is.na(position)){
            hrefxpath_position <- paste0("/html/body/div[contains(@class, 'main')]
                                         /div[contains(@class, 'left')]
                                         /div[contains(@class, 'left2')]
                                         /div/div/div/table/tbody/tr[", j, "]
                                         /td[2]/font")
            findall_position <- xml_find_all(doc_province, hrefxpath_position)
            position <- xml_text(findall_position)[1]
        }
        if (is.na(position)){
            hrefxpath_position <- paste0("/html/body/div[contains(@class, 'main')]
                                         /div[contains(@class, 'left')]
                                         /div[contains(@class, 'left2')]
                                         /div/div/table/tbody/tr[", j, "]
                                         /td[2]/font")
            findall_position <- xml_find_all(doc_province, hrefxpath_position)
            position <- xml_text(findall_position)[1]
        }

        
        # url
        hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                /div[contains(@class, 'left')]
                                /div[contains(@class, 'left2')]
                                /div/div/table/tbody/tr[", j, "]/td[1]/p/b/span/a")
        findall_url <- xml_find_all(doc_province, hrefxpath_url)
        link <- xml_attr(findall_url, "href")[1]
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                    /div[contains(@class, 'left')]
                                    /div[contains(@class, 'left2')]
                                    /div/div/table/tbody/tr[", j, "]/td[1]/span/span/span/strong/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                    /div[contains(@class, 'left')]
                                    /div[contains(@class, 'left2')]
                                    /div/div/table/tbody/tr[", j, "]/td[1]/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        
        
        print(link)
        if (length(link) == 0){
            link <- NA
        }
        
        tempdf <- data.frame(
            province <- province,
            name <- name,
            position <- position,
            link <- link)
        
        namelist3 <- rbind(namelist3, tempdf)
    }
}


# 30
namelist4 <- data.frame(
    province <- character(0),
    name <- character(0),
    position <- character(0),
    link <- character(0)
)

for(i in 30:30){
    if (is.na(provincelist[i,2]) == T){
        next
    } 
    
    for(j in 1:15){
        tempdf <- data.frame(
            province = character(0),
            name = character(0),
            position = character(0),
            link = character(0))
        
        url_province <- as.character(provincelist[i,2])
        doc_province <- read_html(url_province, encoding = "GB18030")
        
        # province
        province <- provincelist[i,1]
        
        
        # name
        hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                 /div[contains(@class, 'left')]
                                 /div[contains(@class, 'left2')]
                                 /div/div/table/tbody/tr[", j, "]
                                 /td[1]/p/a/font/strong")
        findall_name <- xml_find_all(doc_province, hrefxpath_name)
        name <- xml_text(findall_name)[1]
        
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                     /div[contains(@class, 'left')]
                                     /div[contains(@class, 'left2')]
                                     /div/div/table/tbody/tr[", j, "]
                                     /td[1]/p/b/span/a/font")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                     /div[contains(@class, 'left')]
                                     /div[contains(@class, 'left2')]
                                     /div/div/table/tbody/tr[", j, "]
                                     /td[1]/span/p/b/span/a/font")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                     /div[contains(@class, 'left')]
                                     /div[contains(@class, 'left2')]
                                     /div/div/table/tbody/tr[", j, "]
                                     /td[1]/a/font/strong")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        
        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                     /div[contains(@class, 'left')]
                                     /div[contains(@class, 'left2')]
                                     /div/div/table/tbody/tr[", j, "]
                                     /td[1]/b/span/span/a/font")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }

        if (is.na(name)){
            hrefxpath_name <- paste0("/html/body/div[contains(@class, 'main')]
                                     /div[contains(@class, 'left')]
                                     /div[contains(@class, 'left2')]
                                     /div/div/table/tbody/tr[", j, "]
                                     /td[1]/a/strong/font")
            findall_name <- xml_find_all(doc_province, hrefxpath_name)
            name <- xml_text(findall_name)[1]
        }
        
        
        
        # position
        hrefxpath_position <- paste0("/html/body/div[contains(@class, 'main')]
                                     /div[contains(@class, 'left')]
                                     /div[contains(@class, 'left2')]
                                     /div/div/table/tbody/tr[", j, "]
                                     /td[2]/p/span/font")
        findall_position <- xml_find_all(doc_province, hrefxpath_position)
        position <- xml_text(findall_position)[1]
        if (is.na(position)){
            hrefxpath_position <- paste0("/html/body/div[contains(@class, 'main')]
                                         /div[contains(@class, 'left')]
                                         /div[contains(@class, 'left2')]
                                         /div/div/div/table/tbody/tr[", j, "]
                                         /td[2]/p/span/font")
            findall_position <- xml_find_all(doc_province, hrefxpath_position)
            position <- xml_text(findall_position)[1]
        }
        if (is.na(position)){
            hrefxpath_position <- paste0("/html/body/div[contains(@class, 'main')]
                                         /div[contains(@class, 'left')]
                                         /div[contains(@class, 'left2')]
                                         /div/div/div/table/tbody/tr[", j, "]
                                         /td[2]/p/span/font")
            findall_position <- xml_find_all(doc_province, hrefxpath_position)
            position <- xml_text(findall_position)[1]
        }
        
        if (is.na(position)){
            hrefxpath_position <- paste0("/html/body/div[contains(@class, 'main')]
                                         /div[contains(@class, 'left')]
                                         /div[contains(@class, 'left2')]
                                         /div/div/table/tbody/tr[", j, "]
                                         /td[2]/span/font/font/font/font/font/font/font/font/font/font/font")
            findall_position <- xml_find_all(doc_province, hrefxpath_position)
            position <- xml_text(findall_position)[1]
        }
        if (is.na(position)){
            hrefxpath_position <- paste0("/html/body/div[contains(@class, 'main')]
                                         /div[contains(@class, 'left')]
                                         /div[contains(@class, 'left2')]
                                         /div/div/table/tbody/tr[", j, "]
                                         /td[2]/span/font")
            findall_position <- xml_find_all(doc_province, hrefxpath_position)
            position <- xml_text(findall_position)[1]
        }
        
        if (is.na(position)){
            hrefxpath_position <- paste0("/html/body/div[contains(@class, 'main')]
                                         /div[contains(@class, 'left')]
                                         /div[contains(@class, 'left2')]
                                         /div/div/table/tbody/tr[", j, "]
                                         /td[2]/font/font/font/font/font/font/font/font/font/font/font/font")
            findall_position <- xml_find_all(doc_province, hrefxpath_position)
            position <- xml_text(findall_position)[1]
        }
        
        if (is.na(position)){
            hrefxpath_position <- paste0("/html/body/div[contains(@class, 'main')]
                                         /div[contains(@class, 'left')]
                                         /div[contains(@class, 'left2')]
                                         /div/div/table/tbody/tr[", j, "]
                                         /td[2]/font")
            findall_position <- xml_find_all(doc_province, hrefxpath_position)
            position <- xml_text(findall_position)[1]
        }
        
        # url
        hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                /div[contains(@class, 'left')]
                                /div[contains(@class, 'left2')]
                                /div/div/table/tbody/tr[", j, "]/td[1]/p/a")
        findall_url <- xml_find_all(doc_province, hrefxpath_url)
        link <- xml_attr(findall_url, "href")[1]
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                    /div[contains(@class, 'left')]
                                    /div[contains(@class, 'left2')]
                                    /div/div/table/tbody/tr[", j, "]/td[1]/p/b/span/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                    /div[contains(@class, 'left')]
                                    /div[contains(@class, 'left2')]
                                    /div/div/table/tbody/tr[", j, "]/td[1]/span/p/b/span/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }
        if (is.na(link)){
            hrefxpath_url <- paste0("/html/body/div[contains(@class, 'main')]
                                    /div[contains(@class, 'left')]
                                    /div[contains(@class, 'left2')]
                                    /div/div/table/tbody/tr[", j, "]/td[1]/a")
            findall_url <- xml_find_all(doc_province, hrefxpath_url)
            link <- xml_attr(findall_url, "href")[1]
        }

        
        print(link)
        if (length(link) == 0){
            link <- NA
        }
        
        tempdf <- data.frame(
            province <- province,
            name <- name,
            position <- position,
            link <- link)
        
        namelist4 <- rbind(namelist4, tempdf)
    }
}


namelist_grand_1 <- rbind(namelist, namelist2, namelist3, namelist4)
for (i in length(namelist_grand_1[,1]):1){
    if(is.na(namelist_grand_1[i,2])){
        namelist_grand_1 <- namelist_grand_1[-i,]
    }
}
for (i in length(namelist_grand_1[,1]):1){
    if(is.na(namelist_grand_1[i,4])){
        namelist_grand_1 <- namelist_grand_1[-i,]
    }
}


# Preparation
colnames(namelist_grand_1) <- c("province", "name", "position", "link")


namelist_grand_1$province <- as.character(namelist_grand_1$province)
namelist_grand_1$name<- as.character(namelist_grand_1$name)
namelist_grand_1$position<- as.character(namelist_grand_1$position)

temp_df <- data.frame(index <- 1:length(namelist_grand_1$name))
namelist_grand_1 <- cbind(temp_df, namelist_grand_1)
colnames(namelist_grand_1)[1] <- "index"
rownames(namelist_grand_1) <- namelist_grand_1[,1]

bio_df <- data.frame(
    index = character(0),
    province = character(0),
    name = character(0),
    bio = character(0)
)

for (i in 1:length(namelist_grand_1[,2])){
    tryCatch({
        bio = character(0)
        tempdf <- data.frame(
            index = character(0),
            province = character(0),
            name = character(0),
            bio = character(0))
        
        url_name <- as.character(namelist_grand_1[i,5])
        tryCatch({
        }, error=function(e){})
        doc_name <- read_html(url_name, encoding = "GB18030")
        
        # index?
        index <- i
        
        # name
        name <- namelist_grand_1[i,3]
        
        # province
        province <- namelist_grand_1[i,2]
        
        
        # bio
        hrefxpath_bio <- "//*[@id='articleText']"
        findall_bio <- xml_find_all(doc_name, hrefxpath_bio)
        bio <- xml_text(findall_bio)
        
        tempdf <- data.frame(
            index = index,
            province = province,
            name = name,
            bio = bio)
        if (ncol(tempdf) == ncol(bio_df)){
            tempdf$province <- as.character(tempdf$province)
            tempdf$name <- as.character(tempdf$name)
            tempdf$bio <- as.character(tempdf$bio)
            
            con <- file(paste0(province, " - ", name, ".csv"), encoding="UTF-8")
            write.csv(tempdf, con)
        }
    }, error=function(e){cat("ERROR",index, conditionMessage(e), "\n")})
    
}

con <- file("namelist.csv", encoding="UTF-8")
write.csv(namelist_grand_1, con)
