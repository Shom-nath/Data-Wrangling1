library(dplyr)
library(tidyr)
install.packages("data.table")
library(data.table)
install.packages("dummies")
library(dummies)

head(refine_original)
View(refine_original)
refine_modified <- refine_original
refine_modified1 <- refine_modified %>% mutate(company = ifelse(company %like% "^P" | 
                                                                  company %like% "^p" |
                                                                  company %like% "^f","philips",
                                                                
                                                          ifelse(company %like% "^A" | 
                                                                 company %like% "^a","azko",
  
                                                          ifelse(company %like% "^V" | 
                                                                 company %like% "^v", "van houten",
  
                                                          ifelse(company %like% "^U" | 
                                                                 company %like% "^u", "unilever",company)))))


View(refine_modified1)
 refine_sep <- refine_modified1 %>% separate(`Product code / number`,c("product_code","product_number"), sep = "-")
 
 refine_sep_catg <- refine_sep %>% mutate(Product_category = ifelse(product_code =="p","Smartphone", 
                                          ifelse(product_code =="v","TV",
                                          ifelse(product_code =="x","Laptop",
                                          ifelse(product_code =="q","Tablet","NA")))))
 
 refine_add <- refine_sep_catg %>% unite(full_address,address,city,country,sep = ",",remove=TRUE)

 dvar1 <- dummy(refine_add$company,sep = "_") 
 
 dvar3 <- c(ifelse(grepl(" ", colnames(dvar1)) == "TRUE", gsub(" " , "_" , colnames(dvar1),fixed = TRUE),colnames(dvar1)))
 dvar3
 colnames(dvar1) <- dvar3
 dvar1
 
 dvar2 <- dummy(refine_add$Product_category,sep = "_") 
  
refine_add <- cbind(refine_add,dvar1,dvar2)
head(refine_add)
View(refine_add)
write.csv(refine_add, "~/Desktop/RScript/Example 1/refine_clean.csv")
