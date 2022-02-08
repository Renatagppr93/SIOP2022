

temp <- read.csv("initial_data_screen.csv", header=FALSE, na.strings="")   ## NOTE: 404 vars (9/28/21) make sure to check indexing used throughout script

x <- paste("item", sep="",1:404)
y <- t(temp[2,])
## decluttering Qualtrics excess
data <- temp[-c(1:3),]                                           ## Getting rid of all 3 weird Qualtrics rows
colnames(data) <- x

incomplete <- read.csv("inprogress.csv", header=FALSE, na.strings="")   ## NOTE: 404 vars (9/28/21) make sure to check indexing used throughout script

data2 <- incomplete[-c(1:2),-c(10,12,16,113,199,285,371,379,400, 406:411)]                                          

data3 <- as.data.frame(cbind(data2$V8,data2$V9,data2$V1,data2$V1,data2$V1,data2$V1,data2$V1,data2$V1,data2))

colnames(data3) <- x

rm(x, y, temp)       

use <- rbind(data,data3)

## STILL NEED FILE WITH BAD RESPONDENTS IDENTIFIED (E.G., Item_18=2, na > 200, longstring > 20, CARELESS CHECKS > 1)

library(careless)
use$careless_long <- longstring(use[18:399])
use$missing <- rowSums(is.na(use[18:404])) 

different <- use[ which(use$item18 == 2 | use$careless_long > 20 | use$missing > 200), ]

attention <- use[which(use$item61 == 5 & use$item145 == 5 & use$item248 == 2 & use$item308 == 3), ]

data <- data.frame(lapply(attention, function(x) as.numeric(as.character(x))))
data <- data[,c(1:404)]

numtot <- nrow(use)
numkicked <- nrow(different)
numvalid <- nrow(data)

data$item82[data$item82 == 8]  <- 1
data$item82[data$item82 == 9]  <- 2
data$item82[data$item82 == 10] <- 3
data$item82[data$item82 == 11] <- 4
data$item82[data$item82 == 12] <- 5

data$item118[data$item118 == 9]  <- 1
data$item118[data$item118 == 10] <- 2
data$item118[data$item118 == 11] <- 3
data$item118[data$item118 == 12] <- 4
data$item118[data$item118 == 13] <- 5

data$item119[data$item119 == 11] <- 1
data$item119[data$item119 == 12] <- 2
data$item119[data$item119 == 13] <- 3
data$item119[data$item119 == 14] <- 4
data$item119[data$item119 == 15] <- 5

data$item120[data$item120 == 6]  <- 1
data$item120[data$item120 == 7]  <- 2
data$item120[data$item120 == 8]  <- 3
data$item120[data$item120 == 9]  <- 4
data$item120[data$item120 == 10] <- 5

data$item121[data$item121 == 6]  <- 1
data$item121[data$item121 == 7]  <- 2
data$item121[data$item121 == 8]  <- 3
data$item121[data$item121 == 9]  <- 4
data$item121[data$item121 == 10] <- 5

data$item122[data$item122 == 6]  <- 1
data$item122[data$item122 == 7]  <- 2
data$item122[data$item122 == 8]  <- 3
data$item122[data$item122 == 9]  <- 4
data$item122[data$item122 == 10] <- 5

data$item123[data$item123 == 6]  <- 1
data$item123[data$item123 == 7]  <- 2
data$item123[data$item123 == 8]  <- 3
data$item123[data$item123 == 9]  <- 4
data$item123[data$item123 == 10] <- 5

data$item124[data$item124 == 6]  <- 1
data$item124[data$item124 == 7]  <- 2
data$item124[data$item124 == 8]  <- 3
data$item124[data$item124 == 9]  <- 4
data$item124[data$item124 == 10] <- 5

data$item125[data$item125 == 6]  <- 1
data$item125[data$item125 == 7]  <- 2
data$item125[data$item125 == 8]  <- 3
data$item125[data$item125 == 9]  <- 4
data$item125[data$item125 == 10] <- 5

data$item126[data$item126 == 11]  <- 1
data$item126[data$item126 == 12]  <- 2
data$item126[data$item126 == 13]  <- 3
data$item126[data$item126 == 14]  <- 4
data$item126[data$item126 == 15]  <- 5

data$item127[data$item127 == 4]  <- 1
data$item127[data$item127 == 5]  <- 2
data$item127[data$item127 == 6]  <- 3
data$item127[data$item127 == 7]  <- 4
data$item127[data$item127 == 8]  <- 5

data$item128[data$item128 == 11]  <- 1
data$item128[data$item128 == 12]  <- 2
data$item128[data$item128 == 13]  <- 3
data$item128[data$item128 == 14]  <- 4
data$item128[data$item128 == 15]  <- 5

data$item129[data$item129 == 11]  <- 1
data$item129[data$item129 == 12]  <- 2
data$item129[data$item129 == 13]  <- 3
data$item129[data$item129 == 14]  <- 4
data$item129[data$item129 == 15]  <- 5

data$item130[data$item130 == 11]  <- 1
data$item130[data$item130 == 12]  <- 2
data$item130[data$item130 == 13]  <- 3
data$item130[data$item130 == 14]  <- 4
data$item130[data$item130 == 15]  <- 5

data$item131[data$item131 == 11]  <- 1
data$item131[data$item131 == 12]  <- 2
data$item131[data$item131 == 13]  <- 3
data$item131[data$item131 == 14]  <- 4
data$item131[data$item131 == 15]  <- 5

data$item132[data$item132 == 11]  <- 1
data$item132[data$item132 == 12]  <- 2
data$item132[data$item132 == 13]  <- 3
data$item132[data$item132 == 14]  <- 4
data$item132[data$item132 == 15]  <- 5

data$item133[data$item133 == 11]  <- 1
data$item133[data$item133 == 12]  <- 2
data$item133[data$item133 == 13]  <- 3
data$item133[data$item133 == 14]  <- 4
data$item133[data$item133 == 15]  <- 5

data$item134[data$item134 == 11]  <- 1
data$item134[data$item134 == 12]  <- 2
data$item134[data$item134 == 13]  <- 3
data$item134[data$item134 == 14]  <- 4
data$item134[data$item134 == 15]  <- 5

data$item135[data$item135 == 6]  <- 1
data$item135[data$item135 == 7]  <- 2
data$item135[data$item135 == 8]  <- 3
data$item135[data$item135 == 9]  <- 4
data$item135[data$item135 == 10] <- 5

data$item136[data$item136 == 6]  <- 1
data$item136[data$item136 == 7]  <- 2
data$item136[data$item136 == 8]  <- 3
data$item136[data$item136 == 9]  <- 4
data$item136[data$item136 == 10] <- 5

data$item137[data$item137 == 6]  <- 1
data$item137[data$item137 == 7]  <- 2
data$item137[data$item137 == 8]  <- 3
data$item137[data$item137 == 9]  <- 4
data$item137[data$item137 == 10] <- 5

data$item138[data$item138 == 16]  <- 1
data$item138[data$item138 == 17]  <- 2
data$item138[data$item138 == 18]  <- 3
data$item138[data$item138 == 19]  <- 4
data$item138[data$item138 == 20]  <- 5

data$item139[data$item139 == 6]  <- 1
data$item139[data$item139 == 7]  <- 2
data$item139[data$item139 == 8]  <- 3
data$item139[data$item139 == 9]  <- 4
data$item139[data$item139 == 10] <- 5

data$item140[data$item140 == 6]  <- 1
data$item140[data$item140 == 7]  <- 2
data$item140[data$item140 == 8]  <- 3
data$item140[data$item140 == 9]  <- 4
data$item140[data$item140 == 10] <- 5

data$item141[data$item141 == 6]  <- 1
data$item141[data$item141 == 7]  <- 2
data$item141[data$item141 == 8]  <- 3
data$item141[data$item141 == 9]  <- 4
data$item141[data$item141 == 10] <- 5

data$item142[data$item142 == 6]  <- 1
data$item142[data$item142 == 7]  <- 2
data$item142[data$item142 == 8]  <- 3
data$item142[data$item142 == 9]  <- 4
data$item142[data$item142 == 10] <- 5

data$item143[data$item143 == 6]  <- 1
data$item143[data$item143 == 7]  <- 2
data$item143[data$item143 == 8]  <- 3
data$item143[data$item143 == 9]  <- 4
data$item143[data$item143 == 10] <- 5

data$item144[data$item144 == 6]  <- 1
data$item144[data$item144 == 7]  <- 2
data$item144[data$item144 == 8]  <- 3
data$item144[data$item144 == 9]  <- 4
data$item144[data$item144 == 10] <- 5

## Attention check = 145

data$item146[data$item146 == 16]  <- 1
data$item146[data$item146 == 17]  <- 2
data$item146[data$item146 == 18]  <- 3
data$item146[data$item146 == 19]  <- 4
data$item146[data$item146 == 20]  <- 5

data$item147[data$item147 == 6]  <- 1
data$item147[data$item147 == 7]  <- 2
data$item147[data$item147 == 8]  <- 3
data$item147[data$item147 == 9]  <- 4
data$item147[data$item147 == 10] <- 5

data$item148[data$item148 == 6]  <- 1
data$item148[data$item148 == 7]  <- 2
data$item148[data$item148 == 8]  <- 3
data$item148[data$item148 == 9]  <- 4
data$item148[data$item148 == 10] <- 5

data$item149[data$item149 == 6]  <- 1
data$item149[data$item149 == 7]  <- 2
data$item149[data$item149 == 8]  <- 3
data$item149[data$item149 == 9]  <- 4
data$item149[data$item149 == 10] <- 5

data$item150[data$item150 == 11]  <- 1
data$item150[data$item150 == 12]  <- 2
data$item150[data$item150 == 13]  <- 3
data$item150[data$item150 == 14]  <- 4
data$item150[data$item150 == 15]  <- 5

data$item151[data$item151 == 11]  <- 1
data$item151[data$item151 == 12]  <- 2
data$item151[data$item151 == 13]  <- 3
data$item151[data$item151 == 14]  <- 4
data$item151[data$item151 == 15]  <- 5

data$item152[data$item152 == 11]  <- 1
data$item152[data$item152 == 12]  <- 2
data$item152[data$item152 == 13]  <- 3
data$item152[data$item152 == 14]  <- 4
data$item152[data$item152 == 15]  <- 5

data$item153[data$item153 == 11]  <- 1
data$item153[data$item153 == 12]  <- 2
data$item153[data$item153 == 13]  <- 3
data$item153[data$item153 == 14]  <- 4
data$item153[data$item153 == 15]  <- 5

data$item154[data$item154 == 11]  <- 1
data$item154[data$item154 == 12]  <- 2
data$item154[data$item154 == 13]  <- 3
data$item154[data$item154 == 14]  <- 4
data$item154[data$item154 == 15]  <- 5

data$item155[data$item155 == 11]  <- 1
data$item155[data$item155 == 12]  <- 2
data$item155[data$item155 == 13]  <- 3
data$item155[data$item155 == 14]  <- 4
data$item155[data$item155 == 15]  <- 5

data$item156[data$item156 == 11]  <- 1
data$item156[data$item156 == 12]  <- 2
data$item156[data$item156 == 13]  <- 3
data$item156[data$item156 == 14]  <- 4
data$item156[data$item156 == 15]  <- 5

data$item157[data$item157 == 11]  <- 1
data$item157[data$item157 == 12]  <- 2
data$item157[data$item157 == 13]  <- 3
data$item157[data$item157 == 14]  <- 4
data$item157[data$item157 == 15]  <- 5

data$item158[data$item158 == 6]  <- 1
data$item158[data$item158 == 7]  <- 2
data$item158[data$item158 == 8]  <- 3
data$item158[data$item158 == 9]  <- 4
data$item158[data$item158 == 10] <- 5

data$item159[data$item159 == 11]  <- 1
data$item159[data$item159 == 12]  <- 2
data$item159[data$item159 == 13]  <- 3
data$item159[data$item159 == 14]  <- 4
data$item159[data$item159 == 15]  <- 5

data$item160[data$item160 == 11]  <- 1
data$item160[data$item160 == 12]  <- 2
data$item160[data$item160 == 13]  <- 3
data$item160[data$item160 == 14]  <- 4
data$item160[data$item160 == 15]  <- 5

data$item161[data$item161 == 6]  <- 1
data$item161[data$item161 == 7]  <- 2
data$item161[data$item161 == 8]  <- 3
data$item161[data$item161 == 9]  <- 4
data$item161[data$item161 == 10] <- 5

data$item162[data$item162 == 11]  <- 1
data$item162[data$item162 == 12]  <- 2
data$item162[data$item162 == 13]  <- 3
data$item162[data$item162 == 14]  <- 4
data$item162[data$item162 == 15]  <- 5

data$item163[data$item163 == 11]  <- 1
data$item163[data$item163 == 12]  <- 2
data$item163[data$item163 == 13]  <- 3
data$item163[data$item163 == 14]  <- 4
data$item163[data$item163 == 15]  <- 5

data$item164[data$item164 == 11]  <- 1
data$item164[data$item164 == 12]  <- 2
data$item164[data$item164 == 13]  <- 3
data$item164[data$item164 == 14]  <- 4
data$item164[data$item164 == 15]  <- 5

data$item165[data$item165 == 11]  <- 1
data$item165[data$item165 == 12]  <- 2
data$item165[data$item165 == 13]  <- 3
data$item165[data$item165 == 14]  <- 4
data$item165[data$item165 == 15]  <- 5

data$item166[data$item166 == 11]  <- 1
data$item166[data$item166 == 12]  <- 2
data$item166[data$item166 == 13]  <- 3
data$item166[data$item166 == 14]  <- 4
data$item166[data$item166 == 15]  <- 5

data$item167[data$item167 == 18]  <- 1
data$item167[data$item167 == 19]  <- 2
data$item167[data$item167 == 20]  <- 3
data$item167[data$item167 == 21]  <- 4
data$item167[data$item167 == 22]  <- 5

data$item168[data$item168 == 11]  <- 1
data$item168[data$item168 == 12]  <- 2
data$item168[data$item168 == 13]  <- 3
data$item168[data$item168 == 14]  <- 4
data$item168[data$item168 == 15]  <- 5

data$item169[data$item169 == 11]  <- 1
data$item169[data$item169 == 12]  <- 2
data$item169[data$item169 == 13]  <- 3
data$item169[data$item169 == 14]  <- 4
data$item169[data$item169 == 15]  <- 5

data$item170[data$item170 == 11]  <- 1
data$item170[data$item170 == 12]  <- 2
data$item170[data$item170 == 13]  <- 3
data$item170[data$item170 == 14]  <- 4
data$item170[data$item170 == 15]  <- 5

data$item171[data$item171 == 11]  <- 1
data$item171[data$item171 == 12]  <- 2
data$item171[data$item171 == 13]  <- 3
data$item171[data$item171 == 14]  <- 4
data$item171[data$item171 == 15]  <- 5

data$item172[data$item172 == 11]  <- 1
data$item172[data$item172 == 12]  <- 2
data$item172[data$item172 == 13]  <- 3
data$item172[data$item172 == 14]  <- 4
data$item172[data$item172 == 15]  <- 5

data$item173[data$item173 == 11]  <- 1
data$item173[data$item173 == 12]  <- 2
data$item173[data$item173 == 13]  <- 3
data$item173[data$item173 == 14]  <- 4
data$item173[data$item173 == 15]  <- 5

data$item174[data$item174 == 11]  <- 1
data$item174[data$item174 == 12]  <- 2
data$item174[data$item174 == 13]  <- 3
data$item174[data$item174 == 14]  <- 4
data$item174[data$item174 == 15]  <- 5

data$item175[data$item175 == 11]  <- 1
data$item175[data$item175 == 12]  <- 2
data$item175[data$item175 == 13]  <- 3
data$item175[data$item175 == 14]  <- 4
data$item175[data$item175 == 15]  <- 5

data$item176[data$item176 == 11]  <- 1
data$item176[data$item176 == 12]  <- 2
data$item176[data$item176 == 13]  <- 3
data$item176[data$item176 == 14]  <- 4
data$item176[data$item176 == 15]  <- 5

data$item177[data$item177 == 11]  <- 1
data$item177[data$item177 == 12]  <- 2
data$item177[data$item177 == 13]  <- 3
data$item177[data$item177 == 14]  <- 4
data$item177[data$item177 == 15]  <- 5

data$item178[data$item178 == 11]  <- 1
data$item178[data$item178 == 12]  <- 2
data$item178[data$item178 == 13]  <- 3
data$item178[data$item178 == 14]  <- 4
data$item178[data$item178 == 15]  <- 5

data$item179[data$item179 == 11]  <- 1
data$item179[data$item179 == 12]  <- 2
data$item179[data$item179 == 13]  <- 3
data$item179[data$item179 == 14]  <- 4
data$item179[data$item179 == 15]  <- 5

data$item180[data$item180 == 11]  <- 1
data$item180[data$item180 == 12]  <- 2
data$item180[data$item180 == 13]  <- 3
data$item180[data$item180 == 14]  <- 4
data$item180[data$item180 == 15]  <- 5

data$item181[data$item181 == 11]  <- 1
data$item181[data$item181 == 12]  <- 2
data$item181[data$item181 == 13]  <- 3
data$item181[data$item181 == 14]  <- 4
data$item181[data$item181 == 15]  <- 5

data$item182[data$item182 == 11]  <- 1
data$item182[data$item182 == 12]  <- 2
data$item182[data$item182 == 13]  <- 3
data$item182[data$item182 == 14]  <- 4
data$item182[data$item182 == 15]  <- 5

data$item183[data$item183 == 11]  <- 1
data$item183[data$item183 == 12]  <- 2
data$item183[data$item183 == 13]  <- 3
data$item183[data$item183 == 14]  <- 4
data$item183[data$item183 == 15]  <- 5

data$item184[data$item184 == 21]  <- 1
data$item184[data$item184 == 22]  <- 2
data$item184[data$item184 == 23]  <- 3
data$item184[data$item184 == 24]  <- 4
data$item184[data$item184 == 25]  <- 5

data$item185[data$item185 == 11]  <- 1
data$item185[data$item185 == 12]  <- 2
data$item185[data$item185 == 13]  <- 3
data$item185[data$item185 == 14]  <- 4
data$item185[data$item185 == 15]  <- 5

data$item186[data$item186 == 11]  <- 1
data$item186[data$item186 == 12]  <- 2
data$item186[data$item186 == 13]  <- 3
data$item186[data$item186 == 14]  <- 4
data$item186[data$item186 == 15]  <- 5

data$item187[data$item187 == 11]  <- 1
data$item187[data$item187 == 12]  <- 2
data$item187[data$item187 == 13]  <- 3
data$item187[data$item187 == 14]  <- 4
data$item187[data$item187 == 15]  <- 5

data$item188[data$item188 == 11]  <- 1
data$item188[data$item188 == 12]  <- 2
data$item188[data$item188 == 13]  <- 3
data$item188[data$item188 == 14]  <- 4
data$item188[data$item188 == 15]  <- 5

data$item189[data$item189 == 11]  <- 1
data$item189[data$item189 == 12]  <- 2
data$item189[data$item189 == 13]  <- 3
data$item189[data$item189 == 14]  <- 4
data$item189[data$item189 == 15]  <- 5

data$item190[data$item190 == 11]  <- 1
data$item190[data$item190 == 12]  <- 2
data$item190[data$item190 == 13]  <- 3
data$item190[data$item190 == 14]  <- 4
data$item190[data$item190 == 15]  <- 5

data$item191[data$item191 == 11]  <- 1
data$item191[data$item191 == 12]  <- 2
data$item191[data$item191 == 13]  <- 3
data$item191[data$item191 == 14]  <- 4
data$item191[data$item191 == 15]  <- 5

data$item192[data$item192 == 11]  <- 1
data$item192[data$item192 == 12]  <- 2
data$item192[data$item192 == 13]  <- 3
data$item192[data$item192 == 14]  <- 4
data$item192[data$item192 == 15]  <- 5

data$item193[data$item193 == 11]  <- 1
data$item193[data$item193 == 12]  <- 2
data$item193[data$item193 == 13]  <- 3
data$item193[data$item193 == 14]  <- 4
data$item193[data$item193 == 15]  <- 5

data$item194[data$item194 == 11]  <- 1
data$item194[data$item194 == 12]  <- 2
data$item194[data$item194 == 13]  <- 3
data$item194[data$item194 == 14]  <- 4
data$item194[data$item194 == 15]  <- 5

data$item195[data$item195 == 11]  <- 1
data$item195[data$item195 == 12]  <- 2
data$item195[data$item195 == 13]  <- 3
data$item195[data$item195 == 14]  <- 4
data$item195[data$item195 == 15]  <- 5

data$item196[data$item196 == 11]  <- 1
data$item196[data$item196 == 12]  <- 2
data$item196[data$item196 == 13]  <- 3
data$item196[data$item196 == 14]  <- 4
data$item196[data$item196 == 15]  <- 5

data$item197[data$item197 == 11]  <- 1
data$item197[data$item197 == 12]  <- 2
data$item197[data$item197 == 13]  <- 3
data$item197[data$item197 == 14]  <- 4
data$item197[data$item197 == 15]  <- 5

data$item198[data$item198 == 11]  <- 1
data$item198[data$item198 == 12]  <- 2
data$item198[data$item198 == 13]  <- 3
data$item198[data$item198 == 14]  <- 4
data$item198[data$item198 == 15]  <- 5

data$item199[data$item199 == 11]  <- 1
data$item199[data$item199 == 12]  <- 2
data$item199[data$item199 == 13]  <- 3
data$item199[data$item199 == 14]  <- 4
data$item199[data$item199 == 15]  <- 5

data$item200[data$item200 == 11]  <- 1
data$item200[data$item200 == 12]  <- 2
data$item200[data$item200 == 13]  <- 3
data$item200[data$item200 == 14]  <- 4
data$item200[data$item200 == 15]  <- 5

data$item201[data$item201 == 11]  <- 1
data$item201[data$item201 == 12]  <- 2
data$item201[data$item201 == 13]  <- 3
data$item201[data$item201 == 14]  <- 4
data$item201[data$item201 == 15]  <- 5

data$item202[data$item202 == 11]  <- 1
data$item202[data$item202 == 12]  <- 2
data$item202[data$item202 == 13]  <- 3
data$item202[data$item202 == 14]  <- 4
data$item202[data$item202 == 15]  <- 5

data$item203[data$item203 == 9]   <- 1
data$item203[data$item203 == 10]  <- 2
data$item203[data$item203 == 11]  <- 3
data$item203[data$item203 == 12]  <- 4
data$item203[data$item203 == 13]  <- 5

data$item204[data$item204 == 11]  <- 1
data$item204[data$item204 == 12]  <- 2
data$item204[data$item204 == 13]  <- 3
data$item204[data$item204 == 14]  <- 4
data$item204[data$item204 == 15]  <- 5

data$item205[data$item205 == 6]  <- 1
data$item205[data$item205 == 7]  <- 2
data$item205[data$item205 == 8]  <- 3
data$item205[data$item205 == 9]  <- 4
data$item205[data$item205 == 10] <- 5

data$item206[data$item206 == 6]  <- 1
data$item206[data$item206 == 7]  <- 2
data$item206[data$item206 == 8]  <- 3
data$item206[data$item206 == 9]  <- 4
data$item206[data$item206 == 10] <- 5

data$item207[data$item207 == 6]  <- 1
data$item207[data$item207 == 7]  <- 2
data$item207[data$item207 == 8]  <- 3
data$item207[data$item207 == 9]  <- 4
data$item207[data$item207 == 10] <- 5

data$item208[data$item208 == 6]  <- 1
data$item208[data$item208 == 7]  <- 2
data$item208[data$item208 == 8]  <- 3
data$item208[data$item208 == 9]  <- 4
data$item208[data$item208 == 10] <- 5

data$item209[data$item209 == 6]  <- 1
data$item209[data$item209 == 7]  <- 2
data$item209[data$item209 == 8]  <- 3
data$item209[data$item209 == 9]  <- 4
data$item209[data$item209 == 10] <- 5

data$item210[data$item210 == 6]  <- 1
data$item210[data$item210 == 7]  <- 2
data$item210[data$item210 == 8]  <- 3
data$item210[data$item210 == 9]  <- 4
data$item210[data$item210 == 10] <- 5

data$item211[data$item211 == 11]  <- 1
data$item211[data$item211 == 12]  <- 2
data$item211[data$item211 == 13]  <- 3
data$item211[data$item211 == 14]  <- 4
data$item211[data$item211 == 15]  <- 5

data$item212[data$item212 == 4]  <- 1
data$item212[data$item212 == 5]  <- 2
data$item212[data$item212 == 6]  <- 3
data$item212[data$item212 == 7]  <- 4
data$item212[data$item212 == 8]  <- 5

data$item213[data$item213 == 11]  <- 1
data$item213[data$item213 == 12]  <- 2
data$item213[data$item213 == 13]  <- 3
data$item213[data$item213 == 14]  <- 4
data$item213[data$item213 == 15]  <- 5

data$item214[data$item214 == 11]  <- 1
data$item214[data$item214 == 12]  <- 2
data$item214[data$item214 == 13]  <- 3
data$item214[data$item214 == 14]  <- 4
data$item214[data$item214 == 15]  <- 5

data$item215[data$item215 == 11]  <- 1
data$item215[data$item215 == 12]  <- 2
data$item215[data$item215 == 13]  <- 3
data$item215[data$item215 == 14]  <- 4
data$item215[data$item215 == 15]  <- 5

data$item216[data$item216 == 11]  <- 1
data$item216[data$item216 == 12]  <- 2
data$item216[data$item216 == 13]  <- 3
data$item216[data$item216 == 14]  <- 4
data$item216[data$item216 == 15]  <- 5

data$item217[data$item217 == 11]  <- 1
data$item217[data$item217 == 12]  <- 2
data$item217[data$item217 == 13]  <- 3
data$item217[data$item217 == 14]  <- 4
data$item217[data$item217 == 15]  <- 5

data$item218[data$item218 == 11]  <- 1
data$item218[data$item218 == 12]  <- 2
data$item218[data$item218 == 13]  <- 3
data$item218[data$item218 == 14]  <- 4
data$item218[data$item218 == 15]  <- 5

data$item219[data$item219 == 11]  <- 1
data$item219[data$item219 == 12]  <- 2
data$item219[data$item219 == 13]  <- 3
data$item219[data$item219 == 14]  <- 4
data$item219[data$item219 == 15]  <- 5

data$item220[data$item220 == 6]  <- 1
data$item220[data$item220 == 7]  <- 2
data$item220[data$item220 == 8]  <- 3
data$item220[data$item220 == 9]  <- 4
data$item220[data$item220 == 10] <- 5

data$item221[data$item221 == 6]  <- 1
data$item221[data$item221 == 7]  <- 2
data$item221[data$item221 == 8]  <- 3
data$item221[data$item221 == 9]  <- 4
data$item221[data$item221 == 10] <- 5

data$item222[data$item222 == 6]  <- 1
data$item222[data$item222 == 7]  <- 2
data$item222[data$item222 == 8]  <- 3
data$item222[data$item222 == 9]  <- 4
data$item222[data$item222 == 10] <- 5

data$item223[data$item223 == 16] <- 1
data$item223[data$item223 == 17] <- 2
data$item223[data$item223 == 18] <- 3
data$item223[data$item223 == 19] <- 4
data$item223[data$item223 == 20] <- 5

data$item224[data$item224 == 6]  <- 1
data$item224[data$item224 == 7]  <- 2
data$item224[data$item224 == 8]  <- 3
data$item224[data$item224 == 9]  <- 4
data$item224[data$item224 == 10] <- 5

data$item225[data$item225 == 6]  <- 1
data$item225[data$item225 == 7]  <- 2
data$item225[data$item225 == 8]  <- 3
data$item225[data$item225 == 9]  <- 4
data$item225[data$item225 == 10] <- 5

data$item226[data$item226 == 6]  <- 1
data$item226[data$item226 == 7]  <- 2
data$item226[data$item226 == 8]  <- 3
data$item226[data$item226 == 9]  <- 4
data$item226[data$item226 == 10] <- 5

data$item227[data$item227 == 6]  <- 1
data$item227[data$item227 == 7]  <- 2
data$item227[data$item227 == 8]  <- 3
data$item227[data$item227 == 9]  <- 4
data$item227[data$item227 == 10] <- 5

data$item228[data$item228 == 6]  <- 1
data$item228[data$item228 == 7]  <- 2
data$item228[data$item228 == 8]  <- 3
data$item228[data$item228 == 9]  <- 4
data$item228[data$item228 == 10] <- 5

data$item229[data$item229 == 6]  <- 1
data$item229[data$item229 == 7]  <- 2
data$item229[data$item229 == 8]  <- 3
data$item229[data$item229 == 9]  <- 4
data$item229[data$item229 == 10] <- 5

data$item230[data$item230 == 16] <- 1
data$item230[data$item230 == 17] <- 2
data$item230[data$item230 == 18] <- 3
data$item230[data$item230 == 19] <- 4
data$item230[data$item230 == 20] <- 5

data$item231[data$item231 == 6]  <- 1
data$item231[data$item231 == 7]  <- 2
data$item231[data$item231 == 8]  <- 3
data$item231[data$item231 == 9]  <- 4
data$item231[data$item231 == 10] <- 5

data$item232[data$item232 == 6]  <- 1
data$item232[data$item232 == 7]  <- 2
data$item232[data$item232 == 8]  <- 3
data$item232[data$item232 == 9]  <- 4
data$item232[data$item232 == 10] <- 5

data$item233[data$item233 == 6]  <- 1
data$item233[data$item233 == 7]  <- 2
data$item233[data$item233 == 8]  <- 3
data$item233[data$item233 == 9]  <- 4
data$item233[data$item233 == 10] <- 5

data$item234[data$item234 == 11]  <- 1
data$item234[data$item234 == 12]  <- 2
data$item234[data$item234 == 13]  <- 3
data$item234[data$item234 == 14]  <- 4
data$item234[data$item234 == 15]  <- 5

data$item235[data$item235 == 11]  <- 1
data$item235[data$item235 == 12]  <- 2
data$item235[data$item235 == 13]  <- 3
data$item235[data$item235 == 14]  <- 4
data$item235[data$item235 == 15]  <- 5

data$item236[data$item236 == 11]  <- 1
data$item236[data$item236 == 12]  <- 2
data$item236[data$item236 == 13]  <- 3
data$item236[data$item236 == 14]  <- 4
data$item236[data$item236 == 15]  <- 5

data$item237[data$item237 == 11]  <- 1
data$item237[data$item237 == 12]  <- 2
data$item237[data$item237 == 13]  <- 3
data$item237[data$item237 == 14]  <- 4
data$item237[data$item237 == 15]  <- 5

data$item238[data$item238 == 11]  <- 1
data$item238[data$item238 == 12]  <- 2
data$item238[data$item238 == 13]  <- 3
data$item238[data$item238 == 14]  <- 4
data$item238[data$item238 == 15]  <- 5

data$item239[data$item239 == 11]  <- 1
data$item239[data$item239 == 12]  <- 2
data$item239[data$item239 == 13]  <- 3
data$item239[data$item239 == 14]  <- 4
data$item239[data$item239 == 15]  <- 5

data$item240[data$item240 == 11]  <- 1
data$item240[data$item240 == 12]  <- 2
data$item240[data$item240 == 13]  <- 3
data$item240[data$item240 == 14]  <- 4
data$item240[data$item240 == 15]  <- 5

data$item241[data$item241 == 11]  <- 1
data$item241[data$item241 == 12]  <- 2
data$item241[data$item241 == 13]  <- 3
data$item241[data$item241 == 14]  <- 4
data$item241[data$item241 == 15]  <- 5

data$item242[data$item242 == 6]  <- 1
data$item242[data$item242 == 7]  <- 2
data$item242[data$item242 == 8]  <- 3
data$item242[data$item242 == 9]  <- 4
data$item242[data$item242 == 10] <- 5

data$item243[data$item243 == 11]  <- 1
data$item243[data$item243 == 12]  <- 2
data$item243[data$item243 == 13]  <- 3
data$item243[data$item243 == 14]  <- 4
data$item243[data$item243 == 15]  <- 5

data$item244[data$item244 == 11]  <- 1
data$item244[data$item244 == 12]  <- 2
data$item244[data$item244 == 13]  <- 3
data$item244[data$item244 == 14]  <- 4
data$item244[data$item244 == 15]  <- 5

data$item245[data$item245 == 6]  <- 1
data$item245[data$item245 == 7]  <- 2
data$item245[data$item245 == 8]  <- 3
data$item245[data$item245 == 9]  <- 4
data$item245[data$item245 == 10] <- 5

data$item246[data$item246 == 11]  <- 1
data$item246[data$item246 == 12]  <- 2
data$item246[data$item246 == 13]  <- 3
data$item246[data$item246 == 14]  <- 4
data$item246[data$item246 == 15]  <- 5

data$item247[data$item247 == 11]  <- 1
data$item247[data$item247 == 12]  <- 2
data$item247[data$item247 == 13]  <- 3
data$item247[data$item247 == 14]  <- 4
data$item247[data$item247 == 15]  <- 5

## Attention check

data$item249[data$item249 == 11]  <- 1
data$item249[data$item249 == 12]  <- 2
data$item249[data$item249 == 13]  <- 3
data$item249[data$item249 == 14]  <- 4
data$item249[data$item249 == 15]  <- 5

data$item250[data$item250 == 11]  <- 1
data$item250[data$item250 == 12]  <- 2
data$item250[data$item250 == 13]  <- 3
data$item250[data$item250 == 14]  <- 4
data$item250[data$item250 == 15]  <- 5

data$item251[data$item251 == 11]  <- 1
data$item251[data$item251 == 12]  <- 2
data$item251[data$item251 == 13]  <- 3
data$item251[data$item251 == 14]  <- 4
data$item251[data$item251 == 15]  <- 5

data$item252[data$item252 == 18]  <- 1
data$item252[data$item252 == 19]  <- 2
data$item252[data$item252 == 20]  <- 3
data$item252[data$item252 == 21]  <- 4
data$item252[data$item252 == 22]  <- 5

data$item253[data$item253 == 11]  <- 1
data$item253[data$item253 == 12]  <- 2
data$item253[data$item253 == 13]  <- 3
data$item253[data$item253 == 14]  <- 4
data$item253[data$item253 == 15]  <- 5

data$item254[data$item254 == 11]  <- 1
data$item254[data$item254 == 12]  <- 2
data$item254[data$item254 == 13]  <- 3
data$item254[data$item254 == 14]  <- 4
data$item254[data$item254 == 15]  <- 5

data$item255[data$item255 == 11]  <- 1
data$item255[data$item255 == 12]  <- 2
data$item255[data$item255 == 13]  <- 3
data$item255[data$item255 == 14]  <- 4
data$item255[data$item255 == 15]  <- 5

data$item256[data$item256 == 11]  <- 1
data$item256[data$item256 == 12]  <- 2
data$item256[data$item256 == 13]  <- 3
data$item256[data$item256 == 14]  <- 4
data$item256[data$item256 == 15]  <- 5

data$item257[data$item257 == 11]  <- 1
data$item257[data$item257 == 12]  <- 2
data$item257[data$item257 == 13]  <- 3
data$item257[data$item257 == 14]  <- 4
data$item257[data$item257 == 15]  <- 5

data$item258[data$item258 == 11]  <- 1
data$item258[data$item258 == 12]  <- 2
data$item258[data$item258 == 13]  <- 3
data$item258[data$item258 == 14]  <- 4
data$item258[data$item258 == 15]  <- 5

data$item259[data$item259 == 11]  <- 1
data$item259[data$item259 == 12]  <- 2
data$item259[data$item259 == 13]  <- 3
data$item259[data$item259 == 14]  <- 4
data$item259[data$item259 == 15]  <- 5

data$item260[data$item260 == 11]  <- 1
data$item260[data$item260 == 12]  <- 2
data$item260[data$item260 == 13]  <- 3
data$item260[data$item260 == 14]  <- 4
data$item260[data$item260 == 15]  <- 5

data$item261[data$item261 == 11]  <- 1
data$item261[data$item261 == 12]  <- 2
data$item261[data$item261 == 13]  <- 3
data$item261[data$item261 == 14]  <- 4
data$item261[data$item261 == 15]  <- 5

data$item262[data$item262 == 11]  <- 1
data$item262[data$item262 == 12]  <- 2
data$item262[data$item262 == 13]  <- 3
data$item262[data$item262 == 14]  <- 4
data$item262[data$item262 == 15]  <- 5

data$item263[data$item263 == 11]  <- 1
data$item263[data$item263 == 12]  <- 2
data$item263[data$item263 == 13]  <- 3
data$item263[data$item263 == 14]  <- 4
data$item263[data$item263 == 15]  <- 5

data$item264[data$item264 == 11]  <- 1
data$item264[data$item264 == 12]  <- 2
data$item264[data$item264 == 13]  <- 3
data$item264[data$item264 == 14]  <- 4
data$item264[data$item264 == 15]  <- 5

data$item265[data$item265 == 11]  <- 1
data$item265[data$item265 == 12]  <- 2
data$item265[data$item265 == 13]  <- 3
data$item265[data$item265 == 14]  <- 4
data$item265[data$item265 == 15]  <- 5

data$item266[data$item266 == 11]  <- 1
data$item266[data$item266 == 12]  <- 2
data$item266[data$item266 == 13]  <- 3
data$item266[data$item266 == 14]  <- 4
data$item266[data$item266 == 15]  <- 5

data$item267[data$item267 == 11]  <- 1
data$item267[data$item267 == 12]  <- 2
data$item267[data$item267 == 13]  <- 3
data$item267[data$item267 == 14]  <- 4
data$item267[data$item267 == 15]  <- 5

data$item268[data$item268 == 11]  <- 1
data$item268[data$item268 == 12]  <- 2
data$item268[data$item268 == 13]  <- 3
data$item268[data$item268 == 14]  <- 4
data$item268[data$item268 == 15]  <- 5

data$item269[data$item269 == 21]  <- 1
data$item269[data$item269 == 22]  <- 2
data$item269[data$item269 == 23]  <- 3
data$item269[data$item269 == 24]  <- 4
data$item269[data$item269 == 25]  <- 5

data$item270[data$item270 == 11]  <- 1
data$item270[data$item270 == 12]  <- 2
data$item270[data$item270 == 13]  <- 3
data$item270[data$item270 == 14]  <- 4
data$item270[data$item270 == 15]  <- 5

data$item271[data$item271 == 11]  <- 1
data$item271[data$item271 == 12]  <- 2
data$item271[data$item271 == 13]  <- 3
data$item271[data$item271 == 14]  <- 4
data$item271[data$item271 == 15]  <- 5

data$item272[data$item272 == 11]  <- 1
data$item272[data$item272 == 12]  <- 2
data$item272[data$item272 == 13]  <- 3
data$item272[data$item272 == 14]  <- 4
data$item272[data$item272 == 15]  <- 5

data$item273[data$item273 == 11]  <- 1
data$item273[data$item273 == 12]  <- 2
data$item273[data$item273 == 13]  <- 3
data$item273[data$item273 == 14]  <- 4
data$item273[data$item273 == 15]  <- 5

data$item274[data$item274 == 11]  <- 1
data$item274[data$item274 == 12]  <- 2
data$item274[data$item274 == 13]  <- 3
data$item274[data$item274 == 14]  <- 4
data$item274[data$item274 == 15]  <- 5

data$item275[data$item275 == 11]  <- 1
data$item275[data$item275 == 12]  <- 2
data$item275[data$item275 == 13]  <- 3
data$item275[data$item275 == 14]  <- 4
data$item275[data$item275 == 15]  <- 5

data$item276[data$item276 == 11]  <- 1
data$item276[data$item276 == 12]  <- 2
data$item276[data$item276 == 13]  <- 3
data$item276[data$item276 == 14]  <- 4
data$item276[data$item276 == 15]  <- 5

data$item277[data$item277 == 11]  <- 1
data$item277[data$item277 == 12]  <- 2
data$item277[data$item277 == 13]  <- 3
data$item277[data$item277 == 14]  <- 4
data$item277[data$item277 == 15]  <- 5

data$item278[data$item278 == 11]  <- 1
data$item278[data$item278 == 12]  <- 2
data$item278[data$item278 == 13]  <- 3
data$item278[data$item278 == 14]  <- 4
data$item278[data$item278 == 15]  <- 5

data$item279[data$item279 == 11]  <- 1
data$item279[data$item279 == 12]  <- 2
data$item279[data$item279 == 13]  <- 3
data$item279[data$item279 == 14]  <- 4
data$item279[data$item279 == 15]  <- 5

data$item280[data$item280 == 11]  <- 1
data$item280[data$item280 == 12]  <- 2
data$item280[data$item280 == 13]  <- 3
data$item280[data$item280 == 14]  <- 4
data$item280[data$item280 == 15]  <- 5

data$item281[data$item281 == 11]  <- 1
data$item281[data$item281 == 12]  <- 2
data$item281[data$item281 == 13]  <- 3
data$item281[data$item281 == 14]  <- 4
data$item281[data$item281 == 15]  <- 5

data$item282[data$item282 == 11]  <- 1
data$item282[data$item282 == 12]  <- 2
data$item282[data$item282 == 13]  <- 3
data$item282[data$item282 == 14]  <- 4
data$item282[data$item282 == 15]  <- 5

data$item283[data$item283 == 11]  <- 1
data$item283[data$item283 == 12]  <- 2
data$item283[data$item283 == 13]  <- 3
data$item283[data$item283 == 14]  <- 4
data$item283[data$item283 == 15]  <- 5

data$item284[data$item284 == 11]  <- 1
data$item284[data$item284 == 12]  <- 2
data$item284[data$item284 == 13]  <- 3
data$item284[data$item284 == 14]  <- 4
data$item284[data$item284 == 15]  <- 5

data$item285[data$item285 == 11]  <- 1
data$item285[data$item285 == 12]  <- 2
data$item285[data$item285 == 13]  <- 3
data$item285[data$item285 == 14]  <- 4
data$item285[data$item285 == 15]  <- 5

data$item286[data$item286 == 11]  <- 1
data$item286[data$item286 == 12]  <- 2
data$item286[data$item286 == 13]  <- 3
data$item286[data$item286 == 14]  <- 4
data$item286[data$item286 == 15]  <- 5

data$item287[data$item287 == 11]  <- 1
data$item287[data$item287 == 12]  <- 2
data$item287[data$item287 == 13]  <- 3
data$item287[data$item287 == 14]  <- 4
data$item287[data$item287 == 15]  <- 5

data$item288[data$item288 == 9]   <- 1
data$item288[data$item288 == 10]  <- 2
data$item288[data$item288 == 11]  <- 3
data$item288[data$item288 == 12]  <- 4
data$item288[data$item288 == 13]  <- 5

data$item289[data$item289 == 11]  <- 1
data$item289[data$item289 == 12]  <- 2
data$item289[data$item289 == 13]  <- 3
data$item289[data$item289 == 14]  <- 4
data$item289[data$item289 == 15]  <- 5

data$item290[data$item290 == 6]  <- 1
data$item290[data$item290 == 7]  <- 2
data$item290[data$item290 == 8]  <- 3
data$item290[data$item290 == 9]  <- 4
data$item290[data$item290 == 10] <- 5

data$item291[data$item291 == 6]  <- 1
data$item291[data$item291 == 7]  <- 2
data$item291[data$item291 == 8]  <- 3
data$item291[data$item291 == 9]  <- 4
data$item291[data$item291 == 10] <- 5

data$item292[data$item292 == 6]  <- 1
data$item292[data$item292 == 7]  <- 2
data$item292[data$item292 == 8]  <- 3
data$item292[data$item292 == 9]  <- 4
data$item292[data$item292 == 10] <- 5

data$item293[data$item293 == 6]  <- 1
data$item293[data$item293 == 7]  <- 2
data$item293[data$item293 == 8]  <- 3
data$item293[data$item293 == 9]  <- 4
data$item293[data$item293 == 10] <- 5

data$item294[data$item294 == 6]  <- 1
data$item294[data$item294 == 7]  <- 2
data$item294[data$item294 == 8]  <- 3
data$item294[data$item294 == 9]  <- 4
data$item294[data$item294 == 10] <- 5

data$item295[data$item295 == 6]  <- 1
data$item295[data$item295 == 7]  <- 2
data$item295[data$item295 == 8]  <- 3
data$item295[data$item295 == 9]  <- 4
data$item295[data$item295 == 10] <- 5

data$item296[data$item296 == 11]  <- 1
data$item296[data$item296 == 12]  <- 2
data$item296[data$item296 == 13]  <- 3
data$item296[data$item296 == 14]  <- 4
data$item296[data$item296 == 15]  <- 5

data$item297[data$item297 == 4]  <- 1
data$item297[data$item297 == 5]  <- 2
data$item297[data$item297 == 6]  <- 3
data$item297[data$item297 == 7]  <- 4
data$item297[data$item297 == 8]  <- 5

data$item298[data$item298 == 11]  <- 1
data$item298[data$item298 == 12]  <- 2
data$item298[data$item298 == 13]  <- 3
data$item298[data$item298 == 14]  <- 4
data$item298[data$item298 == 15]  <- 5

data$item299[data$item299 == 11]  <- 1
data$item299[data$item299 == 12]  <- 2
data$item299[data$item299 == 13]  <- 3
data$item299[data$item299 == 14]  <- 4
data$item299[data$item299 == 15]  <- 5

data$item300[data$item300 == 11]  <- 1
data$item300[data$item300 == 12]  <- 2
data$item300[data$item300 == 13]  <- 3
data$item300[data$item300 == 14]  <- 4
data$item300[data$item300 == 15]  <- 5

data$item301[data$item301 == 11]  <- 1
data$item301[data$item301 == 12]  <- 2
data$item301[data$item301 == 13]  <- 3
data$item301[data$item301 == 14]  <- 4
data$item301[data$item301 == 15]  <- 5

data$item302[data$item302 == 11]  <- 1
data$item302[data$item302 == 12]  <- 2
data$item302[data$item302 == 13]  <- 3
data$item302[data$item302 == 14]  <- 4
data$item302[data$item302 == 15]  <- 5

data$item303[data$item303 == 11]  <- 1
data$item303[data$item303 == 12]  <- 2
data$item303[data$item303 == 13]  <- 3
data$item303[data$item303 == 14]  <- 4
data$item303[data$item303 == 15]  <- 5

data$item304[data$item304 == 11]  <- 1
data$item304[data$item304 == 12]  <- 2
data$item304[data$item304 == 13]  <- 3
data$item304[data$item304 == 14]  <- 4
data$item304[data$item304 == 15]  <- 5

data$item305[data$item305 == 6]  <- 1
data$item305[data$item305 == 7]  <- 2
data$item305[data$item305 == 8]  <- 3
data$item305[data$item305 == 9]  <- 4
data$item305[data$item305 == 10] <- 5

data$item306[data$item306 == 6]  <- 1
data$item306[data$item306 == 7]  <- 2
data$item306[data$item306 == 8]  <- 3
data$item306[data$item306 == 9]  <- 4
data$item306[data$item306 == 10] <- 5

data$item307[data$item307 == 6]  <- 1
data$item307[data$item307 == 7]  <- 2
data$item307[data$item307 == 8]  <- 3
data$item307[data$item307 == 9]  <- 4
data$item307[data$item307 == 10] <- 5

## Attention check

data$item309[data$item309 == 16]  <- 1
data$item309[data$item309 == 17]  <- 2
data$item309[data$item309 == 18]  <- 3
data$item309[data$item309 == 19]  <- 4
data$item309[data$item309 == 20]  <- 5

data$item310[data$item310 == 6]  <- 1
data$item310[data$item310 == 7]  <- 2
data$item310[data$item310 == 8]  <- 3
data$item310[data$item310 == 9]  <- 4
data$item310[data$item310 == 10] <- 5

data$item311[data$item311 == 6]  <- 1
data$item311[data$item311 == 7]  <- 2
data$item311[data$item311 == 8]  <- 3
data$item311[data$item311 == 9]  <- 4
data$item311[data$item311 == 10] <- 5

data$item312[data$item312 == 6]  <- 1
data$item312[data$item312 == 7]  <- 2
data$item312[data$item312 == 8]  <- 3
data$item312[data$item312 == 9]  <- 4
data$item312[data$item312 == 10] <- 5

data$item313[data$item313 == 6]  <- 1
data$item313[data$item313 == 7]  <- 2
data$item313[data$item313 == 8]  <- 3
data$item313[data$item313 == 9]  <- 4
data$item313[data$item313 == 10] <- 5

data$item314[data$item314 == 6]  <- 1
data$item314[data$item314 == 7]  <- 2
data$item314[data$item314 == 8]  <- 3
data$item314[data$item314 == 9]  <- 4
data$item314[data$item314 == 10] <- 5

data$item315[data$item315 == 6]  <- 1
data$item315[data$item315 == 7]  <- 2
data$item315[data$item315 == 8]  <- 3
data$item315[data$item315 == 9]  <- 4
data$item315[data$item315 == 10] <- 5

data$item316[data$item316 == 16]  <- 1
data$item316[data$item316 == 17]  <- 2
data$item316[data$item316 == 18]  <- 3
data$item316[data$item316 == 19]  <- 4
data$item316[data$item316 == 20]  <- 5

data$item317[data$item317 == 6]  <- 1
data$item317[data$item317 == 7]  <- 2
data$item317[data$item317 == 8]  <- 3
data$item317[data$item317 == 9]  <- 4
data$item317[data$item317 == 10] <- 5

data$item318[data$item318 == 6]  <- 1
data$item318[data$item318 == 7]  <- 2
data$item318[data$item318 == 8]  <- 3
data$item318[data$item318 == 9]  <- 4
data$item318[data$item318 == 10] <- 5

data$item319[data$item319 == 6]  <- 1
data$item319[data$item319 == 7]  <- 2
data$item319[data$item319 == 8]  <- 3
data$item319[data$item319 == 9]  <- 4
data$item319[data$item319 == 10] <- 5

data$item320[data$item320 == 11] <- 1
data$item320[data$item320 == 12] <- 2
data$item320[data$item320 == 13] <- 3
data$item320[data$item320 == 14] <- 4
data$item320[data$item320 == 15] <- 5

data$item321[data$item321 == 11] <- 1
data$item321[data$item321 == 12] <- 2
data$item321[data$item321 == 13] <- 3
data$item321[data$item321 == 14] <- 4
data$item321[data$item321 == 15] <- 5

data$item322[data$item322 == 11] <- 1
data$item322[data$item322 == 12] <- 2
data$item322[data$item322 == 13] <- 3
data$item322[data$item322 == 14] <- 4
data$item322[data$item322 == 15] <- 5

data$item323[data$item323 == 11] <- 1
data$item323[data$item323 == 12] <- 2
data$item323[data$item323 == 13] <- 3
data$item323[data$item323 == 14] <- 4
data$item323[data$item323 == 15] <- 5

data$item324[data$item324 == 11] <- 1
data$item324[data$item324 == 12] <- 2
data$item324[data$item324 == 13] <- 3
data$item324[data$item324 == 14] <- 4
data$item324[data$item324 == 15] <- 5

data$item325[data$item325 == 11] <- 1
data$item325[data$item325 == 12] <- 2
data$item325[data$item325 == 13] <- 3
data$item325[data$item325 == 14] <- 4
data$item325[data$item325 == 15] <- 5

data$item326[data$item326 == 11] <- 1
data$item326[data$item326 == 12] <- 2
data$item326[data$item326 == 13] <- 3
data$item326[data$item326 == 14] <- 4
data$item326[data$item326 == 15] <- 5

data$item327[data$item327 == 11] <- 1
data$item327[data$item327 == 12] <- 2
data$item327[data$item327 == 13] <- 3
data$item327[data$item327 == 14] <- 4
data$item327[data$item327 == 15] <- 5

data$item328[data$item328 == 6]  <- 1
data$item328[data$item328 == 7]  <- 2
data$item328[data$item328 == 8]  <- 3
data$item328[data$item328 == 9]  <- 4
data$item328[data$item328 == 10] <- 5

data$item329[data$item329 == 11] <- 1
data$item329[data$item329 == 12] <- 2
data$item329[data$item329 == 13] <- 3
data$item329[data$item329 == 14] <- 4
data$item329[data$item329 == 15] <- 5

data$item330[data$item330 == 11] <- 1
data$item330[data$item330 == 12] <- 2
data$item330[data$item330 == 13] <- 3
data$item330[data$item330 == 14] <- 4
data$item330[data$item330 == 15] <- 5

data$item331[data$item331 == 6]  <- 1
data$item331[data$item331 == 7]  <- 2
data$item331[data$item331 == 8]  <- 3
data$item331[data$item331 == 9]  <- 4
data$item331[data$item331 == 10] <- 5

data$item332[data$item332 == 11] <- 1
data$item332[data$item332 == 12] <- 2
data$item332[data$item332 == 13] <- 3
data$item332[data$item332 == 14] <- 4
data$item332[data$item332 == 15] <- 5

data$item333[data$item333 == 11] <- 1
data$item333[data$item333 == 12] <- 2
data$item333[data$item333 == 13] <- 3
data$item333[data$item333 == 14] <- 4
data$item333[data$item333 == 15] <- 5

data$item334[data$item334 == 11] <- 1
data$item334[data$item334 == 12] <- 2
data$item334[data$item334 == 13] <- 3
data$item334[data$item334 == 14] <- 4
data$item334[data$item334 == 15] <- 5

data$item335[data$item335 == 11] <- 1
data$item335[data$item335 == 12] <- 2
data$item335[data$item335 == 13] <- 3
data$item335[data$item335 == 14] <- 4
data$item335[data$item335 == 15] <- 5

data$item336[data$item336 == 11] <- 1
data$item336[data$item336 == 12] <- 2
data$item336[data$item336 == 13] <- 3
data$item336[data$item336 == 14] <- 4
data$item336[data$item336 == 15] <- 5

data$item337[data$item337 == 18] <- 1
data$item337[data$item337 == 19] <- 2
data$item337[data$item337 == 20] <- 3
data$item337[data$item337 == 21] <- 4
data$item337[data$item337 == 22] <- 5

data$item338[data$item338 == 11] <- 1
data$item338[data$item338 == 12] <- 2
data$item338[data$item338 == 13] <- 3
data$item338[data$item338 == 14] <- 4
data$item338[data$item338 == 15] <- 5

data$item339[data$item339 == 11] <- 1
data$item339[data$item339 == 12] <- 2
data$item339[data$item339 == 13] <- 3
data$item339[data$item339 == 14] <- 4
data$item339[data$item339 == 15] <- 5

data$item340[data$item340 == 11] <- 1
data$item340[data$item340 == 12] <- 2
data$item340[data$item340 == 13] <- 3
data$item340[data$item340 == 14] <- 4
data$item340[data$item340 == 15] <- 5

data$item341[data$item341 == 11] <- 1
data$item341[data$item341 == 12] <- 2
data$item341[data$item341 == 13] <- 3
data$item341[data$item341 == 14] <- 4
data$item341[data$item341 == 15] <- 5

data$item342[data$item342 == 11] <- 1
data$item342[data$item342 == 12] <- 2
data$item342[data$item342 == 13] <- 3
data$item342[data$item342 == 14] <- 4
data$item342[data$item342 == 15] <- 5

data$item343[data$item343 == 11] <- 1
data$item343[data$item343 == 12] <- 2
data$item343[data$item343 == 13] <- 3
data$item343[data$item343 == 14] <- 4
data$item343[data$item343 == 15] <- 5

data$item344[data$item344 == 11] <- 1
data$item344[data$item344 == 12] <- 2
data$item344[data$item344 == 13] <- 3
data$item344[data$item344 == 14] <- 4
data$item344[data$item344 == 15] <- 5

data$item345[data$item345 == 11] <- 1
data$item345[data$item345 == 12] <- 2
data$item345[data$item345 == 13] <- 3
data$item345[data$item345 == 14] <- 4
data$item345[data$item345 == 15] <- 5

data$item346[data$item346 == 11] <- 1
data$item346[data$item346 == 12] <- 2
data$item346[data$item346 == 13] <- 3
data$item346[data$item346 == 14] <- 4
data$item346[data$item346 == 15] <- 5

data$item347[data$item347 == 11] <- 1
data$item347[data$item347 == 12] <- 2
data$item347[data$item347 == 13] <- 3
data$item347[data$item347 == 14] <- 4
data$item347[data$item347 == 15] <- 5

data$item348[data$item348 == 11] <- 1
data$item348[data$item348 == 12] <- 2
data$item348[data$item348 == 13] <- 3
data$item348[data$item348 == 14] <- 4
data$item348[data$item348 == 15] <- 5

data$item349[data$item349 == 11] <- 1
data$item349[data$item349 == 12] <- 2
data$item349[data$item349 == 13] <- 3
data$item349[data$item349 == 14] <- 4
data$item349[data$item349 == 15] <- 5

data$item350[data$item350 == 11] <- 1
data$item350[data$item350 == 12] <- 2
data$item350[data$item350 == 13] <- 3
data$item350[data$item350 == 14] <- 4
data$item350[data$item350 == 15] <- 5

data$item351[data$item351 == 11] <- 1
data$item351[data$item351 == 12] <- 2
data$item351[data$item351 == 13] <- 3
data$item351[data$item351 == 14] <- 4
data$item351[data$item351 == 15] <- 5

data$item352[data$item352 == 11] <- 1
data$item352[data$item352 == 12] <- 2
data$item352[data$item352 == 13] <- 3
data$item352[data$item352 == 14] <- 4
data$item352[data$item352 == 15] <- 5

data$item353[data$item353 == 11] <- 1
data$item353[data$item353 == 12] <- 2
data$item353[data$item353 == 13] <- 3
data$item353[data$item353 == 14] <- 4
data$item353[data$item353 == 15] <- 5

data$item354[data$item354 == 21] <- 1
data$item354[data$item354 == 22] <- 2
data$item354[data$item354 == 23] <- 3
data$item354[data$item354 == 24] <- 4
data$item354[data$item354 == 25] <- 5

data$item355[data$item355 == 11] <- 1
data$item355[data$item355 == 12] <- 2
data$item355[data$item355 == 13] <- 3
data$item355[data$item355 == 14] <- 4
data$item355[data$item355 == 15] <- 5

data$item356[data$item356 == 11] <- 1
data$item356[data$item356 == 12] <- 2
data$item356[data$item356 == 13] <- 3
data$item356[data$item356 == 14] <- 4
data$item356[data$item356 == 15] <- 5

data$item357[data$item357 == 11] <- 1
data$item357[data$item357 == 12] <- 2
data$item357[data$item357 == 13] <- 3
data$item357[data$item357 == 14] <- 4
data$item357[data$item357 == 15] <- 5

data$item358[data$item358 == 11] <- 1
data$item358[data$item358 == 12] <- 2
data$item358[data$item358 == 13] <- 3
data$item358[data$item358 == 14] <- 4
data$item358[data$item358 == 15] <- 5

data$item359[data$item359 == 11] <- 1
data$item359[data$item359 == 12] <- 2
data$item359[data$item359 == 13] <- 3
data$item359[data$item359 == 14] <- 4
data$item359[data$item359 == 15] <- 5

data$item360[data$item360 == 11] <- 1
data$item360[data$item360 == 12] <- 2
data$item360[data$item360 == 13] <- 3
data$item360[data$item360 == 14] <- 4
data$item360[data$item360 == 15] <- 5

data$item361[data$item361 == 11] <- 1
data$item361[data$item361 == 12] <- 2
data$item361[data$item361 == 13] <- 3
data$item361[data$item361 == 14] <- 4
data$item361[data$item361 == 15] <- 5

data$item362[data$item362 == 11] <- 1
data$item362[data$item362 == 12] <- 2
data$item362[data$item362 == 13] <- 3
data$item362[data$item362 == 14] <- 4
data$item362[data$item362 == 15] <- 5

data$item363[data$item363 == 11] <- 1
data$item363[data$item363 == 12] <- 2
data$item363[data$item363 == 13] <- 3
data$item363[data$item363 == 14] <- 4
data$item363[data$item363 == 15] <- 5

data$item364[data$item364 == 11] <- 1
data$item364[data$item364 == 12] <- 2
data$item364[data$item364 == 13] <- 3
data$item364[data$item364 == 14] <- 4
data$item364[data$item364 == 15] <- 5

data$item365[data$item365 == 11] <- 1
data$item365[data$item365 == 12] <- 2
data$item365[data$item365 == 13] <- 3
data$item365[data$item365 == 14] <- 4
data$item365[data$item365 == 15] <- 5

data$item366[data$item366 == 11] <- 1
data$item366[data$item366 == 12] <- 2
data$item366[data$item366 == 13] <- 3
data$item366[data$item366 == 14] <- 4
data$item366[data$item366 == 15] <- 5

data$item367[data$item367 == 11] <- 1
data$item367[data$item367 == 12] <- 2
data$item367[data$item367 == 13] <- 3
data$item367[data$item367 == 14] <- 4
data$item367[data$item367 == 15] <- 5

data$item368[data$item368 == 11] <- 1
data$item368[data$item368 == 12] <- 2
data$item368[data$item368 == 13] <- 3
data$item368[data$item368 == 14] <- 4
data$item368[data$item368 == 15] <- 5

data$item369[data$item369 == 11] <- 1
data$item369[data$item369 == 12] <- 2
data$item369[data$item369 == 13] <- 3
data$item369[data$item369 == 14] <- 4
data$item369[data$item369 == 15] <- 5

data$item370[data$item370 == 11] <- 1
data$item370[data$item370 == 12] <- 2
data$item370[data$item370 == 13] <- 3
data$item370[data$item370 == 14] <- 4
data$item370[data$item370 == 15] <- 5

data$item371[data$item371 == 11] <- 1
data$item371[data$item371 == 12] <- 2
data$item371[data$item371 == 13] <- 3
data$item371[data$item371 == 14] <- 4
data$item371[data$item371 == 15] <- 5

data$item372[data$item372 == 11] <- 1
data$item372[data$item372 == 12] <- 2
data$item372[data$item372 == 13] <- 3
data$item372[data$item372 == 14] <- 4
data$item372[data$item372 == 15] <- 5

write.csv(data, "SIOPcleaned.csv")

## saving above recodes for SIOP poster .Rmds - 2/8/22
