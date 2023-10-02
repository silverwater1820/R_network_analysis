#파일 위치 설정
setwd("C:/.1.EWHA WOMAS UNIVERSITY/4-3/캡스톤/riss_data") 

#설치되어있는 경우에는 이부분 생략
#install.packages("pacman")
pacman::p_load('tidymodels','tidytext','NLP4kec' ,'stringr','magrittr','tm', 'network','GGally', 'sna', 'RColorBrewer')

riss_geder_conflict_title <- read.csv('gender_conflict_title.csv')
RGCT_txt <- as.data.frame(riss_geder_conflict_title, stringsAsFactors = F)

# 텍스트의 중복된 행 제거
RGCT_txt <- unique(RGCT_txt)

# 텍스트의 공백을 제거. NLP4kec 형태소 분석기가 띄어쓰기를 구분
RGCT_txt <- sapply(RGCT_txt,str_remove_all,'\\s+')
RGCT_txt <- as.data.frame(RGCT_txt,stringsAsFactors = FALSE)
colnames(RGCT_txt) <- c('content')

#열 이름을 content와 id로 설정
generateIDs <- function(obj, index = 'id') {
    # 객체의 종류에 따라 길이 계산
    if (obj %>% class() == 'data.frame') {
        n <- nrow(x = obj)
    } else {
        n <- length(x = obj)
    }
    # id 생성 
    id <- str_c(
        index, 
        str_pad(
            string = 1:n, 
            width = ceiling(x = log10(x = n)), 
            side = 'left', 
            pad = '0') )
    # 결과 반환
    return(id)
}  
RGCT_txt$id <- generateIDs(obj = RGCT_txt, index = 'doc')
#열 이름을 content와 id로 설정 
names(RGCT_txt) <- c("content","id")

#형태소 분석(NLP4kec 패키지)
Parsed_RGCT <- r_parser_r(RGCT_txt$content,language = "ko")
Parsed_RGCT <- Parsed_RGCT[Parsed_RGCT != ""]

#corpus 생성
corp <- VCorpus(VectorSource(Parsed_RGCT))
#특수문자 제거
corp <-  tm_map(corp, removePunctuation)

#dtmTfIdf
dtmTfIdf <- DocumentTermMatrix( x = corp, control = list( removeNumbers = TRUE, wordLengths = c(2, Inf), weighting = function(x) weightTfIdf(x, normalize = TRUE) ))  

# dtmTfIdf 차원 축소
dtmTfIdf <- removeSparseTerms(x =  dtmTfIdf, sparse = as.numeric(x = 0.99))

#corTerms
dtmTfIdf %>% as.matrix() %>% cor() -> corTerms

# 키워드 유무를 확인합니다.
checkCorTerms <- function(x,n = 10, keyword) {
    
    # 키워드 유무를 확인합니다.
    x %>%
        colnames() %>%
        str_subset(pattern = keyword) %>%
        print()
    
    # 연관 키워드가 있는 컬럼의 전체 단어를 한꺼번에 출력합니다.
    corRef <- data.frame()
    
    # 상관계수 높은 순서로 정렬합니다.
    corRef <- x[ , keyword] %>%
        sort(decreasing = TRUE) %>%
        data.frame() %>%
        set_colnames(c('corr'))
    
    # 미리보기 합니다.
    head(x = corRef, n = n + 1)
}

checkCorTerms(corTerms, 10 ,'갈등')

#corTerms to network obj
netTerms <- network(x = corTerms, directed = FALSE)

#상관행렬 크기 조정
corTerms[corTerms <= 0.1] <- 0
netTerms <- network(x = corTerms, directed = FALSE)
plot(netTerms, vertex.cex = 1)

#매개중심성 계산
btnTerms <- betweenness(netTerms) 
btnTerms[1:10]

#매개중심성 표현
netTerms %v% 'mode' <-
    ifelse(
        test = btnTerms >= quantile(x = btnTerms, probs = 0.90, na.rm = TRUE), 
        yes = 'Top', 
        no = 'Rest')
nodeColors <- c('Top' = 'gold', 'Rest' = 'lightgrey')
set.edge.value(netTerms, attrname = 'edgeSize', value = corTerms * 3)
ggnet2(
    net = netTerms,
    mode = 'fruchtermanreingold',
    layout.par = list(cell.jitter = 0.001),
    size.min = 15,
    label = TRUE,
    label.size = 3,
    node.color = 'mode',
    palette = nodeColors,
    node.size = sna::degree(dat = netTerms),
    edge.size = 'edgeSize',
    family = 'mono')+
    labs(title = "매개중심성 반영한 단어-네트워크맵")
)


