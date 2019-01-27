## 본 코드는 백영민, "R를 이용한 텍스트 마이닝"에서 발췌하여 일부 수정한 것입니다.
## 참고로, 사회확학을 전공하는 자가 텍스트 마이닝 등 관련 분석을 할 때
## 해당 저서가 매우 유용할 수 있습니다.

## tm.R의 경우 데이터 정제가 전혀 이루어지지 않은 경우여서, 데이터 정제 코드를 추가한 tm_2.R 버전 제공

## corpus 작업을 위한 라이브러리인 tm 패키지 로딩
library(tm)

## 지정된 경로에 있는 여러 txt 파일을 VCorpus 함수로 corpus로 변형
petition <- VCorpus(DirSource("~/R/petition/"))

## 전체 corpus 확인
petition

## corpus 중 일부 확인
petition[[16]]$content

## corpus 중 확인한 문서(petition[[16]]$content)의 구조가 여러 개의 문자열로 구성되어 있으므로, 이를 하나의 문자열로 변형
for (i in 1:length(petition)) {
petition[[i]]$content <- paste(unlist(strsplit(petition[[i]]$content, " ")), collapse = " ")
}

## 변형된 문서 확인
petition[[16]]$content

## 의미없는 숫자들이 상당수 있으므로, tm 패키지의 tm_map 함수에서 removeNumbers 인자를 사용하여 숫자를 제거
petition.rm.num <- tm_map(petition, removeNumbers)
petition[[16]]$content

## 특수문자 제거
petition.rm.punct <- tm_map(petition.rm.num, removePunctuation)
petition.rm.punct[[16]]$content

## 추가 문자 제거 - 대소 영문자, 괄호, 작은 따옴표, 가운데 점, 슬래시, 가운데 줄 또는 하이픈, 물음표 등

## corpus 전체에 문자 제거를 적용하기 위한 사용자 함수 정의
myrmpuncfun <- function(myobject, oldexp, newexp) {
  newobject <- tm_map(myobject, content_transformer(function(x, pattern) gsub(pattern, newexp, x)), oldexp)
  newobject
}

petition.rm.punct[[16]]$content

library(stringr)

## 영문 소문자 제거
petition.rm.punct.re <- myrmpuncfun(petition.rm.punct, "[[:lower:]]", "")

## 영문 대문자 제거
petition.rm.punct.re <- myrmpuncfun(petition.rm.punct, "[[:upper:]]", "")

## 왼쪼 괄호(() 제거
petition.rm.punct.re <- myrmpuncfun(petition.rm.punct.re, "\\(","")

##오른쪼 괄호()) 제거
petition.rm.punct.re <- myrmpuncfun(petition.rm.punct.re, "\\)","")

## 작은 따옴표(') 제거
petition.rm.punct.re <- myrmpuncfun(petition.rm.punct.re, "'","")
petition.rm.punct.re <- myrmpuncfun(petition.rm.punct.re, "‘","")
petition.rm.punct.re <- myrmpuncfun(petition.rm.punct.re, "’","")

## 말줄임표(..) 제거
petition.rm.punct.re <- myrmpuncfun(petition.rm.punct.re, "\\..","")

## 여러 하이픈 제거
petition.rm.punct.re <- myrmpuncfun(petition.rm.punct.re, "-", "")
petition.rm.punct.re <- myrmpuncfun(petition.rm.punct.re, "ㅡ", "")

## 물음표 제거
petition.rm.punct.re <- myrmpuncfun(petition.rm.punct.re, "\\?","")


## 공란(whitespace)가 두 개 이상인 경우 하나로 축소
petition.rm.punct.re.whitespace <- tm_map(petition.rm.punct.re, stripWhitespace)

## 형태소 분석(명사 추출)을 위한 라이브러리인 KoNLP 패키지 로딩
library(KoNLP)

fun.corpus.noun <- function(corpus){
  nouns <- paste(extractNoun(corpus), collapse = " ")
  nouns
}

petition.rm.punct.re.whitespace.noun <- petition.rm.punct.re.whitespace

petition.rm.punct.re.whitespace.noun[[1]]$content

for (i in 1:length(petition.rm.punct.re.whitespace)) {
  petition.rm.punct.re.whitespace.noun[[i]]$content <- fun.corpus.noun(petition.rm.punct.re.whitespace[[i]]$content)
}

## 분석 대상이 되는 텍스트 데이터에서 지정된 표현을 추출하여 표로 작성
table(unlist(lapply(petition.rm.punct.re.whitespace.noun, function(x) str_extract_all(x, boundary("word")))))                                            

## dtm 만들기
dtm <- tm::DocumentTermMatrix(petition.rm.punct.re.whitespace.noun)

## dtm 확인
dtm
rownames(dtm[,])
colnames(dtm[,])

## dtm 중 일부 확인
inspect(dtm[1:3, 50:55])

## tfidf 만들기
dtm.tfidf <- DocumentTermMatrix(petition.rm.punct.re.whitespace.noun, 
                                control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

## tfidf 확인
dtm.tfidf
inspect(dtm.tfidf[1:3, 50:55])

