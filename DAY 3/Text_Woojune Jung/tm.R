## 본 코드는 백영민, "R를 이용한 텍스트 마이닝"에서 발췌하여 일부 수정한 것입니다.
## 참고로, 사회확학을 전공하는 자가 텍스트 마이닝 등 관련 분석을 할 때
## 해당 저서가 매우 유용할 수 있습니다.

## corpus 작업을 위한 라이브러리인 tm 패키지 로딩
library(tm)

## 지정된 경로에 있는 여러 txt 파일을 VCorpus 함수로 corpus로 변형
petition <- VCorpus(DirSource("~/R/petition/"))

## 전체 corpus 확인
petition

## corpus 중 일부 확인
petition[[16]]$content

## 형태소 분석(명사 추출)을 위한 라이브러리인 
## KoNLP 패키지 로딩
library(KoNLP)

fun.corpus.noun <- function(corpus){
  nouns <- paste(extractNoun(corpus), collapse = '')
  nouns
}

petition.noun <- petition

for (i in 1:length(petition)) {
  petition.noun[[i]]$content <- fun.corpus.noun(petition[[i]]$content)
}

## 분석 대상이 되는 텍스트 데이터에서 지정된 표현을 추출하기 위해 
## stringr 패키지를 로딩하고 str_extract_all() 함수를 사용
library(stringr)
table(unlist(lapply(petition.noun, function(x) str_extract_all(x, boundary("word")))))                                            

## dtm을 만들기 위해 tm 패키지의 DocumentTermMatrix 함수 활용
dtm <- tm::DocumentTermMatrix(petition.noun)
dtm

rownames(dtm[,])

colnames(dtm[,])

inspect(dtm[1:3, 50:55])

dtm.tfidf <- DocumentTermMatrix(petition.noun, 
                control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

dtm.tfidf