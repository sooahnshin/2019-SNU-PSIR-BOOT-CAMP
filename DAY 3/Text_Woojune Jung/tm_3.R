## 본 코드는 백영민, "R를 이용한 텍스트 마이닝"에서 발췌하여 일부 수정한 것입니다.
## 참고로, 사회확학을 전공하는 자가 텍스트 마이닝 등 관련 분석을 할 때
## 해당 저서가 매우 유용할 수 있습니다.

## 품사 분석을 위해 KoNLP 패키지를 로드하고 SimplePos22 함수에 petition의 첫 번째 문서를 전달함.
library(KoNLP)
pos22 <- SimplePos22(unlist(petition.rm.punct.re.whitespace.noun[[1]]))

## 품사 분석 결과를 확인해 봄
pos22

## corpus에서 보통명사(NC)를 추출할 수 있는 사용자 함수를 정의
nc.fun <- function(corpus) {
  pos22 <- SimplePos22(corpus)
  corpus.new <- corpus
  length.corpus <- length(pos22)
  location <- regexpr(pattern = '/NC+', pos22)
  for(i in 1:length.corpus){
    location <- regexpr(pattern = '/NC', pos22[i])
    corpus.new[i] <- substr(corpus[i], 1, location[[1]][1]-1)
    corpus.new[i] <- gsub("[[:alnum:]]/[[:upper:]]{1,}\\+","", corpus.new[i])
  }
  corpus.new <- unlist(corpus.new)
  corpus.new <- corpus.new[nchar(corpus.new)>0]
  corpus.new
}

## 품사 분석을 할 corpus 중 일부를 위에서 정의된 함수에 전달하여 결과 확인
nc.fun(petition.rm.punct.re.whitespace.noun[[1]]$content)
nc.fun(petition.rm.punct.re.whitespace.noun[[2]]$content)



