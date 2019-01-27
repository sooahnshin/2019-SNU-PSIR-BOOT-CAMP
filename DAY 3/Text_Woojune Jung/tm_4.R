## 본 코드는 백영민, "R를 이용한 텍스트 마이닝"에서 발췌하여 일부 수정한 것입니다.
## 참고로, 사회확학을 전공하는 자가 텍스트 마이닝 등 관련 분석을 할 때
## 해당 저서가 매우 유용할 수 있습니다.

## dtm을 이용하여 단어의 출현 빈도를 구함
word.freq <- apply(dtm[, ], 2, sum)

## 빈도가 높은 것부터 낮은 것으로 정렬
decreasing.word.freq <- sort(word.freq, decreasing = TRUE)
decreasing.word.freq[1:20]

# 누적 빈도 구하기
cum.sum.word.freq <- cumsum(decreasing.word.freq)
cum.sum.word.freq[1:20]

# 전체 합이 1이 되도록 출현 빈도를 비율로 변형
prop.word.freq <- cum.sum.word.freq/cum.sum.word.freq[length(cum.sum.word.freq)]
prop.word.freq

