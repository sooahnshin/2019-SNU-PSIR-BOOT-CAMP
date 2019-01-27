## 본 코드는 백영민, "R를 이용한 텍스트 마이닝"에서 발췌하여 일부 수정한 것입니다.
## 참고로, 사회확학을 전공하는 자가 텍스트 마이닝 등 관련 분석을 할 때
## 해당 저서가 매우 유용할 수 있습니다.

## 누적 빈도 그래프 그리기(%)
plot(1:length(word.freq), prop.word.freq, type = 'l',
     xlab = 'Order of Word Frequency', ylab = 'Cumulative Proportion',
     main = "", axes = FALSE)
axis(1, at = round(67.8*(0:10)), labels = paste(10*(0:10), "%", sep = ""))
axis(2, at = 0.200*(0:5), labels = paste(20*(0:5), "%", sep=""))
for (i in 1:9) {
  text(6.8*10*i, 0.05 + prop.word.freq[6.8*10*i],
  labels = paste(round(100*prop.word.freq[6.8*10*i]), "%", sep = ""))
  points(6.8*10*i, prop.word.freq[6.8*10*i], pch = 19)
}

## 워드클라우드 그리기
library(wordcloud)
wordcloud(names(word.freq), freq = word.freq, scale = c(4, 0.2),
          rot.per = 0.0, min.freq = 5, random.order = FALSE)
