library(readr)
library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)
library(topicmodels)
manifest = read.csv("manifest.csv")
view(manifest)

manifest$manifest <- as.character(manifest$manifest)
co_manifest <- corpus(manifest, docid_field = "party", text_field = "manifest")
summary(co_manifest)

to_manifest <- tokens(co_manifest)
summary(to_manifest)

d_manifest <- dfm(to_manifest)
d_manifest <- d_manifest %>% dfm_remove(pattern = "^[ぁ-ん]+$", valuetype = "regex",min_nchar=2)

topfeatures(d_manifest,80)
textplot_wordcloud(d_manifest, min_count = 50, color = RColorBrewer::brewer.pal(5, "Dark2"))
textplot_wordcloud(d_manifest, comparison = T, min_count = 50, color = RColorBrewer::brewer.pal(5, "Dark2"))

topic_manifest <- LDA(convert(d_manifest,to="topicmodels"),k=7)
get_terms(topic_manifest,10)

wf_manifest <-  textmodel_wordfish(d_manifest, dir = c(3,1))
summary(wf_manifest)
textplot_scale1d(wf_manifest)
textplot_scale1d(wf_manifest, margin="features")

# 考察
# トピックモデルより、日本維新の会は安倍政権への批判や年金問題への政策などを中心に公約に掲げていると推定できる。
# 共起ネットワーク分析より、各政党のマニフェストにおいて「教育、医療、防災」などの語と「支援、推進」の語が比較的頻繁にあわせて用いられていることがわかる。
# wordfishモデルより、出現単語によって政党を位置付けると、自民と日本維新の会、共産と社会民主、国民民主が似通った公約を掲げていると考えられる。また、立民、国民民主は外交、安全保障、国際協調を重視していると考えられる。
