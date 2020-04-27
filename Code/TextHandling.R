
# Text Handling -----------------------------------------------------------

# 利用パッケージ
library(RMeCab)
library(tidyverse)

# テキストファイルの保存先を指定
file_path <- "text_data/lyrics/unamas"

# 形態素解析
res_mecab <- docDF(file_path, type = 1)


## 抽出する単語の指定
# 品詞(大分類)を指定
PoS_1 <- "名詞|形容詞|形容動詞|^動詞|副詞|連体詞"

# 品詞(細分類)を指定
PoS_2 <- "一般|^自立"

# 最低出現頻度を指定
Freq <- 3

# 抽出しない単語を指定
stop_words <- "[a-zA-Z]"


# 各文書における各語彙の出現回数(N_dv)
N_dv <- res_mecab %>% 
  filter(grepl(PoS_1, POS1)) %>%        # 指定した品詞(大分類)を取り出す
  filter(grepl(PoS_2, POS2)) %>%        # 指定した品詞(細分類)を取り出す
  filter(!grepl(stop_words, TERM)) %>%  # ストップワードを除く
  select(-c(TERM, POS1, POS2)) %>%      # 数値列のみを残す
  filter(apply(., 1, sum) >= Freq) %>%  # 指定した頻度以上の語彙を取り出す
  t()                                   # 転置

# 確認用の行列名
dimnames(N_dv) <- list(paste0("d=", 1:nrow(N_dv)), # 行名
                       paste0("v=", 1:ncol(N_dv))) # 列名

# 各文書の単語数
N_d <- apply(N_dv, 1, sum) # 行方向に和をとる

# 各語彙の単語数
N_v <- apply(N_dv, 2, sum) # 列方向に和をとる

# 文書数(D)
D <- nrow(N_dv)

# 総語彙数(V)
V <- ncol(N_dv)


# 語彙インデックス
tmp_v <- res_mecab %>% 
  filter(grepl(PoS_1, POS1)) %>%        # 指定した品詞(大分類)を取り出す
  filter(grepl(PoS_2, POS2)) %>%        # 指定した品詞(細分類)を取り出す
  filter(!grepl(stop_words, TERM)) %>%  # ストップワードを除く
  select(-c(TERM, POS1, POS2)) %>%      # 数値列のみを残す
  apply(., 1, sum) >= Freq # 指定した頻度以上の語彙を取り出す
v_index <- res_mecab %>% 
  filter(grepl(PoS_1, POS1)) %>%        # 指定した品詞(大分類)を取り出す
  filter(grepl(PoS_2, POS2)) %>%        # 指定した品詞(細分類)を取り出す
  filter(!grepl(stop_words, TERM)) %>%  # ストップワードを除く
  select(TERM) %>% 
  filter(tmp_v)


