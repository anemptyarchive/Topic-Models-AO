
# 各トピックの出現確率の上位語 ---------------------------------------------------------------------

# 文書インデックス(d)
d_index <- list.files(file_path) %>% 
  str_remove_all(".txt")


## 語彙インデックス(v)
# 指定した出現回数以上の単語の行番号
num <- res_mecab %>% 
  select(-c(TERM, POS1, POS2)) %>% 
  apply(1, sum) >= Freq # 抽出する総出現回数を指定する

v_index <- res_mecab[num, ] %>% 
  filter(grepl(PoS_1, POS1)) %>% # 抽出する品詞を指定する|^動詞
  filter(grepl(PoS_2, POS2)) %>% 
  filter(!grepl(stop_words, TERM)) %>% 
  .[, 1]  # 単語の列を抽出する

# データフレームを作成
phi_df_wide2 <- cbind(as.data.frame(t(phi_kv)), 
                      v_index) %>% 
  as.data.frame()
colnames(phi_df_wide2) <- c(paste0("topic", 1:K), "word") # key用の行名を付与

# データフレームの整形
phi_df_long2 <- data.frame()
for(i in 1:K) {
  tmp_df <- phi_df_wide2 %>% 
    select(paste0("topic", i), word) %>% 
    arrange(-.[, 1]) %>%  # 降順に並べ替え
    head(20) %>%          # 任意で指定した上位n語を抽出
    gather(key = "topic", value = "prob", -word) # long型に変換
  
  phi_df_long2 <- rbind(phi_df_long2, tmp_df)
}


# 描画
ggplot(phi_df_long2, aes(x = reorder(x = word, X = prob), y = prob, fill = word)) + 
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  coord_flip() +                                    # グラフの向き
  facet_wrap( ~ topic, scales = "free") +           # グラフの分割
  theme(legend.position = "none") +                 # 凡例
  labs(title = "Mixture of Unigram Models")     # タイトル


