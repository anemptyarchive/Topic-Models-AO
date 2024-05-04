
# ガンマ関数とディガンマ関数の関係 -------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# パッケージ名の省略用
library(ggplot2)


# 対数ガンマ関数とディガンマ関数の関係 ------------------------------------------------------

# x軸の範囲を指定
x_vec <- seq(from = 0, to = 10, length.out = 5001)
x_vec[x_vec == 0] <- 1e-10 # (x = 0のときInfになり意図しない作図処理になるのを回避)

# 関数の曲線の座標を作成
fnc_curve_df <- tibble::tibble(
  x = x_vec |> 
    rep(times = 2), # 曲線の数
  f_x = c(
    lgamma(x_vec), 
    digamma(x_vec)
  ), 
  fnc = c("log_gamma_x", "digamma_x") |> 
    rep(each = length(x_vec))
)


# フレーム数を指定
frame_num <- 300

# 接点の座標を作成
x1_vals <- seq(from = 0, to = max(x_vec), length.out = frame_num)
x1_vals[x1_vals == 0] <- 1e-10
y1_vals <- lgamma(x1_vals)


# 接線の傾き・切片を作成
a1_vals <- digamma(x1_vals)
anim_tangent_df <- tibble::tibble(
  frame_i = 1:frame_num, 
  a = a1_vals, 
  b = y1_vals - a1_vals * x1_vals
)


# 関数の点の座標を作成
anim_fnc_point_df <- tibble::tibble(
  frame_i = 1:frame_num |> 
    rep(times = 2), # 点の数
  x = x1_vals |> 
    rep(times = 2), 
  f_x = c(
    digamma(x1_vals), 
    lgamma(x1_vals)
  ), 
  fnc = c("digamma_x", "log_gamma_x") |> 
    rep(each = frame_num)
)

# 関数の線分の座標を作成
anim_fnc_seg_df <- tibble::tibble(
  frame_i = 1:frame_num |> 
    rep(times = 3), # 線分の数
  x_from = c(
    x1_vals, 
    x1_vals + 1, 
    x1_vals
  ), 
  y_from = c(
    rep(0, times = frame_num), 
    y1_vals, 
    rep(0, times = frame_num)
  ), 
  x_to = x_from, 
  y_to = c(
    a1_vals, 
    y1_vals + a1_vals, 
    y1_vals
  ), 
  fnc = c("digamma_x", "digamma_x", "log_gamma_x") |> 
    rep(each = frame_num), 
  w = c(2, 1, 1) |> # 線幅を指定
    rep(each = frame_num)
)


# 補助線分の座標を作成
anim_auxil_seg_df <- tibble::tibble(
  frame_i = 1:frame_num, 
  x_from = x1_vals, 
  y_from = y1_vals, 
  x_to   = x1_vals + 1, 
  y_to   = y1_vals
)

# 目盛線の座標を作成
anim_tick_x_df <- tibble::tibble(
  frame_i = 1:frame_num |> 
    rep(times = 2), # 線の数
  x = c(
    x1_vals, 
    x1_vals + 1
  ), 
  label = c("x[1]", "x[1] + 1") |> 
    rep(each = frame_num)
)
anim_tick_y_df <- tibble::tibble(
  frame_i = 1:frame_num |> 
    rep(times = 2), # 線の数
  y = c(
    digamma(x1_vals), 
    lgamma(x1_vals)
  ), 
  label = c("Psi(x[1])", "log~Gamma(x[1])") |> 
    rep(each = frame_num)
)


# 軸サイズを指定
axis_y_size <- 10

# ラベル用の文字列を作成
def_label <- "Psi(x) == frac(d ~ log~Gamma(x), d ~ x)"

# 配色の共通化用のカラーコードを作成
color_vec <- scales::hue_pal()(n = 4)

# ガンマ関数とディガンマ関数のアニメーションを作図
anim <- ggplot() + 
  geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                             xend = c(Inf, 0), yend = c(0, Inf)), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
  geom_vline(data = anim_tick_x_df,
             mapping = aes(xintercept = x), 
             linetype ="dotted") + # x座標の目盛線
  geom_hline(data = anim_tick_y_df,
             mapping = aes(yintercept = y), 
             linetype ="dotted") + # y座標の目盛線
  geom_label(data = anim_tick_x_df, 
             mapping = aes(x = x, y = -Inf, label = label), parse = TRUE, 
             fill = "gray92", label.size = 0, label.padding = unit(0, units = "points"), 
             hjust = 0.5, vjust = 0, size = 3,) + # 変数ラベル
  geom_label(data = anim_tick_y_df, 
             mapping = aes(x = -Inf, y = y, label = label), parse = TRUE, 
             fill = "gray92", label.size = 0, label.padding = unit(0, units = "points"), 
            hjust = 0, vjust = 0.5, size = 3) + # 関数ラベル
  geom_line(data = fnc_curve_df, 
            mapping = aes(x = x, y = f_x, color = fnc)) + # 関数曲線
  geom_point(data = anim_fnc_point_df, 
             mapping = aes(x = x, y = f_x, color = fnc), 
             size = 3) + # 関数点
  geom_segment(data = anim_auxil_seg_df,
               mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to)) + # 補助線分
  geom_segment(data = anim_fnc_seg_df,
               mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
                             color = fnc, linewidth = w)) + # 関数線分
  geom_abline(data = anim_tangent_df, 
              mapping = aes(slope = a, intercept = b)) + # 接線
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
  scale_color_manual(breaks = c("digamma_x", "log_gamma_x"), 
                     values = color_vec[c(1, 2)], 
                     labels = c(expression(Psi(x)), expression(log~Gamma(x))), 
                     name = "function") + # 凡例の表示用
  scale_linewidth_identity() +
  theme(legend.text.align = 0) + # 図の体裁
  coord_cartesian(xlim = c(0, max(x_vec)), 
                  ylim = c(-axis_y_size, axis_y_size)) + # 描画領域の設定
  labs(title = "gamma function and digamma function", 
       subtitle = parse(text = def_label), 
       x = expression(x), 
       y = expression(f(x)))

# 動画を作成
gganimate::animate(
  plot = anim, nframes = frame_num, fps = 30, width = 8, height = 6, units = "in", res = 200, 
  renderer = gganimate::av_renderer(file = "figure/ch2/digamma.mp4")
)


# 対数ガンマ関数上の2点とディガンマ関数の関係 --------------------------------------------------

# x軸の範囲を指定
x_vec <- seq(from = 0, to = 10, length.out = 5001)
x_vec[x_vec == 0] <- 1e-10 # (x = 0のときInfになり意図しない作図処理になるのを回避)

# 関数の曲線の座標を作成
fnc_curve_df <- tibble::tibble(
  x   = x_vec, 
  f_x = lgamma(x)
)


# フレーム数を指定
frame_num <- 210

# 接点の座標を指定
x1 <- 3
y1 <- lgamma(x1)

# 曲線上の点の座標を作成:(x2 > x1)
x2_vals <- seq(from = max(x_vec), to = x1, length.out = frame_num)
y2_vals <- lgamma(x2_vals)


# 接線の傾き・切片を計算
a1 <- digamma(x1)
b1 <- y1 - a1 * x1
a2_vals <- digamma(x2_vals)

# 2点を結ぶ直線の傾き・切片を計算
a_vals <- (y2_vals - y1) / (x2_vals - x1)
b_vals <- y1 - a_vals * x1

# 直線の傾き・切片を格納
anim_oblique_df <- tibble::tibble(
  frame_i = 1:frame_num |> 
    rep(times = 2), # 直線の数
  a = c(
    rep(a1, times = frame_num), 
    a_vals
  ), 
  b = c(
    rep(b1, times = frame_num), 
    b_vals
  ), 
  fnc = c("digamma_x", "target") |> 
    rep(each = frame_num)
)


# 曲線上の点の座標を作成
anim_fnc_point_df <- tibble::tibble(
  frame_i = 1:frame_num |> 
    rep(times = 2), #  点の数
  x = c(
    rep(x1, times = frame_num), 
    x2_vals
  ), 
  f_x = c(
    rep(y1, times = frame_num), 
    y2_vals
  )
)

# 関数の線分の座標を作成
anim_fnc_seg_df <- tibble::tibble(
  frame_i = 1:frame_num |> 
    rep(times = 3), # 線分点の数
  x_from = c(
    rep(x1, times = frame_num) + 1, 
    rep(x1, times = frame_num), 
    rep(x1, times = frame_num) + 1
  ), 
  y_from = c(
    rep(y1, times = frame_num), 
    rep(0, times = frame_num), 
    rep(y1, times = frame_num)
  ), 
  x_to = x_from, 
  y_to = c(
    rep(y1+a1, times = frame_num), 
    rep(y1, times = frame_num), 
    y1 + a_vals
  ), 
  fnc = c("digamma_x", "log_gamma_x", "target") |> 
    rep(each = frame_num), 
  w = c(2, 1, 1) |> # 線幅を指定
    rep(each = frame_num)
)


# 補助線分の座標を作成
anim_auxil_seg_df <- tibble::tibble(
  frame_i = 1:frame_num |> 
    rep(times = 3), #  線分の数
  x_from = c(
    rep(x1, times = frame_num), 
    rep(x1, times = frame_num), 
    x2_vals
  ), 
  y_from = c(
    rep(y1, times = frame_num), 
    rep(y1, times = frame_num), 
    rep(y1, times = frame_num)
  ), 
  x_to = c(
    rep(x1, times = frame_num) + 1, 
    x2_vals, 
    x2_vals
  ), 
  y_to = c(
    rep(y1, times = frame_num), 
    rep(y1, times = frame_num), 
    y2_vals
  )
)

# 補助線分ラベルの座標を作成
anim_auxil_label_df <- tibble::tibble(
  frame_i = 1:frame_num |> 
    rep(times = 2), # ラベルの数
  x = c(
    x1 + 0.5*(x2_vals - x1), 
    x2_vals
  ), 
  y = c(
    rep(y1, times = frame_num), 
    y1 + 0.5*(y2_vals - y1)
  ), 
  label = c(
    "x[2] - x[1]", 
    "log~Gamma(x[2]) - log~Gamma(x[1])"
  ) |> 
    rep(each = frame_num), 
  a = c(0, 90) |> 
    rep(each = frame_num), 
  h = c(0.5, 0.5) |> 
    rep(each = frame_num), 
  v = c(1.2, 1.2) |> 
    rep(each = frame_num)
)

# 目盛線の座標を作成
anim_tick_x_df <- tibble::tibble(
  frame_i = 1:frame_num |> 
    rep(times = 3), # 線の数
  x = c(
    rep(x1, times = frame_num), 
    rep(x1+1, times = frame_num), 
    x2_vals
  ), 
  label = c("x[1]", "x[1] + 1", "x[2]") |> 
    rep(each = frame_num)
)
anim_tick_y_df <- tibble::tibble(
  frame_i = 1:frame_num |> 
    rep(times = 2), # 線の数
  y = c(
    rep(y1, times = frame_num), 
    y2_vals
  ), 
  label = c("log~Gamma(x[1])", "log~Gamma(x[2])") |> 
    rep(each = frame_num)
)


# 軸サイズを指定
axis_y_size <- 10

# ラベル用の文字列を作成
def_label <- paste0(
  "frac(log~Gamma(x[2]) - log~Gamma(x[1]), x[2] - x[1]) > Psi(x[1])", 
  "~~~~ (x[2] > x[1])"
)

# 配色の共通化用のカラーコードを作成
color_vec <- scales::hue_pal()(n = 4)

# ガンマ関数とディガンマ関数のアニメーションを作図
anim <- ggplot() + 
  geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                             xend = c(Inf, 0), yend = c(0, Inf)), 
               arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
  geom_vline(anim_tick_x_df, 
             mapping = aes(xintercept = x), 
             linetype ="dotted") + # x座標の目盛線
  geom_hline(data = anim_tick_y_df, 
             mapping = aes(yintercept = y), 
             linetype ="dotted") + # y座標の目盛線
  geom_label(data = anim_tick_x_df, 
             mapping = aes(x = x, y = -Inf, label = label), parse = TRUE, 
             fill = "gray92", label.size = 0, label.padding = unit(0, units = "points"), 
             hjust = 0.5, vjust = 0, size = 3,) + # 変数ラベル
  geom_label(data = anim_tick_y_df, 
             mapping = aes(x = -Inf, y = y, label = label), parse = TRUE, 
             fill = "gray92", label.size = 0, label.padding = unit(0, units = "points"), 
             hjust = 0, vjust = 0.5, size = 3) + # 関数ラベル
  geom_line(data = fnc_curve_df, 
            mapping = aes(x = x, y = f_x)) + # 関数曲線
  geom_point(data = anim_fnc_point_df, 
             mapping = aes(x = x, y = f_x), 
             size = 3) + # 関数点
  geom_segment(data = anim_auxil_seg_df, 
               mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to)) + # 補助線分
  geom_text(data = anim_auxil_label_df, 
            mapping = aes(x = x, y = y, label = label, angle = a, hjust = h, vjust = v), parse = TRUE, 
            size = 3) + # 補助線分ラベル
  geom_segment(data = anim_fnc_seg_df,
               mapping = aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
                             color = fnc, linewidth = w)) + # 関数線分
  geom_abline(data = anim_oblique_df, 
              mapping = aes(slope = a, intercept = b, color = fnc), 
              show.legend = FALSE) + # 接線・2点上の直線
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
  scale_color_manual(breaks = c("digamma_x", "log_gamma_x", "target"), 
                     values = color_vec[c(1, 2, 4)], 
                     labels = c(expression(Psi(x)), expression(log~Gamma(x)), expression(frac(log~Gamma(x[2])-log~Gamma(x[1]), x[2]-x[1]))), 
                     name = "function") + # 凡例の表示用
  scale_linewidth_identity() +
  theme(legend.text.align = 0) + # 図の体裁
  coord_cartesian(xlim = c(0, max(x_vec)), 
                  ylim = c(-0.5*axis_y_size, axis_y_size)) + # 描画領域の設定
  labs(title = "gamma function and digamma function", 
       subtitle = parse(text = def_label), 
       x = expression(x), 
       y = expression(f(x)))

# 動画を作成
gganimate::animate(
  plot = anim, nframes = frame_num, fps = 30, width = 8, height = 6, units = "in", res = 200, 
  renderer = gganimate::av_renderer(file = "figure/ch2/log_gamma.mp4")
)


# 対数ガンマ関数のテイラー展開 ----------------------------------------------------------
 
# x軸の範囲を指定
x_vec <- seq(from = 0, to = 10, length.out = 5001)
x_vec[x_vec == 0] <- 1e-10 # (x = 0のときInfになり意図しない作図処理になるのを回避)

# 関数の曲線の座標を作成
fnc_curve_df <- tibble::tibble(
  x   = x_vec, 
  f_x = lgamma(x)
)

# 最大次数を指定
n <- 100

# 中心のx座標を指定
a <- 2.5

# テイラー級数を計算
anim_fnc_curve_df <- tidyr::expand_grid(
  i = 0:n, # 次数
  x = x_vec
) |> # 次数ごとに変数を複製
  dplyr::mutate(
    f_x = dplyr::if_else(
      i > 0, 
      true  = psigamma(a, deriv = i-1) / gamma(i+1) * (x - a)^i, 
      false = lgamma(a)
    )
  ) |> 
  dplyr::mutate(
    f_x = cumsum(f_x), .by = x
  )


# 軸サイズを指定
axis_y_size <- 10

# ラベル用の文字列を作成
def_label <- paste0(
  "list(", 
  "f(x) %~~% sum(frac(f^{(i)}*(a), i*'!') ~ (x - a), i==0, n)^i, ", 
  "a == ", a, ", ", 
  "n == ", n, 
  ")"
)

# テイラー展開を作図
graph <- ggplot() + 
  geom_vline(mapping = aes(xintercept = a), 
             linetype = "dotted") + # 中心の位置
  geom_line(data = fnc_curve_df, 
            mapping = aes(x = x, y = f_x)) + # 対象の関数
  geom_line(data = anim_fnc_curve_df, 
            mapping = aes(x = x, y = f_x, color = factor(i))) + # 近似関数
  scale_x_continuous(sec.axis = sec_axis(trans = ~., breaks = a, labels = expression(a))) + # 中心の位置
  coord_cartesian(xlim = c(0, max(x_vec)), 
                  ylim = c(-axis_y_size, axis_y_size)) + # 描画領域の設定
  theme(legend.text.align = 0) + # 図の体裁
  guides(color = "none") + # 凡例の体裁
  labs(title = "Taylor expansion", 
       subtitle = parse(text = def_label), 
       color = expression(n), 
       y = expression(f(x)), 
       x = expression(x))

# グラフを保存
ggplot2::ggsave(
  filename = "figure/ch2/approx_log_gamma.png", plot = graph, 
  width = 8, height = 6, dpi = 100
)


# ラベル用の文字列を作成
label_df <- tibble::tibble(
  i = 0:n, 
  x = a, 
  label = paste0(
    "list(", 
    "a == ", a, ", ", 
    "n == ", i, 
    ")"
  )
)

# テイラー展開のアニメーションを作成
anim <- ggplot() + 
  geom_vline(data = label_df, 
             mapping = aes(xintercept = x), 
             linetype = "dotted") + # 中心の位置
  geom_line(data = fnc_curve_df, 
            mapping = aes(x = x, y = f_x, color = "target")) + # 対象の関数
  geom_line(data = anim_fnc_curve_df, 
             mapping = aes(x = x, y = f_x, color = "approx")) + # 近似関数
  geom_label(data = label_df, 
             mapping = aes(x = -Inf, y = Inf, label = label), parse = TRUE, 
             hjust = 0, vjust = 1, alpha = 0.5) + # (subtitleの代用)
  gganimate::transition_manual(frames = i) + # フレーム制御
  scale_color_manual(breaks = c("target", "approx"), 
                     values = c("black", "red"), 
                     labels = c(expression(log~Gamma(x)), expression(sum(frac(f^{(i)}*(a), i*'!')~(x - a)^i, i==0, n))), 
                     name = "function") + # 凡例の表示用
  scale_x_continuous(sec.axis = sec_axis(trans = ~., breaks = a, labels = expression(a))) + # 中心の位置
  coord_cartesian(xlim = c(0, max(x_vec)), 
                  ylim = c(-axis_y_size, axis_y_size)) + # 描画領域の設定
  theme(legend.text.align = 0) + # 図の体裁
  labs(title = "Taylor expansion", 
       y = expression(f(x)), 
       x = expression(x))

# 動画を作成
gganimate::animate(
  plot = anim, nframes = n+1, fps = 10, width = 8, height = 6, units = "in", res = 200, 
  renderer = gganimate::av_renderer(file = "figure/ch2/approx_log_gamma.mp4")
)


