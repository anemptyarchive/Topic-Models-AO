
# chapter 3.1
# 混合ユニグラムモデル
# 生成過程

# %%

#　利用ライブラリ
import numpy as np
import matplotlib.pyplot as plt

# %%

### 簡易文書データの作成：(混合モデル)

# 文書数を指定
D = 30

# 語彙数を指定
V = 100

# トピック数を指定
true_K = 10

# トピック分布のハイパーパラメータを指定
true_alpha   = 1.0
true_alpha_k = np.repeat(true_alpha, repeats=true_K) # 一様なパラメータの場合
#true_alpha_k = np.random.uniform(low=1, high=2, size=true_K) # 多様なパラメータの場合

# 語彙分布のハイパーパラメータを指定
true_beta   = 1.0
true_beta_v = np.repeat(true_beta, repeats=V) # 一様なパラメータの場合
#true_beta_v = np.random.uniform(low=1, high=2, size=V) # 多様なパラメータの場合

# トピック分布を生成
true_theta_k = np.random.dirichlet(alpha=true_alpha_k, size=1).reshape(true_K)

# 語彙分布を生成
true_phi_kv = np.random.dirichlet(alpha=true_beta_v, size=true_K)

# 文書ごとの各単語の語彙を初期化
w_dic = {}

# 各文書のトピックを初期化
true_z_d = np.tile(np.nan, reps=D)

# 各文書の単語数を初期化
N_d = np.zeros(shape=D, dtype='int')

# 文書ごとの各語彙の単語数を初期化
N_dv = np.zeros(shape=(D, V), dtype='int')

# 文書データを生成
for d in range(D): # 文書ごと
    
    # トピックを生成
    onehot_k = np.random.multinomial(n=1, pvals=true_theta_k, size=1).reshape(true_K) # one-hot符号
    k, = np.where(onehot_k == 1) # トピック番号
    k  = k.item()
    
    # トピックを割当
    true_z_d[d] = k

    # 単語数を生成
    N_d[d] = np.random.randint(low=100, high=200, size=1) # 下限・上限を指定

    # 各単語の語彙を初期化
    tmp_w_n = np.repeat(np.nan, repeats=N_d[d]) # 各単語の語彙

    for n in range(N_d[d]): # 単語ごと

        # 語彙を生成
        onehot_v = np.random.multinomial(n=1, pvals=true_phi_kv[k], size=1).reshape(V) # one-hot符号
        v, = np.where(onehot_v == 1) # 語彙番号
        v  = v.item()
        
        # 語彙を割当
        tmp_w_n[n] = v

        # 頻度をカウント
        N_dv[d, v] += 1
    
    # データを記録
    w_dic[d] = tmp_w_n.copy()
    
    # 途中経過を表示
    print(f'document: {d+1}, words: {N_d[d]}, topic: {k+1}')

# %%

### 作図の準備

# 作図用に変換
true_z_d = true_z_d.astype('int')

# 配色の共通化用のカラーマップを作成
cmap = plt.get_cmap("tab10")

# カラーマップの色数を設定:(カラーマップに応じて固定)
color_num = 10

# %%

### 真のトピック集合の可視化

# 各トピックの文書数を集計
true_D_k = np.zeros(shape=true_K)
for d in range(D):
    k = true_z_d[d]
    true_D_k[k] += 1

# 描画する文書数を指定
#doc_num = D
doc_num = 10

# グラフサイズを設定
u = 5
axis_Ndv_max = np.ceil(N_dv[:doc_num].max() /u)*u # u単位で切り上げ
axis_Dk_max  = np.ceil(true_D_k.sum() /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < D+1)
col_num = 3

# サブプロットの行数を計算:(1行or1列だとエラー)
row_num = np.ceil((doc_num+1) / col_num).astype('int')

# 文書データを作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(30, 20), dpi=100, facecolor='white')

for d in range(doc_num):
    
    # サブプロットのインデックスを計算
    r = d // col_num
    c = d % col_num
    
    # サブプロットを抽出
    ax = axes[r, c]

    # トピック番号を取得
    k = true_z_d[d]
    
    # 語彙頻度を描画
    ax.bar(x=np.arange(stop=V)+1, height=N_dv[d], 
           color=cmap(k%color_num)) # 単語数
    ax.set_ylim(ymin=0, ymax=axis_Ndv_max)
    ax.set_xlabel('vocabulary ($v$)')
    ax.set_ylabel('frequency ($N_{dv}$)')
    ax.set_title(f'$d = {d+1}, k = {k+1}$', loc='left')
    ax.grid()

# 残りのサブプロットを非表示
if c == col_num-1: # (最後の文書が右端の列の場合)
    r = row_num - 1
    c = -1 
for c in range(c+1, col_num-1):
    axes[r, c].axis('off')
    
# トピックの割当を描画
ax = axes[row_num-1, col_num-1]
ax.bar(x=np.arange(stop=true_K)+1, height=true_D_k, 
        color=[cmap(k%color_num) for k in range(true_K)]) # 文書数
ax.set_ylim(ymin=0, ymax=axis_Dk_max)
ax.set_xlabel('topic ($k$)')
ax.set_ylabel('count ($D_k$)')
ax.set_title(f'$D = {D}$', loc='left')
ax.grid()

fig.supxlabel('document ($d$)')
fig.supylabel('document ($d$)')
fig.suptitle('document data (true topic)', fontsize=20)
plt.show()

# %%

### 真のトピック分布の可視化

# グラフサイズを設定
u = 0.1
axis_size = np.ceil(true_theta_k.max() /u)*u # u単位で切り上げ

# トピック分布を作図
fig, ax = plt.subplots(figsize=(8, 5), dpi=100, facecolor='white')
ax.bar(x=np.arange(stop=true_K)+1, height=true_theta_k, 
       color=[cmap(k%color_num) for k in range(true_K)]) # 確率
ax.set_ylim(ymin=0, ymax=axis_size)
ax.set_xlabel('topic ($k$)')
ax.set_ylabel('probability ($\\theta_k$)')
ax.set_title(f'$\\alpha = {true_alpha}$', loc='left') # (一様パラメータ用)
#ax.set_title('$\\alpha_k = ({})$'.format(', '.join([str(val.round(2)) for val in true_alpha_k])), loc='left') # (多様パラメータ用)
fig.suptitle('topic distribution (truth)', fontsize=20)
ax.grid()
plt.show()

# %%

### 真の単語分布の可視化

# 描画するトピック数を指定
#topic_num = true_K
topic_num = 9


# グラフサイズを設定
u = 0.01
axis_size = np.ceil(true_phi_kv[:topic_num].max() /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < K)
col_num = 3

# サブプロットの行数を計算:(1行or1列だとエラー)
row_num = np.ceil(topic_num / col_num).astype('int')

# 語彙分布を作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(30, 15), dpi=100, facecolor='white')

for k in range(topic_num):
    
    # サブプロットのインデックスを計算
    r = k // col_num
    c = k % col_num

    # サブプロットを抽出
    ax = axes[r, c]

    # 語彙分布を描画
    ax.bar(x=np.arange(stop=V)+1, height=true_phi_kv[k], 
           color=cmap(k%color_num)) # 確率
    ax.set_ylim(ymin=0, ymax=axis_size)
    ax.set_xlabel('vocabulary ($v$)')
    ax.set_ylabel('probability ($\phi_{kv}$)')
    ax.set_title(f'$k = {k+1}, \\beta = {true_beta}$', loc='left') # (一様パラメータ用)
    #ax.set_title('$k = {}, \\beta_v = ({}, ...)$'.format(k+1, ', '.join([str(true_beta_v[v].round(2)) for v in range(5)])), loc='left') # (多様パラメータ用)
    ax.grid()

# 残りのサブプロットを非表示
for c in range(c+1, col_num):
    axes[r, c].axis('off')

fig.supxlabel('topic ($k$)')
fig.supylabel('topic ($k$)')
fig.suptitle('word distribution (truth)', fontsize=20)
plt.show()

# %%


