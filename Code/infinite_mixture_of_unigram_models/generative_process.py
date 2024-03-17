
# chapter 8.1
# 無限混合ユニグラムモデル
# 生成過程

# %%

# 利用ライブラリ
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# %%

### 簡易文書データの作成：(中華料理店過程)

# 文書数を指定
D = 30

# 語彙数を指定
V = 100

# トピック分布のハイパーパラメータを指定
true_alpha  = 2.0

# 語彙分布のハイパーパラメータを指定
true_beta   = 1.0
true_beta_v = np.repeat(true_beta, repeats=V) # 一様なパラメータの場合
#true_beta_v = np.random.uniform(low=1, high=2, size=V) # 多様なパラメータの場合

# トピック数を初期化
true_K = 0

# 文書ごとの各単語の語彙を初期化
w_dic = {}

# 各文書のトピックを初期化
true_z_d = np.tile(np.nan, reps=D)

# 各トピックの割り当て文書数を初期化
true_D_k = np.array([])

# 各文書の単語数を初期化
N_d = np.zeros(shape=D, dtype='int')

# 文書ごとの各語彙の単語数を初期化
N_dv = np.zeros(shape=(D, V), dtype='int')

# 文書データを生成
for d in range(D): # 文書ごと
    
    # トピック分布を生成
    if true_K == 0: # (初回の場合)
        true_theta_k = np.array([1.0])
    else:
        true_theta_k = np.hstack([true_D_k, true_alpha]) / (d + true_alpha) # 既存・新規の確率を結合
    
    # トピックを生成
    onehot_k = np.random.multinomial(n=1, pvals=true_theta_k, size=1).reshape(true_K+1) # one-hot符号
    k, = np.where(onehot_k == 1) # トピック番号
    k  = k.item()
    
    # トピックを割当
    true_z_d[d] = k

    if k == true_K: # (新規で割り当てられた場合)
        
        # トピックを追加
        true_K  += 1
        true_D_k = np.hstack([true_D_k, 0])

        # 語彙分布を生成
        if true_K == 1: # (初回の場合)
            true_phi_kv = np.random.dirichlet(alpha=true_beta_v, size=1)
        else:
            true_phi_kv = np.vstack(
                [true_phi_kv, np.random.dirichlet(alpha=true_beta_v, size=1)] # 既存・新規の確率を結合
            )
    
    # 割当数をカウント
    true_D_k[k] += 1

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

# 描画する文書数を指定
#doc_num = D
doc_num = 10

# グラフサイズを設定
u = 5
axis_Ndv_max = np.ceil(N_dv[:doc_num].max() /u)*u # u単位で切り上げ
axis_Dk_max  = np.ceil(true_D_k.max() /u)*u # u単位で切り上げ

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
ax.set_title(f'$D = {D}, K = {true_K}$', loc='left')
ax.grid()

fig.supxlabel('document ($d$)')
fig.supylabel('document ($d$)')
fig.suptitle('document data (true topic)', fontsize=20)
plt.show()

# %%

### 真のトピック分布の可視化

# D+1番目の文書のトピック分布を計算
true_theta_k = np.hstack([true_D_k, true_alpha]) / (D + true_alpha) # 既存・新規の確率を結合

# グラフサイズを設定
u = 0.1
axis_size = np.ceil(true_theta_k.max() /u)*u # u単位で切り上げ

# トピック分布を作図
fig, ax = plt.subplots(figsize=(8, 5), dpi=100, facecolor='white')
ax.bar(x=np.arange(stop=true_K+1)+1, height=true_theta_k, 
       color=[cmap(k%color_num) for k in range(true_K)]+['whitesmoke'], 
       edgecolor='black', linestyle='dashed', linewidth=[0]*true_K+[1]) # 確率
ax.set_ylim(ymin=0, ymax=axis_size)
ax.set_xlabel('topic ($k$)')
ax.set_ylabel('probability ($\\theta_k$)')
ax.set_title(f'$\\alpha = {true_alpha}, K = {true_K}$', loc='left')
fig.suptitle('topic distribution (truth)', fontsize=20)
ax.grid()
plt.show()

# %%

### 真のトピック分布の推移の可視化

# グラフサイズを設定
u = 5
axis_freq_max = np.ceil(true_D_k.max() /u)*u # u単位で切り上げ
axis_prob_max = 1.0

# グラフオブジェクトを初期化
fig, axes = plt.subplots(nrows=1, ncols=2, constrained_layout=True, 
                         figsize=(16, 5), dpi=100, facecolor='white')
fig.suptitle('topic distribution (truth)', fontsize=20)

# 作図処理を定義
def update(d):
    
    # 前フレームのグラフを初期化
    [ax.cla() for ax in axes]
    
    # (文書dまでの)トピック数を取得
    if d == 0: # (初回の場合)
        tmp_K = 0
    else:
        tmp_K = true_z_d[:d].max() + 1
    
    # 各トピックの文書数を集計
    tmp_D_k = np.array([np.sum(true_z_d[:d] == k) for k in range(true_K)])

    # トピックの割当を描画
    ax = axes[0]
    ax.bar(x=np.arange(stop=true_K)+1, height=tmp_D_k, 
           color=[cmap(k%color_num) for k in range(true_K)]) # 文書数
    ax.set_xlim(xmin=0.5, xmax=true_K+1.5)
    ax.set_ylim(ymin=0, ymax=axis_freq_max)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('frequency ($D_k$)')
    ax.set_title(f'$D = {d}$', loc='left')
    ax.grid()
    
    # トピック分布を計算
    if tmp_K == 0: # (初回の場合)
        tmp_theta_k = np.array([1.0])
    else:
        tmp_theta_k = np.hstack([tmp_D_k[:tmp_K], true_alpha]) / (d + true_alpha) # 既存・新規の確率を結合
    
    # トピック分布を描画
    ax = axes[1]
    ax.bar(x=np.arange(stop=tmp_K+1)+1, height=tmp_theta_k, 
           color=[cmap(k%color_num) for k in range(tmp_K)]+['whitesmoke'], 
           edgecolor='black', linestyle='dashed', linewidth=[0]*tmp_K+[1]) # 確率
    ax.set_xlim(xmin=0.5, xmax=true_K+1.5)
    ax.set_ylim(ymin=0, ymax=axis_prob_max)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('probability ($\\theta_k$)')
    ax.set_title(f'$\\alpha = {true_alpha}, K = {tmp_K}$', loc='left')
    ax.grid()

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=D+1, interval=500)

# 動画を書出
ani.save(
    filename='../figure/ch8/ch8_1_tocip_dist.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i+1} / {n}')
)

# %%

### 真の単語分布の可視化

# 描画するトピック数を指定
topic_num = true_K
#topic_num = 5

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


