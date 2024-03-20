
# chapter 4.5
# トピックモデル
# 生成過程

# %%

#　利用ライブラリ
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# %%

### 簡易文書データの作成：(LDA)

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

# トピック分布のパラメータを生成
true_theta_dk = np.random.dirichlet(alpha=true_alpha_k, size=D)

# 語彙分布のパラメータを生成
true_phi_kv = np.random.dirichlet(alpha=true_beta_v, size=true_K)

# 文書ごとの各単語の語彙を初期化
w_dic = {}

# 文書ごとの各単語のトピックを初期化
true_z_dic = {}

# 各文書の単語数を初期化
N_d = np.zeros(shape=D, dtype='int')

# 文書ごとの各語彙の単語数を初期化
N_dv = np.zeros(shape=(D, V), dtype='int')

# 文書データを生成
for d in range(D): # 文書ごと

    # 単語数を生成
    N_d[d] = np.random.randint(low=100, high=200, size=1) # 下限・上限を指定

    # 各単語の語彙を初期化
    tmp_w_n = np.repeat(np.nan, repeats=N_d[d])
    
    # 各単語のトピックを初期化
    tmp_z_n = np.repeat(np.nan, repeats=N_d[d])
    
    for n in range(N_d[d]): # 単語ごと
        
        # トピックを生成
        onehot_k = np.random.multinomial(n=1, pvals=true_theta_dk[d], size=1).reshape(true_K) # one-hot符号
        k, = np.where(onehot_k == 1) # トピック番号
        k  = k.item()
        
        # トピックを割当
        tmp_z_n[n] = k
    
        # 語彙を生成
        onehot_v = np.random.multinomial(n=1, pvals=true_phi_kv[k], size=1).reshape(V) # one-hot符号
        v, = np.where(onehot_v == 1) # 語彙番号
        v  = v.item()

        # 語彙を割当
        tmp_w_n[n] = v
        
        # 頻度をカウント
        N_dv[d, v] += 1
    
    # データを記録
    w_dic[d]      = tmp_w_n.astype('int').copy()
    true_z_dic[d] = tmp_z_n.astype('int').copy()
    
    # 途中経過を表示
    print(f'document: {d+1}, words: {N_d[d]}, topics: {np.unique(true_z_dic[d])}')

# %%

### 作図の準備

# 配色の共通化用のカラーマップを作成
cmap = plt.get_cmap("tab10")

# カラーマップの色数を設定:(カラーマップに応じて固定)
color_num = 10

# %%

### 真のトピック集合の可視化

# 文書ごとの各トピックの単語数を集計
true_N_dk = np.zeros(shape=(D, true_K))
for d in range(D):
    for n in range(N_d[d]):
        k = true_z_dic[d][n]
        true_N_dk[d, k] += 1

# 描画する文書数を指定
#doc_num = D
doc_num = 5

# グラフサイズを設定
u = 5
axis_Ndv_max = (np.ceil(N_dv[:doc_num].max() /u)*u).astype('int') # u単位で切り上げ
axis_Ndk_max = (np.ceil(true_N_dk[:doc_num].max() /u)*u).astype('int') # u単位で切り上げ

# 文書データを作図
fig, axes = plt.subplots(nrows=doc_num, ncols=2, constrained_layout=True, 
                         figsize=(20, 25), dpi=100, facecolor='white')

for d in range(doc_num):

    # 各語彙の単語数を集計・各単語のトピックを格納
    tmp_z_mv = np.tile(np.nan, reps=(axis_Ndv_max, V))
    for n in range(N_d[d]):
        v = w_dic[d][n]
        m = np.sum(w_dic[d][:n+1] == v)
        k = true_z_dic[d][n]
        tmp_z_mv[m-1, v] = k % color_num # (配色の共通化用)
    
    # 単語データを描画
    ax = axes[d, 0]
    ax.pcolor(tmp_z_mv, cmap=cmap, vmin=0, vmax=color_num-1) # 頻度・トピック
    ax.set_xlabel('vocabulary ($v$)')
    ax.set_ylabel('frequency ($N_{dv}$)')
    ax.set_title(f'$d = {d+1}, N_d = {N_d[d]}$', loc='left')
    ax.grid()

    # 語彙の出現順を描画
    for n in range(N_d[d]):
        v = w_dic[d][n]
        m = np.sum(w_dic[d][:n+1] == v)
        ax.text(x=v+0.5, y=m-0.5, s=str(n+1), 
                size=5, ha='center', va='center') # 単語番号
    
    # トピックの割当を描画
    ax = axes[d, 1]
    ax.bar(x=np.arange(stop=true_K)+1, height=true_N_dk[d], 
           color=[cmap(k%color_num) for k in range(true_K)]) # 単語数
    ax.set_ylim(ymin=0, ymax=axis_Ndk_max)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('frequency ($N_{dk}$)')
    ax.set_title(f'$d = {d+1}, N_d = {N_d[d]}$', loc='left')
    ax.grid()

fig.supylabel('document ($d$)')
fig.suptitle('document data (true topic)', fontsize=20)
plt.show()

# %%

### 真のトピック集合の推移の可視化

# 描画する文書数を指定
#doc_num = D
doc_num = 5

# 最大単語数を取得
max_Nd = N_d[:doc_num].max()

# グラフサイズを設定
u = 5
axis_Ndv_max = (np.ceil(N_dv[:doc_num].max() /u)*u).astype('int') # u単位で切り上げ
axis_Ndk_max = (np.ceil(true_N_dk[:doc_num].max() /u)*u).astype('int') # u単位で切り上げ

# グラフオブジェクトを初期化
fig, axes = plt.subplots(nrows=doc_num, ncols=2, constrained_layout=True, 
                         figsize=(20, 25), dpi=100, facecolor='white')
fig.supylabel('document ($d$)')
fig.suptitle('document data (true topic)', fontsize=20)

# 作図処理を定義
def update(n):
    
    for d in range(doc_num):
        if n < N_d[d]: # (単語数に達すればそのまま)
            
            # 前フレームのグラフを初期化
            [ax.cla() for ax in axes[d]]
            
            # 各語彙の単語数を集計・各単語のトピックを格納
            tmp_z_mv = np.tile(np.nan, reps=(axis_Ndv_max, V))
            for tmp_n in range(n+1):
                v = w_dic[d][tmp_n]
                m = np.sum(w_dic[d][:tmp_n+1] == v)
                k = true_z_dic[d][tmp_n]
                tmp_z_mv[m-1, v] = k % color_num # (配色の共通化用)
            
            # 単語データを描画
            ax = axes[d, 0]
            ax.pcolor(tmp_z_mv, cmap=cmap, vmin=0, vmax=color_num-1) # 頻度・トピック
            ax.set_xlabel('vocabulary ($v$)')
            ax.set_ylabel('frequency ($N_{dv}$)')
            ax.set_title(f'$d = {d+1}, N_d = {n+1}$', loc='left')
            ax.grid()
            
            # 語彙の出現順を描画
            for tmp_n in range(n+1):
                v = w_dic[d][tmp_n]
                m = np.sum(w_dic[d][:tmp_n+1] == v)
                ax.text(x=v+0.5, y=m-0.5, s=str(tmp_n+1), 
                        size=5, ha='center', va='center') # 単語番号
            
            # 各トピックの単語数を集計
            tmp_M_k = np.zeros(shape=true_K)
            for tmp_n in range(n+1):
                k = true_z_dic[d][tmp_n]
                tmp_M_k[k] += 1
            
            # トピックの割当を描画
            ax = axes[d, 1]
            ax.bar(x=np.arange(stop=true_K)+1, height=tmp_M_k, 
                   color=[cmap(k%color_num) for k in range(true_K)]) # 単語数
            ax.set_ylim(ymin=0, ymax=axis_Ndk_max)
            ax.set_xlabel('topic ($k$)')
            ax.set_ylabel('frequency ($N_{dk}$)')
            ax.set_title(f'$d = {d+1}, N_d = {n+1}$', loc='left')
            ax.grid()

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=max_Nd, interval=100)

# 動画を書出
ani.save(
    filename='../figure/ch4/ch4_2_true_topic_set.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i+1} / {n}')
)

# %%

### 真のトピック分布の可視化

# 描画する文書数を指定
#doc_num = D
doc_num = 9

# グラフサイズを設定
u = 0.1
axis_size = np.ceil(true_theta_dk[:doc_num].max() /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < D)
col_num = 3

# サブプロットの行数を計算:(1行or1列だとエラー)
row_num = np.ceil(doc_num / col_num).astype('int')

# トピック分布を作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(24, 18), dpi=100, facecolor='white')

for d in range(doc_num):
    
    # サブプロットのインデックスを計算
    r = d // col_num
    c = d % col_num
    
    # サブプロットを抽出
    ax = axes[r, c]

    # トピック分布を描画
    ax.bar(x=np.arange(stop=true_K)+1, height=true_theta_dk[d], 
           color=[cmap(k%color_num) for k in range(true_K)]) # 確率
    ax.set_ylim(ymin=0, ymax=axis_size)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('probability ($\\theta_{dk}$)')
    ax.set_title(f'$d = {d+1}, \\alpha = {true_alpha}$', loc='left') # (一様パラメータ用)
    #ax.set_title('$d = {}, \\alpha_k = ({})$'.format(d+1, ', '.join([str(val.round(2)) for val in true_alpha_k])), loc='left') # (多様パラメータ用)
    ax.grid()

# 残りのサブプロットを非表示
for c in range(c+1, col_num):
    axes[r, c].axis('off')

fig.supxlabel('document ($d$)')
fig.supylabel('document ($d$)')
fig.suptitle('topic distribution (truth)', fontsize=20)
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


