
# chapter 8.2 無限次元トピックモデル

# %%

# 利用ライブラリ
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# %%

### 簡易文書データの作成：(フランチャイズ中華料理店過程)

# 文書数を指定
D = 5

# 語彙数を指定
V = 50

# ハイパーパラメータを指定
true_alpha = 4.0 # トピック分布
true_beta  = 2.0 # 語彙分布
true_gamma = 2.0 # テーブル分布

# 総テーブル数を初期化
true_T = 0

# トピック数を初期化
true_K = 0

# 受け皿を初期化
true_c_dic = {} # 各単語のテーブル
true_z_dic = {} # 各テーブルのトピック
true_T_k = np.zeros(shape=true_K) # 各トピックが割り当てられたテーブル数
true_T_d = np.zeros(shape=D, dtype='int') # 各文書のテーブル数
N_d  = np.zeros(shape=D, dtype='int') # 各文書の単語数
N_dv = np.zeros(shape=(D, V), dtype='int') # 各文書の語彙ごとの単語数
doc_dic = {} # 文書データ

# 文書データを生成
for d in range(D): # 文書ごと
    
    # 単語数を生成
    N_d[d] = np.random.randint(low=100, high=200, size=1) # 下限・上限を指定

    # 文書dのテーブル数を初期化
    tmp_T   = 0
    tmp_M_t = np.zeros(shape=tmp_T) # 各テーブルが割り当てられた単語数

    # 受け皿を初期化
    tmp_c_n = np.tile(np.nan, reps=N_d[d]) # 各単語のテーブル
    tmp_z_t = np.array([]) # 各テーブルのトピック
    tmp_word_dic = {} # 単語データ

    for n in range(N_d[d]): # 単語ごと

        # テーブル分布を生成
        if tmp_T == 0: # (初回の場合)
            true_tau_t = np.array([1.0])
        else:
            true_tau_t = np.hstack(
                [tmp_M_t[:tmp_T] / (n + true_gamma)] + [true_gamma / (n + true_gamma)] # 既存・新規の確率を結合
            )
            true_tau_t /= true_tau_t.sum() # 正規化
        
        # テーブルを生成
        onehot_t = np.random.multinomial(n=1, pvals=true_tau_t, size=1).reshape(tmp_T+1) # one-hot符号
        t, = np.where(onehot_t == 1) # テーブル番号
        t  = t.item()
        
        # テーブルを割当
        tmp_c_n[n] = t

        if t == tmp_T: # (新規で割り当てられた場合)
            
            # テーブルを追加
            true_T += 1
            tmp_T  += 1
            tmp_M_t = np.hstack([tmp_M_t, 0])
        
            # トピック分布を生成
            if true_K == 0: # (初回の場合)
                true_theta_k = np.array([1.0])
            else:
                true_theta_k = np.hstack(
                    [true_T_k / (true_T-1 + true_alpha)] + [true_alpha / (true_T-1 + true_alpha)] # 既存・新規の確率を結合
                )
                true_theta_k /= true_theta_k.sum() # 正規化
            
            # トピックを生成
            onehot_k = np.random.multinomial(n=1, pvals=true_theta_k, size=1).reshape(true_K+1) # one-hot符号
            k, = np.where(onehot_k == 1) # トピック番号
            k  = k.item()
        
            # トピックを割当
            tmp_z_t = np.hstack([tmp_z_t, k])

            if k == true_K: # (新規で割り当てられた場合)
                
                # トピックを追加
                true_K  += 1
                true_T_k = np.hstack([true_T_k, 0])

                # 語彙分布を生成
                if true_K == 1: # (初回の場合)
                    true_phi_kv = np.random.dirichlet(alpha=np.repeat(true_beta, repeats=V), size=1)
                else:
                    true_phi_kv = np.vstack(
                        [true_phi_kv] + [np.random.dirichlet(alpha=np.repeat(true_beta, repeats=V), size=1)] # 既存・新規の確率を結合
                    )
            
            # 割り当て数をカウント
            true_T_k[k] += 1
        
        else: # (既存に割り当てられた場合)
            
            # トピック番号を取得
            k = tmp_z_t[t].astype('int')

        # 割り当て数をカウント
        tmp_M_t[t] += 1

        # 語彙を生成
        onehot_v = np.random.multinomial(n=1, pvals=true_phi_kv[k], size=1).reshape(V) # one-hot符号
        v, = np.where(onehot_v == 1) # 語彙番号
        v  = v.item()

        # 頻度をカウント
        N_dv[d, v] += 1
        
        # データを記録
        tmp_word_dic[n] = (t, k, v, N_dv[d, v]) # テーブル番号, トピック番号, 語彙番号, 頻度
    
    # データを記録
    true_T_d[d]   = tmp_T
    true_c_dic[d] = tmp_c_n.astype('int').copy()
    true_z_dic[d] = tmp_z_t.astype('int').copy()
    doc_dic[d]    = tmp_word_dic.copy()
    
    # 途中経過を表示
    print(
        'document: ' + str(d+1) + ', ' + 
        'words: '    + str(N_d[d]) + ', ' + 
        'tables: '   + str(true_T) + ', ' + 
        'topics: '   + str(np.unique(true_z_dic[d]))
    )

# %%

### 真のトピック集合の可視化

# 最大テーブル数を取得
max_Td = true_T_d.max()

# 配列に変換
N_dk = np.zeros(shape=(D, true_K)) # 各文書におけるトピックごとの単語数
N_dt = np.zeros(shape=(D, max_Td)) # 各文書におけるテーブルごとの単語数
for d in range(D):
    for n in range(N_d[d]):
        t, k, v, i = doc_dic[d][n]
        N_dk[d, k] += 1
        N_dt[d, t] += 1

# グラフサイズを設定
u = 5
axis_Ndv_max = (np.ceil(N_dv.max() /u)*u).astype('int') # u単位で切り上げ
axis_Ndk_max = (np.ceil(N_dk.max() /u)*u).astype('int') # u単位で切り上げ
axis_Ndt_max = (np.ceil(N_dt.max() /u)*u).astype('int') # u単位で切り上げ

# 配色の共通化用のカラーマップを作成
cmap = plt.get_cmap("tab10")
color_num = 10 # (カラーマップに応じて固定)

# 文書データを作図
fig, axes = plt.subplots(nrows=D, ncols=3, constrained_layout=True, 
                         figsize=(24, 30), facecolor='white')
fig.supylabel('document ($d$)')
fig.suptitle('document data (true topic)', fontsize=20)

for d in range(D):

    # 配列に変換
    z_nv = np.tile(np.nan, reps=(axis_Ndv_max, V))
    for n in range(N_d[d]):
        t, k, v, i = doc_dic[d][n]
        z_nv[i-1, v] = k % color_num # (配色の共通化用)
    
    # 単語のトピック・語彙の頻度を描画
    ax = axes[d, 0]
    ax.pcolor(z_nv, cmap=cmap, vmin=0, vmax=color_num-1) # 単語データ
    ax.set_xlabel('vocabulary ($v$)')
    ax.set_ylabel('frequency ($N_{dv}$)')
    ax.set_title('$d = {}, N_d = {}$'.format(d+1, N_d[d]), loc='left')
    ax.grid()

    # 語彙の出現順を描画
    for n in range(N_d[d]):
        t, k, v, i = doc_dic[d][n]
        ax.text(x=v+0.5, y=i-0.5, s=str(n+1), 
                size=7, ha='center', va='center') # 単語番号
    
    # (文書dまでの)トピック数を取得
    tmp_K = max([true_z_dic[tmp_d].max() for tmp_d in range(d+1)]) + 1
    
    # トピックの割当を描画
    ax = axes[d, 1]
    ax.bar(x=np.arange(stop=true_K)+1, height=N_dk[d], 
           color=[cmap(k%color_num) for k in range(true_K)]) # 単語数
    ax.set_ylim(ymin=0, ymax=axis_Ndk_max)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('frequency ($N_{dk}$)')
    ax.set_title('$K = {}$'.format(tmp_K), loc='left')
    ax.grid()

    # テーブル数を取得
    tmp_T = true_T_d[d]

    ## テーブルの割当を描画
    ax = axes[d, 2]
    ax.bar(x=np.arange(stop=max_Td)+1, height=N_dt[d], 
           color=[cmap(k%color_num) for k in true_z_dic[d]]) # 単語数
    ax.set_ylim(ymin=0, ymax=axis_Ndt_max)
    ax.set_xlabel('table ($t$)')
    ax.set_ylabel('frequency ($N_{dt}$)')
    ax.set_title('$T_d = {}$'.format(tmp_T), loc='left')
    ax.grid()

plt.show()

# %%

### 真のテーブル分布の可視化

# 最大テーブル数を取得
max_Td = true_T_d.max()

# N_d+1番目の単語のテーブル分布を計算
true_tau_dt = np.zeros(shape=(D, max_Td+1))
for d in range(D):
    tmp_T   = true_T_d[d]
    tmp_M_t = np.array([np.sum(true_c_dic[d] == t) for t in range(tmp_T)])
    true_tau_dt[d, :tmp_T+1] = np.hstack(
        [tmp_M_t / (N_d[d] + true_gamma)] + [true_gamma / (N_d[d] + true_gamma)] # 既存・新規の確率を結合
    )
true_tau_dt /= true_tau_dt.sum(axis=1, keepdims=True) # 正規化

# グラフサイズを設定
u = 0.1
axis_size = np.ceil(true_tau_dt.max() /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < D)
col_num = 3

# サブプロットの行数を計算:(1行or1列だとエラー)
row_num = np.ceil(D / col_num).astype('int')

# テーブル分布を作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(12, 9), dpi=100, facecolor='white')

for d in range(D):
    
    # サブプロットを抽出
    r = d // col_num
    c = d % col_num
    ax = axes[r, c]

    # テーブル数を取得
    tmp_T = true_T_d[d]

    # テーブル分布を描画
    ax.bar(x=np.arange(stop=tmp_T+1)+1, height=true_tau_dt[d, :tmp_T+1], 
           color=[cmap(k%color_num) for k in true_z_dic[d]]+['whitesmoke'], 
           edgecolor='black', linestyle='dashed', linewidth=[0]*tmp_T+[1]) # 確率
    ax.set_xlim(xmin=0, xmax=max_Td+2)
    ax.set_ylim(ymin=0, ymax=axis_size)
    ax.set_xlabel('table ($t$)')
    ax.set_ylabel('probability ($\\tau_{dt}$)')
    ax.set_title('$d = {}, N_d = {}, T_d = {}, \gamma = {}$'.format(d+1, N_d[d], tmp_T, true_gamma), loc='left')
    ax.grid()

# 余りを非表示化
for i in range(c+1, col_num):
    axes[r, i].axis('off')

fig.supxlabel('document ($d$)')
fig.supylabel('document ($d$)')
fig.suptitle('table distribution (truth)', fontsize=20)
plt.show()

# %%

### 真のテーブル分布の推移の可視化

# 最大単語数を取得
max_Nd = N_d.max()

# 最大テーブル数を取得
max_Td = true_T_d.max()

# グラフサイズを設定
u = 5
axis_freq_max = np.ceil(max([np.sum(true_c_dic[d] == t) for t in range(true_T_d[d]) for d in range(D)]) /u)*u
axis_prob_max = 1.0

# グラフオブジェクトを初期化
fig, axes = plt.subplots(nrows=D, ncols=2, constrained_layout=True, 
                         figsize=(12, 20), dpi=100, facecolor='white')
fig.suptitle('table distribution (truth)', fontsize=20)

# 作図処理を定義
def update(n):

    for d in range(D):
        if n <= N_d[d]: # (単語数に達すると終了)

            # 各単語のテーブルを取得
            tmp_c_n = true_c_dic[d][:n]

            # テーブル数を取得
            if n == 0:
                tmp_T = 0
            else:
                tmp_T = tmp_c_n.max() + 1

            # 各テーブルを割り当てられた単語数を集計
            tmp_M_t = np.array([np.sum(tmp_c_n == t) for t in range(tmp_T)])

            # 各テーブルのトピックを取得
            tmp_z_t = true_z_dic[d][:tmp_T]

            # トピックごとのテーブル数を描画
            ax = axes[d, 0]
            ax.cla() # 前フレームのグラフを初期化
            ax.bar(x=np.arange(stop=tmp_T)+1, height=tmp_M_t, 
                               color=[cmap(k%color_num) for k in tmp_z_t]) # 文書数
            ax.set_xlim(xmin=0, xmax=max_Td+2)
            ax.set_ylim(ymin=0, ymax=axis_freq_max)
            ax.set_xlabel('table ($t$)')
            ax.set_ylabel('frequency ($N_{dt}$)')
            ax.set_title('$d = {}, N_d = {}, T_d = {}$'.format(d+1, n+1, tmp_T), loc='left')
            ax.grid()

            # テーブル分布を計算
            if tmp_T == 0: # (初回の場合)
                tmp_tau_t = np.array([1.0])
            else:
                tmp_tau_t = np.hstack(
                    [tmp_M_t / (n + true_gamma)] + [true_gamma / (n + true_gamma)] # 既存・新規の確率を結合
                )
                tmp_tau_t /= tmp_tau_t.sum() # 正規化
            
            # テーブル分布を描画
            ax = axes[d, 1]
            ax.cla() # 前フレームのグラフを初期化
            ax.bar(x=np.arange(stop=tmp_T+1)+1, height=tmp_tau_t, 
                   color=[cmap(k%color_num) for k in tmp_z_t]+['whitesmoke'], 
                   edgecolor='black', linestyle='dashed', linewidth=[0]*tmp_T+[1]) # 確率
            ax.set_xlim(xmin=0, xmax=max_Td+2)
            ax.set_ylim(ymin=0, ymax=axis_prob_max)
            ax.set_xlabel('table ($t$)')
            ax.set_ylabel('probability ($\\tau_{dt}$)')
            ax.set_title('$\gamma = {}$'.format(true_gamma), loc='left')
            ax.grid()

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=max_Nd+1, interval=100)

# 動画を書出
ani.save(
    filename='../figure/ch8/ch8_2_table_dist.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)

# %%

### 真のトピック分布の可視化

# 最大テーブル数を取得
max_Td = true_T_d.max()

# T+1番目のテーブルのトピック分布を計算
true_T_k = np.array([np.sum([np.sum(true_z_dic[d] == k) for d in range(D)]) for k in range(true_K)])
true_theta_k = np.hstack(
    [true_T_k / (true_T + true_alpha)] + [true_alpha / (true_T + true_alpha)] # 既存・新規の確率を結合
)
true_theta_k /= true_theta_k.sum() # 正規化

# グラフサイズを設定
u = 0.1
axis_size = np.ceil(true_theta_k.max() /u)*u # u単位で切り上げ

# トピック分布を作図
fig, ax = plt.subplots(figsize=(8, 6), dpi=100, facecolor='white')
ax.bar(x=np.arange(stop=true_K+1)+1, height=true_theta_k, 
       color=[cmap(k%color_num) for k in range(true_K)]+['whitesmoke'], 
       edgecolor='black', linestyle='dashed', linewidth=[0]*true_K+[1]) # 確率
ax.set_ylim(ymin=0, ymax=axis_size)
ax.set_xlabel('topic ($k$)')
ax.set_ylabel('probability ($\\theta_k$)')
ax.set_title('$K = {}, \\alpha = {}$'.format(true_K, true_alpha), loc='left')
fig.suptitle('topic distribution (truth)', fontsize=20)
ax.grid()
plt.show()

# %%

### 真のトピック分布の推移の可視化

# 配列に変換
z_i = np.hstack([true_z_dic[d] for d in range(D)])
d_i = np.hstack([-1] + [np.repeat(d, repeats=len(true_z_dic[d])) for d in range(D)])
t_i = np.hstack([-1] + [np.arange(stop=len(true_z_dic[d])) for d in range(D)])

# 総テーブル数を取得
T = len(z_i)

# グラフサイズを設定
u = 5
axis_freq_max = np.ceil(true_T_k.max() /u)*u # u単位で切り上げ
axis_prob_max = 1.0

# グラフオブジェクトを初期化
fig, axes = plt.subplots(nrows=1, ncols=2, constrained_layout=True, 
                         figsize=(12, 9), dpi=100, facecolor='white')
fig.suptitle('topic distribution (truth)', fontsize=20)

# 作図処理を定義
def update(i):
    
    # 前フレームのグラフを初期化
    [ax.cla() for ax in axes]

    # トピック数を取得
    if i == 0: # (初回の場合)
        tmp_K = 0
    else:
        tmp_K = z_i[:i].max() + 1

    # トピックごとのテーブル数を集計
    tmp_T_k = np.array([np.sum(z_i[:i] == k) for k in range(tmp_K)])
    
    # トピックごとのテーブル数を描画
    ax = axes[0]
    ax.bar(x=np.arange(stop=tmp_K)+1, height=tmp_T_k, 
           color=[cmap(k%color_num) for k in range(tmp_K)]) # 文書数
    ax.set_xlim(xmin=0, xmax=true_K+1)
    ax.set_ylim(ymin=0, ymax=axis_freq_max)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('frequency ($T_k$)')
    ax.set_title('$D = {}, T_d = {}, T = {}, K = {}$'.format(d_i[i]+1, t_i[i]+1, i, tmp_K), loc='left')
    ax.grid()
    
    # トピック分布を生成
    if tmp_K == 0: # (初回の場合)
        tmp_theta_k = np.array([1.0])
    else:
        tmp_theta_k = np.hstack(
            [tmp_T_k / (i + true_alpha)] + [true_alpha / (i + true_alpha)] # 既存・新規の確率を結合
        )
        tmp_theta_k /= tmp_theta_k.sum() # 正規化
    
    # トピック分布を描画
    ax = axes[1]
    ax.bar(x=np.arange(stop=tmp_K+1)+1, height=tmp_theta_k, 
           color=[cmap(k%color_num) for k in range(tmp_K)]+['whitesmoke'], 
           edgecolor='black', linestyle='dashed', linewidth=[0]*tmp_K+[1]) # 確率
    ax.set_xlim(xmin=0, xmax=true_K+2)
    ax.set_ylim(ymin=0, ymax=axis_prob_max)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('probability ($\\theta_k$)')
    ax.set_title('$\\alpha = {}$'.format(true_alpha), loc='left')
    ax.grid()

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=T+1, interval=500)

# 動画を書出
ani.save(
    filename='../figure/ch8/ch8_2_tocip_dist.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i} / {n}')
)

# %%

### 真の単語分布の可視化

# グラフサイズを設定
u = 0.01
axis_size = np.ceil(true_phi_kv.max() /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < K)
col_num = 3

# サブプロットの行数を計算:(1行or1列だとエラー)
row_num = np.ceil(true_K / col_num).astype('int')

# 語彙分布を作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(12, 9), dpi=100, facecolor='white')

for k in range(true_K):
    
    # サブプロットを抽出
    r = k // col_num
    c = k % col_num
    ax = axes[r, c]

    # 分布を描画
    ax.bar(x=np.arange(start=1, stop=V+1), height=true_phi_kv[k], 
           color=cmap(k%color_num)) # 確率
    ax.set_ylim(ymin=0, ymax=axis_size)
    ax.set_xlabel('vocabulary ($v$)')
    ax.set_ylabel('probability ($\phi_{kv}$)')
    ax.set_title('$k = {}$'.format(k+1), loc='left')
    ax.grid()

# 余りを非表示化
for i in range(c+1, col_num):
    axes[r, i].axis('off')

fig.supxlabel('topic ($k$)')
fig.supylabel('topic ($k$)')
fig.suptitle('word distribution (truth)', fontsize=20)
plt.show()

# %%


