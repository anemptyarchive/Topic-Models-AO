
# chapter 8.2
# 無限次元トピックモデル
# 生成過程

# %%

# 利用ライブラリ
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# %%

### 簡易文書データの作成：(フランチャイズ中華料理店過程)

# 文書数を指定
D = 30

# 語彙数を指定
V = 100

# トピック分布のハイパーパラメータを指定
true_alpha = 4.0

# 語彙分布のハイパーパラメータを指定
true_beta   = 1.0
true_beta_v = np.repeat(true_beta, repeats=V) # 一様なパラメータの場合
#true_beta_v = np.random.uniform(low=1, high=2, size=V) # 多様なパラメータの場合

# テーブル分布のハイパーパラメータを指定
true_gamma = 2.0

# 総テーブル数を初期化
true_T = 0

# トピック数を初期化
true_K = 0

# 文書ごとの各単語の語彙を初期化
w_dic = {}

# 文書ごとの各単語のテーブルを初期化
true_c_dic = {}

# 文書ごとの各テーブルのトピックを初期化
true_z_dic = {}

# 各トピックの割り当てテーブル数を初期化
true_T_k = np.array([])

# 各文書のテーブル数を初期化
true_T_d = np.zeros(shape=D, dtype='int')

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

    # 各単語のテーブルを初期化
    tmp_c_n = np.tile(np.nan, reps=N_d[d])

    # テーブル数を初期化
    tmp_T = 0

    # 各テーブルのトピックを初期化
    tmp_z_t = np.array([])

    # 各テーブルの割り当て単語数を初期化
    tmp_M_t = np.array([])

    for n in range(N_d[d]): # 単語ごと

        # テーブル分布のパラメータを計算
        if tmp_T == 0: # (初回の場合)
            true_tau_t = np.array([1.0])
        else:
            true_tau_t = np.hstack([tmp_M_t, true_gamma]) / (n + true_gamma) # 既存・新規の確率を結合
        
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
        
            # トピック分布のパラメータを計算
            if true_K == 0: # (初回の場合)
                true_theta_k = np.array([1.0])
            else:
                true_theta_k = np.hstack([true_T_k, true_alpha]) / (true_T-1 + true_alpha) # 既存・新規の確率を結合
            
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

                # 語彙分布のパラメータを生成
                if true_K == 1: # (初回の場合)
                    true_phi_kv = np.random.dirichlet(alpha=true_beta_v, size=1)
                else:
                    true_phi_kv = np.vstack(
                        [true_phi_kv, np.random.dirichlet(alpha=true_beta_v, size=1)] # 既存・新規の確率を結合
                    )
            
            # 割当数をカウント
            true_T_k[k] += 1
        
        else: # (既存に割り当てられた場合)
            
            # トピック番号を取得
            k = tmp_z_t[t].astype('int')

        # 割当数をカウント
        tmp_M_t[t] += 1

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
    true_c_dic[d] = tmp_c_n.astype('int').copy()
    true_z_dic[d] = tmp_z_t.astype('int').copy()
    true_T_d[d]   = tmp_T
    
    # 途中経過を表示
    print(f'document: {d+1}, words: {N_d[d]}, tables: {true_T}, topics: {np.unique(true_z_dic[d])}')

# %%

### 作図の準備

# 配色の共通化用のカラーマップを作成
cmap = plt.get_cmap('tab10')

# カラーマップの色数を設定:(カラーマップに応じて固定)
color_num = 10

# %%

### 真のトピック集合の可視化

# 最大テーブル数を取得
max_Td = true_T_d.max()

# 単語数を集計
N_dk = np.zeros(shape=(D, true_K)) # 文書ごとの各トピックの単語数
N_dt = np.zeros(shape=(D, max_Td)) # 文書ごとの各テーブルの単語数
for d in range(D):
    for n in range(N_d[d]):
        t = true_c_dic[d][n]
        k = true_z_dic[d][t]
        N_dk[d, k] += 1
        N_dt[d, t] += 1

# 描画する文書数を指定
#doc_num = D
doc_num = 5

# グラフサイズを設定
u = 5
axis_Ndv_max = (np.ceil(N_dv[:doc_num].max() /u)*u).astype('int') # u単位で切り上げ
axis_Ndk_max = (np.ceil(N_dk[:doc_num].max() /u)*u).astype('int') # u単位で切り上げ
axis_Ndt_max = (np.ceil(N_dt[:doc_num].max() /u)*u).astype('int') # u単位で切り上げ

# 文書データを作図
fig, axes = plt.subplots(nrows=doc_num, ncols=3, constrained_layout=True, 
                         figsize=(30, 25), facecolor='white')

for d in range(doc_num):

    # 各語彙の単語数を集計・各単語のトピックを格納
    tmp_z_mv = np.tile(np.nan, reps=(axis_Ndv_max, V))
    for n in range(N_d[d]):
        v = w_dic[d][n]
        m = np.sum(w_dic[d][:n+1] == v)
        t = true_c_dic[d][n]
        k = true_z_dic[d][t]
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
    ax.set_title(f'$d = {d+1}, N_d = {N_d[d]}, K = {tmp_K}$', loc='left')
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
    ax.set_title(f'$d = {d+1}, T_d = {tmp_T}, K = {tmp_K}$', loc='left')
    ax.grid()

fig.supylabel('document ($d$)')
fig.suptitle('document data (true topic)', fontsize=20)
plt.show()

# %%

### 真のテーブル分布の可視化

# 最大テーブル数を取得
max_Td = true_T_d.max()

# N_d+1番目の単語のテーブル分布のパラメータを計算
true_tau_dt = np.zeros(shape=(D, max_Td+1))
for d in range(D):
    tmp_T   = true_T_d[d]
    tmp_M_t = np.array([np.sum(true_c_dic[d] == t) for t in range(tmp_T)])
    true_tau_dt[d, :tmp_T+1] = np.hstack([tmp_M_t, true_gamma]) / (N_d[d] + true_gamma) # 既存・新規の確率を結合

# 描画する文書数を指定
#doc_num = D
doc_num = 9

# グラフサイズを設定
u = 0.1
axis_size = np.ceil(true_tau_dt[:doc_num].max() /u)*u # u単位で切り上げ

# サブプロットの列数を指定:(1 < 列数 < D+1)
col_num = 3

# サブプロットの行数を計算:(1行or1列だとエラー)
row_num = np.ceil(doc_num / col_num).astype('int')

# テーブル分布を作図
fig, axes = plt.subplots(nrows=row_num, ncols=col_num, constrained_layout=True, 
                         figsize=(24, 18), dpi=100, facecolor='white')

for d in range(doc_num):
    
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
    ax.set_xlim(xmin=0.5, xmax=max_Td+1.5)
    ax.set_ylim(ymin=0, ymax=axis_size)
    ax.set_xlabel('table ($t$)')
    ax.set_ylabel('probability ($\\tau_{dt}$)')
    ax.set_title(f'$d = {d+1}, \gamma = {true_gamma}, T_d = {tmp_T}$', loc='left')
    ax.grid()

# 残りのサブプロットを非表示
for c in range(c+1, col_num):
    axes[r, c].axis('off')

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

# 描画する文書数を指定
#doc_num = D
doc_num = 5

# グラフサイズを設定
u = 5
axis_freq_max = np.ceil(max([np.sum(true_c_dic[d] == t) for t in range(true_T_d[d]) for d in range(doc_num)]) /u)*u
axis_prob_max = 1.0

# グラフオブジェクトを初期化
fig, axes = plt.subplots(nrows=doc_num, ncols=2, constrained_layout=True, 
                         figsize=(16, 30), dpi=100, facecolor='white')
fig.suptitle('table distribution (truth)', fontsize=20)

# 作図処理を定義
def update(n):
    
    for d in range(doc_num):
        if n <= N_d[d]: # (単語数に達すればそのまま)
            
            # 前フレームのグラフを初期化
            [ax.cla() for ax in axes[d]]

            # 各単語のテーブルを取得
            tmp_c_n = true_c_dic[d][:n]

            # テーブル数を取得
            if n == 0: # (初回の場合)
                tmp_T = 0
            else:
                tmp_T = tmp_c_n.max() + 1

            # 各テーブルの単語数を集計
            tmp_M_t = np.array([np.sum(tmp_c_n == t) for t in range(tmp_T)])

            # 各テーブルのトピックを取得
            tmp_z_t = true_z_dic[d][:tmp_T]

            # トピックの割当を描画
            ax = axes[d, 0]
            ax.bar(x=np.arange(stop=tmp_T)+1, height=tmp_M_t, 
                               color=[cmap(k%color_num) for k in tmp_z_t]) # 単語数
            ax.set_xlim(xmin=0.5, xmax=max_Td+0.5)
            ax.set_ylim(ymin=0, ymax=axis_freq_max)
            ax.set_xlabel('table ($t$)')
            ax.set_ylabel('frequency ($N_{dt}$)')
            ax.set_title(f'$d = {d+1}, N_d = {n+1}$', loc='left')
            ax.grid()

            # テーブル分布を計算
            if tmp_T == 0: # (初回の場合)
                tmp_tau_t = np.array([1.0])
            else:
                tmp_tau_t = np.hstack([tmp_M_t, true_gamma]) / (n + true_gamma) # 既存・新規の確率を結合
            
            # テーブル分布を描画
            ax = axes[d, 1]
            ax.bar(x=np.arange(stop=tmp_T+1)+1, height=tmp_tau_t, 
                   color=[cmap(k%color_num) for k in tmp_z_t]+['whitesmoke'], 
                   edgecolor='black', linestyle='dashed', linewidth=[0]*tmp_T+[1]) # 確率
            ax.set_xlim(xmin=0.5, xmax=max_Td+1.5)
            ax.set_ylim(ymin=0, ymax=axis_prob_max)
            ax.set_xlabel('table ($t$)')
            ax.set_ylabel('probability ($\\tau_{dt}$)')
            ax.set_title(f'$d = {d+1}, \gamma = {true_gamma}, T_d = {tmp_T}$', loc='left')
            ax.grid()

# 動画を作成
ani = FuncAnimation(fig=fig, func=update, frames=max_Nd, interval=100)

# 動画を書出
ani.save(
    filename='../figure/ch8/ch8_2_true_table_dist.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i+1} / {n}')
)

# %%

### 真のトピック分布の可視化

# 最大テーブル数を取得
max_Td = true_T_d.max()

# T+1番目のテーブルのトピック分布のパラメータを計算
true_theta_k = np.hstack([true_T_k, true_alpha]) / (true_T + true_alpha) # 既存・新規の確率を結合

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
ax.set_title(f'$\\alpha = {true_alpha}, K = {true_K}$', loc='left')
fig.suptitle('topic distribution (truth)', fontsize=20)
ax.grid()
plt.show()

# %%

### 真のトピック分布の推移の可視化

# 配列に変換
z_i = np.hstack([true_z_dic[d] for d in range(D)]) # (初期値に対応する要素を含まない)
d_i = np.hstack([-1] + [np.repeat(d, repeats=len(true_z_dic[d])) for d in range(D)])
t_i = np.hstack([-1] + [np.arange(stop=len(true_z_dic[d])) for d in range(D)])

# 総テーブル数を取得
T = true_T_d.sum()

# グラフサイズを設定
u = 5
axis_freq_max = np.ceil(true_T_k.max() /u)*u # u単位で切り上げ
axis_prob_max = 1.0

# グラフオブジェクトを初期化
fig, axes = plt.subplots(nrows=1, ncols=2, constrained_layout=True, 
                         figsize=(16, 6), dpi=100, facecolor='white')
fig.suptitle('topic distribution (truth)', fontsize=20)

# 作図処理を定義
def update(i):
    
    # 前フレームのグラフを初期化
    [ax.cla() for ax in axes]

    # トピック数を取得
    if i == 0: # (初回の場合)
        tmp_K = 0
    else:
        tmp_K = z_i[:i].max().astype('int') + 1
    
    # 各トピックのテーブル数を集計
    tmp_T_k = np.array([np.sum(z_i[:i] == k) for k in range(tmp_K)])
    
    # トピックの割当を描画
    ax = axes[0]
    ax.bar(x=np.arange(stop=tmp_K)+1, height=tmp_T_k, 
           color=[cmap(k%color_num) for k in range(tmp_K)]) # テーブル数
    ax.set_xlim(xmin=0.5, xmax=true_K+0.5)
    ax.set_ylim(ymin=0, ymax=axis_freq_max)
    ax.set_xlabel('topic ($k$)')
    ax.set_ylabel('frequency ($T_k$)')
    ax.set_title(f'$D = {d_i[i]+1}, T_d = {t_i[i]+1}, T = {i}$', loc='left')
    ax.grid()
    
    # トピック分布のパラメータを計算
    if tmp_K == 0: # (初回の場合)
        tmp_theta_k = np.array([1.0])
    else:
        tmp_theta_k = np.hstack([tmp_T_k, true_alpha]) / (i + true_alpha) # 既存・新規の確率を結合
    
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
ani = FuncAnimation(fig=fig, func=update, frames=T+1, interval=500)

# 動画を書出
ani.save(
    filename='../figure/ch8/ch8_2_topic_dist.mp4', 
    progress_callback = lambda i, n: print(f'frame: {i+1} / {n}')
)

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
    
    # サブプロットを抽出
    r = k // col_num
    c = k % col_num
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


