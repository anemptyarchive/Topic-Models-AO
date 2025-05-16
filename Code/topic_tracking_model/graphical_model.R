
# chapter 5.5
# トピック追跡モデル

# グラフィカルモデル表現 ---------------------------------------------------------------

# 利用パッケージ
library(DiagrammeR)
library(DiagrammeRsvg)


# グラフィカルモデルの作図 ------------------------------------------------------------

# トピック追跡モデルのグラフィカルモデルを作図
graph <- DiagrammeR::grViz("
  digraph dot{
    
    graph [rankdir = LR, newrank = true]
    node  [shape = circle, fixedsize = ture, height = 0.6, fontname = 'Times-Italic']
    edge  []
    
    subgraph cluster_t_fin{
      label = ' '
      color = white
      
      theta_fin [label = ' ', shape = none]
      phi_fin   [label = ' ', shape = none]
      
      theta_fin -> phi_fin [penwidth = 0, dir = none];
    }
    
    alpha [label = '&alpha;@_{d}@^{(t)}']
    
    subgraph cluster_d{
      label = 'D'
      
      theta [label = <<B>&theta;</B>@_{d}@^{(t)}>]
      
      subgraph cluster_n{
        label = 'N@_{d}'
        
        z [label = 'z@_{dn}@^{(t)}']
        w [label = 'w@_{dn}@^{(t)}', style = filled, filledcolor = gray]
      }
    }
    
    beta [label = '&beta;@_{k}@^{(t)}']
    
    subgraph cluster_k{
      label     = 'K'
      labeljust = r
      
      phi [label = <<B>&phi;</B>@_{'k}@^{(t)}>]
    }
    
    alpha_old [label = '&alpha;@_{d}@^{(t-1)}']
    
    subgraph cluster_d_old{
      label = 'D'
      
      theta_old [label = <<B>&theta;</B>@_{d}@^{(t-1)}>]
      
      subgraph cluster_n_old{
        label = 'N@_{d}'
        
        z_old [label = 'z@_{dn}@^{(t-1)}']
        w_old [label = 'w@_{dn}@^{(t-1)}', style = filled, filledcolor = gray]
      }
    }
    
    beta_old [label = '&beta;@_{k}@^{(t-1)}']
    
    subgraph cluster_k_old{
      label     = 'K'
      labeljust = r
      
      phi_old [label = <<B>&phi;</B>@_{'k}@^{(t-1)}>]
    }
    
    subgraph cluster_t_init{
      color = white
      
      label    = 'topic tracking model'
      labelloc = c
      fontsize = 20
      
      theta_init [label = ' ', shape = none]
      phi_init   [label = ' ', shape = none]
      
      theta_init -> phi_init [penwidth = 0, dir = none];
    }
    
    {rank = same; w_old; w}
    {rank = same; theta_init; theta_old; theta; theta_fin}
    {rank = same; phi_init; phi_old; phi; phi_fin}
    
    alpha_old -> theta_old -> z_old -> w_old;
    w_old -> phi_old -> beta_old [dir = back];
    
    alpha -> theta -> z -> w;
    w -> phi -> beta [dir = back];
    
    theta_init -> theta_old -> theta -> theta_fin;
    phi_init -> phi_old -> phi -> phi_fin;
  }
")

## ( `{'k}` は書き出し時にφの添字が重なってしまう対策用の小細工)
## ( `_init, _fin` のクラスタは時間軸方向のエッジの長さ調整用の小細工)
## (ダミークラスタの余白が空くので図タイトルをクラスタタイトルで代用)
## ( 変数の定義順と上下の表示順が逆順になるのが謎)

# グラフを書出
DiagrammeRsvg::export_svg(gv = graph) |> # svgファイルに変換
  charToRaw() |> 
  rsvg::rsvg(height = 1500) |> # ビットマップに変換
  png::writePNG(target = "figure/graphical_model/topic_tracking_model.png", dpi = 100) # pngファイルに変換


