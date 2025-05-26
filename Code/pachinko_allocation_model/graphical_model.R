
# chapter 6.2
# パチンコ配分モデル

# グラフィカルモデル表現 ---------------------------------------------------------------

# 利用パッケージ
library(DiagrammeR)
library(DiagrammeRsvg)


# グラフィカルモデルの作図 ------------------------------------------------------------

### ・2階層の場合 -----

# パチンコ配分モデルのグラフィカルモデルを作図
graph <- DiagrammeR::grViz("
  digraph dot{
    label    = 'pachinko allocation model'
    labelloc = t
    fontsize = 20
    
    graph [rankdir = LR, newrank = true]
    node  [shape = circle, fixedsize = ture, height = 0.6, fontname = 'Times-Italic']
    edge  []
    
    subgraph cluster_d{
      label     = 'D'
      labeljust = r
      fontsize  = 14
      
      subgraph cluster_0_dummy{
        label    = ''
        color = white
        
        theta_d [label = <<B>&theta;</B>@_{d}>]
      }
      
      subgraph cluster_s_param{
        label    = 'S'
        fontsize = 14
        
        theta_ds [label = <<B>&theta;</B>@_{ds}>]
      }
      
      subgraph cluster_n{
        label    = 'N@_{d}'
        fontsize = 14
        
        y [label = 'y@_{dn}']
        z [label = 'z@_{dn}']
        w [label = 'w@_{dn}', style = filled, filledcolor = gray]
      }
    }
    
    alpha_0 [label = <<B>&alpha;</B>@_{0}>]
    
    subgraph cluster_s_hyparam{
      label    = 'S'
      fontsize = 14
      
      alpha_s [label = <<B>&alpha;</B>@_{s}>]
    }
    
    beta [label = <<B>&beta;</B>>]
    
    subgraph cluster_k{
      label    = 'K'
      fontsize = 14
      
      phi [label = <<B>&phi;</B>@_{'k}>]
    }
    
    {rank = same; alpha_0; theta_d; y}
    {rank = same; alpha_s; theta_ds; z}
    
    alpha_0 -> theta_d -> y -> z -> w;
    alpha_s -> theta_ds -> z;
    w -> phi -> beta [dir = back];
  }
")

## ( `{'k}` は書き出し時にφの添字が重なってしまう対策用の小細工)
## (ダミークラスタはθdの配置の調整用の小細工)

# グラフを書出
graph |> 
  DiagrammeRsvg::export_svg(gv = _) |> # svgファイルに変換
  charToRaw() |> 
  rsvg::rsvg(height = 1500) |> # ビットマップに変換
  png::writePNG(target = "figure/graphical_model/pachinko_allocation_model.png", dpi = 100) # pngファイルに変換
