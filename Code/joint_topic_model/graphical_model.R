
# chapter 5.1
# 結合トピックモデル

# グラフィカルモデル表現 ---------------------------------------------------------------

# 利用パッケージ
library(DiagrammeR)
library(DiagrammeRsvg)


# グラフィカルモデルの作図 ------------------------------------------------------------

# 結合トピックモデルのグラフィカルモデルを作図
graph <- DiagrammeR::grViz("
  digraph dot{
    label    = 'joint topic model'
    labelloc = t
    fontsize = 20
    
    graph [rankdir = LR]
    node  [shape = circle, fixedsize = ture, height = 0.6, fontname = 'Times-Italic']
    edge  []
    
    alpha [label = <<B>&alpha;</B>>]
    beta  [label = <<B>&beta;</B>>]
    gamma [label = <<B>&gamma;</B>>]
    
    subgraph cluster_k{
      label    = 'K'
      fontsize = 14
      
      phi [label = <<B>&phi;</B>@_{'k}>]
      psi [label = <<B>&psi;</B>@_{'k}>]
    }
    
    subgraph cluster_d{
      label    = 'D'
      fontsize = 14
      
      theta [label = <<B>&theta;</B>@_{d}>]
      
      subgraph cluster_n{
        label    = 'N@_{d}'
        fontsize = 14
        
        z [label = 'z@_{dn}']
        w [label = 'w@_{dn}', style = filled, filledcolor = gray]
      }
      subgraph cluster_m{
        label    = 'M@_{d}'
        fontsize = 14
        
        y [label = 'y@_{dm}']
        x [label = 'x@_{dm}', style = filled, filledcolor = gray]
      }
      
    }
    
    alpha -> theta -> {z, y};
    z -> w;
    w -> phi -> beta [dir = back];
    y -> x;
    x -> psi -> gamma [dir = back];
  }
")

## ( `{'k}` は書き出し時にφ,ψの添字が重なってしまう対策用の小細工)

# グラフを書出
DiagrammeRsvg::export_svg(gv = graph) |> # svgファイルに変換
  charToRaw() |> 
  rsvg::rsvg(height = 1000) |> # ビットマップに変換
  png::writePNG(target = "figure/graphical_model/joint_topic_model.png", dpi = 100) # pngファイルに変換


