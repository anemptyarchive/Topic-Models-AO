
# chapter 5.3
# ノイズあり対応トピックモデル

# グラフィカルモデル表現 ---------------------------------------------------------------

# 利用パッケージ
library(DiagrammeR)
library(DiagrammeRsvg)


# グラフィカルモデルの作図 ------------------------------------------------------------

# ノイズあり対応トピックモデルのグラフィカルモデルを作図
graph <- DiagrammeR::grViz("
  digraph dot{
    label    = 'noisy correspondence topic model'
    labelloc = t
    fontsize = 20
    
    graph [rankdir = LR, newrank = true]
    node  [shape = circle, fixedsize = ture, height = 0.6, fontname = 'Times-Italic']
    edge  []
    
    alpha [label = <<B>&alpha;</B>>]
    beta  [label = <<B>&beta;</B>>]
    gamma [label = <<B>&gamma;</B>>]
    eta   [label = '&eta;']
    
    lambda [label = '&lambda;']
    
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
        r [label = 'r@_{dm}']
        x [label = 'x@_{dm}', style = filled, filledcolor = gray]
      }
    }
    
    subgraph cluster_k{
      label    = 'K'
      fontsize = 14
      
      phi [label = <<B>&phi;</B>@_{'k}>]
    }
    
    subgraph cluster_k1{
      label    = 'K+1'
      fontsize = 14
      
      psi [label = <<B>&psi;</B>@_{'k}>]
    }
    
    {rank = same; z; y; r}
    
    alpha -> theta -> z -> {w, y};
    eta -> lambda -> r;
    {y, r} -> x;
    w -> phi -> beta [dir = back];
    x -> psi -> gamma [dir = back];
  }
")

## ( `{'k}` は書き出し時にφ,ψの添字が重なってしまう対策用の小細工)

# グラフを書出
DiagrammeRsvg::export_svg(gv = graph) |> # svgファイルに変換
  charToRaw() |> 
  rsvg::rsvg(height = 2000) |> # ビットマップに変換
  png::writePNG(target = "figure/graphical_model/noisy_correspondence_topic_model.png", dpi = 100) # pngファイルに変換


