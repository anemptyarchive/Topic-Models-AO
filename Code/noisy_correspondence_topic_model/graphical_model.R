
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
    graph [rankdir = LR, newrank = true, 
           label = 'noisy correspondence topic model', labelloc = 't', fontsize = 20]
    node  [shape = circle, fixedsize = ture, fontname = 'Times-Italic']
    edge  []
    
    alpha [label = <<B>&alpha;</B>>]
    beta  [label = <<B>&beta;</B>>]
    gamma [label = <<B>&gamma;</B>>]
    eta   [label = <<B>&eta;</B>>]
    
    lambda [label = '&lambda;']
    
    subgraph cluster_d{
      label = 'D'
      
      theta [label = <<B>&theta;</B>@_{d}>]
      
      subgraph cluster_n{
        label = 'N@_{d}'
        
        z [label = 'z@_{dn}']
        w [label = 'w@_{dn}', style = filled, filledcolor = 'gray']
      }
      
      subgraph cluster_m{
        label = 'M@_{d}'
        
        y [label = 'y@_{dm}']
        r [label = 'r@_{dm}']
        x [label = 'x@_{dm}', style = filled, filledcolor = 'gray']
      }
    }
    
    subgraph cluster_k{
      label = 'K'
      
      phi [label = <<B>&phi;</B>@_{'k}>]
    }
    
    subgraph cluster_k1{
      label = 'K+1'
      
      psi [label = <<B>&psi;</B>@_{'k}>]
    }
    
    {rank = same; z; y; r}
    
    alpha -> theta -> z -> {w, y};
    w -> phi[dir = back];
    phi -> beta[dir = back];
    eta -> lambda -> r;
    {y, r} -> x;
    x -> psi[dir = back];
    psi -> gamma[dir = back];
  }
")

## ( `{'k}` は書き出し時にφ,ψの添字が重なってしまう対策用の小細工)

# グラフを書出
DiagrammeRsvg::export_svg(gv = graph) |> # svgファイルに変換
  charToRaw() |> 
  rsvg::rsvg(height = 2000) |> # ビットマップに変換
  png::writePNG(target = "figure/graphical_model/noisy_correspondence_topic_model.png", dpi = 100) # pngファイルに変換


