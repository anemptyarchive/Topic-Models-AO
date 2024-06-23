
# chapter 5.4
# 著者トピックモデル

# グラフィカルモデル表現 ---------------------------------------------------------------

# 利用パッケージ
library(DiagrammeR)
library(DiagrammeRsvg)


# グラフィカルモデルの作図 ------------------------------------------------------------

# 著者トピックモデルのグラフィカルモデルを作図
graph <- DiagrammeR::grViz("
  digraph dot{
    graph [rankdir = LR, 
           label = 'author topic model', labelloc = 't', fontsize = 20]
    node  [shape = circle, fixedsize = ture, fontname = 'Times-Italic']
    edge  []
    
    alpha [label = <<B>&alpha;</B>>]
    beta  [label = <<B>&beta;</B>>]
    
    subgraph cluster_s{
      label = 'S'
      
      theta [label = <<B>&theta;</B>@_{s}>]
    }
    
    subgraph cluster_d{
      label = 'D'
      
      a [label = 'a@_{d}', style = filled, filledcolor = 'gray']
      
      subgraph cluster_n{
        label = 'N@_{d}'
        
        y [label = 'y@_{dn}']
        z [label = 'z@_{dn}']
        w [label = 'w@_{dn}', style = filled, filledcolor = 'gray']
      }
    }
    
    subgraph cluster_k{
      label = 'K'
      
      phi [label = <<B>&phi;</B>@_{'k}>]
    }
    
    alpha -> theta;
    a -> y
    {theta, y} -> z -> w;
    w -> phi [dir = back];
    phi -> beta [dir = back];
  }
")

## ( `{'k}` は書き出し時にφ,ψの添字が重なってしまう対策用の小細工)

# グラフを書出
DiagrammeRsvg::export_svg(gv = graph) |> # svgファイルに変換
  charToRaw() |> 
  rsvg::rsvg(height = 1000) |> # ビットマップに変換
  png::writePNG(target = "figure/graphical_model/author_topic_model.png", dpi = 100) # pngファイルに変換


