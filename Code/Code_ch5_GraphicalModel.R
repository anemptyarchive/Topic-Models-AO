
# Chapter5 トピックモデルの拡張：グラフィカルモデル -----------------------------------------------------

# 利用パッケージ
library(DiagrammeR)


# 5.1 結合トピックモデル -----------------------------------------------------------

# 結合トピックモデル
DiagrammeR::grViz("
  digraph dot{
    graph [rankdir = LR]
    node [shape = circle, fixedsize = ture, fontname = 'Times-Italic']
    
    alpha [label = '&alpha;']

    subgraph cluster_D{
      label = D

      theta [label = <<B>&theta;</B>@_{d}>]
      
      subgraph cluster_N{
        label = 'N@_{d}'

        z [label = 'z@_{dn}']
        w [label = 'w@_{dn}', style = filled, filledcolor = 'gray']
      }
      
      subgraph cluster_M{
        label = 'M@_{d}'
        
        y [label = 'y@_{dm}']
        x [label = 'x@_{dm}', style = filled, filledcolor = 'gray']
      }
    }
    
    beta [label = '&beta;']
    gamma [label = '&gamma;']
    
    subgraph cluster_K{
      label = K
      
      phi [label = <<B>&phi;</B>@_{k}>]
      psi [label = <<B>&psi;</B>@_{k}>]
    }
    
    edge []
      alpha -> theta;
      theta -> z -> w;
      w -> phi[dir = back];
      phi -> beta[dir = back];
      theta -> y -> x;
      x -> psi[dir = back];
      psi -> gamma[dir = back];
  }
")

