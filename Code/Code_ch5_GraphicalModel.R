
# Chapter5 トピックモデルの拡張：グラフィカルモデル -----------------------------------------------------

# 利用パッケージ
library(DiagrammeR)


# ch5.1 結合トピックモデル -----------------------------------------------------------

# 結合トピックモデル
DiagrammeR::grViz("
  digraph JointTM{
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
      alpha -> theta -> {z, y};
      z -> w;
      w -> phi[dir = back];
      phi -> beta[dir = back];
      y -> x;
      x -> psi[dir = back];
      psi -> gamma[dir = back];
  }
")


# ch5.2 対応トピックモデル -----------------------------------------------------------

# 対応トピックモデル
DiagrammeR::grViz("
  digraph CorrespondenceTM{
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
      alpha -> theta -> z;
      z -> {w, y};
      w -> phi[dir = back];
      phi -> beta[dir = back];
      y -> x;
      x -> psi[dir = back];
      psi -> gamma[dir = back];
  }
")


# ch5.3 ノイズあり対応トピックモデル -----------------------------------------------------------

# ノイズあり対応トピックモデル
DiagrammeR::grViz("
  digraph NoisyCorrTM{
    graph [rankdir = LR]
    node [shape = circle, fixedsize = ture, fontname = 'Times-Italic']
    
    alpha [label = '&alpha;']
    eta [label = '&eta;']
    lambda [label = '&lambda;']
    {rank = same; alpha; lambda}
    
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
        r [label = 'r@_{dm}']
        x [label = 'x@_{dm}', style = filled, filledcolor = 'gray']
      }
    }
    
    beta [label = '&beta;']
    gamma [label = '&gamma;']
    {rank = same; beta; gamma}
    
    subgraph cluster_K{
      label = K
      
      phi [label = <<B>&phi;</B>@_{k}>]
    }
    
    subgraph cluster_K1{
      label = 'K+1'
      
      psi [label = <<B>&psi;</B>@_{k}>]
    }
    
    edge []
      alpha -> theta -> z;
      z -> {w, y};
      w -> phi[dir = back];
      phi -> beta[dir = back];
      eta -> lambda -> r;
      {y, r} -> x;
      x -> psi[dir = back];
      psi -> gamma[dir = back];
  }
")


