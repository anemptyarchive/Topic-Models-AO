
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


# ch5.4 著者トピックモデル ---------------------------------------------------------

# 著者トピックモデル
DiagrammeR::grViz("
  digraph AuthorTM{
    graph [rankdir = LR]
    node [shape = circle, fixedsize = ture, fontname = 'Times-Italic']
    
    alpha [label = '&alpha;']
    subgraph cluster_S{
      label = S
      theta [label = <<B>&theta;</B>@_{s}>]
    }
    
    beta [label = '&beta;']
    subgraph cluster_K{
      label = K
      phi [label = <<B>&phi;</B>@_{k}>]
    }
    
    subgraph cluster_D{
      label = D
      a [label = a, style = filled, filledcolor = 'gray']
      subgraph cluster_N{
        label = N
        y [label = 'y@_{dn}']
        z [label = 'z@_{dn}']
        w [label = 'w@_{dn}', style = filled, filledcolor = 'gray']
      }
    }
    
    edge []
      alpha -> theta -> z;
      phi -> beta [dir = back];
      w -> phi [dir = back];
      a -> y -> z -> w;
  }
")


# ch5.5 トピック追跡モデル ---------------------------------------------------------

# トピック追跡モデル
DiagrammeR::grViz("
  digraph TrackingTM{
    graph [rankdir = LR]
    node [shape = circle, fixedsize = ture, fontname = 'Times-Italic']
    
    alpha_0 [label = <<B>&alpha;</B>@^{(t-1)}>]
    subgraph cluster_D_0{
      label = D
      theta_0 [label = <<B>&theta;</B>@_{d}@^{(t-1)}>]
      subgraph cluster_N_0{
        label = 'N@_{d}'
        z_0 [label = 'z@_{dn}@^{(t-1)}']
        w_0 [label = 'w@_{dn}@^{(t-1)}', style = filled, filledcolor = 'gray']
      }
    }
    
    beta_0 [label = <<B>&beta;</B>@^{(t-1)}>]
    subgraph cluster_K_0{
      label = K
      phi_0 [label = <<B>&phi;</B>@_{k}@^{(t-1)}>]
    }
    
    alpha [label = <<B>&alpha;</B>@^{(t)}>]
    subgraph cluster_D{
      label = D
      theta [label = <<B>&theta;</B>@_{d}@^{(t)}>]
      subgraph cluster_N{
        label = 'N@_{d}'
        z [label = 'z@_{dn}@^{(t)}']
        w [label = 'w@_{dn}@^{(t)}', style = filled, filledcolor = 'gray']
      }
    }
    
    beta [label = <<B>&beta;</B>@^{(t)}>]
    subgraph cluster_K{
      label = K
      phi [label = <<B>&phi;</B>@_{k}@^{(t)}>]
    }
    
    edge []
      alpha -> theta -> z -> w;
      phi -> beta [dir = back];
      w -> phi [dir = back];
      
      alpha_0 -> theta_0 -> z_0 -> w_0;
      phi_0 -> beta_0 [dir = back];
      w_0 -> phi_0 [dir = back];
      
      theta_0 -> theta;
      phi_0 -> phi;
  }
")


