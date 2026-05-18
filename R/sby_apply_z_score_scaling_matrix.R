#' Aplicar z-score em matrix double
#'
#' @details
#' A funcao implementa uma unidade interna do fluxo de balanceamento com contrato de entrada explicito e retorno controlado
#' A documentacao descreve a intencao operacional para apoiar manutencao, auditoria e revisao tecnica do pacote
#'
#' @param sby_x_matrix Matriz numerica a ser padronizada
#' @param sby_scaling_info Lista com centros e escalas de padronizacao
#'
#' @return Matriz numerica padronizada por z-score
#' @noRd
sby_apply_z_score_scaling_matrix <- function(sby_x_matrix, sby_scaling_info){
  
  # Normaliza entrada para matriz numerica de precisao dupla
  sby_x_matrix <- sby_adanear_as_numeric_matrix(
    sby_predictor_data = sby_x_matrix
  )

  # Valida parametros de escala contra a quantidade de colunas
  sby_validate_scaling_info(
    sby_scaling_info           = sby_scaling_info,
    sby_predictor_column_count = NCOL(sby_x_matrix)
  )

  # Aplica implementacao nativa quando disponivel
  if(sby_adanear_native_available()){

    # Calcula z-score por chamada nativa registrada no pacote
    sby_scaled <- .Call(
      OU_ApplyZScoreC,
      sby_x_matrix,
      as.numeric(sby_scaling_info$centers),
      as.numeric(sby_scaling_info$scales),
      FALSE
    )
  }else{

    # Fallback puramente em R quando o kernel nativo nao esta disponivel.
    # Quando Rfast esta instalado, usa eachrow (mais rapido em bases grandes);
    # caso contrario, recorre a sweep base.
    if(requireNamespace("Rfast", quietly = TRUE)){
      sby_centered <- Rfast::eachrow(
        x = sby_x_matrix,
        y = sby_scaling_info$centers,
        oper = "-"
      )
      sby_scaled <- Rfast::eachrow(
        x = sby_centered,
        y = sby_scaling_info$scales,
        oper = "/"
      )
    }else{
      sby_centered <- sweep(sby_x_matrix, MARGIN = 2L, STATS = sby_scaling_info$centers, FUN = "-")
      sby_scaled <- sweep(sby_centered, MARGIN = 2L, STATS = sby_scaling_info$scales, FUN = "/")
    }
  }

  # Garante armazenamento numerico double apos o calculo
  storage.mode(sby_scaled) <- "double"

  # Preserva nomes de colunas da matriz original
  colnames(sby_scaled) <- colnames(sby_x_matrix)

  # Retorna matriz padronizada
  return(sby_scaled)
}
####
## Fim
#
