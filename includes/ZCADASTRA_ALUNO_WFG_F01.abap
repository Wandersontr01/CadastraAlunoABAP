*&---------------------------------------------------------------------*
*& Include          ZCADASTRA_ALUNO_WFG_F01
*&---------------------------------------------------------------------*

FORM f_inicio.

  IF sy-ucomm EQ 'BT1'.
    PERFORM f_cadastrar.
  ELSEIF sy-ucomm EQ 'BT2'.
    PERFORM f_consultar.
  ELSEIF sy-ucomm EQ 'BT3'.
    PERFORM f_deletar.
  ELSE.
    MESSAGE: 'Escolha uma opção!' TYPE 'I' DISPLAY LIKE 'E'.
  ENDIF.


ENDFORM. "f_inicio

FORM f_fazconsuta USING     VALUE(x_nom) TYPE any
                            VALUE(x_mat) TYPE any
                  CHANGING  VALUE(x_ret) LIKE gt_alunos.

  IF p_nome EQ '' AND p_matri EQ ''. "Se os parametros de pesquisa estiverem vazios
    "puxa TODOS os dados da tabela
    SELECT *
      FROM ztbaluno_wfg
      INTO TABLE x_ret.

  ELSEIF p_nome NE '' AND p_matri EQ ''. "SENAO pega apenas os dados informados nos parametros (pesquisar sobre select like)
    SELECT *
      FROM ztbaluno_wfg
      INTO TABLE @x_ret
      WHERE nome LIKE @x_nom.

  ELSE.
    SELECT *
      FROM ztbaluno_wfg
      INTO TABLE @x_ret
      WHERE matricula LIKE @x_mat.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&                          CONSULTAR ALUNO
*&---------------------------------------------------------------------*
FORM f_consultar.
  DATA: lv_nomeconca  TYPE string,
        lv_matriconca TYPE string.

  CONCATENATE '%' p_nome '%' INTO lv_nomeconca.
  CONCATENATE '%' p_matri '%' INTO lv_matriconca.

  PERFORM f_fazconsuta USING lv_nomeconca lv_matriconca CHANGING gt_alunos.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE: 'Nenhum aluno encontrado' TYPE 'I' DISPLAY LIKE 'I'.
  ELSE.
    PERFORM f_create_fcatmanual.
    CLEAR gt_alunos.
  ENDIF.

ENDFORM. "f_consultar

*&---------------------------------------------------------------------*
*&                          CADASTRAR ALUNO
*&---------------------------------------------------------------------*
FORM f_cadastrar.
  TRY.
      DATA: lo_aluno TYPE REF TO zcl_aluno_wfg.

      IF p_nome EQ '' OR p_matri EQ '' OR p_nota1 EQ '' OR p_nota2 EQ '' OR p_nota3 EQ ''.
        MESSAGE: 'Preencha TODOS os campos' TYPE 'I' DISPLAY LIKE 'E'.

      ELSE.

        "Colocando o valor da matricula em uma variavel srring pois estava dando erro ao criar o objeto
        DATA lv_matr TYPE string.
        lv_matr = p_matri.

*******Criar Objeto do ALUNO
        lo_aluno = NEW zcl_aluno_wfg(
          i_matricula = lv_matr
          i_nota1     = p_nota1
          i_nota2     = p_nota2
          i_nota3     = p_nota3
          i_nome      = p_nome
        ).

********ADICIONA ALUNO A TABELA INTERNA
        gt_aluno-matricula = lo_aluno->get_matricula( ).
        gt_aluno-nome = lo_aluno->get_nome( ).
        gt_aluno-nota1 = lo_aluno->get_nota1( ).
        gt_aluno-nota2 = lo_aluno->get_nota2( ).
        gt_aluno-nota3 = lo_aluno->get_nota3( ).
        gt_aluno-media = lo_aluno->get_media( ).
        gt_aluno-situacao = lo_aluno->get_situacao( ).
        gt_aluno-usrcriacao = sy-uname.
        gt_aluno-dtcriacao = sy-datum.
        gt_aluno-hrcriacao = sy-uzeit.

        INSERT gt_aluno INTO TABLE gt_alunos.

        INSERT ztbaluno_wfg FROM TABLE gt_alunos.


        IF sy-subrc IS INITIAL.
          COMMIT WORK.

        ELSE.
          ROLLBACK WORK.

        ENDIF.

        MESSAGE: 'Aluno Cadastrado com Sucesso!' TYPE 'I' DISPLAY LIKE 'S'.
        CLEAR gt_alunos.

      ENDIF.

    CATCH cx_sy_open_sql_db.
      MESSAGE: 'Essa matricula já existe' TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.

  CLEAR: p_nome, p_matri, p_nota1, p_nota2, p_nota3.

ENDFORM. "f_cadastrar


*&---------------------------------------------------------------------*
*&                          DELETAR ALUNO
*&---------------------------------------------------------------------*
FORM f_deletar.
  "Consultar a matricula do aluno
  DATA lt_aluno TYPE TABLE OF ztbaluno_wfg.

  IF p_nome EQ '' AND p_matri EQ ''. "Se os parametros de pesquisa estiverem vazios
    MESSAGE: 'Preencha o nome ou matricula para Deletar' TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    PERFORM f_fazconsuta USING      p_nome
                                    p_matri
                         CHANGING   lt_aluno.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE: 'Nenhum aluno encontrado' TYPE 'I' DISPLAY LIKE 'I'.
    ELSE.
      DELETE FROM ztbaluno_wfg
        WHERE matricula EQ p_matri
           OR nome      EQ p_nome.

      MESSAGE: 'Aluno Deletado com Sucesso!' TYPE 'I' DISPLAY LIKE 'S'.
    ENDIF.

    CLEAR: p_nome, p_matri, p_nota1, p_nota2, p_nota3.

  ENDIF.
ENDFORM. "f_deletar




*&---------------------------------------------------------------------*
*&                          ALV - FCATMANUAL
*&---------------------------------------------------------------------*
FORM f_create_fcatmanual.
  gt_fieldcat = VALUE slis_t_fieldcat_alv(
                                           (  fieldname = 'matricula'   outputlen = 15 reptext_ddic = 'Matricula'  )
                                           (  fieldname = 'NOME'        outputlen = 30 reptext_ddic = 'Nome Aluno' )
                                           (  fieldname = 'nota1'       outputlen = 6  reptext_ddic = 'Nota 1'     )
                                           (  fieldname = 'nota2'       outputlen = 6  reptext_ddic = 'Nota 2'     )
                                           (  fieldname = 'nota3'       outputlen = 6  reptext_ddic = 'Nota 3'     )
                                           (  fieldname = 'media'       outputlen = 5  reptext_ddic = 'Média'      )
                                           (  fieldname = 'Situacao'    outputlen = 10 reptext_ddic = 'Situação'   )
                                           (  fieldname = 'usrcriacao'  outputlen = 9  reptext_ddic = 'Usuario'   )
                                           (  fieldname = 'dtcriacao'   outputlen = 9  reptext_ddic = 'Data Criação'   )
                                           (  fieldname = 'hrcriacao'   outputlen = 9  reptext_ddic = 'Hora Criação'   )
                                           ).

  PERFORM f_display_alv.

ENDFORM. "f_create_fcatmanual


*&---------------------------------------------------------------------*
*&                           ALV - DISPLAY ALV
*&---------------------------------------------------------------------*
FORM f_display_alv.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = gt_fieldcat
    TABLES
      t_outtab           = gt_alunos "tabela interna com os dados extraidos
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM. "f_display_alv


FORM f_inicialization .

  btn1 = 'Cadastrar'.
  btn2 = |Consultar|.
  btn3 = |Deletar|.

ENDFORM.