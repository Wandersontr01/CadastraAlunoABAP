*&---------------------------------------------------------------------*
*& Report ZCADASTRA_ALUNO_WFG
*&---------------------------------------------------------------------*
*&Cadastra Aluno WFG
*&
*& Wanderson Franca
*& https://www.linkedin.com/in/wandersonfg/
*& 
*& O programa calcula a média de um aluno e cadastra em uma tabela interna 
*&
*&---------------------------------------------------------------------*
REPORT ZCADASTRA_ALUNO_WFG NO STANDARD PAGE HEADING.


INCLUDE: ZCADASTRA_ALUNO_WFG_top,     " Global Data
         ZCADASTRA_ALUNO_WFG_src,     " Screen Fields
         ZCADASTRA_ALUNO_WFG_f01.     " FORM-Routines


*----------------------------------------------------------------------*
*     INICIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM f_inicialization.


*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  AT SELECTION-SCREEN.
  PERFORM f_inicio.