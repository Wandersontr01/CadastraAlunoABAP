*&---------------------------------------------------------------------*
*& Include          ZCADASTRA_ALUNO_WFG_TOP
*&---------------------------------------------------------------------*

DATA: gv_cont         TYPE i,
      gt_fieldcat     TYPE slis_t_fieldcat_alv,
      gt_aluno        TYPE ztbaluno_wfg,
      gt_alunos       TYPE TABLE OF ztbaluno_wfg.