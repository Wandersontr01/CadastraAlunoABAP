*&---------------------------------------------------------------------*
*& Include          ZCADASTRA_ALUNO_WFG_SRC
*&---------------------------------------------------------------------*



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
  SELECTION-SCREEN SKIP 1.
    PARAMETERS: p_matri       TYPE c LENGTH 8,
              p_nome        TYPE string,
              p_nota1       TYPE ztbaluno_wfg-nota1,
              p_nota2       TYPE ztbaluno_wfg-nota2,
              p_nota3       TYPE ztbaluno_wfg-nota3.

  SELECTION-SCREEN SKIP 1.

  SELECTION-SCREEN: PUSHBUTTON /01(10) btn1 USER-COMMAND bt1,
                    PUSHBUTTON 15(10) btn2 USER-COMMAND bt2,
                    PUSHBUTTON 30(10) btn3 USER-COMMAND bt3.

SELECTION-SCREEN END OF BLOCK b1.