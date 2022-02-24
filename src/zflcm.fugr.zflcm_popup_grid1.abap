FUNCTION ZFLCM_POPUP_GRID1.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(FULL_DEMO) DEFAULT SPACE
*"     REFERENCE(PART_DEMO) DEFAULT SPACE
*"     REFERENCE(ROW_SELECTION_MODE) OPTIONAL
*"     REFERENCE(HIDE_TOOLBAR) DEFAULT SPACE
*"     REFERENCE(START_POS_X) OPTIONAL
*"     REFERENCE(START_POS_Y) OPTIONAL
*"     REFERENCE(END_POS_X) OPTIONAL
*"     REFERENCE(END_POS_Y) OPTIONAL
*"     REFERENCE(WINDOW_TITLE) OPTIONAL
*"     REFERENCE(GRID_TITLE) OPTIONAL
*"     REFERENCE(HEADER1) OPTIONAL
*"     REFERENCE(HEADER2) OPTIONAL
*"     REFERENCE(HEADER3) OPTIONAL
*"     REFERENCE(HEADER4) OPTIONAL
*"     REFERENCE(HEADER5) OPTIONAL
*"     REFERENCE(TRAILER1) OPTIONAL
*"     REFERENCE(TRAILER2) OPTIONAL
*"     REFERENCE(TRAILER3) OPTIONAL
*"     REFERENCE(TRAILER4) OPTIONAL
*"     REFERENCE(TRAILER5) OPTIONAL
*"     REFERENCE(BUTTON1) OPTIONAL
*"     REFERENCE(BUTTON2) OPTIONAL
*"     REFERENCE(BUTTON3) OPTIONAL
*"     REFERENCE(BUTTON4) OPTIONAL
*"     REFERENCE(BUTTON5) OPTIONAL
*"     REFERENCE(IS_LAYOUT) TYPE  LVC_S_LAYO OPTIONAL
*"     REFERENCE(IT_CELL_SET) TYPE  LVC_T_CELL OPTIONAL
*"     REFERENCE(IT_ROW_SET) TYPE  LVC_T_ROW OPTIONAL
*"     REFERENCE(IT_COLUMN_SET) TYPE  LVC_T_COL OPTIONAL
*"  EXPORTING
*"     VALUE(RETURN_CODE)
*"     VALUE(IT_SELECTED_CELL) TYPE  LVC_T_CELL
*"     VALUE(IT_SELECTED_ROW) TYPE  LVC_T_ROW
*"     VALUE(IT_SELECTED_COLUMN) TYPE  LVC_T_COL
*"     VALUE(SELECTED_CELL_VALUE)
*"  TABLES
*"      IT_FIELDCAT OPTIONAL
*"      IT_DATA OPTIONAL
*"      IT_SORT TYPE  LVC_T_SORT OPTIONAL
*"      IT_TOP_OF_PAGE OPTIONAL
*"      IT_END_OF_PAGE OPTIONAL
*"      IT_TOP_OF_LIST OPTIONAL
*"      IT_END_OF_LIST OPTIONAL
*"      IT_TOOLBAR_EXCLUDING TYPE  UI_FUNCTIONS OPTIONAL
*"      IT_TOOLBAR_ADDING STRUCTURE  STB_BUTTON OPTIONAL
*"      IT_CALLBACK_FORM STRUCTURE  ZVU_CALLBACK_FORM_GRID OPTIONAL
*"  EXCEPTIONS
*"      EMPTY_TABLE_PASSED
*"      AT_LEAST_ONE_BUTTON_REQUIRED
*"--------------------------------------------------------------------

************************************************************************
*                      Canadian National
*
* Function Module Name: ZVU_POPUP_GRID1
* Function Group  Name: ZVU_GRID
* Functional design: DEV-ENH-4B1
* Created by: Steven Qiu
* Created on: 2002-12-17
* Version:    1.0
*
* Purpose: Create a custom pop up window with standard SAP grid
*          functionality in it.
*          Demo program YZVU_POPUP_GRID_DEMO shows different
*          functionalities of this custom pop up function.
*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------    ----------          ---------------       *
* Steven Qiu                 2002-12-17          DV3K926317            *
*                                                                      *
* Short Description: Initial creation                                  *
*                                                                      *
*----------------------------------------------------------------------*

  INCLUDE <ICON>.

  CLEAR V_FULL_DEMO_FLAG.

  REFRESH: I_DATA_DEMO, I_FIELDCAT_DEMO.

  CLEAR V_WINDOW_TITLE.

  IF FULL_DEMO <> SPACE.
    V_FULL_DEMO_FLAG = 'X'.
    PERFORM F_CREATE_DEMO_DATA.
    PERFORM F_POPULATE_DEMO_ELEMENT.
  ELSEIF PART_DEMO <> SPACE.
    PERFORM F_CREATE_DEMO_DATA.
  ELSE.
    ASSIGN IT_DATA[]     TO <I_DATA>.
    ASSIGN IT_FIELDCAT[] TO <I_FIELDCAT>.
  ENDIF.

  IF FULL_DEMO IS INITIAL.

* Fill Layout if specified
    GS_LAYOUT = IS_LAYOUT.

* Window position
    V_START_X = START_POS_X.
    V_START_Y = START_POS_Y.
    V_END_X   = END_POS_X.
    V_END_Y   = END_POS_Y.

    IF V_START_X IS INITIAL AND
       V_START_Y IS INITIAL AND
       V_END_X IS INITIAL AND
       V_END_Y IS INITIAL.

* Set default
      V_START_X = 10.
      V_START_Y = 1.
      V_END_X   = 100.
      V_END_Y   = 28.
    ENDIF.

* Window title
    IF NOT WINDOW_TITLE IS INITIAL.
      V_WINDOW_TITLE = WINDOW_TITLE.
    ENDIF.

* Grid title
    IF NOT GRID_TITLE IS INITIAL.
      GS_LAYOUT-GRID_TITLE = GRID_TITLE.
    ENDIF.

* Row selection
    IF ROW_SELECTION_MODE <> SPACE.
      IF ROW_SELECTION_MODE = 'A' OR
         ROW_SELECTION_MODE = 'B' OR
         ROW_SELECTION_MODE = 'C' OR
         ROW_SELECTION_MODE = 'D'.
        GS_LAYOUT-SEL_MODE = ROW_SELECTION_MODE.
      ELSE.
        GS_LAYOUT-SEL_MODE = 'A'.
      ENDIF.
    ELSE.
      GS_LAYOUT-SEL_MODE = SPACE.
    ENDIF.

* Hide toolbar
    IF HIDE_TOOLBAR <> SPACE.
      GS_LAYOUT-NO_TOOLBAR = 'X'.
    ENDIF.

* Header
    V_HEADER1 = HEADER1.
    V_HEADER2 = HEADER2.
    V_HEADER3 = HEADER3.
    V_HEADER4 = HEADER4.
    V_HEADER5 = HEADER5.

* Trailer
    V_TRAILER1 = TRAILER1.
    V_TRAILER2 = TRAILER2.
    V_TRAILER3 = TRAILER3.
    V_TRAILER4 = TRAILER4.
    V_TRAILER5 = TRAILER5.

* Button
    V_BUTTON1 = BUTTON1.
    V_BUTTON2 = BUTTON2.
    V_BUTTON3 = BUTTON3.
    V_BUTTON4 = BUTTON4.
    V_BUTTON5 = BUTTON5.

* For Printing
    ASSIGN IT_TOP_OF_PAGE[]     TO <I_TOP_OF_PAGE>.
    ASSIGN IT_END_OF_PAGE[]     TO <I_END_OF_PAGE>.
    ASSIGN IT_TOP_OF_LIST[]     TO <I_TOP_OF_LIST>.
    ASSIGN IT_END_OF_LIST[]     TO <I_END_OF_LIST>.

  ENDIF.

* Data and field catalog are mandatory
  IF <I_DATA>     IS INITIAL OR
     <I_FIELDCAT> IS INITIAL.
    RAISE EMPTY_TABLE_PASSED.
    EXIT.
  ENDIF.

  I_SORT[]              = IT_SORT[].
  I_TOOLBAR_EXCLUDING[] = IT_TOOLBAR_EXCLUDING[].
  I_TOOLBAR_ADDING[]    = IT_TOOLBAR_ADDING[].
  I_CALLBACK_FORM[]     = IT_CALLBACK_FORM[].

  I_CELL_SET   = IT_CELL_SET.
  I_ROW_SET    = IT_ROW_SET.
  I_COLUMN_SET = IT_COLUMN_SET.

* At least one button must be assigned as exit point
  IF V_BUTTON1 IS INITIAL AND
     V_BUTTON2 IS INITIAL AND
     V_BUTTON3 IS INITIAL AND
     V_BUTTON4 IS INITIAL AND
     V_BUTTON5 IS INITIAL.
    RAISE AT_LEAST_ONE_BUTTON_REQUIRED.
    EXIT.
  ENDIF.

  CALL SCREEN 1000 STARTING AT V_START_X  V_START_Y
                   ENDING   AT V_END_X    V_END_Y.

* Pass back value to calling program
  RETURN_CODE = SAVE_OK.
  IT_SELECTED_CELL    = I_SELECTED_CELL.
  IT_SELECTED_ROW     = I_SELECTED_ROW.
  IT_SELECTED_COLUMN  = I_SELECTED_COLUMN.
  SELECTED_CELL_VALUE = V_SELECTED_CELL_VALUE.

ENDFUNCTION.
