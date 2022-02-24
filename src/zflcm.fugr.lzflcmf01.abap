*----------------------------------------------------------------------*
***INCLUDE LZVU_GRIDF01 .
*----------------------------------------------------------------------*
************************************************************************
*                      Canadian National
*
* Function Group  Name: ZVU_GRID
* Functional design: DEV-ENH-4B1
* Created by: Steven Qiu
* Created on: 2002-12-17
* Version:    1.0
*
* Purpose: Create a custom pop up window with standard SAP grid
*          functionality in it.
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

*---------------------------------------------------------------------*
*       FORM F_POPULATE_DEMO_ELEMENT                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM F_POPULATE_DEMO_ELEMENT.

* Window position
  V_START_X = 10.
  V_START_Y = 1.
  V_END_X   = 100.
  V_END_Y   = 28.

* Window title
  V_WINDOW_TITLE = 'This is my pop up window title'.

* Grid title
  GS_LAYOUT-GRID_TITLE = 'This is my grid pop up title'.

* Row selection
  GS_LAYOUT-SEL_MODE = 'A'.

* Header
  V_HEADER1 = 'This is header 1. You can have up to 5 headers.'.
  V_HEADER2 = 'This is header 2. Put whatever you want here.'.
  V_HEADER3 = 'This is header 3. You can pass any value.'.
  V_HEADER4 = 'Look at the function documentation for more detail.'.
  V_HEADER5 = 'This is header 5. Last header.'.

* Trailer
  V_TRAILER1 = 'This is trailer area.'.
  V_TRAILER2 = 'This is trailer 2.'.
  V_TRAILER3 = 'You can define up to 5 buttons.'.
  V_TRAILER4 = 'Each button has its own function code.'.
  V_TRAILER5 = 'This is trailer 5. Last trailer.'.

* Button
  V_BUTTON1 = 'Button 1'.
  V_BUTTON2 = 'Button 2'.
  V_BUTTON3 = 'Button 3'.
  V_BUTTON4 = 'Button 4'.
  V_BUTTON5 = 'Button 5'.

* For printing
  REFRESH I_TOP_OF_LIST_DEMO.
  CLEAR V_LINE.
  V_LINE = 'This is top of list'.
  APPEND V_LINE TO I_TOP_OF_LIST_DEMO.

  REFRESH I_END_OF_LIST_DEMO.
  CLEAR V_LINE.
  V_LINE = 'This is end of list'.
  APPEND V_LINE TO I_END_OF_LIST_DEMO.

  REFRESH I_TOP_OF_PAGE_DEMO.
  CLEAR V_LINE.
  V_LINE = 'This is top of page'.
  APPEND V_LINE TO I_TOP_OF_PAGE_DEMO.

  REFRESH I_END_OF_PAGE_DEMO.
  CLEAR V_LINE.
  V_LINE = 'This is end of page'.
  APPEND V_LINE TO I_END_OF_PAGE_DEMO.

* Build text box
  CLEAR I_TEXTBOX_DEMO.
  I_TEXTBOX_DEMO = '    *** This is for DEMO only ***'.
  APPEND I_TEXTBOX_DEMO.

  CLEAR I_TEXTBOX_DEMO.
  I_TEXTBOX_DEMO = 'You can pass an internal table containing text'.
  APPEND I_TEXTBOX_DEMO.

  CLEAR I_TEXTBOX_DEMO.
  I_TEXTBOX_DEMO = 'And it will be displayed in this text box'.
  APPEND I_TEXTBOX_DEMO.

  CLEAR I_TEXTBOX_DEMO.
  I_TEXTBOX_DEMO =
      'The scroll bar will be automatically activated when needed'.
  APPEND I_TEXTBOX_DEMO.

  CLEAR I_TEXTBOX_DEMO.
  I_TEXTBOX_DEMO = 'You can pass as many as texts you want in here'.
  APPEND I_TEXTBOX_DEMO.

  CLEAR I_TEXTBOX_DEMO.
  I_TEXTBOX_DEMO =
      'If you do not pass any text, the text box will be empty.'.
  APPEND I_TEXTBOX_DEMO.

  CLEAR I_TEXTBOX_DEMO.
  I_TEXTBOX_DEMO = '    *** This is the end of DEMO ***'.
  APPEND I_TEXTBOX_DEMO.

  ASSIGN I_TEXTBOX_DEMO[] TO <I_TEXTBOX>.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_CREATE_DEMO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CREATE_DEMO_DATA.

  REFRESH: I_DATA_DEMO, I_FIELDCAT_DEMO.
  CLEAR:  V_PERSONNEL_ID.

** Build data
  DO 10 TIMES.

    PERFORM F_BUILD_DATA USING:
         'John'  'Smith'   '24'  'A normal guy'  'DEMO ONLY',
         'Mary'  'Johnson' '25'  'A nice woman'  'DEMO ONLY',
         'Harry' 'Potter'  '26'  'A witch'       'DEMO ONLY',
         'Tom'   'Hanks'   '27'  'An actor'      'DEMO ONLY'.

    PERFORM F_BUILD_DATA USING:
         'John'  'Smith'   '34'  'A normal guy'  'DEMO ONLY',
         'Mary'  'Johnson' '35'  'A nice woman'  'DEMO ONLY',
         'Harry' 'Potter'  '36'  'A witch'       'DEMO ONLY',
         'Tom'   'Hanks'   '37'  'An actor'      'DEMO ONLY'.

    PERFORM F_BUILD_DATA USING:
         'John'  'Smith'   '44'  'A normal guy'  'DEMO ONLY',
         'Mary'  'Johnson' '45'  'A nice woman'  'DEMO ONLY',
         'Harry' 'Potter'  '46'  'A witch'       'DEMO ONLY',
         'Tom'   'Hanks'   '47'  'An actor'      'DEMO ONLY'.

  ENDDO.


** Build fieldcat
  PERFORM F_BUILD_FIELDCAT USING 'PERSONNEL_ID'
                                 'Pers.ID'
                                 'N'
                                  6.
  PERFORM F_BUILD_FIELDCAT USING 'FIRST_NAME'
                                 'First Name'
                                 'C'
                                  10.
  PERFORM F_BUILD_FIELDCAT USING 'LAST_NAME'
                                 'Last Name'
                                 'C'
                                  10.
  PERFORM F_BUILD_FIELDCAT USING 'AGE'
                                 'Age'
                                 'N'
                                  5.
  PERFORM F_BUILD_FIELDCAT USING 'COMMENT1'
                                 'Comment 1'
                                 'C'
                                  15.

  PERFORM F_BUILD_FIELDCAT USING 'COMMENT2'
                                 'Comment 2'
                                 'C'
                                  10.

  ASSIGN I_DATA_DEMO[]     TO <I_DATA>.
  ASSIGN I_FIELDCAT_DEMO[] TO <I_FIELDCAT>.

ENDFORM.                    " F_CREATE_DEMO_DATA

*---------------------------------------------------------------------*
*       FORM f_build_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIRST_NAME                                                  *
*  -->  P_LAST_NAME                                                   *
*  -->  P_AGE                                                         *
*---------------------------------------------------------------------*
FORM F_BUILD_DATA USING
                    P_FIRST_NAME
                    P_LAST_NAME
                    P_AGE
                    P_COMMENT1
                    P_COMMENT2.

  CLEAR WA_DATA.
  V_PERSONNEL_ID       = V_PERSONNEL_ID + 1.
  WA_DATA-PERSONNEL_ID = V_PERSONNEL_ID.
  WA_DATA-FIRST_NAME   = P_FIRST_NAME.
  WA_DATA-LAST_NAME    = P_LAST_NAME.
  WA_DATA-AGE          = P_AGE.
  WA_DATA-COMMENT1     = P_COMMENT1.
  WA_DATA-COMMENT2     = P_COMMENT2.
  APPEND WA_DATA TO I_DATA_DEMO.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM f_build_fieldcat                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDNAME                                                   *
*  -->  P_COLTEXT                                                     *
*  -->  P_INTTYPE                                                     *
*---------------------------------------------------------------------*
FORM F_BUILD_FIELDCAT USING P_FIELDNAME
                            P_COLTEXT
                            P_INTTYPE
                            P_LENGTH.

  CLEAR I_FIELDCAT_DEMO.
  I_FIELDCAT_DEMO-FIELDNAME = P_FIELDNAME.
  I_FIELDCAT_DEMO-COLTEXT   = P_COLTEXT.
  I_FIELDCAT_DEMO-NO_ZERO   = 'X'.

  I_FIELDCAT_DEMO-INTTYPE   = P_INTTYPE.
  I_FIELDCAT_DEMO-OUTPUTLEN = P_LENGTH.

  APPEND I_FIELDCAT_DEMO.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_WRAP_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_WRAP_UP.

DATA:
  L_VALID.

* Refresh data
  CALL METHOD GRID1->CHECK_CHANGED_DATA
    IMPORTING
      E_VALID        = L_VALID.

* Get the selected row, column, cell info
  REFRESH I_SELECTED_ROW.
  CALL METHOD GRID1->GET_SELECTED_ROWS
   IMPORTING
      ET_INDEX_ROWS  = I_SELECTED_ROW.

  REFRESH I_SELECTED_COLUMN.
  CALL METHOD GRID1->GET_SELECTED_COLUMNS
   IMPORTING
      ET_INDEX_COLUMNS  = I_SELECTED_COLUMN.

  REFRESH I_SELECTED_CELL.
  CALL METHOD GRID1->GET_SELECTED_CELLS
   IMPORTING
      ET_CELL  = I_SELECTED_CELL.

  CLEAR V_SELECTED_CELL_VALUE.
  CALL METHOD GRID1->GET_CURRENT_CELL
   IMPORTING
      E_VALUE        = V_SELECTED_CELL_VALUE.

* Free object and memory
  FREE EVENT_RECEIVER.
  CLEAR EVENT_RECEIVER.

  CALL METHOD GRID1->FREE.
  CALL METHOD CUSTOM_CONTAINER1->FREE.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  CLEAR GRID1.
  CLEAR CUSTOM_CONTAINER1.

* Leave the current screen
  LEAVE TO SCREEN 0.

ENDFORM.                    " F_WRAP_UP
*&---------------------------------------------------------------------*
*&      Form  F_CALL_USER_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_CALL_USER_FORM.

  READ TABLE I_CALLBACK_FORM INTO I_CALLBACK_FORM
    WITH KEY FUNCTION_CODE = SAVE_OK.
  IF SY-SUBRC EQ 0.
    PERFORM  F_EXPORT_CELL_INFO_TO_MEMORY.
    ASSIGN I_CALLBACK_FORM-FORM_NAME TO <FORM_NAME>.
    ASSIGN I_CALLBACK_FORM-PROGRAM_NAME TO <PROGRAM_NAME>.
    PERFORM (<FORM_NAME>) IN PROGRAM (<PROGRAM_NAME>).
  ENDIF.

ENDFORM.                    " F_CALL_USER_FORM
*&---------------------------------------------------------------------*
*&      Form  F_EXPORT_CELL_INFO_TO_MEMORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_EXPORT_CELL_INFO_TO_MEMORY.

  DATA: I_CELL_FROM_GRID TYPE LVC_T_CELL.
  DATA: I_ROW_FROM_GRID TYPE LVC_T_ROW.
  DATA: I_COLUMN_FROM_GRID TYPE LVC_T_COL.
  DATA: V_CELL_VALUE_FROM_GRID(70) TYPE C.

* Refresh data
  CALL METHOD GRID1->CHECK_CHANGED_DATA.

  REFRESH I_ROW_FROM_GRID.

  CALL METHOD GRID1->GET_SELECTED_ROWS
   IMPORTING
      ET_INDEX_ROWS  = I_ROW_FROM_GRID.

  REFRESH I_COLUMN_FROM_GRID.

  CALL METHOD GRID1->GET_SELECTED_COLUMNS
   IMPORTING
      ET_INDEX_COLUMNS  = I_COLUMN_FROM_GRID.

  REFRESH I_CELL_FROM_GRID.

  CALL METHOD GRID1->GET_SELECTED_CELLS
   IMPORTING
      ET_CELL  = I_CELL_FROM_GRID.

  CLEAR V_CELL_VALUE_FROM_GRID.
  CALL METHOD GRID1->GET_CURRENT_CELL
   IMPORTING
      E_VALUE        = V_CELL_VALUE_FROM_GRID.

  FREE MEMORY ID 'ROW_GRID'.
  FREE MEMORY ID 'COL_GRID'.
  FREE MEMORY ID 'CELL_GRID'.
  FREE MEMORY ID 'CELL_VAL_GRID'.

  EXPORT I_ROW_FROM_GRID        TO MEMORY ID 'ROW_GRID'.
  EXPORT I_COLUMN_FROM_GRID     TO MEMORY ID 'COL_GRID'.
  EXPORT I_CELL_FROM_GRID       TO MEMORY ID 'CELL_GRID'.
  EXPORT V_CELL_VALUE_FROM_GRID TO MEMORY ID 'CELL_VAL_GRID'.

ENDFORM.                    " F_EXPORT_CELL_INFO_TO_MEMORY
