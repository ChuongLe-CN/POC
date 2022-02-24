*----------------------------------------------------------------------*
***INCLUDE LZVU_GRIDO01 .
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

*&---------------------------------------------------------------------*
*&      Module  SET_GUI_TITLEBAR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_GUI_TITLEBAR OUTPUT.
  SET PF-STATUS 'STATUS_1000'.
  SET TITLEBAR 'TITLE_1000' WITH V_WINDOW_TITLE.
ENDMODULE.                 " SET_GUI_TITLEBAR  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  HIDE_SHOW_ELEMENT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HIDE_SHOW_ELEMENT OUTPUT.

  LOOP AT SCREEN.
    CASE SCREEN-NAME.
* Header
      WHEN 'V_HEADER1'.
        IF V_HEADER1 IS INITIAL.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      WHEN 'V_HEADER2'.
        IF V_HEADER2 IS INITIAL.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      WHEN 'V_HEADER3'.
        IF V_HEADER3 IS INITIAL.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      WHEN 'V_HEADER4'.
        IF V_HEADER4 IS INITIAL.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      WHEN 'V_HEADER5'.
        IF V_HEADER5 IS INITIAL.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
* Trailer
      WHEN 'V_TRAILER1'.
        IF V_TRAILER1 IS INITIAL.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      WHEN 'V_TRAILER2'.
        IF V_TRAILER2 IS INITIAL.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      WHEN 'V_TRAILER3'.
        IF V_TRAILER3 IS INITIAL.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      WHEN 'V_TRAILER4'.
        IF V_TRAILER4 IS INITIAL.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      WHEN 'V_TRAILER5'.
        IF V_TRAILER5 IS INITIAL.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
* Button
      WHEN 'V_BUTTON1'.
        IF V_BUTTON1 IS INITIAL.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      WHEN 'V_BUTTON2'.
        IF V_BUTTON2 IS INITIAL.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      WHEN 'V_BUTTON3'.
        IF V_BUTTON3 IS INITIAL.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      WHEN 'V_BUTTON4'.
        IF V_BUTTON4 IS INITIAL.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
      WHEN 'V_BUTTON5'.
        IF V_BUTTON5 IS INITIAL.
          SCREEN-INVISIBLE = '1'.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.


ENDMODULE.                 " HIDE_SHOW_ELEMENT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_GRID  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_GRID OUTPUT.

  IF CUSTOM_CONTAINER1 IS INITIAL.
* create a custom container control for our ALV Control
    CREATE OBJECT CUSTOM_CONTAINER1
        EXPORTING
            CONTAINER_NAME = V_CONTAINER1.

* create an instance of alv control
    CREATE OBJECT GRID1
           EXPORTING I_PARENT = CUSTOM_CONTAINER1.

   CALL METHOD GRID1->SET_READY_FOR_INPUT
        EXPORTING
                   I_READY_FOR_INPUT = 1.

*
    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING
                   IS_LAYOUT        = GS_LAYOUT
                   IT_TOOLBAR_EXCLUDING = I_TOOLBAR_EXCLUDING
         CHANGING  IT_OUTTAB        = <I_DATA>
                   IT_FIELDCATALOG  = <I_FIELDCAT>
                   IT_SORT          = I_SORT[].


********
* ->Create Object to receive events and link them to handler methods.
* When the ALV Control raises the event for the specified instance
* the corresponding method is automatically called.
*
    CREATE OBJECT EVENT_RECEIVER.
    SET HANDLER EVENT_RECEIVER->HANDLE_TOP_OF_LIST  FOR GRID1.
    SET HANDLER EVENT_RECEIVER->HANDLE_TOP_OF_PAGE  FOR GRID1.
    SET HANDLER EVENT_RECEIVER->HANDLE_END_OF_LIST  FOR GRID1.
    SET HANDLER EVENT_RECEIVER->HANDLE_END_OF_PAGE  FOR GRID1.
    SET HANDLER EVENT_RECEIVER->HANDLE_TOOLBAR      FOR GRID1.
    SET HANDLER EVENT_RECEIVER->HANDLE_DOUBLE_CLICK FOR GRID1.
    SET HANDLER EVENT_RECEIVER->HANDLE_USER_COMMAND FOR GRID1.
    SET HANDLER EVENT_RECEIVER->HANDLE_DATA_CHANGED_FINISHED
        FOR GRID1.
* Call method 'set_toolbar_interactive' to raise event TOOLBAR.
    CALL METHOD GRID1->SET_TOOLBAR_INTERACTIVE.
*
********

  ENDIF.

* Controls are not integrated into the TAB-Order
* Call "set_focus" if you want to make sure that 'the cursor'
* is active in your control.
  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS EXPORTING CONTROL = GRID1.

* Set cell, row, column
  IF NOT I_CELL_SET IS INITIAL.
    CALL METHOD GRID1->SET_SELECTED_CELLS
      EXPORTING
        IT_CELLS = I_CELL_SET.
  ENDIF.

  IF NOT I_COLUMN_SET IS INITIAL.
    CALL METHOD GRID1->SET_SELECTED_COLUMNS
      EXPORTING
        IT_COL_TABLE = I_COLUMN_SET.
  ENDIF.

  IF NOT I_ROW_SET IS INITIAL.
    CALL METHOD GRID1->SET_SELECTED_ROWS
      EXPORTING
        IT_INDEX_ROWS = I_ROW_SET.
  ENDIF.

ENDMODULE.                 " CREATE_GRID  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_TEXTBOX  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_TEXTBOX OUTPUT.

  CLEAR: V_REPID,
         V_DYNNR.

  V_REPID = SY-REPID.
  V_DYNNR = SY-DYNNR.

* Create object
  CREATE OBJECT TEXT_CONTROL1
    EXPORTING
      REPID             = V_REPID
      DYNNR             = V_DYNNR
      DYNPRO_CONTAINER  = 'V_TEXT_CONTROL'
      WORDWRAP_MODE     = 2                    "Fixed pos. wordwrap
      WORDWRAP_POSITION = 80
      WORDWRAP_TO_LINEBREAK_MODE = 1.

* Set mode
  CALL METHOD TEXT_CONTROL1->SET_READONLY_MODE.

* Status bar (display line no, total lines, etc..)
  CALL METHOD TEXT_CONTROL1->SET_STATUSBAR_MODE
         EXPORTING
               STATUSBAR_MODE = V_SHOW_STATUSBAR.

* Buttons: copy, paste, download, etc..
  CALL METHOD TEXT_CONTROL1->SET_TOOLBAR_MODE
         EXPORTING
               TOOLBAR_MODE = V_SHOW_TOOLBAR.

* Flush control
  CALL METHOD C_TEXTEDIT_CONTROL=>FLUSH
          EXCEPTIONS
               FLUSH_ERROR = 01.

  IF NOT <I_TEXTBOX> IS INITIAL.

* Put text into edit control
    CALL METHOD TEXT_CONTROL1->SET_TEXT_AS_R3TABLE
                    EXPORTING
                          TABLE = <I_TEXTBOX>.

  ENDIF.


ENDMODULE.                 " CREATE_TEXTBOX  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_GRID2  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_GRID2 OUTPUT.

* First Grid Object
  IF CUSTOM_CONTAINER1 IS INITIAL.
* create a custom container control for our ALV Control
    CREATE OBJECT CUSTOM_CONTAINER1
        EXPORTING
            CONTAINER_NAME = V_CONTAINER1.

* create an instance of alv control
    CREATE OBJECT GRID1
           EXPORTING I_PARENT = CUSTOM_CONTAINER1.

*
    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING
                   IS_LAYOUT        = GS_LAYOUT1
                   IT_TOOLBAR_EXCLUDING = I_TOOLBAR_EXCLUDING
         CHANGING  IT_OUTTAB        = <I_DATA1>
                   IT_FIELDCATALOG  = <I_FIELDCAT1>
                   IT_SORT          = I_SORT1[].

* Call method 'set_toolbar_interactive' to raise event TOOLBAR.
    CALL METHOD GRID1->SET_TOOLBAR_INTERACTIVE.
*
  ENDIF.

* Second Grid Object
  IF CUSTOM_CONTAINER2 IS INITIAL.
* create a custom container control for our ALV Control
    CREATE OBJECT CUSTOM_CONTAINER2
        EXPORTING
            CONTAINER_NAME = V_CONTAINER2.

* create an instance of alv control
    CREATE OBJECT GRID2
           EXPORTING I_PARENT = CUSTOM_CONTAINER2.

*
    CALL METHOD GRID2->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING
                   IS_LAYOUT        = GS_LAYOUT2
                   IT_TOOLBAR_EXCLUDING = I_TOOLBAR_EXCLUDING
         CHANGING  IT_OUTTAB        = <I_DATA2>
                   IT_FIELDCATALOG  = <I_FIELDCAT2>
                   IT_SORT          = I_SORT2[].

********

* Call method 'set_toolbar_interactive' to raise event TOOLBAR.
    CALL METHOD GRID2->SET_TOOLBAR_INTERACTIVE.
*
  ENDIF.

  IF NOT CUSTOM_CONTAINER1 IS INITIAL OR
     NOT CUSTOM_CONTAINER2 IS INITIAL.

********
* ->Create Object to receive events and link them to handler methods.
* When the ALV Control raises the event for the specified instance
* the corresponding method is automatically called.
*
    CREATE OBJECT EVENT_RECEIVER.

    IF NOT CUSTOM_CONTAINER1 IS INITIAL.

      SET HANDLER EVENT_RECEIVER->HANDLE_TOP_OF_LIST  FOR GRID1.
      SET HANDLER EVENT_RECEIVER->HANDLE_TOP_OF_PAGE  FOR GRID1.
      SET HANDLER EVENT_RECEIVER->HANDLE_END_OF_LIST  FOR GRID1.
      SET HANDLER EVENT_RECEIVER->HANDLE_END_OF_PAGE  FOR GRID1.
      SET HANDLER EVENT_RECEIVER->HANDLE_TOOLBAR      FOR GRID1.

    ELSE.

      SET HANDLER EVENT_RECEIVER->HANDLE_TOP_OF_LIST  FOR GRID2.
      SET HANDLER EVENT_RECEIVER->HANDLE_TOP_OF_PAGE  FOR GRID2.
      SET HANDLER EVENT_RECEIVER->HANDLE_END_OF_LIST  FOR GRID2.
      SET HANDLER EVENT_RECEIVER->HANDLE_END_OF_PAGE  FOR GRID2.
      SET HANDLER EVENT_RECEIVER->HANDLE_TOOLBAR      FOR GRID2.

    ENDIF.

  ENDIF.


ENDMODULE.                 " CREATE_GRID2  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_GUI_TITLEBAR_V2  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_GUI_TITLEBAR_V2 OUTPUT.

  SET PF-STATUS 'STATUS_1000_V2'.
  SET TITLEBAR 'TITLE_1000' WITH V_WINDOW_TITLE.

ENDMODULE.                 " SET_GUI_TITLEBAR_V2  OUTPUT
