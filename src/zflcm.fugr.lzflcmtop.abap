*----------------------------------------------------------------------*
***INCLUDE LZVU_GRIDTOP .
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

FUNCTION-POOL ZFLCM.                     "MESSAGE-ID ..

*********
* Predefine a local class for event handling to allow the
* declaration of a reference variable.
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.
*********

FIELD-SYMBOLS:
  <I_FIELDCAT>     TYPE ANY,
  <I_DATA>         TYPE ANY,
  <I_FIELDCAT1>    TYPE ANY,
  <I_DATA1>        TYPE ANY,
  <I_FIELDCAT2>    TYPE ANY,
  <I_DATA2>        TYPE ANY,
  <I_TOP_OF_PAGE>  TYPE ANY,
  <I_END_OF_PAGE>  TYPE ANY,
  <I_TOP_OF_LIST>  TYPE ANY,
  <I_END_OF_LIST>  TYPE ANY,
  <I_TEXTBOX>      TYPE ANY,
  <PROGRAM_NAME>   TYPE ANY,
  <FORM_NAME>      TYPE ANY.

CONSTANTS:
  C_FCODE_DOUBLE_CLICK(10) TYPE C VALUE 'DBLCLICK'.

DATA:
      OKCODE_1000       LIKE SY-UCOMM,
      OKCODE_2000       LIKE SY-UCOMM,
      OKCODE_3000       LIKE SY-UCOMM,
      OKCODE_4000       LIKE SY-UCOMM,
      SAVE_OK           LIKE SY-UCOMM,
      GS_LAYOUT         TYPE LVC_S_LAYO,
      GS_LAYOUT1        TYPE LVC_S_LAYO,
      GS_LAYOUT2        TYPE LVC_S_LAYO,
      I_SELECTED_CELL   TYPE LVC_T_CELL,
      I_SELECTED_COLUMN TYPE LVC_T_COL,
      I_SELECTED_ROW    TYPE LVC_T_ROW,
      V_SELECTED_CELL_VALUE(70) TYPE C,
      I_CELL_SET        TYPE LVC_T_CELL,
      I_ROW_SET         TYPE LVC_T_ROW,
      I_COLUMN_SET      TYPE LVC_T_COL,
      I_CALLBACK_FORM TYPE ZVU_CALLBACK_FORM_GRID OCCURS 0
          WITH HEADER LINE,
      I_TOOLBAR_EXCLUDING TYPE UI_FUNCTIONS,
      I_TOOLBAR_ADDING  TYPE STB_BUTTON OCCURS 0 WITH HEADER LINE,
      I_SORT            TYPE LVC_S_SORT OCCURS 0 WITH HEADER LINE,
      I_SORT1           TYPE LVC_S_SORT OCCURS 0 WITH HEADER LINE,
      I_SORT2           TYPE LVC_S_SORT OCCURS 0 WITH HEADER LINE,
      V_CONTAINER1      TYPE SCRFNAME VALUE 'V_CONTAINER1',
      V_CONTAINER2      TYPE SCRFNAME VALUE 'V_CONTAINER2',
      GRID1             TYPE REF TO CL_GUI_ALV_GRID,
      GRID2             TYPE REF TO CL_GUI_ALV_GRID,
      CUSTOM_CONTAINER1 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CUSTOM_CONTAINER2 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      EVENT_RECEIVER    TYPE REF TO LCL_EVENT_RECEIVER.

DATA: V_REPID LIKE SY-REPID,
      V_DYNNR LIKE SY-DYNNR.

DATA: TEXT_CONTROL1     TYPE REF TO C_TEXTEDIT_CONTROL,
      V_SHOW_STATUSBAR  TYPE I,
      V_SHOW_TOOLBAR    TYPE I.

DATA:
  V_START_X(3)       TYPE N,
  V_START_Y(3)       TYPE N,
  V_END_X(3)         TYPE N,
  V_END_Y(3)         TYPE N,
  V_WINDOW_TITLE(72) TYPE C,
  V_GRID_TITLE(72)   TYPE C,
  V_HEADER1(72)      TYPE C,
  V_HEADER2(72)      TYPE C,
  V_HEADER3(72)      TYPE C,
  V_HEADER4(72)      TYPE C,
  V_HEADER5(72)      TYPE C,
  V_TRAILER1(91)     TYPE C,
  V_TRAILER2(91)     TYPE C,
  V_TRAILER3(91)     TYPE C,
  V_TRAILER4(91)     TYPE C,
  V_TRAILER5(91)     TYPE C,
  V_BUTTON1(15)      TYPE C,
  V_BUTTON2(15)      TYPE C,
  V_BUTTON3(15)      TYPE C,
  V_BUTTON4(15)      TYPE C,
  V_BUTTON5(15)      TYPE C.

* Work area for print event
DATA: V_LINE(72) TYPE C,
      I_LINE LIKE V_LINE OCCURS 0.

** For demo purpose **** Begin ******
DATA: I_FIELDCAT_DEMO TYPE LVC_T_FCAT WITH HEADER LINE.
TYPES: BEGIN OF T_DATA,
         PERSONNEL_ID(6) TYPE N,
         FIRST_NAME(20)  TYPE C,
         LAST_NAME(20)   TYPE C,
         AGE(3)          TYPE N,
         COMMENT1(20)    TYPE C,
         COMMENT2(10)    TYPE C,
       END OF T_DATA.

DATA: I_DATA_DEMO  TYPE STANDARD TABLE OF T_DATA
      INITIAL SIZE 30,
      WA_DATA TYPE T_DATA.

DATA: BEGIN OF I_TEXTBOX_DEMO OCCURS 0,
        LINE(80) TYPE C,
      END OF I_TEXTBOX_DEMO.

DATA: BEGIN OF I_TOP_OF_LIST_DEMO OCCURS 0,
        LINE(72) TYPE C,
      END OF I_TOP_OF_LIST_DEMO.

DATA: BEGIN OF I_END_OF_LIST_DEMO OCCURS 0,
        LINE(72) TYPE C,
      END OF I_END_OF_LIST_DEMO.

DATA: BEGIN OF I_TOP_OF_PAGE_DEMO OCCURS 0,
        LINE(72) TYPE C,
      END OF I_TOP_OF_PAGE_DEMO.

DATA: BEGIN OF I_END_OF_PAGE_DEMO OCCURS 0,
        LINE(72) TYPE C,
      END OF I_END_OF_PAGE_DEMO.

DATA: V_PERSONNEL_ID(6) TYPE N.
DATA: V_FULL_DEMO_FLAG(1) TYPE C.

** For demo purpose **** End ******


****************************************************************
* LOCAL CLASSES: Definition
****************************************************************
* Definition:
* ~~~~~~~~~~~
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
* Define a method for each print event you need.
    METHODS:
    HANDLE_TOP_OF_PAGE
        FOR EVENT PRINT_TOP_OF_PAGE OF CL_GUI_ALV_GRID,

    HANDLE_END_OF_PAGE
        FOR EVENT PRINT_END_OF_PAGE OF CL_GUI_ALV_GRID,

    HANDLE_TOP_OF_LIST
        FOR EVENT PRINT_TOP_OF_LIST OF CL_GUI_ALV_GRID,

    HANDLE_END_OF_LIST
        FOR EVENT PRINT_END_OF_LIST OF CL_GUI_ALV_GRID,

    HANDLE_TOOLBAR
        FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
            IMPORTING E_OBJECT E_INTERACTIVE,

    HANDLE_DOUBLE_CLICK
        FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW E_COLUMN,

    HANDLE_USER_COMMAND
        FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
            IMPORTING E_UCOMM,


    handle_data_changed_finished
       for event data_changed_finished of cl_gui_alv_grid
             importing e_modified
                       et_good_cells.


    methods:
      handle_data_changed
         for event data_changed of cl_gui_alv_grid
             importing er_data_changed.



ENDCLASS.

*
* c_event_receiver (Definition)
*===============================================================


****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
*===============================================================
* class c_event_receiver (Implementation)
*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD HANDLE_TOP_OF_PAGE.

    CLEAR V_LINE.
    REFRESH I_LINE.
    IF V_FULL_DEMO_FLAG <> SPACE.
      ASSIGN I_TOP_OF_PAGE_DEMO[] TO <I_TOP_OF_PAGE>.
    ENDIF.
    I_LINE[] = <I_TOP_OF_PAGE>.
    LOOP AT I_LINE INTO V_LINE.
      WRITE: / V_LINE.
    ENDLOOP.

  ENDMETHOD.                           "handle_top_of_page
*-------------------------------------------
  METHOD HANDLE_END_OF_PAGE.
    CLEAR V_LINE.
    REFRESH I_LINE.
    IF V_FULL_DEMO_FLAG <> SPACE.
      ASSIGN I_END_OF_PAGE_DEMO[] TO <I_END_OF_PAGE>.
    ENDIF.
    I_LINE[] = <I_END_OF_PAGE>.
    LOOP AT I_LINE INTO V_LINE.
      WRITE: / V_LINE.
    ENDLOOP.

  ENDMETHOD.                           "handle_end_of_page
*-------------------------------------------
  METHOD HANDLE_TOP_OF_LIST.
    CLEAR V_LINE.
    REFRESH I_LINE.
    IF V_FULL_DEMO_FLAG <> SPACE.
      ASSIGN I_TOP_OF_LIST_DEMO[] TO <I_TOP_OF_LIST>.
    ENDIF.
    I_LINE[] = <I_TOP_OF_LIST>.
    LOOP AT I_LINE INTO V_LINE.
      WRITE: / V_LINE.
    ENDLOOP.

  ENDMETHOD.                           "handle_top_of_list
*-------------------------------------------
  METHOD HANDLE_END_OF_LIST.
    CLEAR V_LINE.
    REFRESH I_LINE.
    IF V_FULL_DEMO_FLAG <> SPACE.
      ASSIGN I_END_OF_LIST_DEMO[] TO <I_END_OF_LIST>.
    ENDIF.
    I_LINE[] = <I_END_OF_LIST>.
    LOOP AT I_LINE INTO V_LINE.
      WRITE: / V_LINE.
    ENDLOOP.

  ENDMETHOD.                           "handle_end_of_list
*-------------------------------------------
  METHOD HANDLE_TOOLBAR.
    LOOP AT I_TOOLBAR_ADDING INTO I_TOOLBAR_ADDING.
      APPEND I_TOOLBAR_ADDING TO E_OBJECT->MT_TOOLBAR.
    ENDLOOP.
  ENDMETHOD.
*-------------------------------------------
  METHOD HANDLE_DOUBLE_CLICK.
    SAVE_OK = C_FCODE_DOUBLE_CLICK.
    PERFORM F_CALL_USER_FORM.
  ENDMETHOD.
*-------------------------------------------
  METHOD HANDLE_USER_COMMAND.
    SAVE_OK = E_UCOMM.
    PERFORM F_CALL_USER_FORM.
  ENDMETHOD.
*-------------------------------------------
  METHOD HANDLE_DATA_CHANGED_FINISHED.

    DATA:  ls_stable type lvc_s_stbl,
           ls_good type lvc_s_modi,
           lv_split1 type cawn-atwrt,
           lv_split2 type tcurr-tcurr.




  ENDMETHOD.                    "HANDLE_DATA_CHANGED_FINISHED
  METHOD HANDLE_DATA_CHANGED.

    data: ls_good type lvc_s_modi.



  ENDMETHOD.                    "HANDLE_DATA_CHANGED

ENDCLASS.
