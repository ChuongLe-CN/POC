FUNCTION ZFLCM_POPUP_GRID3_V2.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(FULL_DEMO) DEFAULT SPACE
*"     REFERENCE(PART_DEMO) DEFAULT SPACE
*"     REFERENCE(SHOW_TOOLBAR_GRID) DEFAULT 'X'
*"     REFERENCE(SHOW_STATUSBAR_TEXTBOX) DEFAULT SPACE
*"     REFERENCE(SHOW_TOOLBAR_TEXTBOX) DEFAULT 'X'
*"     REFERENCE(START_POS_X) OPTIONAL
*"     REFERENCE(START_POS_Y) OPTIONAL
*"     REFERENCE(END_POS_X) OPTIONAL
*"     REFERENCE(END_POS_Y) OPTIONAL
*"     REFERENCE(WINDOW_TITLE) OPTIONAL
*"     REFERENCE(GRID_TITLE1) OPTIONAL
*"     REFERENCE(GRID_TITLE2) OPTIONAL
*"     REFERENCE(TRAILER1) OPTIONAL
*"     REFERENCE(TRAILER2) OPTIONAL
*"     REFERENCE(BUTTON1) OPTIONAL
*"     REFERENCE(BUTTON2) OPTIONAL
*"     REFERENCE(BUTTON3) OPTIONAL
*"     REFERENCE(BUTTON4) OPTIONAL
*"     REFERENCE(BUTTON5) OPTIONAL
*"     REFERENCE(IS_LAYOUT1) TYPE  LVC_S_LAYO OPTIONAL
*"     REFERENCE(IS_LAYOUT2) TYPE  LVC_S_LAYO OPTIONAL
*"  EXPORTING
*"     VALUE(RETURN_CODE)
*"  TABLES
*"      IT_FIELDCAT1 OPTIONAL
*"      IT_DATA1 OPTIONAL
*"      IT_FIELDCAT2 OPTIONAL
*"      IT_DATA2 OPTIONAL
*"      IT_TEXTBOX OPTIONAL
*"      IT_SORT1 TYPE  LVC_T_SORT OPTIONAL
*"      IT_SORT2 TYPE  LVC_T_SORT OPTIONAL
*"      IT_TOP_OF_PAGE OPTIONAL
*"      IT_END_OF_PAGE OPTIONAL
*"      IT_TOP_OF_LIST OPTIONAL
*"      IT_END_OF_LIST OPTIONAL
*"      IT_TOOLBAR_EXCLUDING TYPE  UI_FUNCTIONS OPTIONAL
*"  EXCEPTIONS
*"      EMPTY_TABLE_PASSED
*"      AT_LEAST_ONE_BUTTON_REQUIRED
*"--------------------------------------------------------------------

************************************************************************
*                      Canadian National
*
* Function Module Name: ZVU_POPUP_GRID3_V2
* Function Group  Name: ZVU_GRID
* Functional design: DEV-ENH-4B1
* Created by: Steven Qiu
* Created on: 2007-06-11
* Version:    1.0
*
* Purpose: Create a custom pop up window with standard SAP grid
*          functionality in it.
*          To test it, you just just put 'X' in the parameter of
*          FULL_DEMO, and run the function.
*          Demo program YZVU_POPUP_GRID_DEMO shows different
*          functionalities of custom pop up function.
*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------    ----------          ---------------       *
* Steven Qiu                 2007-06-11          DV5K900382            *
*                                                                      *
* Short Description:                                                   *
*  - Based on existing functionality, enable the CANCEL button on the  *
*    top right corner. The return code for Cancel is CANC (In addition *
*    to the pre-defined buttons: F_B1, F_B2, F_B3, F_B4, F_B5).        *
*----------------------------------------------------------------------*
* Steven Qiu                 2003-01-29          DV3K926317            *
*                                                                      *
* Short Description: Initial creation                                  *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************

  INCLUDE <ICON>.

  CLEAR V_FULL_DEMO_FLAG.

  REFRESH: I_DATA_DEMO, I_FIELDCAT_DEMO, I_TEXTBOX_DEMO.

  CLEAR: V_WINDOW_TITLE, GS_LAYOUT1, GS_LAYOUT2.

  IF FULL_DEMO <> SPACE.
    V_FULL_DEMO_FLAG = 'X'.

    PERFORM F_CREATE_DEMO_DATA.
    ASSIGN I_DATA_DEMO[]     TO <I_DATA1>.
    ASSIGN I_FIELDCAT_DEMO[] TO <I_FIELDCAT1>.
    ASSIGN I_DATA_DEMO[]     TO <I_DATA2>.
    ASSIGN I_FIELDCAT_DEMO[] TO <I_FIELDCAT2>.

    PERFORM F_POPULATE_DEMO_ELEMENT.
    GS_LAYOUT1-GRID_TITLE = 'This is my first Grid window'.
    GS_LAYOUT2-GRID_TITLE = 'This is my second Grid window'.

    V_SHOW_TOOLBAR = 1.                   "Text box
    GS_LAYOUT1-NO_TOOLBAR = SPACE.        "Grid 1
    GS_LAYOUT2-NO_TOOLBAR = SPACE.        "Grid 2

  ELSEIF PART_DEMO <> SPACE.
    PERFORM F_CREATE_DEMO_DATA.
    ASSIGN I_DATA_DEMO[]     TO <I_DATA1>.
    ASSIGN I_FIELDCAT_DEMO[] TO <I_FIELDCAT1>.
    ASSIGN I_DATA_DEMO[]     TO <I_DATA2>.
    ASSIGN I_FIELDCAT_DEMO[] TO <I_FIELDCAT2>.
  ELSE.
    ASSIGN IT_DATA1[]     TO <I_DATA1>.
    ASSIGN IT_FIELDCAT1[] TO <I_FIELDCAT1>.
    ASSIGN IT_DATA2[]     TO <I_DATA2>.
    ASSIGN IT_FIELDCAT2[] TO <I_FIELDCAT2>.
  ENDIF.

  IF FULL_DEMO IS INITIAL.

* Fill Layout if specified
    GS_LAYOUT1 = IS_LAYOUT1.
    GS_LAYOUT2 = IS_LAYOUT2.

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

* Grid title1
    IF NOT GRID_TITLE1 IS INITIAL.
      GS_LAYOUT1-GRID_TITLE = GRID_TITLE1.
    ENDIF.

* Grid title2
    IF NOT GRID_TITLE2 IS INITIAL.
      GS_LAYOUT2-GRID_TITLE = GRID_TITLE2.
    ENDIF.

* Hide grid toolbar
    IF SHOW_TOOLBAR_GRID <> SPACE.
      GS_LAYOUT1-NO_TOOLBAR = SPACE.
      GS_LAYOUT2-NO_TOOLBAR = SPACE.
    ELSE.
      GS_LAYOUT1-NO_TOOLBAR = 'X'.
      GS_LAYOUT2-NO_TOOLBAR = 'X'.
    ENDIF.

* Status bar (Text box)
    IF SHOW_STATUSBAR_TEXTBOX <> SPACE.
      V_SHOW_STATUSBAR = 1.
    ELSE.
      V_SHOW_STATUSBAR = 0.
    ENDIF.

* Tool bar (Text box)
    IF SHOW_TOOLBAR_TEXTBOX <> SPACE.
      V_SHOW_TOOLBAR = 1.
    ELSE.
      V_SHOW_TOOLBAR = 0.
    ENDIF.

* Trailer
    V_TRAILER1 = TRAILER1.
    V_TRAILER2 = TRAILER2.

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

* For the text box
    ASSIGN IT_TEXTBOX[]         TO <I_TEXTBOX>.

  ENDIF.

* Data and field catalog are mandatory
  IF <I_DATA1>     IS INITIAL OR
     <I_FIELDCAT1> IS INITIAL OR
     <I_DATA2>     IS INITIAL OR
     <I_FIELDCAT2> IS INITIAL.
    RAISE EMPTY_TABLE_PASSED.
    EXIT.
  ENDIF.

  I_SORT1[]             = IT_SORT1[].
  I_SORT2[]             = IT_SORT2[].
  I_TOOLBAR_EXCLUDING[] = IT_TOOLBAR_EXCLUDING[].

* At least one button must be assigned as exit point
  IF V_BUTTON1 IS INITIAL AND
     V_BUTTON2 IS INITIAL AND
     V_BUTTON3 IS INITIAL AND
     V_BUTTON4 IS INITIAL AND
     V_BUTTON5 IS INITIAL.
    RAISE AT_LEAST_ONE_BUTTON_REQUIRED.
    EXIT.
  ENDIF.

  CALL SCREEN 4010 STARTING AT V_START_X  V_START_Y
                   ENDING   AT V_END_X    V_END_Y.


* Pass back value to calling program
  RETURN_CODE = SAVE_OK.

ENDFUNCTION.
