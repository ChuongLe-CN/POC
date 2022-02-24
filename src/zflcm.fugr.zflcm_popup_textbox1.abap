FUNCTION ZFLCM_POPUP_TEXTBOX1.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(FULL_DEMO) DEFAULT SPACE
*"     REFERENCE(SHOW_STATUSBAR) DEFAULT SPACE
*"     REFERENCE(SHOW_TOOLBAR) DEFAULT SPACE
*"     REFERENCE(START_POS_X) OPTIONAL
*"     REFERENCE(START_POS_Y) OPTIONAL
*"     REFERENCE(END_POS_X) OPTIONAL
*"     REFERENCE(END_POS_Y) OPTIONAL
*"     REFERENCE(WINDOW_TITLE) OPTIONAL
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
*"  EXPORTING
*"     VALUE(RETURN_CODE)
*"  TABLES
*"      IT_TEXTBOX OPTIONAL
*"  EXCEPTIONS
*"      AT_LEAST_ONE_BUTTON_REQUIRED
*"--------------------------------------------------------------------

************************************************************************
*                      Canadian National
*
* Function Module Name: ZVU_POPUP_TEXTBOX1
* Function Group  Name: ZVU_GRID
* Functional design: DEV-ENH-4B1
* Created by: Steven Qiu
* Created on: 2003-01-20
* Version:    1.0
*
* Purpose: Create a custom pop up window with standard SAP text box
*          functionality in it.
*          To test run this function, put 'X' in the parameter of
*          FULL_DEMO.
*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------    ----------          ---------------       *
* Steven Qiu                 2003-01-20          DV3K926317            *
*                                                                      *
* Short Description: Initial creation                                  *
*                                                                      *
*----------------------------------------------------------------------*

  INCLUDE <ICON>.

  CLEAR V_FULL_DEMO_FLAG.

  REFRESH: I_TEXTBOX_DEMO.

  CLEAR V_WINDOW_TITLE.

  IF FULL_DEMO <> SPACE.
    V_FULL_DEMO_FLAG = 'X'.
    PERFORM F_POPULATE_DEMO_ELEMENT.
  ENDIF.

  IF FULL_DEMO IS INITIAL.

* Status bar
    IF SHOW_STATUSBAR <> SPACE.
      V_SHOW_STATUSBAR = 1.
    ELSE.
      V_SHOW_STATUSBAR = 0.
    ENDIF.

* Tool bar
    IF SHOW_TOOLBAR <> SPACE.
      V_SHOW_TOOLBAR = 1.
    ELSE.
      V_SHOW_TOOLBAR = 0.
    ENDIF.

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

* For the text box
    ASSIGN IT_TEXTBOX[]         TO <I_TEXTBOX>.

  ENDIF.

* At least one button must be assigned as exit point
  IF V_BUTTON1 IS INITIAL AND
     V_BUTTON2 IS INITIAL AND
     V_BUTTON3 IS INITIAL AND
     V_BUTTON4 IS INITIAL AND
     V_BUTTON5 IS INITIAL.
    RAISE AT_LEAST_ONE_BUTTON_REQUIRED.
    EXIT.
  ENDIF.

  CALL SCREEN 3000 STARTING AT V_START_X  V_START_Y
                   ENDING   AT V_END_X    V_END_Y.


* Pass back value to calling program
  RETURN_CODE = SAVE_OK.

ENDFUNCTION.
