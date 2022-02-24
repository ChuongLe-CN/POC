*----------------------------------------------------------------------*
***INCLUDE LZVU_GRIDI01 .
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
*&      Module  MO_1000_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MO_1000_USER_COMMAND INPUT.

  SAVE_OK = OKCODE_1000.

  CLEAR OKCODE_1000.

  CASE SAVE_OK.

    WHEN 'F_B1' OR 'F_B2' OR 'F_B3' OR 'F_B4' OR 'F_B5'.

      PERFORM F_WRAP_UP.

    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " MO_1000_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Module  MO_2000_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MO_2000_USER_COMMAND INPUT.

  SAVE_OK = OKCODE_2000.

  CLEAR OKCODE_2000.

  CASE SAVE_OK.

    WHEN 'F_B1' OR 'F_B2' OR 'F_B3' OR 'F_B4' OR 'F_B5'.

      CALL METHOD TEXT_CONTROL1->DESTROY_CONTROL.

      PERFORM F_WRAP_UP.

    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " MO_2000_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  MO_3000_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MO_3000_USER_COMMAND INPUT.

  SAVE_OK = OKCODE_3000.

  CLEAR OKCODE_3000.

  CASE SAVE_OK.

    WHEN 'F_B1' OR 'F_B2' OR 'F_B3' OR 'F_B4' OR 'F_B5'.

      CALL METHOD TEXT_CONTROL1->DESTROY_CONTROL.

      CALL METHOD C_TEXTEDIT_CONTROL=>FLUSH
              EXCEPTIONS
                   FLUSH_ERROR = 01.

      CALL METHOD CL_GUI_CFW=>FLUSH.

* Leave the current screen
      LEAVE TO SCREEN 0.

    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " MO_3000_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  MO_4000_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MO_4000_USER_COMMAND INPUT.

  SAVE_OK = OKCODE_4000.

  CLEAR OKCODE_4000.

  CASE SAVE_OK.

    WHEN 'F_B1' OR 'F_B2' OR 'F_B3' OR 'F_B4' OR 'F_B5'.

      CALL METHOD TEXT_CONTROL1->DESTROY_CONTROL.

* Free object and memory
      FREE EVENT_RECEIVER.
      CLEAR EVENT_RECEIVER.

      CALL METHOD GRID1->FREE.
      CALL METHOD CUSTOM_CONTAINER1->FREE.

      CALL METHOD GRID2->FREE.
      CALL METHOD CUSTOM_CONTAINER2->FREE.


      CALL METHOD CL_GUI_CFW=>FLUSH.

      CLEAR GRID1.
      CLEAR CUSTOM_CONTAINER1.

      CLEAR GRID2.
      CLEAR CUSTOM_CONTAINER2.

* Leave the current screen
      LEAVE TO SCREEN 0.


    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " MO_4000_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  MO_1010_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MO_1010_USER_COMMAND INPUT.

  SAVE_OK = OKCODE_1000.

  CLEAR OKCODE_1000.

  CASE SAVE_OK.

    WHEN 'F_B1' OR 'F_B2' OR 'F_B3' OR 'F_B4' OR 'F_B5' OR 'CANC'.

      PERFORM F_WRAP_UP.

    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " MO_1010_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  MO_2010_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MO_2010_USER_COMMAND INPUT.

  SAVE_OK = OKCODE_2000.

  CLEAR OKCODE_2000.

  CASE SAVE_OK.

    WHEN 'F_B1' OR 'F_B2' OR 'F_B3' OR 'F_B4' OR 'F_B5' OR 'CANC'.

      CALL METHOD TEXT_CONTROL1->DESTROY_CONTROL.

      PERFORM F_WRAP_UP.

    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " MO_2010_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  MO_3010_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MO_3010_USER_COMMAND INPUT.

  SAVE_OK = OKCODE_3000.

  CLEAR OKCODE_3000.

  CASE SAVE_OK.

    WHEN 'F_B1' OR 'F_B2' OR 'F_B3' OR 'F_B4' OR 'F_B5' OR 'CANC'.

      CALL METHOD TEXT_CONTROL1->DESTROY_CONTROL.

      CALL METHOD C_TEXTEDIT_CONTROL=>FLUSH
              EXCEPTIONS
                   FLUSH_ERROR = 01.

      CALL METHOD CL_GUI_CFW=>FLUSH.

* Leave the current screen
      LEAVE TO SCREEN 0.

    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " MO_3010_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  MO_4010_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MO_4010_USER_COMMAND INPUT.

  SAVE_OK = OKCODE_4000.

  CLEAR OKCODE_4000.

  CASE SAVE_OK.

    WHEN 'F_B1' OR 'F_B2' OR 'F_B3' OR 'F_B4' OR 'F_B5' OR 'CANC'.

      CALL METHOD TEXT_CONTROL1->DESTROY_CONTROL.

* Free object and memory
      FREE EVENT_RECEIVER.
      CLEAR EVENT_RECEIVER.

      CALL METHOD GRID1->FREE.
      CALL METHOD CUSTOM_CONTAINER1->FREE.

      CALL METHOD GRID2->FREE.
      CALL METHOD CUSTOM_CONTAINER2->FREE.


      CALL METHOD CL_GUI_CFW=>FLUSH.

      CLEAR GRID1.
      CLEAR CUSTOM_CONTAINER1.

      CLEAR GRID2.
      CLEAR CUSTOM_CONTAINER2.

* Leave the current screen
      LEAVE TO SCREEN 0.


    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " MO_4010_USER_COMMAND  INPUT
