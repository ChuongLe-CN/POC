FUNCTION zmm_flcm_tds_send.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TDS_DATA_TABLE) TYPE  ZMM_FLCM_TDS_INTERFACE_TABLE
*"----------------------------------------------------------------------
************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZMM_FLCM_TDS_SEND                                        *
* Title    :  Receive data from TDS and store it to ZMM_FLCM_TDS       *
* Work Unit:  SM-INT-60A-001                                           *
* Created by: Rob West                                                 *
* Created on: December 2010                                            *
* Version:    1.0                                                      *
*                                                                      *
* Purpose:    Receive data from TDS and store it to ZMM_FLCM_TDS       *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2011/03/01          DV5K961203             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

  CALL FUNCTION 'ZMM_FLCM_TDS_SEND_ASYNC' STARTING NEW TASK c_smint60a DESTINATION IN GROUP DEFAULT
    EXPORTING
      tds_data_table = tds_data_table[].

ENDFUNCTION.
