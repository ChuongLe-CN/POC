FUNCTION-POOL zmm_smint60a.                 "MESSAGE-ID .

************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name: LZMM_SMINT60ATOP                                          *
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
* Chari Flor Supino          2017/08/03          DV5K9A0ARX            *
*                                                                      *
* Short Description: CR302137 TK365122                                 *
*                    Add delivery date to determine duplicate          *
*----------------------------------------------------------------------*
* Rob West                  2011/03/01          DV5K961203             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

TYPE-POOLS: abap,
            slis.

TYPES: BEGIN OF t_flcm_ref,
         ztds_tran_ref_nb  TYPE zztds_tran_ref_nbr,
         ztds_tran_type    TYPE zztds_tran_type,
         ztds_trml_id      TYPE zztds_terminal_id,
         ztds_bol_nbr      TYPE zztds_bol_nbr,
         ztds_canc_rbil    TYPE zztds_cancel_rebill,
         ztds_tran_dt      TYPE zztds_transaction_dt, "I - XT22260 - DV5K9A0ARX - CR302137 TK365122
         flcm_pointer      TYPE REF TO zcl_smint60a_zmm_flcm_tds,
       END OF t_flcm_ref,

       t_flcm_ref_table    TYPE STANDARD TABLE OF t_flcm_ref
                                WITH DEFAULT KEY.

CONSTANTS: c_smint60a      TYPE c LENGTH 8 VALUE 'SMINT60A'.
