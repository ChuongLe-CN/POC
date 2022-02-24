*&---------------------------------------------------------------------*
*&  Include           ZME_SMENH819_PROC_TDS_BOL_TOP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                 Canadian National                                    *
*                                                                      *
* Program     : ZME_SMENH819_PROC_TDS_BOL                              *
* Created by  : Eric Lagasca                                           *
* Created on  : January 06, 2011                                       *
* Description : This requirement includes the creation of a new        *
*               program to process diesel purchase orders and their    *
*               goods receipts based on the bill of lading information *
*               received real time from TDS                            *
* Task number : DV5K961164                                             *
*----------------------------------------------------------------------*
*        MODIFICATION LOGS                                             *
*----------------------------------------------------------------------*
* Changed by            Prj/Tsk    Date                Tracking number *
*---------------------  -------    ----------          --------------- *
* <Name> (XTxxxxx)      <...>      MM/DD/YYYY            xxxxxxxxxx    *
************************************************************************

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: zmm_flcm_tds.
*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPE-POOLS: abap.
*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS: c_include       TYPE c VALUE 'I',
           c_equal         TYPE c LENGTH 2 VALUE 'EQ',
           c_tran_type_502 TYPE zmm_flcm_tds-ztds_tran_type VALUE '502',
           c_err_class     TYPE sy-msgid VALUE 'ZZ_FLCM',
           c_success       TYPE sy-msgty VALUE 'S',
           c_err_055       TYPE sy-msgno VALUE '055'.
*----------------------------------------------------------------------*
* DATA DECLARATIONS
*----------------------------------------------------------------------*
DATA: i_tds  TYPE zmm_flcm_tds_t,
      wa_tds LIKE LINE OF i_tds.
DATA: ob_trx_proc_bol TYPE REF TO zcl_flcm_trx_proc_bol.
*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
INCLUDE zme_smenh821_proc_gi_fuel_f01.
SELECTION-SCREEN   END OF BLOCK b1.
