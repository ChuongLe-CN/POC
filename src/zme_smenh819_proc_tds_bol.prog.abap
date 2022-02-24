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
* Task number : DV5K961214                                             *
*----------------------------------------------------------------------*
*        MODIFICATION LOGS                                             *
*----------------------------------------------------------------------*
* Changed by            Prj/Tsk    Date                Tracking number *
*---------------------  -------    ----------          --------------- *
* <Name> (XTxxxxx)      <...>      MM/DD/YYYY            xxxxxxxxxx    *
************************************************************************

REPORT zme_smenh819_proc_tds_bol
  NO STANDARD PAGE HEADING.

*----------------------------------------------------------------------*
* GLOBAL DATA
*----------------------------------------------------------------------*
INCLUDE zme_smenh819_proc_tds_bol_top. "GLOBAL DATA
INCLUDE zme_smenh819_proc_tds_bol_f01. "SUBROUTINES

*----------------------------------------------------------------------*
* PROGRAM LOGIC
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Retrieve the data from ZMM_FLCM_TDS
  PERFORM get_data USING sy-batch.
  CHECK i_tds IS NOT INITIAL.
* Create instance of the class ZCL_FLCM_TRX_PROC_BOL
  CREATE OBJECT ob_trx_proc_bol
    EXPORTING
      it_trans = i_tds.
* Execute the program logic
  ob_trx_proc_bol->execute( ).
