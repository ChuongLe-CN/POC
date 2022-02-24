*----------------------------------------------------------------------*
*                 Canadian National                                    *
*                                                                      *
* Program   : ZME_SMENH822_PROC_TDS_EOD                                *
* Created by: Bob Legault                                              *
* Created on: December 9, 2010                                         *
* Report Description     : This enhancement is covering the processign *
*                          of End of Day records which have been loaded*
*                          in the ZMM_FLCM_TDS table from the interface*
*                          SM-INT-60A. Basically Bapi to perform a     *
*                          Goodsmovement creation will be performed    *
*                          along with Bapi for physical inventory count*
*                          The z table status will be updated to       *
*                          indicate the status of the record           *
*----------------------------------------------------------------------*
*        MODIFICATION LOGS                                             *
*----------------------------------------------------------------------*
* Changed by            Prj/Tsk    Date                Tracking number *
*---------------------  -------    ----------          --------------- *
* Almeria, Von(XT18912) CR223656 TK249827   10/30/2013  DV5K982660     *
*                                                                      *
* Short Description: Include status field on the selection screen      *
*                                                                      *
*----------------------------------------------------------------------*
* 12/9/2010  Bob Legault (XT11105)                       DV5K960890    *
* Initial Creation                                                     *
************************************************************************
REPORT  zme_smenh822_proc_tds_eod.
*----------------------------------------------------------------------*
*        INCLUDES                                                      *
*----------------------------------------------------------------------*
INCLUDE: zme_smenh822_proc_tds_eod_f01, "include for data declaration
         zme_smenh822_proc_tds_eod_f02. "include for subroutines

*----------------------------------------------------------------------*
*        INITIALIZATION                                                *
*----------------------------------------------------------------------*
INITIALIZATION.
PERFORM f_initialization.

*----------------------------------------------------------------------*
*  Start-of-Selection / End-of-Selection                               *
*----------------------------------------------------------------------*
START-OF-SELECTION.
PERFORM f_validate_data.                                                "I - XT18912 - DV5K982660 - CR223656 TK249827
PERFORM f_get_data.
PERFORM f_data_processing.
