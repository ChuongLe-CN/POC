*----------------------------------------------------------------------*
*                      Canadian National                               *
*                                                                      *
* ABAP Name         : ZMI_SMINT029_TDS_FUEL_TO_EDW                     *
* Created by        : Bob Legault                                      *
* Created on        : 2011-04-08                                       *
* Function Design   : SM-INT-029                                       *
*                                                                      *
* Purpose: Daily extract of TDS fuel records in the ZMM_FLSM_TDS table *
*          which have either been processed or not will be extracted   *
*          and sent to Datacity                                        *
*                                                                      *
* Short Description : Extract of TDS records in the ZMM_FLSCM_TDS table*
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
* Manuel Gonzaga Jr.      2016-11-02                    DV5K9A052C     *
*                                                                      *
* Short Description : CR310744 TK342077                                *
*  - Add transaction date in the screen. Add radiobutton for option    *
*    to select flagged entries from table ZMM_FLSCM_TDS                *
*  - Change filename format if date range is entered                   *
*----------------------------------------------------------------------*
* Von Kaisser Almeria 2014-08-19                        DV5K989493     *
*                                                                      *
* Short Description : CR250144 TK271028                                *
*   Add new fields from ZMM_FLCM_TDS at the end of the output file     *
*----------------------------------------------------------------------*
* Bob Legault         2011-11-10  Defect 652            DV5K967547     *
*                                                                      *
* Short Description : Add writing to the app log                       *
*                                                                      *
*----------------------------------------------------------------------*
* Bob Legault         2011-04-08                        DV5K9960962    *
*                                                                      *
* Short Description : Initial Development                              *
*                                                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
REPORT  zmi_smint029_tds_fuel_to_edw NO STANDARD PAGE HEADING LINE-SIZE 340
        MESSAGE-ID zz_flcm.
TABLES:zmm_flcm_tds.
* Declarations - internal tables, work areas, variables & constants
INCLUDE ZMI_SMINT029_TOP.

*&---------------------------------------------------------------------*
*&      SELECTION SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
        s_trnsid    FOR zmm_flcm_tds-ztds_tran_type NO INTERVALS,
        s_trnsts    FOR zmm_flcm_tds-ztran_stus     NO INTERVALS.
SELECTION-SCREEN END OF BLOCK a1.

***Start of Insert - XT21326 -  DV5K9A052C - CR310744 TK342077
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME TITLE text-015.
SELECT-OPTIONS: s_trndat  FOR zmm_flcm_tds-ztds_tran_dt NO-EXTENSION.
PARAMETERS: rb_flag       RADIOBUTTON GROUP g1 DEFAULT 'X',
            rb_unf        RADIOBUTTON GROUP g1,
            rb_all        RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK a2.
***End of Insert - XT21326 -  DV5K9A052C - CR310744 TK342077

*&---------------------------------------------------------------------*
*&      INITIALIZATION
*&---------------------------------------------------------------------*
PERFORM f_initialize.

*&---------------------------------------------------------------------*
*&      START-OF-SELECTION
*&---------------------------------------------------------------------*
* Get the data from database table
** Process the data / Calculation of Volume
PERFORM f_data_retrieval CHANGING v_subrc.

* Create the Unix file for FTP Server
PERFORM f_create_unix USING space.

* Contains the subroutines
INCLUDE ZMI_SMINT029_CREF01.
