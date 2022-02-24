*&---------------------------------------------------------------------*
*&  Include           ZFI_FIINT5FM_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by             Date                Tracking number *
* -------------------------        ----------          --------------- *
* Aldrien Baugbog                  2014/12/17          DV5K991085      *
*                                                                      *
* Short Description :                                                  *
*      D#50 - Add On-Demand request functionality                      *
*----------------------------------------------------------------------*
* Von Kaisser Almeria              2014/09/16          DV5K990246      *
*                                                                      *
* Short Description :                                                  *
*      D#38 - FI-INT-5Fx_Create control files with summation           *
*----------------------------------------------------------------------*
* Mia Calleen Avila (XT18903)     2014/04/07          DV5K983744       *
*                                 2014/03/06          DV5K983744       *
*                                 2014/02/11          DV5K983744       *
*                                 2013/12/05          DV5K983744       *
*                                                                      *
* Short Description : CR234468 TK250623                                *
*       - G/L Account changes                                          *
*       - Modify retrieval for posting period data and local currency  *
*       - Initial Creation                                             *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*        TABLES DECLARATION                                            *
*----------------------------------------------------------------------*
TABLES: tracv_accitem.
*----------------------------------------------------------------------*
*        TYPES DECLARATION                                             *
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF t_posting_lines,
         dis_flowtype         TYPE tracv_accitem-dis_flowtype,
         gl_account           TYPE tracv_accitem-gl_account,
         position_amt         TYPE tracv_accitem-position_amt,
         position_curr        TYPE tracv_accitem-position_curr,
         local_amt            TYPE tracv_accitem-local_amt,
         acpostingperiod      TYPE tracv_accitem-acpostingperiod,
         acpostingyear        TYPE tracv_accitem-acpostingyear,
         company_code         TYPE tracv_accitem-company_code,
         product_type         TYPE tracv_accitem-product_type,
         deal_number          TYPE tracv_accitem-deal_number,
         tr_rev_reason        TYPE tracv_accitem-tr_rev_reason,
  END OF t_posting_lines,

***BEGIN OF INSERT - XT18912 - D#38 - DV5K990246
  BEGIN OF t_ctrl_output,
    field1  TYPE string,
    field2  TYPE char12,
    field3  TYPE char10,
    field4  TYPE char10,
    field5  TYPE string,
    field6  TYPE string,
    field7  TYPE string,
    field8  TYPE string,
    field9  TYPE char23,
  END OF t_ctrl_output,
***END OF INSERT - XT18912 - D#38 - DV5K990246

  BEGIN OF t_posting_output,
          acpostingyear       TYPE string,
          acpostingperiod     TYPE string,
          company_code        TYPE string,
          product_type        TYPE string,
          deal_number         TYPE string,
          dis_flowtype        TYPE string,
          gl_account          TYPE string,
          position_curr       TYPE string,
          amount              TYPE string,
   END OF  t_posting_output.

***Start of Insert - XT18910 - D#50 - DV5K991085
TYPES: BEGIN OF t_setleaf,
        setname   TYPE setleaf-setname,
        valsign   TYPE setleaf-valsign,
        valoption TYPE setleaf-valoption,
        valfrom   TYPE setleaf-valfrom,
        valto     TYPE setleaf-valto,
       END OF t_setleaf.
***eND of Insert - XT18910 - D#50 - DV5K991085

*----------------------------------------------------------------------*
*        DATA DECLARATION                                              *
*----------------------------------------------------------------------*
DATA:
   i_posting_lines   TYPE STANDARD TABLE OF t_posting_lines,
   i_posting_output  TYPE STANDARD TABLE OF t_posting_output,
   i_t_saktiv        TYPE TABLE OF dd07v INITIAL SIZE 0,
   i_ctrl_output     TYPE STANDARD TABLE OF t_ctrl_output,   "I - XT18912 - D#38 - DV5K990246
   r_status          TYPE RANGE OF tracv_accitem-tr_rev_reason,
   wa_output_file    TYPE t_posting_output,
***BEGIN OF INSERT - XT18912 - D#38 - DV5K990246
   v_start           TYPE timestamp,
   v_end             TYPE timestamp,
   v_start_date      TYPE sy-datum,
   v_end_date        TYPE sy-datum,
   v_start_time      TYPE sy-uzeit,
   v_end_time        TYPE sy-uzeit,
   v_count           TYPE i,
   v_sum             TYPE faglflext-ksl02,
***END OF INSERT - XT18912 - D#38 - DV5K990246
***Start of Insert - XT18903 - DV5K983744 - CR234468 TK250623
   v_up_to_period    TYPE poper,
   v_current_period  TYPE poper.
***End of Insert - XT18903 - DV5K983744 - CR234468 TK250623

***Start of Insert - XT18910 - D#50 - DV5K991085
DATA: v_fyear       TYPE tracv_accitem-acpostingyear,
      v_period      TYPE tracv_accitem-acpostingperiod,
      v_filename    TYPE string,
      v_email       TYPE string,
      v_tm1         TYPE c.

DATA: r_ccode       TYPE RANGE OF tracv_accitem-company_code,
      r_ptype       TYPE RANGE OF tracv_accitem-product_type,
      r_utype       TYPE RANGE OF tracv_accitem-dis_flowtype,
      r_gl          TYPE RANGE OF tracv_accitem-gl_account,
      r_period      TYPE RANGE OF tracv_accitem-acpostingperiod.
***End of Insert - XT18910 - D#50 - DV5K991085
*----------------------------------------------------------------------*
*        CONSTANTS DECLARATION                                         *
*----------------------------------------------------------------------*
CONSTANTS:
  c_cn_accounts  TYPE t004-ktopl VALUE 'COA1',
  c_english(2)   TYPE c          VALUE 'EN',
  c_tc_curr(3)   TYPE c          VALUE 'TC_',
  c_lc_curr(3)   TYPE c          VALUE 'LC_',        "I - XT18903 - DV5K983744 - CR234468 TK250623
*  c_lc_curr(2)   TYPE c          VALUE 'LC',        "D - XT18903 - DV5K983744 - CR234468 TK250623
*  c_copy(2)      TYPE c          VALUE 'cp',        "D - XT18903 - DV5K983744 - CR234468 TK250623
*  c_system(6)    TYPE c          VALUE 'SYSTEM',    "D - XT18903 - DV5K983744 - CR234468 TK250623
*  c_command(7)   TYPE c          VALUE 'COMMAND',   "D - XT18903 - DV5K983744 - CR234468 TK250623
  c_error        TYPE c          VALUE 'E',
  c_comma        TYPE c          VALUE ',',
  c_dbl_quote    TYPE c          VALUE '"',
  c_amp          TYPE c          VALUE '&',
*  c_file_path    TYPE string     VALUE '/iface/&/out/com/fidata/FIFM01.csv', "D - XT18912 - D#38 - DV5K990246
***BEGIN OF INSERT - XT18912 - D#38 - DV5K990246
  c_file_path    TYPE string     VALUE '/iface/&/out/com/fidata/&',
  c_ctrlfilepath TYPE string     VALUE '/iface/&/out/com/fidata/CTRL_&',
***END OF INSERT - XT18912 - D#38 - DV5K990246
  c_archive      TYPE string     VALUE '/iface/&/out/ia/arc/fidata/fifm01_&&.csv'.

***Start of Insert - XT18910 - D#50 - DV5K991085
CONSTANTS: c_intid  TYPE zifad-zintid VALUE 'FIFM01',
           c_file   TYPE string       VALUE '/iface/&/in/com/fidata/fifm01.dat',
           c_ptype1 TYPE string VALUE 'Product Type',
           c_ptype2 TYPE string VALUE 'Product Type Set',
           c_utype1 TYPE string VALUE 'Update Type',
           c_utype2 TYPE string VALUE 'Update Type Set',
           c_gl1    TYPE string VALUE 'G/L Account',
           c_gl2    TYPE string VALUE 'G/L Account Set'.
***End of Insert - XT18910 - D#50 - DV5K991085
*----------------------------------------------------------------------*
*        SELECTION-SCREEN                                              *
*----------------------------------------------------------------------*
***Start of Delete - XT18910 - D#50 - DV5K991085
*SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
*SELECT-OPTIONS: s_ccode   FOR tracv_accitem-company_code,        "Company Code
*                s_ptype   FOR tracv_accitem-product_type,        "Product Type
*                s_status  FOR tracv_accitem-tr_rev_reason,       "Active Status
*                s_utype   FOR tracv_accitem-dis_flowtype.        "Update type
*SELECTION-SCREEN END OF BLOCK b1.
*
**SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
*****Start of Delete - XT18903 - DV5K983744 - CR234468 TK250623
***PARAMETERS: p_fy  TYPE tracv_accitem-acpostingyear   OBLIGATORY, "Fiscal Year
***            p_pp  TYPE tracv_accitem-acpostingperiod OBLIGATORY, "Posting Period
***            p_gl  TYPE tracv_accitem-gl_account.                 "G/L Account
*****End of Delete - XT18903 - DV5K983744 - CR234468 TK250623
*****Start of Insert - XT18903 - DV5K983744 - CR234468 TK250623
**PARAMETERS:     p_fy      TYPE tracv_accitem-acpostingyear   OBLIGATORY. "Fiscal Year
**SELECT-OPTIONS: s_period  FOR  tracv_accitem-acpostingperiod,            "Posting Period
**                s_gl_acnt FOR  tracv_accitem-gl_account.                 "G/L Account
***PARAMETERS:     p_gl      TYPE tracv_accitem-gl_account.                 "G/L Account
*****End of Insert - XT18903 - DV5K983744 - CR234468 TK250623
**SELECTION-SCREEN END OF BLOCK b2.
*
****Start of Insert - XT18903 - DV5K983744 - CR234468 TK250623
*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
*PARAMETERS:     p_fy      TYPE tracv_accitem-acpostingyear OBLIGATORY.    "Fiscal Year
*SELECT-OPTIONS: s_period  FOR  tracv_accitem-acpostingperiod NO-EXTENSION, "Posting Period
*                s_gl      FOR  tracv_accitem-gl_account.                   "G/L Account
*PARAMETERS:  p_file TYPE string DEFAULT 'SAP_TREASURY.CSV' OBLIGATORY. "I - XT18912 - D#38 - DV5K990246
*SELECTION-SCREEN END OF BLOCK b2.
***End of Insert - XT18903 - DV5K983744 - CR234468 TK250623
***End of Delete - XT18910 - D#50 - DV5K991085

***Start of Insert - XT18910 - D#50 - DV5K991085
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_ccode   FOR tracv_accitem-company_code,         "Company Code
                s_ptype   FOR tracv_accitem-product_type.         "Product Type
PARAMETERS:     p_ptypes  TYPE setleaf-setname.                   "Product Type Set
SELECT-OPTIONS: s_status  FOR tracv_accitem-tr_rev_reason,        "Active Status
                s_utype   FOR tracv_accitem-dis_flowtype.         "Update type
PARAMETERS:     p_utypes  TYPE setleaf-setname.                   "Update Type Set
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS:     p_fy      TYPE tracv_accitem-acpostingyear OBLIGATORY.     "Fiscal Year
SELECT-OPTIONS: s_period  FOR  tracv_accitem-acpostingperiod NO-EXTENSION, "Posting Period
                s_gl      FOR  tracv_accitem-gl_account.                   "G/L Account
PARAMETERS:     p_gls     TYPE setleaf-setname.                            "G/L Account Set
PARAMETERS:     p_file TYPE string DEFAULT 'SAP_TREASURY.CSV' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.
***End of Insert - XT18910 - D#50 - DV5K991085
