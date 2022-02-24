*&---------------------------------------------------------------------*
*&  Include           ZME_SMENH822_PROC_TDS_EOD_F01
*&---------------------------------------------------------------------*
*        MODIFICATION LOGS                                             *
*----------------------------------------------------------------------*
* Changed by            Prj/Tsk    Date                Tracking number *
*---------------------  -------    ----------          --------------- *
* Almeria, Von(XT18912) CR223656 TK249827   10/30/2013  DV5K982660     *
*                                                                      *
* Short Description: Include status field on the selection screen      *
*                                                                      *
*----------------------------------------------------------------------*
* 12/9/2010  Bob Legault (XT11105)                      DV5K960890     *
* Short Description: Include program for screen declarations of        *
*                    SM-ENH-822                                        *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*           T A B L E S
*----------------------------------------------------------------------*
TABLES: zmm_flcm_tds.

TYPE-POOLS: ABAP.

*----------------------------------------------------------------------*
*           T Y P E S
*----------------------------------------------------------------------*


TYPES: BEGIN OF t_maramarc,
        matnr               TYPE mara-matnr,
        meins               TYPE mara-meins,
        werks               TYPE marc-werks,
        umlmc               TYPE marc-umlmc,
       END OF t_maramarc.

***Start of Insert - XT18912 -  DV5K982660 - CR223656 TK249827
  TYPES:
    BEGIN OF t_dd07t,
      domvalue_l  TYPE dd07t-domvalue_l,
      ddtext      TYPE dd07t-ddtext,
    END OF t_dd07t.
***End of Insert - XT18912 -  DV5K982660 - CR223656 TK249827

*----------------------------------------------------------------------*
*           INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA: i_zmm_flcm_tds      TYPE STANDARD TABLE OF zmm_flcm_tds,
      i_zmm_flcm_tds_head TYPE STANDARD TABLE OF zmm_flcm_tds,
      i_item              TYPE STANDARD TABLE OF bapi2017_gm_item_create,
      i_ph_item           TYPE STANDARD TABLE OF bapi_physinv_create_items,
      i_maramarc          TYPE STANDARD TABLE OF t_maramarc,
      i_return            TYPE STANDARD TABLE OF bapiret2,
      i_return2           TYPE STANDARD TABLE OF bapiret2.

*----------------------------------------------------------------------*
*           WORK AREA DECLARATION
*----------------------------------------------------------------------*
DATA: wa_zmm_flcm_tds     TYPE zmm_flcm_tds.

*----------------------------------------------------------------------*
*   UNIT CONVERSION FIELDS
*----------------------------------------------------------------------*

DATA: lv_output           TYPE gsmng,
      lv_inv_tot_count    TYPE gsmng.



*----------------------------------------------------------------------*
*           RANGES DECLARATION
*----------------------------------------------------------------------*
DATA: r_trans_type TYPE RANGE OF zmm_flcm_tds-ztds_tran_type,
      r_bwart      TYPE RANGE OF zmm_flcm_parms-zval_from,
      r_bwart2     TYPE RANGE OF zmm_flcm_parms-zval_from.

*----------------------------------------------------------------------*
*           CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: c_progname   TYPE zmm_flcm_parms-progname      VALUE 'ZME_SMENH822_PROC_TDS_EOD',
           c_eod        TYPE zmm_flcm_tds-ztds_tran_type  VALUE '710',
           c_ready      TYPE zmm_flcm_tds-ztran_stus      VALUE '00',
           c_process    TYPE bapi2017_gm_code             VALUE '03',
           c_error      TYPE zmm_flcm_tds-ztran_stus      VALUE '04',
           c_bapi_error TYPE bapi_mtype                   VALUE 'E',
           c_abend      TYPE bapi_mtype                   VALUE 'A',
           c_err_class  TYPE sy-msgid                     VALUE 'ZZ_FLCM',
           c_success    TYPE sy-msgty                     VALUE 'S',
           c_err_055    TYPE sy-msgno                     VALUE '055',
           c_physical_gain_or_loss_failed
                        TYPE zmm_flcm_tds-ztran_sub_stus  VALUE '001',
           c_phgl       TYPE wempf                        VALUE 'PHG/L',
           c_x          TYPE xfeld                        VALUE 'X',
           c_one        TYPE bstar                        VALUE '1'.

***Start of Insert - XT18912 -  DV5K982660 - CR223656 TK249827
  CONSTANTS: c_actv     TYPE as4local                     VALUE 'A',
             c_lang     TYPE ddlanguage                   VALUE 'E',
             c_domname  TYPE domname                      VALUE 'ZZTRAN_STUS'.
***End of Insert - XT18912 -  DV5K982660 - CR223656 TK249827
*----------------------------------------------------------------------*
*        SELECTION SCREEN                                              *
*----------------------------------------------------------------------*
PARAMETERS:
        p_date         TYPE datum.


SELECT-OPTIONS:
        s_trnref    FOR zmm_flcm_tds-ztds_tran_ref_nb NO INTERVALS,
        s_folnum    FOR zmm_flcm_tds-ztds_folio_nbr NO INTERVALS,
        s_folmon    FOR zmm_flcm_tds-ztds_folio_mth NO INTERVALS,
        s_folyer    FOR zmm_flcm_tds-ztds_folio_yr  NO INTERVALS,
        s_accnt     FOR zmm_flcm_tds-werks.
***Start of Insert - XT18912 -  DV5K982660 - CR223656 TK249827
PARAMETERS:
        p_status    TYPE char02.

AT SELECTION-SCREEN on VALUE-REQUEST FOR p_status.
  PERFORM f_f4_status.
***End of Insert - XT18912 -  DV5K982660 - CR223656 TK249827
