*&---------------------------------------------------------------------*
*&  Include           ZME_SMENH822_PROC_TDS_EOD_F02
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
* Short Description: Include program for subroutines of SM-ENH-822     *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  f_initialization
*&---------------------------------------------------------------------*
FORM f_initialization.

  p_date = sy-datum.

* Refresh internal tables and ranges before using.
  REFRESH:  i_zmm_flcm_tds,
            i_return,
            i_return2,
            r_trans_type,
            r_bwart,
            r_bwart2.

* Clear work areas before using
  CLEAR: wa_zmm_flcm_tds.

ENDFORM.                    "f_initialization

*&---------------------------------------------------------------------*
*&      Form  f_get_data
*&---------------------------------------------------------------------*
FORM f_get_data.

  DATA: li_zmm_flcm_tds TYPE STANDARD TABLE OF zmm_flcm_tds.

  REFRESH: i_zmm_flcm_tds,
           i_zmm_flcm_tds_head,
           i_maramarc.

***Start of Insert - XT18912 -  DV5K982660 - CR223656 TK249827
  IF p_status IS NOT INITIAL.
    SELECT *
      FROM zmm_flcm_tds
      INTO TABLE i_zmm_flcm_tds_head
      WHERE ztds_tran_type   EQ c_eod
        AND ztds_tran_ref_nb IN s_trnref
        AND werks            IN s_accnt
        AND ztds_folio_yr    IN s_folyer
        AND ztds_folio_mth   IN s_folmon
        AND ztds_folio_nbr   IN s_folnum
        AND ztran_stus       EQ p_status.
  ELSE.
***End of Insert - XT18912 -  DV5K982660 - CR223656 TK249827
* Retrieve end of day to be processed from table
    SELECT *
      FROM zmm_flcm_tds
      INTO TABLE i_zmm_flcm_tds_head
      WHERE ztds_tran_type   EQ c_eod    "710
        AND ztds_tran_ref_nb IN s_trnref
        AND werks            IN s_accnt
        AND ztds_folio_yr    IN s_folyer
        AND ztds_folio_mth   IN s_folmon
        AND ztds_folio_nbr   IN s_folnum
        AND ( ztran_stus     EQ c_ready
        OR    ztran_stus     EQ c_error ).
***Start of Insert - XT18912 -  DV5K982660 - CR223656 TK249827
  ENDIF.
***End of Insert - XT18912 -  DV5K982660 - CR223656 TK249827

* since this is a job we will write out to the
  IF ( sy-subrc NE 0 ).

*   Error: No lines selected for processing
    MESSAGE ID c_err_class TYPE c_success NUMBER c_err_055.

  ENDIF.



ENDFORM.                    "f_get_data

*&---------------------------------------------------------------------*
*&      Form  f_data_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_data_processing.

  DATA:
      lo_trx_end_of_day TYPE REF TO zcl_flcm_trx_end_of_day.


* Create instance of the class ZCL_FLCM_TDS_PROC_BOL
  CREATE OBJECT lo_trx_end_of_day
    EXPORTING
      it_trans = i_zmm_flcm_tds_head
      id_date  = p_date.

* Execute the program logic in class ZCL_FLCM_TDS_PROC_BOL
  lo_trx_end_of_day->execute( ).

ENDFORM.                    "f_data_processing
***Start of Insert - XT18912 -  DV5K982660 - CR223656 TK249827
*&---------------------------------------------------------------------*
*&      Form  F_F4_STATUS
*&---------------------------------------------------------------------*
*       get valid value for p_status
*----------------------------------------------------------------------*
FORM f_f4_status .
  DATA: li_dd07t  TYPE STANDARD TABLE OF t_dd07t,
        li_retur  LIKE STANDARD TABLE OF ddshretval WITH HEADER LINE.

  SELECT domvalue_l
         ddtext
    INTO TABLE li_dd07t
    FROM dd07t
   WHERE domname    EQ c_domname
     AND ddlanguage EQ c_lang
     AND as4local   EQ c_actv.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'DOMVALUE_L'
      value_org       = 'S'
    TABLES
      value_tab       = li_dd07t
      return_tab      = li_retur
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc EQ 0.
    p_status = li_retur-fieldval.
  ENDIF.

  FREE: li_dd07t,
        li_retur.
ENDFORM.                    " F_F4_STATUS
*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_validate_data.
  IF p_status IS NOT INITIAL AND NOT p_status CO '1234567890'.
    MESSAGE TEXT-E01 TYPE 'I' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.                    " F_VALIDATE_DATA
***End of Insert - XT18912 -  DV5K982660 - CR223656 TK249827
