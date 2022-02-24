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

*&---------------------------------------------------------------------*
*&  Include           ZME_SMENH819_PROC_TDS_BOL_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Retrieve data from database
*----------------------------------------------------------------------*
FORM get_data USING p_batch TYPE sy-batch.
  DATA: lr_stus  TYPE RANGE OF zmm_flcm_tds-ztran_stus,
        lwa_stus LIKE LINE OF lr_stus,
        lwa_trnsid LIKE LINE OF s_trnsid.

*  CLEAR lwa_stus.
*  lwa_stus-sign   = c_include.                          "I
*  lwa_stus-option = c_equal.                            "EQ
*  lwa_stus-low    = zcl_flcm_services=>mc_status_error. "04
*  APPEND lwa_stus TO lr_stus.

*  IF sy-batch EQ abap_true.
*   When run in batch, select only 502 transaction type
    REFRESH s_trnsid.
    CLEAR   lwa_trnsid.
    lwa_trnsid-sign   = c_include.       "I
    lwa_trnsid-option = c_equal.         "EQ
    lwa_trnsid-low    = c_tran_type_502. "502
    APPEND lwa_trnsid TO s_trnsid.

*   When run in batch, include status 00 in selection
*    CLEAR: lwa_stus-low.
*    lwa_stus-low = zcl_flcm_services=>mc_status_ready. "00
*    APPEND lwa_stus TO lr_stus.
*  ENDIF.

* Retrieve data from ZMM_FLCM_TDS
  SELECT *
    FROM zmm_flcm_tds
    INTO TABLE i_tds
   WHERE ztds_tran_type   IN s_trnsid
     AND ztds_tran_ref_nb IN s_trnref
     AND werks            IN s_accnt
     AND ztds_folio_yr    IN s_folyer
     AND ztds_folio_mth   IN s_folmon
     AND ztds_folio_nbr   IN s_folnum
     AND ztds_folio_seq   IN s_trnseq
     AND ztds_trml_id     IN s_termid
     AND ztds_bol_nbr     IN s_docnum
     AND ztds_tran_dt     IN s_trndat
     AND ztds_tran_tm     IN s_trntim
     AND zcarrier         IN s_carier
     AND lifnr            IN s_custmr
     AND equnr            IN s_locoid
     AND matnr            IN s_prodid
     AND ztds_canc_rbil   IN s_canreb
     AND ztran_stus       IN s_status
     AND aedat            IN s_lstdat
     AND aezeit           IN s_lsttim.

  IF sy-subrc EQ 0.
*   Sort from oldest to newest based on ZTDS_TRAN_REF_NB
    SORT i_tds BY ztds_tran_ref_nb ASCENDING.
*   Delete fields based on values of parameters
*   CANCEL / REBILL FLAG
*   TDS DIVERSION INDICATOR
    IF p_divers IS NOT INITIAL.
      DELETE i_tds WHERE ztds_dvrt_ind NE p_divers.
    ENDIF.
*   LAST CHANGED BY
    IF p_change IS NOT INITIAL.
      DELETE i_tds WHERE aename NE p_change.
    ENDIF.
  ELSE.
*   Error: No lines selected for processing
    MESSAGE ID c_err_class TYPE c_success NUMBER c_err_055.
  ENDIF.
ENDFORM.                    " GET_DATA
