*&---------------------------------------------------------------------*
*&  Include           ZME_SMENH821_PROC_GI_FUEL_F01
*&---------------------------------------------------------------------*
*        MODIFICATION LOGS                                             *
*----------------------------------------------------------------------*
* Changed by            Prj/Tsk    Date                Tracking number *
*---------------------  -------    ----------          --------------- *
* 12/1/2010  Maria Cristina C. Niniel (XT16578)         DV5K961212     *
* Short Description: Include program for screen declarations of        *
*                    SM-ENH-821                                        *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*        SELECTION SCREEN                                              *
*----------------------------------------------------------------------*

SELECT-OPTIONS:
        s_trnsid    FOR zmm_flcm_tds-ztds_tran_type NO INTERVALS,
        s_trnref    FOR zmm_flcm_tds-ztds_tran_ref_nb NO INTERVALS,
        s_trnseq    FOR zmm_flcm_tds-ztds_folio_seq NO INTERVALS,
        s_trndat    FOR zmm_flcm_tds-ztds_tran_dt,
        s_trntim    FOR zmm_flcm_tds-ztds_tran_tm,
        s_folnum    FOR zmm_flcm_tds-ztds_folio_nbr NO INTERVALS,
        s_folmon    FOR zmm_flcm_tds-ztds_folio_mth NO INTERVALS,
        s_folyer    FOR zmm_flcm_tds-ztds_folio_yr  NO INTERVALS,
        s_termid    FOR zmm_flcm_tds-ztds_trml_id,
        s_accnt     FOR zmm_flcm_tds-werks,
        s_docnum    FOR zmm_flcm_tds-ztds_bol_nbr,
        s_carier    FOR zmm_flcm_tds-zcarrier NO INTERVALS,
        s_custmr    FOR zmm_flcm_tds-lifnr NO INTERVALS,
        s_canreb    FOR zmm_flcm_tds-ztds_canc_rbil NO INTERVALS,
        s_locoid    FOR zmm_flcm_tds-equnr NO INTERVALS,
        s_prodid    FOR zmm_flcm_tds-matnr.

PARAMETERS:
        p_divers    TYPE zmm_flcm_tds-ztds_dvrt_ind.

SELECT-OPTIONS:
        s_status    FOR zmm_flcm_tds-ztran_stus NO INTERVALS,
        s_lstdat    FOR zmm_flcm_tds-aedat,
        s_lsttim    FOR zmm_flcm_tds-aezeit.

PARAMETERS:
        p_change    TYPE zmm_flcm_tds-aename.
