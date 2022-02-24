*&---------------------------------------------------------------------*
*&  Include           ZME_SMENH821_PROC_GI_FUEL_F02
*&---------------------------------------------------------------------*
*        MODIFICATION LOGS                                             *
*----------------------------------------------------------------------*
* Changed by            Prj/Tsk    Date                Tracking number *
*---------------------  -------    ----------          --------------- *
* 12/2/2010  Maria Cristina C. Niniel (XT16578)         DV5K961212     *
* Short Description: Include program for subroutines of SM-ENH-821     *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*           T Y P E S
*----------------------------------------------------------------------*
TYPES: BEGIN OF t_zmm_flcm_tds,
        ztds_tran_type      TYPE zmm_flcm_tds-ztds_tran_type,
        ztds_tran_ref_nbr   TYPE mkpf-bktxt,
        werks               TYPE zmm_flcm_tds-werks,
        ztds_folio_yr       TYPE zmm_flcm_tds-ztds_folio_yr,
        ztds_folio_mth      TYPE zmm_flcm_tds-ztds_folio_mth,
        ztds_folio_nbr      TYPE zmm_flcm_tds-ztds_folio_nbr,
        ztds_folio_seq      TYPE zmm_flcm_tds-ztds_folio_seq,
        ztds_trml_id        TYPE mseg-werks,
        ztds_bol_nbr        TYPE zmm_flcm_tds-ztds_bol_nbr,
        ztds_tran_dt        TYPE zmm_flcm_tds-ztds_tran_dt,
        ztds_tran_tm        TYPE zmm_flcm_tds-ztds_tran_tm,
        ztds_canc_rbil      TYPE zmm_flcm_tds-ztds_canc_rbil,
        equnr               TYPE zmm_flcm_tds-equnr,
        meins               TYPE zmm_flcm_tds-meins,
        ztds_net_vol        TYPE zmm_flcm_tds-ztds_net_vol,
        matnr               TYPE zmm_flcm_tds-matnr,
        ztds_dvrt_ind       TYPE zmm_flcm_tds-ztds_dvrt_ind,
        ztran_stus          TYPE zmm_flcm_tds-ztran_stus,
        aedat               TYPE zmm_flcm_tds-aedat,
        aezeit              TYPE zmm_flcm_tds-aezeit ,
        aename              TYPE zmm_flcm_tds-aename,
        transnum            TYPE zmm_flcm_tds-ztds_tran_ref_nb,
      END OF t_zmm_flcm_tds.

TYPES: BEGIN OF t_mkpf,
        mblnr TYPE mkpf-mblnr,
        mjahr TYPE mkpf-mjahr,
        bktxt TYPE mkpf-bktxt,
        werks TYPE mseg-werks,
       END OF t_mkpf.

TYPES: BEGIN OF t_zmm_flcm_plntdef,
         werks        TYPE zmm_flcm_plntdef-werks,
         lgort        TYPE zmm_flcm_plntdef-lgort,
         bukrs        TYPE zmm_flcm_plntdef-bukrs,
         zskato_fuel  TYPE zmm_flcm_plntdef-zskato_fuel,
         gsber        TYPE zmm_flcm_plntdef-gsber,
         kostl        TYPE zmm_flcm_plntdef-kostl,
*         znxtperopen  type zmm_flcm_plntdef-znxtperopen,         "Uncomment when this field is alraedy available in zmm_flcm_plntdef
       END OF t_zmm_flcm_plntdef.

TYPES: BEGIN OF t_bwart,
        ztds_tran_type      TYPE zmm_flcm_tds-ztds_tran_type,
        ztds_canc_rbil      TYPE zmm_flcm_tds-ztds_canc_rbil,
        bwart(3)     TYPE c,
       END OF t_bwart.

*----------------------------------------------------------------------*
*           INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA: i_zmm_flcm_parms    TYPE STANDARD TABLE OF zmm_flcm_parms,
      i_zmm_flcm_tds      TYPE STANDARD TABLE OF zmm_flcm_tds,
      i_mkpf              TYPE STANDARD TABLE OF t_mkpf,
      i_zmm_flcm_plntdef  TYPE STANDARD TABLE OF t_zmm_flcm_plntdef,
      i_item              TYPE STANDARD TABLE OF bapi2017_gm_item_create,
      i_return            TYPE STANDARD TABLE OF bapiret2,
      i_return2           TYPE STANDARD TABLE OF bapiret2,
      i_zmm_flcm_xref     TYPE STANDARD TABLE OF zmm_flcm_xref.

*----------------------------------------------------------------------*
*           WORK AREA DECLARATION
*----------------------------------------------------------------------*
DATA: wa_zmm_flcm_parms   TYPE zmm_flcm_parms,
      wa_zmm_flcm_tds     TYPE t_zmm_flcm_tds,
      wa_zmm_flcm_plntdef TYPE t_zmm_flcm_plntdef,
      wa_header           TYPE bapi2017_gm_head_01,
      wa_item             TYPE bapi2017_gm_item_create,
      wa_return           TYPE bapiret2,
      wa_zmm_flcm_xref    TYPE zmm_flcm_xref.
*----------------------------------------------------------------------*
*           RANGES DECLARATION
*----------------------------------------------------------------------*
DATA: r_trans_type TYPE RANGE OF zmm_flcm_tds-ztds_tran_type,
      r_bwart      TYPE RANGE OF zmm_flcm_parms-zval_from.

*----------------------------------------------------------------------*
*           CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: c_progname   TYPE zmm_flcm_parms-progname      VALUE 'SMENH820',
           c_zparm_nm1  TYPE zmm_flcm_parms-zparm_nm      VALUE 'ZTDS_TRAN_TYPE',
           c_ready      TYPE zmm_flcm_tds-ztran_stus      VALUE '00',
           c_process    TYPE bapi2017_gm_code             VALUE '03',
           c_error      TYPE zmm_flcm_tds-ztran_stus      VALUE '04',
           c_sign       TYPE c                            VALUE 'I',
           c_option(2)  TYPE c                            VALUE 'EQ',
           c_bwart      TYPE zmm_flcm_xref-zvar_nm        VALUE 'BWART',
           c_err_class  TYPE sy-msgid                     VALUE 'ZZ_FLCM',
           c_success    TYPE sy-msgty                     VALUE 'S',
           c_err_055    TYPE sy-msgno                     VALUE '055',
           c_x          TYPE c                            VALUE 'X',
           c_e          TYPE c                            VALUE 'E'.

*&---------------------------------------------------------------------*
*&      Form  f_initialization
*&---------------------------------------------------------------------*
FORM f_initialization.

* Refresh internal tables and ranges before using.
  REFRESH:  i_zmm_flcm_parms,
            i_zmm_flcm_tds,
            i_mkpf,
            i_zmm_flcm_plntdef,
            i_item,
            i_return,
            i_return2,
            i_zmm_flcm_xref,
            r_trans_type,
            r_bwart.

* Clear work areas before using
  CLEAR: wa_zmm_flcm_parms,
         wa_zmm_flcm_tds,
         wa_zmm_flcm_plntdef,
         wa_header,
         wa_zmm_flcm_xref.
ENDFORM.                    "f_initialization

*&---------------------------------------------------------------------*
*&      Form  f_get_data
*&---------------------------------------------------------------------*
FORM f_get_data.

  DATA: lwa_range       LIKE LINE OF r_bwart,
        li_zmm_flcm_tds TYPE STANDARD TABLE OF t_zmm_flcm_tds,
        lr_status       TYPE RANGE OF zmm_flcm_tds-ztran_stus.

  CLEAR: lwa_range,
         wa_zmm_flcm_parms.

  REFRESH: li_zmm_flcm_tds,
           i_zmm_flcm_parms,
           i_mkpf,
           i_zmm_flcm_plntdef,
           lr_status.

* Retrieve data to be processed from table
  SELECT *
    FROM zmm_flcm_tds
    INTO TABLE i_zmm_flcm_tds
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
      AND aedat            IN s_lstdat
      AND aezeit           IN s_lsttim.
  IF sy-subrc EQ 0.
    SORT i_zmm_flcm_tds BY ztds_tran_type.
*   DELETE i_zmm_flcm_tds WHERE ztds_tran_type NOT IN r_trans_type.

    DELETE i_zmm_flcm_tds WHERE ztran_stus NOT IN lr_status.
    IF s_canreb IS NOT INITIAL.
      DELETE i_zmm_flcm_tds WHERE ztds_canc_rbil NOT IN s_canreb.
    ENDIF.

    IF p_divers IS NOT INITIAL.
      DELETE i_zmm_flcm_tds WHERE ztds_dvrt_ind NE p_divers.
    ENDIF.

    IF s_status IS NOT INITIAL.
      DELETE i_zmm_flcm_tds WHERE ztran_stus NOT IN s_status.
    ENDIF.

    IF p_change IS NOT INITIAL.
      DELETE i_zmm_flcm_tds WHERE aename NE p_change.
    ENDIF.


    REFRESH: li_zmm_flcm_tds[].
    li_zmm_flcm_tds[] = i_zmm_flcm_tds[].
    SORT li_zmm_flcm_tds BY ztds_tran_ref_nbr ztds_trml_id.
    DELETE ADJACENT DUPLICATES FROM li_zmm_flcm_tds COMPARING ztds_tran_ref_nbr ztds_trml_id.
  ENDIF.

ENDFORM.                    "f_get_data

*&---------------------------------------------------------------------*
*&      Form  f_data_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_data_processing.

  DATA:  l_status    TYPE zztran_stus.
********************************************************************************************************************************
*        l_date          TYPE char06.                               "Uncomment when this field is alraedy available in zmm_flcm_plntdef
                                                                    "Please check data type for this field.
********************************************************************************************************************************
  DATA:
      lo_tds_gr_confirm TYPE REF TO zcl_flcm_tds_gr_confirm,
      lo_flcm_exception   TYPE REF TO zcx_flcm_error.

  IF i_zmm_flcm_tds[] is initial.
*   Error: No lines selected for processing
    MESSAGE ID c_err_class TYPE c_success NUMBER c_err_055.
  ENDIF.


  CHECK i_zmm_flcm_tds[] is not initial.

*  ======
    TRY.
*  ======


* Create instance of the class ZCL_FLCM_TDS_GR_CONFIRM
  CREATE OBJECT lo_tds_gr_confirm.

* Execute the program logic in class ZCL_FLCM_TDS_GR_CONFIRM
  lo_tds_gr_confirm->process_transactions( i_zmm_flcm_tds ).

*  ======
    CATCH zcx_flcm_error INTO lo_flcm_exception.
*  ======
*  ======
    ENDTRY.
*  ======
ENDFORM.                    "f_data_processing
