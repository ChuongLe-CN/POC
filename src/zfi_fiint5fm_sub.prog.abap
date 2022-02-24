*&---------------------------------------------------------------------*
*&  Include           ZFI_FIINT5FM_SUB
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
* Aldrien Baugbog                  2014/12/16          DV5K991085      *
*                                                                      *
* Short Description :                                                  *
*      D#52 - Fix logic for negative amounts                           *
*----------------------------------------------------------------------*
* Von Kaisser Almeria              2014/09/16          DV5K990246      *
*                                                                      *
* Short Description :                                                  *
*      D#38 - FI-INT-5Fx_Create control files with summation           *
*----------------------------------------------------------------------*
* Raphael Policarpio              2014/07/10          DV5K988177       *
*                                                                      *
* Short Description : STAB Defect 35                                   *
*       - Enable the file to be downloadable in ZVU_UNIX               *
*----------------------------------------------------------------------*
* Mia Calleen Avila (XT18903)     2014/04/07          DV5K983744       *
*                                 2014/03/06          DV5K983744       *
*                                 2014/02/21          DV5K983744       *
*                                 2014/02/11          DV5K983744       *
*                                 2013/12/05          DV5K983744       *
*                                                                      *
* Short Description : CR234468 TK250623                                *
*       - G/L Account changes                                          *
*       - Modify retrieval for fiscal year and output                  *
*       - Modify retrieval for posting period data and local currency  *
*       - Initial Creation                                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_INITIALIZE
*&---------------------------------------------------------------------*
*       Initialize tables, variables and work areas.
*----------------------------------------------------------------------*
FORM f_initialize.
  REFRESH: i_posting_lines,
           i_posting_output,
           i_t_saktiv.

  CLEAR wa_output_file.
***BEGIN OF INSERT - XT18912 - D#38 - DV5K990246
  CLEAR:
    v_start,
    v_end,
    v_start_date,
    v_start_time,
    v_end_date,
    v_end_time.

  "get start timestamp
  GET TIME.
  v_start_date = sy-datum.
  v_start_time = sy-uzeit.

  GET TIME STAMP FIELD v_start.
***END OF INSERT - XT18912 - D#38 - DV5K990246
ENDFORM.                    " F_INITIALIZE
*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_SCREEN
*&---------------------------------------------------------------------*
*       Validate values on the selection screen.
*----------------------------------------------------------------------*
FORM f_validate_screen.
  DATA: l_bukrs        TYPE t001-bukrs,
        l_product_type TYPE tzpat-gsart,
        l_dis_flowtype TYPE trdc_dflowtype_t-dis_flowtype,
        l_saknr        TYPE skat-saknr,
        lwa_status     LIKE LINE OF r_status.

***Start of Insert - XT18910 - D#50 - DV5K991085
  DATA: l_msg1(40) TYPE c,
        l_msg2(40) TYPE c.
***End of Insert - XT18910 - D#50 - DV5K991085

  CLEAR: l_bukrs,
         l_product_type,
         l_dis_flowtype,
         l_saknr.

***Start of Insert - XT18910 - D#50 - DV5K991085
  IF s_ptype[] IS NOT INITIAL AND p_ptypes IS NOT INITIAL.
    CLEAR: l_msg1,
           l_msg2.

    l_msg1 = text-e01.
    l_msg2 = text-e02.

    REPLACE FIRST OCCURRENCE OF c_amp IN l_msg1 WITH c_ptype1.
    REPLACE FIRST OCCURRENCE OF c_amp IN l_msg2 WITH c_ptype2.
    MESSAGE e952(zz-cn) WITH l_msg1
                             l_msg2.
  ENDIF.

  IF s_utype[] IS NOT INITIAL AND p_utypes IS NOT INITIAL.
    CLEAR: l_msg1,
           l_msg2.

    l_msg1 = text-e01.
    l_msg2 = text-e02.

    REPLACE FIRST OCCURRENCE OF c_amp IN l_msg1 WITH c_utype1.
    REPLACE FIRST OCCURRENCE OF c_amp IN l_msg2 WITH c_utype2.
    MESSAGE e952(zz-cn) WITH l_msg1
                             l_msg2.
  ENDIF.

  IF s_gl[] IS NOT INITIAL AND p_gls IS NOT INITIAL.
    CLEAR: l_msg1,
           l_msg2.

    l_msg1 = text-e01.
    l_msg2 = text-e02.

    REPLACE FIRST OCCURRENCE OF c_amp IN l_msg1 WITH c_gl1.
    REPLACE FIRST OCCURRENCE OF c_amp IN l_msg2 WITH c_gl2.
    MESSAGE e952(zz-cn) WITH l_msg1
                             l_msg2.
  ENDIF.
***End of Insert - XT18910 - D#50 - DV5K991085

  "Company Code
  IF NOT s_ccode IS INITIAL.
    SELECT SINGLE bukrs                                     "#EC *
      INTO l_bukrs
      FROM t001
      WHERE bukrs IN s_ccode.
    IF sy-subrc NE 0.
      MESSAGE e952(zz-cn) WITH text-004.
    ENDIF.
  ENDIF.

  "Product Type
  IF NOT s_ptype IS INITIAL.
    SELECT SINGLE gsart                                     "#EC *
      INTO l_product_type
      FROM tzpat
      WHERE gsart IN s_ptype
        AND spras EQ c_english.
    IF sy-subrc NE 0.
      MESSAGE e952(zz-cn) WITH text-005.
    ENDIF.
  ENDIF.

  "Active Status
  IF NOT s_status IS INITIAL.
    CALL FUNCTION 'FM_DOMAINVALUE_CHECK'
      EXPORTING
        i_domname         = 'T_SAKTIV'
      TABLES
        t_dd07v           = i_t_saktiv
      EXCEPTIONS
        input_error       = 1
        value_not_allowed = 2
        OTHERS            = 3.
    IF sy-subrc EQ 0.
      LOOP AT s_status TO lwa_status.
        READ TABLE i_t_saktiv WITH KEY domvalue_l = lwa_status-low
        TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          READ TABLE i_t_saktiv WITH KEY domvalue_l = lwa_status-high
          TRANSPORTING NO FIELDS.
          IF sy-subrc NE 0.
            MESSAGE e952(zz-cn) WITH text-006.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  "Update type
  IF NOT s_utype IS INITIAL.
    SELECT SINGLE dis_flowtype                              "#EC *
             INTO l_dis_flowtype
             FROM trdc_dflowtype_t
            WHERE dis_flowtype IN s_utype
              AND spras EQ c_english.
    IF sy-subrc NE 0.
      MESSAGE e952(zz-cn) WITH text-007.
    ENDIF.
  ENDIF.
***Start of Insert - XT18903 - DV5K983744 - CR234468 TK250623
  "Fiscal Year
  IF p_fy IS INITIAL.
    MESSAGE e952(zz-cn) WITH text-021.
  ENDIF.

  "Posting period
  IF s_period-low  IS INITIAL AND
     s_period-high IS INITIAL.
    MESSAGE e952(zz-cn) WITH text-021.
***Start of Insert - XT18903 - DV5K983744 - CR234468 TK250623
  ELSEIF NOT s_period-low  IS INITIAL AND
         NOT s_period-high IS INITIAL.
    MESSAGE e952(zz-cn) WITH text-022.
***End of Insert - XT18903 - DV5K983744 - CR234468 TK250623
  ENDIF.
***End of Insert - XT18903 - DV5K983744 - CR234468 TK250623
***Start of Insert - XT18910 - D#50 - DV5K991085
  IF ( s_period-low IS NOT INITIAL AND s_period-low NOT BETWEEN '01' AND '12' ).
    MESSAGE e952(zz-cn) WITH text-e03.
  ENDIF.
***End of Insert - XT18910 - D#50 - DV5K991085

  "G/L Account
*  IF NOT p_gl IS INITIAL.                  "D - XT18903 - DV5K983744 - CR234468 TK250623
  IF NOT s_gl IS INITIAL.                   "I - XT18903 - DV5K983744 - CR234468 TK250623
    SELECT SINGLE saknr                                     "#EC *
             INTO l_saknr
             FROM skat
            WHERE spras EQ c_english
              AND ktopl EQ c_cn_accounts
*              AND saknr EQ p_gl.           "D - XT18903 - DV5K983744 - CR234468 TK250623
              AND saknr IN s_gl.            "I - XT18903 - DV5K983744 - CR234468 TK250623
    IF sy-subrc NE 0.
      MESSAGE e952(zz-cn) WITH text-008.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_VALIDATE_SCREEN
*&---------------------------------------------------------------------*
*&      Form  F_RETRIEVE_DATA
*&---------------------------------------------------------------------*
*       Select data from table
*----------------------------------------------------------------------*
FORM f_retrieve_data.
***Start of Delete - XT18903 - DV5K983744 - CR234468 TK250623
*  IF p_gl IS INITIAL.
*    "Retrieve data when G/L Account is blank on the selection screen
*    SELECT dis_flowtype
*           gl_account
*           position_amt
*           position_curr
*           local_amt
*           acpostingperiod
*           acpostingyear
*           company_code
*           product_type
*           deal_number
*           tr_rev_reason
*    INTO TABLE i_posting_lines
*    FROM tracv_accitem
*    WHERE dis_flowtype    IN s_utype
**      AND acpostingperiod EQ p_pp         "D - XT18903 - DV5K983744 - CR234468 TK250623
*      AND acpostingperiod IN s_period      "I - XT18903 - DV5K983744 - CR234468 TK250623
*      AND acpostingyear   EQ p_fy
*      AND company_code    IN s_ccode
*      AND product_type    IN s_ptype
*      AND tr_rev_reason   IN s_status.
*    IF sy-subrc NE 0.
*      REFRESH i_posting_lines.
*    ENDIF.
*  ELSEIF NOT p_gl IS INITIAL.
  "Retrieve data when G/L Account has value on the selection screen
***End of Delete - XT18903 - DV5K983744 - CR234468 TK250623
***Start of Insert - XT18903 - DV5K983744 - CR234468 TK250623
  CLEAR: v_up_to_period, v_current_period.

  READ TABLE s_period INDEX 1.
  IF s_period-low  IS INITIAL AND NOT
     s_period-high IS INITIAL.
    v_up_to_period =  s_period-high.
  ELSEIF NOT s_period-low IS INITIAL AND
             s_period-high IS INITIAL.
    v_current_period = s_period-low.
  ENDIF.

  IF NOT v_current_period IS INITIAL.
    SELECT dis_flowtype
         gl_account
         position_amt
         position_curr
         local_amt
         acpostingperiod
         acpostingyear
         company_code
         product_type
         deal_number
         tr_rev_reason
    INTO TABLE i_posting_lines
    FROM tracv_accitem
    WHERE dis_flowtype    IN s_utype
      AND gl_account      IN s_gl
      AND acpostingperiod EQ v_current_period
      AND acpostingyear   EQ p_fy
      AND company_code    IN s_ccode
      AND product_type    IN s_ptype
      AND tr_rev_reason   IN s_status.

  ELSEIF NOT v_up_to_period IS INITIAL.
***End of Insert - XT18903 - DV5K983744 - CR234468 TK250623
    SELECT dis_flowtype
           gl_account
           position_amt
           position_curr
           local_amt
           acpostingperiod
           acpostingyear
           company_code
           product_type
           deal_number
           tr_rev_reason
      INTO TABLE i_posting_lines
      FROM tracv_accitem
      WHERE dis_flowtype    IN s_utype
        AND gl_account      IN s_gl           "I - XT18903 - DV5K983744 - CR234468 TK250623
*        AND gl_account      EQ p_gl          "D - XT18903 - DV5K983744 - CR234468 TK250623
*        AND acpostingperiod EQ p_pp          "D - XT18903 - DV5K983744 - CR234468 TK250623
*        AND acpostingperiod IN s_period      "D - XT18903 - DV5K983744 - CR234468 TK250623
*        AND acpostingyear   EQ p_fy          "D - XT18903 - DV5K983744 - CR234468 TK250623
        AND acpostingyear   LE p_fy           "I - XT18903 - DV5K983744 - CR234468 TK250623
        AND company_code    IN s_ccode
        AND product_type    IN s_ptype
        AND tr_rev_reason   IN s_status.
  ENDIF.                                    "I - XT18903 - DV5K983744 - CR234468 TK250623

  IF sy-subrc NE 0.
    REFRESH i_posting_lines.
  ENDIF.
*  ENDIF.                                   "D - XT18903 - DV5K983744 - CR234468 TK250623

  SORT i_posting_lines BY company_code
                          product_type
                          deal_number
                          dis_flowtype
                          gl_account
                          position_curr.
ENDFORM.                    " F_RETRIEVE_DATA
*&---------------------------------------------------------------------*
*&      Form  F_OUTPUT_DATA
*&---------------------------------------------------------------------*
*       Build output table
*----------------------------------------------------------------------*
FORM f_output_data.
  DATA: li_posting_lines    TYPE STANDARD TABLE OF t_posting_lines,
        lwa_posting_output  TYPE t_posting_output,
        lwa_posting_amount  TYPE t_posting_lines,
        l_fiscal_year       TYPE tracv_accitem-acpostingyear,   "I - XT18903 - DV5K983744 - CR234468 TK250623
        l_up_to_period      TYPE numc2,                         "I - XT18903 - DV5K983744 - CR234468 TK250623
        l_position_amount   TYPE tracv_accitem-position_amt,
        l_local_amount      TYPE tracv_accitem-local_amt,
        l_deal_number       TYPE tracv_accitem-deal_number,
        l_amount            TYPE char22,
        l_tabix             TYPE sy-tabix.

  FIELD-SYMBOLS: <lfs_data> TYPE t_posting_lines.

  UNASSIGN <lfs_data>.
  REFRESH li_posting_lines.
  CLEAR:  lwa_posting_output,
          l_deal_number,
          l_amount.

  li_posting_lines[] = i_posting_lines[].
  DELETE ADJACENT DUPLICATES FROM li_posting_lines COMPARING company_code
                                                             product_type
                                                             deal_number
                                                             dis_flowtype
                                                             gl_account
                                                             position_curr.

***Start of Insert - XT18903 - DV5K983744 - CR234468 TK250623
  IF NOT p_fy IS INITIAL.
    l_fiscal_year = p_fy.
  ENDIF.
***Start of Delete - XT18903 - DV5K983744 - CR234468 TK250623
*  "Identify posting period to be used for output
*  LOOP AT s_period INTO lwa_posting_period.
*    IF NOT s_period-high IS INITIAL AND
*           lwa_posting_period-high GE l_posting_period.
*      MOVE s_period-high TO l_posting_period.
*    ELSE.
*      IF NOT s_period-low IS INITIAL AND
*             s_period-low GE l_posting_period.
*        MOVE lwa_posting_period-low TO l_posting_period.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
***End of Delete - XT18903 - DV5K983744 - CR234468 TK250623
***End of Insert - XT18903 - DV5K983744 - CR234468 TK250623
  l_up_to_period = v_up_to_period+1(2).        "I - XT18903 - DV5K983744 - CR234468 TK250623

  "Build output table based on the selection criteria
  LOOP AT li_posting_lines ASSIGNING <lfs_data>.
***Start of Insert - XT18903 - DV5K983744 - CR234468 TK250623
    IF NOT v_up_to_period IS INITIAL.
      "Do not include entry on the same year and greater than the period indicated at selection screen.
      IF l_fiscal_year  EQ <lfs_data>-acpostingyear AND
         <lfs_data>-acpostingperiod GT l_up_to_period.
        CONTINUE.
      ENDIF.

      MOVE: l_up_to_period TO lwa_posting_output-acpostingperiod.
    ELSE.
      MOVE: v_current_period+1(2) TO lwa_posting_output-acpostingperiod.
    ENDIF.
***End of Insert - XT18903 - DV5K983744 - CR234468 TK250623
    "Transfer values for Fiscal Year, Period, Company Code, Product and Update Types
*    MOVE:  <lfs_data>-acpostingyear   TO lwa_posting_output-acpostingyear,    "D - XT18903 - DV5K983744 - CR234468 TK250623
    MOVE:  l_fiscal_year              TO lwa_posting_output-acpostingyear,     "I - XT18903 - DV5K983744 - CR234468 TK250623
*           <lfs_data>-acpostingperiod TO lwa_posting_output-acpostingperiod,  "D - XT18903 - DV5K983744 - CR234468 TK250623
           <lfs_data>-company_code    TO lwa_posting_output-company_code,
           <lfs_data>-product_type    TO lwa_posting_output-product_type,
           <lfs_data>-deal_number     TO l_deal_number,
           <lfs_data>-dis_flowtype    TO lwa_posting_output-dis_flowtype,
           <lfs_data>-gl_account      TO lwa_posting_output-gl_account.

    "Value for Transaction
    PERFORM f_convert_value CHANGING l_deal_number.
    CONCATENATE  <lfs_data>-company_code
                 l_deal_number
           INTO lwa_posting_output-deal_number.

    "Value for G/L Account
    PERFORM f_convert_value CHANGING lwa_posting_output-gl_account.
    CONDENSE lwa_posting_output-gl_account.

    "Find which entry has the same value as is currently processed
    READ TABLE i_posting_lines TRANSPORTING NO FIELDS
    WITH KEY company_code  = <lfs_data>-company_code
             product_type  = <lfs_data>-product_type
             deal_number   = <lfs_data>-deal_number
             dis_flowtype  = <lfs_data>-dis_flowtype
             gl_account    = <lfs_data>-gl_account
             BINARY SEARCH.

    IF sy-subrc EQ 0.
      CLEAR: lwa_posting_amount,
             l_position_amount,
             l_local_amount.

      l_tabix = sy-tabix.
      "If entry still has the same value do subsequent total for amount.
      LOOP AT i_posting_lines INTO lwa_posting_amount FROM l_tabix.
        IF  lwa_posting_amount-company_code  NE lwa_posting_output-company_code
         OR lwa_posting_amount-product_type  NE lwa_posting_output-product_type
         OR lwa_posting_amount-deal_number   NE <lfs_data>-deal_number
         OR lwa_posting_amount-dis_flowtype  NE lwa_posting_output-dis_flowtype
         OR lwa_posting_amount-gl_account    NE <lfs_data>-gl_account.
          EXIT.
        ENDIF.

        "Calculate transaction currency amount
        IF NOT lwa_posting_amount-position_amt IS INITIAL.
          ADD lwa_posting_amount-position_amt TO l_position_amount.
        ENDIF.

        "Calculate local currency amount.
        IF NOT lwa_posting_amount-local_amt IS INITIAL.
          ADD lwa_posting_amount-local_amt TO l_local_amount.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CLEAR: l_amount,
           lwa_posting_output-position_curr,
           lwa_posting_output-amount.

    l_amount = l_position_amount.
    "Format negative-positive amount
    PERFORM f_edit_amount CHANGING l_amount.
    "Transaction currency to start with TC_
    CONCATENATE c_tc_curr
                <lfs_data>-position_curr
            INTO lwa_posting_output-position_curr.
    MOVE l_amount TO lwa_posting_output-amount.
    APPEND lwa_posting_output TO i_posting_output.

    CLEAR: l_amount,
           lwa_posting_output-amount,
           lwa_posting_output-position_curr.

    l_amount = l_local_amount.
    "Format negative-positive amount
    PERFORM f_edit_amount CHANGING l_amount.
    "Local currency as LC
***Start of Insert - XT18903 - DV5K983744 - CR234468 TK250623
    CONCATENATE c_lc_curr c_tc_curr
                <lfs_data>-position_curr
           INTO lwa_posting_output-position_curr.

    MOVE l_amount TO lwa_posting_output-amount.
***End of Insert - XT18903 - DV5K983744 - CR234468 TK250623
***Start of Delete - XT18903 - DV5K983744 - CR234468 TK250623
*    MOVE: c_lc_curr TO lwa_posting_output-position_curr,
*          l_amount  TO lwa_posting_output-amount.
***End of Delete - XT18903 - DV5K983744 - CR234468 TK250623
    APPEND lwa_posting_output TO i_posting_output.

    CLEAR lwa_posting_output.
  ENDLOOP.

  SORT i_posting_output BY company_code
                           product_type
                           deal_number
                           dis_flowtype
                           gl_account
                           position_curr DESCENDING.
ENDFORM.                    " F_OUTPUT_DATA
*&---------------------------------------------------------------------*
*&      Form  F_OUTPUT_FILE
*&---------------------------------------------------------------------*
*       Save output file to UNIX
*----------------------------------------------------------------------*
FORM f_output_file.
*  DATA: lwa_output_file    TYPE line,                         "D - XT18903 - DV5K983744 - CR234468 TK250623
*        l_archive          TYPE rlgrap-filename,              "D - XT18903 - DV5K983744 - CR234468 TK250623
  DATA: l_output           TYPE string,
        l_file             TYPE string.

  CLEAR: l_output,
         l_file,
*         lwa_output_file,                                     "D - XT18903 - DV5K983744 - CR234468 TK250623
         wa_output_file.

  MOVE c_file_path TO l_file.
  REPLACE FIRST OCCURRENCE OF c_amp IN l_file WITH sy-sysid.
  REPLACE FIRST OCCURRENCE OF c_amp IN l_file WITH p_file.     "I - XT18912 - D#38 - DV5K990246
  CLEAR v_sum.                                                 "I - XT18912 - D#38 - DV5K990246
  DESCRIBE TABLE i_posting_output LINES v_count.               "I - XT18912 - D#38 - DV5K990246

  OPEN DATASET l_file IN TEXT MODE FOR OUTPUT ENCODING DEFAULT.
  IF sy-subrc EQ 0.
    "Write Header
    PERFORM f_build_header CHANGING l_output.
    TRANSFER l_output TO l_file.

    CLEAR l_output.
    LOOP AT i_posting_output INTO wa_output_file.
      v_sum = v_sum + wa_output_file-amount.                    "I - XT18912 - D#38 - DV5K990246
      "Create entries for file
      PERFORM f_build_body CHANGING l_output.
      TRANSFER l_output TO l_file.
      CLEAR: wa_output_file,
             l_output.
    ENDLOOP.

    CLOSE DATASET l_file.
***BEGIN OF INSERT - XT18912 - D#38 - DV5K990246
    "get end timestamp
    GET TIME.
    v_end_date = sy-datum.
    v_end_time = sy-uzeit.

    GET TIME STAMP FIELD v_end.

    PERFORM f_generate_ctrl_file.

***END OF INSERT - XT18912 - D#38 - DV5K990246
    MESSAGE i952(zz-cn) WITH text-018.
***Start of Insert - DV5K988177 --------------->>>
    "Register the file in the ZV_UNIX application log so that it can be
    "downloaded via ZV_UNIX
    CALL METHOD zcl_vub302_file_utility=>register_report
      EXPORTING
        im_repid    = sy-repid
        im_filename = l_file.
***End   of Insert - DV5K988177 ---------------<<<
***Start of Delete - XT18903 - DV5K983744 - CR234468 TK250623
*    "Transfer to archive
*    MOVE c_archive TO l_archive.
*    REPLACE FIRST OCCURRENCE OF c_amp IN l_archive WITH sy-sysid.
*    REPLACE FIRST OCCURRENCE OF c_amp IN l_archive WITH sy-datum.
*    REPLACE FIRST OCCURRENCE OF c_amp IN l_archive WITH sy-uzeit.
*
*    DELETE DATASET l_archive.
*    CONCATENATE c_copy
*                l_file
*                l_archive
*           INTO lwa_output_file
*           SEPARATED BY space.
*    CALL c_system ID c_command FIELD lwa_output_file.
***End of Delete - XT18903 - DV5K983744 - CR234468 TK250623
  ELSE.
    MESSAGE i952(zz-cn) WITH text-019.
  ENDIF.
ENDFORM.                    " F_OUTPUT_FILE
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_HEADER
*&---------------------------------------------------------------------*
*       Create Header for the file extract
*----------------------------------------------------------------------*
FORM f_build_header  CHANGING cs_output.
  DATA: lwa_header TYPE t_posting_output.

  lwa_header-acpostingyear    = text-009.   "Fiscal Year
  lwa_header-acpostingperiod  = text-010.   "Posting Period
  lwa_header-company_code     = text-011.   "Company Code
  lwa_header-product_type     = text-012.   "Product Type
  lwa_header-deal_number      = text-013.   "Transaction
  lwa_header-dis_flowtype     = text-014.   "Update Type
  lwa_header-gl_account       = text-015.   "G/L Account
  lwa_header-position_curr    = text-016.   "Transaction Currenc
  lwa_header-amount           = text-017.   "Amount

  PERFORM f_enclode_fields CHANGING lwa_header.

  CONCATENATE lwa_header-acpostingyear
              lwa_header-acpostingperiod
              lwa_header-company_code
              lwa_header-product_type
              lwa_header-deal_number
              lwa_header-dis_flowtype
              lwa_header-gl_account
              lwa_header-position_curr
              lwa_header-amount
         INTO cs_output
    SEPARATED BY c_comma.
ENDFORM.                    " F_BUILD_HEADER
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_BODY
*&---------------------------------------------------------------------*
*       Build entries for the file extract
*----------------------------------------------------------------------*
FORM f_build_body  CHANGING cs_output.
  DATA: lwa_data TYPE t_posting_output.

  lwa_data-acpostingyear    = wa_output_file-acpostingyear.
  lwa_data-acpostingperiod  = wa_output_file-acpostingperiod.
  lwa_data-company_code     = wa_output_file-company_code.
  lwa_data-product_type     = wa_output_file-product_type.
  lwa_data-deal_number      = wa_output_file-deal_number.
  lwa_data-dis_flowtype     = wa_output_file-dis_flowtype.
  lwa_data-gl_account       = wa_output_file-gl_account.
  lwa_data-position_curr    = wa_output_file-position_curr.
  lwa_data-amount           = wa_output_file-amount.

  PERFORM f_enclode_fields CHANGING lwa_data.

  CONCATENATE lwa_data-acpostingyear
              lwa_data-acpostingperiod
              lwa_data-company_code
              lwa_data-product_type
              lwa_data-deal_number
              lwa_data-dis_flowtype
              lwa_data-gl_account
              lwa_data-position_curr
              lwa_data-amount
         INTO cs_output
    SEPARATED BY c_comma.
ENDFORM.                    " F_BUILD_BODY
*&---------------------------------------------------------------------*
*&      Form  F_ENCLODE_FIELDS
*&---------------------------------------------------------------------*
*       Enclose each field with double quote
*----------------------------------------------------------------------*
FORM f_enclode_fields  CHANGING cs_data.
  FIELD-SYMBOLS: <lfs_fields> TYPE ANY.

  DO.
    ASSIGN COMPONENT sy-index OF STRUCTURE cs_data TO <lfs_fields>.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    PERFORM f_format_fields CHANGING  <lfs_fields>.
  ENDDO.
ENDFORM.                    " F_ENCLODE_FIELDS
*&---------------------------------------------------------------------*
*&      Form  F_FORMAT_FIELDS
*&---------------------------------------------------------------------*
*       Format each field with double quote
*----------------------------------------------------------------------*
FORM f_format_fields CHANGING cv_fields TYPE any.

  CONCATENATE c_dbl_quote cv_fields c_dbl_quote INTO cv_fields.

ENDFORM.                    " F_FORMAT_FIELDS
*&---------------------------------------------------------------------*
*&      Form  F_EDIT_AMOUNT
*&---------------------------------------------------------------------*
*       Edit output amount when negative
*----------------------------------------------------------------------*
FORM f_edit_amount CHANGING cv_amount.
*  IF cv_amount LT 0.                                     "D - XT18910 - D#52 - DV5K991085
  CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
    CHANGING
      value = cv_amount.
*  ENDIF.                                                 "D - XT18910 - D#52 - DV5K991085
  WRITE cv_amount TO cv_amount RIGHT-JUSTIFIED.
  CONDENSE cv_amount.
ENDFORM.                    " F_EDIT_AMOUNT
*&---------------------------------------------------------------------*
*&      Form  F_FREE
*&---------------------------------------------------------------------*
*       Free tables used
*----------------------------------------------------------------------*
FORM f_free.
  FREE: i_posting_lines,
        i_posting_output,
        i_t_saktiv.
ENDFORM.                    " F_FREE
*&---------------------------------------------------------------------*
*&      Form  F_CONVERT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_convert_value  CHANGING cv_field TYPE any.

  IF NOT cv_field IS INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = cv_field
      IMPORTING
        output = cv_field.
  ENDIF.
ENDFORM.                    " F_CONVERT_VALUE
***BEGIN OF INSERT - XT18912 - D#38 - DV5K990246
*&---------------------------------------------------------------------*
*&      Form  F_GENERATE_CTRL_FILE
*&---------------------------------------------------------------------*
*       generate control file
*----------------------------------------------------------------------*
FORM f_generate_ctrl_file .
  DATA: lwa_ctrl_output TYPE t_ctrl_output,
        l_diff          TYPE char8,
        l_path          TYPE string,
        l_file          TYPE string.

  REFRESH: i_ctrl_output.

  "LABEL
  CLEAR: lwa_ctrl_output.
  lwa_ctrl_output-field1  = text-c01.
  lwa_ctrl_output-field2  = text-c02.
  lwa_ctrl_output-field3  = text-c03.
  lwa_ctrl_output-field4  = text-c04.
  lwa_ctrl_output-field5  = text-c05.
  lwa_ctrl_output-field6  = text-c06.
  lwa_ctrl_output-field7  = text-c07.
  lwa_ctrl_output-field8  = text-c08.
  lwa_ctrl_output-field9  = text-c09.

  APPEND lwa_ctrl_output TO i_ctrl_output.

  PERFORM f_calculate_time_diff USING v_start
                                      v_end
                             CHANGING l_diff.
  "CONTENT
  CLEAR: lwa_ctrl_output.
  lwa_ctrl_output-field1  = p_file.
  WRITE v_start_date TO lwa_ctrl_output-field2 USING EDIT MASK '____-__-__'..
  WRITE v_start_time TO lwa_ctrl_output-field3.
  WRITE v_end_time   TO lwa_ctrl_output-field4.
  lwa_ctrl_output-field5  = l_diff.
  lwa_ctrl_output-field6  = p_fy.
  READ TABLE s_period INDEX 1.
  IF sy-subrc EQ 0.
    IF s_period-low IS NOT INITIAL.
      lwa_ctrl_output-field7  = s_period-low.
    ELSEIF s_period-high IS NOT INITIAL.
      lwa_ctrl_output-field7  = s_period-high.
    ENDIF.
  ENDIF.
  lwa_ctrl_output-field8  = v_count.
  lwa_ctrl_output-field9  = v_sum.
  PERFORM f_edit_amount CHANGING lwa_ctrl_output-field9.
  CONDENSE lwa_ctrl_output-field8.
  CONDENSE lwa_ctrl_output-field9.

  APPEND lwa_ctrl_output TO i_ctrl_output.

  PERFORM f_enclose_fields_ctrl TABLES i_ctrl_output[].

  MOVE c_ctrlfilepath TO l_path.
  REPLACE FIRST OCCURRENCE OF c_amp IN l_path WITH sy-sysid.
  REPLACE FIRST OCCURRENCE OF c_amp IN l_path WITH p_file.

  OPEN DATASET l_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc EQ 0.
    CLEAR: lwa_ctrl_output,
           l_file.
    LOOP AT i_ctrl_output INTO lwa_ctrl_output.
***Start of Insert - XT18910 - D#50 - DV5K991085
      IF sy-tabix EQ 1
        AND v_tm1 IS NOT INITIAL.
        l_file = v_email.
        TRANSFER l_file TO l_path.
      ENDIF.
***End of Insert - XT18910 - D#50 - DV5K991085
      CONCATENATE lwa_ctrl_output-field1
                  lwa_ctrl_output-field2
                  lwa_ctrl_output-field3
                  lwa_ctrl_output-field4
                  lwa_ctrl_output-field5
                  lwa_ctrl_output-field6
                  lwa_ctrl_output-field7
                  lwa_ctrl_output-field8
                  lwa_ctrl_output-field9
      INTO l_file
      SEPARATED BY c_comma.
      TRANSFER l_file TO l_path.
      CLEAR:  lwa_ctrl_output,
              l_file.
    ENDLOOP.
    CLOSE DATASET l_path.

    CALL METHOD zcl_vub302_file_utility=>register_report
      EXPORTING
        im_repid    = sy-repid
        im_filename = l_path.

    MESSAGE l_path TYPE 'I'.
  ENDIF.
ENDFORM.                    " F_GENERATE_CTRL_FILE
*&---------------------------------------------------------------------*
*&      Form  F_CALCULATE_TIME_DIFF
*&---------------------------------------------------------------------*
*       calculate the time stamp difference
*----------------------------------------------------------------------*
FORM f_calculate_time_diff  USING    p_start
                                     p_end
                            CHANGING c_dif.
  DATA:
    l_diff    TYPE tzntstmpl,
    l_min     TYPE char6,
    l_modmin  TYPE char2,
    l_sec     TYPE char2,
    l_len     TYPE i.

  TRY.
      CALL METHOD cl_abap_tstmp=>subtract
        EXPORTING
          tstmp1 = p_end
          tstmp2 = p_start
        RECEIVING
          r_secs = l_diff.
    CATCH cx_parameter_invalid_range.
    CATCH cx_parameter_invalid_type.
  ENDTRY.

  l_modmin  = l_diff MOD 60.
  l_min     = ( l_diff - l_modmin ) / 60.
  l_sec     = l_modmin.

  IF l_min = 0.
    l_min = '00'.
  ENDIF.
  IF l_sec = 0.
    l_sec = '00'.
  ELSE .
    l_len = STRLEN( l_sec ).
    IF l_len = 1.
      CONCATENATE '0'
                  l_sec
             INTO l_sec.
    ENDIF.
  ENDIF.

  CONCATENATE l_min
              l_sec
         INTO c_dif
    SEPARATED BY ':'.

  CONDENSE c_dif.
ENDFORM.                    " F_CALCULATE_TIME_DIFF
*&---------------------------------------------------------------------*
*&      Form  F_ENCLOSE_FIELDS_CTRL
*&---------------------------------------------------------------------*
*       Enclose all table fields
*----------------------------------------------------------------------*
FORM f_enclose_fields_ctrl TABLES pi_output.
  DATA: lwa_output TYPE t_ctrl_output.

  FIELD-SYMBOLS <lfs_field>.                                "#EC *

  LOOP AT pi_output INTO lwa_output.
    DO.
      ASSIGN COMPONENT sy-index
      OF STRUCTURE lwa_output TO <lfs_field> .
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.

      "remove comma and quote from description
      IF sy-index EQ 3
        OR sy-index EQ 7.
        REPLACE ALL OCCURRENCES OF: c_comma IN <lfs_field> WITH space,
                                    c_dbl_quote IN <lfs_field> WITH space.
      ENDIF.

      PERFORM f_enclose USING c_dbl_quote
                        CHANGING <lfs_field>.
    ENDDO.

    MODIFY pi_output FROM lwa_output.
  ENDLOOP.
ENDFORM.                    " F_ENCLOSE_FIELDS_CTRL
*&---------------------------------------------------------------------*
*&      Form  F_ENCLOSE
*&---------------------------------------------------------------------*
*       enclose string
*----------------------------------------------------------------------*
FORM f_enclose  USING p_enclose_1 TYPE any
                CHANGING cv_string TYPE any.

  CONCATENATE p_enclose_1 cv_string p_enclose_1 INTO cv_string.
ENDFORM.                    " F_ENCLOSE
*&---------------------------------------------------------------------*
*&      Form  F_GEN_EMPTYFILE
*&---------------------------------------------------------------------*
*       generate an empty file
*----------------------------------------------------------------------*
FORM f_gen_emptyfile.
  DATA: l_output           TYPE string,
        l_file             TYPE string.

  CLEAR: l_output,
         l_file,
         wa_output_file.

  MOVE c_file_path TO l_file.
  REPLACE FIRST OCCURRENCE OF c_amp IN l_file WITH sy-sysid.
  REPLACE FIRST OCCURRENCE OF c_amp IN l_file WITH p_file.
  CLEAR: v_sum,
         v_count.

  OPEN DATASET l_file IN TEXT MODE FOR OUTPUT ENCODING DEFAULT.
  IF sy-subrc EQ 0.
    "Write Header
    PERFORM f_build_header CHANGING l_output.
    TRANSFER l_output TO l_file.

    CLOSE DATASET l_file.

    "get end timestamp
    GET TIME.
    v_end_date = sy-datum.
    v_end_time = sy-uzeit.

    GET TIME STAMP FIELD v_end.

    PERFORM f_generate_ctrl_file.

    MESSAGE i952(zz-cn) WITH text-018.

    CALL METHOD zcl_vub302_file_utility=>register_report
      EXPORTING
        im_repid    = sy-repid
        im_filename = l_file.
  ELSE.
    MESSAGE i952(zz-cn) WITH text-019.
  ENDIF.
ENDFORM.                    " F_GEN_EMPTYFILE
***END OF INSERT - XT18912 - D#38 - DV5K990246
***Start of Insert - XT18910 - D#50 - DV5K991085
*&---------------------------------------------------------------------*
*&      Form  F_GET_SET
*&---------------------------------------------------------------------*
FORM f_get_set.

  DATA: li_utypes   TYPE STANDARD TABLE OF t_setleaf,
        li_ptypes   TYPE STANDARD TABLE OF t_setleaf,
        li_gls      TYPE STANDARD TABLE OF t_setleaf,
        lwa_utype   LIKE LINE OF s_utype,
        lwa_ptype   LIKE LINE OF s_ptype,
        lwa_gl      LIKE LINE OF s_gl.

  REFRESH: li_utypes,
           li_ptypes,
           li_gls.

  FIELD-SYMBOLS: <lfs_setleaf> TYPE t_setleaf.

  CLEAR: lwa_utype,
         lwa_ptype,
         lwa_gl.

  UNASSIGN <lfs_setleaf>.

  IF v_tm1 IS NOT INITIAL.
    REFRESH: s_period,
             s_ccode,
             s_ptype,
             s_utype,
             s_gl.

    s_ccode[]  = r_ccode[].
    s_ptype[]  = r_ptype[].
    s_utype[]  = r_utype[].
    s_gl[]     = r_gl[].
    s_period[] = r_period[].

    p_fy   = v_fyear.
    p_file = v_filename.
  ELSE.
    IF p_utypes IS NOT INITIAL
      OR p_ptypes IS NOT INITIAL
      OR p_gls IS NOT INITIAL.

      SELECT setname
             valsign
             valoption
             valfrom
             valto
        FROM setleaf
        INTO TABLE li_utypes
        WHERE setname IN (p_utypes, p_ptypes, p_gls).
      IF sy-subrc EQ 0.
        SORT li_utypes[] BY setname.
        li_ptypes[] = li_utypes[].
        li_gls[] = li_utypes[].

        IF p_utypes IS NOT INITIAL.
          DELETE li_utypes[] WHERE NOT setname EQ p_utypes.

          LOOP AT li_utypes ASSIGNING <lfs_setleaf>.
            MOVE: <lfs_setleaf>-valsign   TO lwa_utype-sign,
                  <lfs_setleaf>-valoption TO lwa_utype-option,
                  <lfs_setleaf>-valfrom   TO lwa_utype-low.

            IF <lfs_setleaf>-valoption EQ 'BT'.
              MOVE <lfs_setleaf>-valto TO lwa_utype-high.
            ENDIF.

            APPEND lwa_utype TO s_utype.
            CLEAR lwa_utype.
          ENDLOOP.
        ELSE.
          REFRESH li_utypes.
        ENDIF.

        IF p_ptypes IS NOT INITIAL.
          DELETE li_ptypes[] WHERE NOT setname EQ p_ptypes.

          LOOP AT li_ptypes ASSIGNING <lfs_setleaf>.
            MOVE: <lfs_setleaf>-valsign   TO lwa_ptype-sign,
                  <lfs_setleaf>-valoption TO lwa_ptype-option,
                  <lfs_setleaf>-valfrom   TO lwa_ptype-low.

            IF <lfs_setleaf>-valoption EQ 'BT'.
              MOVE <lfs_setleaf>-valto TO lwa_ptype-high.
            ENDIF.

            APPEND lwa_ptype TO s_ptype.
            CLEAR lwa_ptype.
          ENDLOOP.
        ELSE.
          REFRESH li_ptypes.
        ENDIF.

        IF p_gls IS NOT INITIAL.
          DELETE li_gls[] WHERE NOT setname EQ p_gls.

          LOOP AT li_gls ASSIGNING <lfs_setleaf>.
            MOVE: <lfs_setleaf>-valsign   TO lwa_gl-sign,
                  <lfs_setleaf>-valoption TO lwa_gl-option,
                  <lfs_setleaf>-valfrom   TO lwa_gl-low.

            IF <lfs_setleaf>-valoption EQ 'BT'.
              MOVE <lfs_setleaf>-valto TO lwa_gl-high.
            ENDIF.

            APPEND lwa_gl TO s_gl.
            CLEAR lwa_gl.
          ENDLOOP.
        ELSE.
          REFRESH li_gls.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_GET_SET
*&---------------------------------------------------------------------*
*&      Form  F_GET_FROM_SETLEAF
*&---------------------------------------------------------------------*
FORM f_get_from_setleaf.

  DATA: li_posting  TYPE STANDARD TABLE OF t_posting_lines,
        li_ptype    TYPE STANDARD TABLE OF t_setleaf,
        li_utype    TYPE STANDARD TABLE OF t_setleaf,
        li_gl       TYPE STANDARD TABLE OF t_setleaf,
        lwa_zifad   TYPE zifad,
        l_ptype     TYPE setleaf-setname,
        l_utype     TYPE setleaf-setname,
        l_gl        TYPE setleaf-setname,
        l_file      TYPE string,
        l_record    TYPE string.

  DATA: lwa_per   LIKE LINE OF s_period,
        lwa_bukrs LIKE LINE OF s_ccode,
        lwa_ptype LIKE LINE OF s_ptype,
        lwa_utype LIKE LINE OF s_utype,
        lwa_gl    LIKE LINE OF s_gl.

  FIELD-SYMBOLS: <lfs_setleaf> TYPE t_setleaf.

  REFRESH: li_ptype,
           li_utype,
           li_gl.

  CLEAR: lwa_zifad,
         lwa_bukrs,
         lwa_ptype,
         lwa_utype,
         lwa_gl,
         l_ptype,
         l_utype,
         l_gl,
         l_file,
         l_record,
         v_tm1.

  l_file = c_file.
  REPLACE FIRST OCCURRENCE OF c_amp IN l_file WITH sy-sysid.

  OPEN DATASET l_file FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc EQ 0.
    v_tm1 = abap_true.
    DO 4 TIMES.
      READ DATASET l_file INTO l_record.
      IF sy-subrc EQ 0.
        CASE sy-index.
          WHEN 1.
            v_fyear         = l_record.
          WHEN 2.
            CLEAR lwa_per.
            lwa_per-sign    = 'I'.
            lwa_per-option  = 'EQ'.
            lwa_per-low     = l_record.
            APPEND lwa_per TO s_period.
            CLEAR lwa_per.
          WHEN 3.
            v_filename      = l_record.
          WHEN 4.
            v_email       = l_record.
          WHEN OTHERS.
            "do nothing
        ENDCASE.
      ENDIF.
    ENDDO.

    CLOSE DATASET l_file.

    p_fy       = v_fyear.
    r_period[] = s_period[].
    p_file     = v_filename.
  ELSE.
    CLEAR v_tm1.
  ENDIF.

  SELECT SINGLE *
    FROM zifad
    INTO lwa_zifad
    WHERE zintid EQ c_intid.
  IF sy-subrc EQ 0.
    lwa_bukrs-sign   = 'I'.
    lwa_bukrs-option = 'BT'.
    SPLIT lwa_zifad-zfield1 AT c_comma INTO lwa_bukrs-low
                                            lwa_bukrs-high.
    APPEND lwa_bukrs TO s_ccode.

    l_ptype = lwa_zifad-zfield2.
    l_utype = lwa_zifad-zfield3.
    l_gl    = lwa_zifad-zfield4.

    IF l_ptype IS NOT INITIAL.
      SELECT setname
             valsign
             valoption
             valfrom
             valto
        FROM setleaf
        INTO TABLE li_ptype
        WHERE setname EQ l_ptype.
      IF sy-subrc EQ 0.
        UNASSIGN <lfs_setleaf>.
        LOOP AT li_ptype ASSIGNING <lfs_setleaf>.
          MOVE: <lfs_setleaf>-valsign   TO lwa_ptype-sign,
                <lfs_setleaf>-valoption TO lwa_ptype-option,
                <lfs_setleaf>-valfrom   TO lwa_ptype-low.

          IF <lfs_setleaf>-valoption EQ 'BT'.
            MOVE <lfs_setleaf>-valto TO lwa_ptype-high.
          ENDIF.

          APPEND lwa_ptype TO s_ptype.
          CLEAR lwa_ptype.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF l_utype IS NOT INITIAL.
      SELECT setname
             valsign
             valoption
             valfrom
             valto
        FROM setleaf
        INTO TABLE li_utype
        WHERE setname EQ l_utype.
      IF sy-subrc EQ 0.
        UNASSIGN <lfs_setleaf>.
        LOOP AT li_utype ASSIGNING <lfs_setleaf>.
          MOVE: <lfs_setleaf>-valsign   TO lwa_utype-sign,
                <lfs_setleaf>-valoption TO lwa_utype-option,
                <lfs_setleaf>-valfrom   TO lwa_utype-low.

          IF <lfs_setleaf>-valoption EQ 'BT'.
            MOVE <lfs_setleaf>-valto TO lwa_utype-high.
          ENDIF.

          APPEND lwa_utype TO s_utype.
          CLEAR lwa_utype.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF l_gl IS NOT INITIAL.
      SELECT setname
             valsign
             valoption
             valfrom
             valto
        FROM setleaf
        INTO TABLE li_gl
        WHERE setname EQ l_gl.
      IF sy-subrc EQ 0.
        UNASSIGN <lfs_setleaf>.
        LOOP AT li_gl ASSIGNING <lfs_setleaf>.
          MOVE: <lfs_setleaf>-valsign   TO lwa_gl-sign,
                <lfs_setleaf>-valoption TO lwa_gl-option,
                <lfs_setleaf>-valfrom   TO lwa_gl-low.

          IF <lfs_setleaf>-valoption EQ 'BT'.
            MOVE <lfs_setleaf>-valto TO lwa_gl-high.
          ENDIF.

          APPEND lwa_gl TO s_gl.
          CLEAR lwa_gl.
        ENDLOOP.
      ENDIF.
    ENDIF.

    REFRESH: r_ccode,
             r_ptype,
             r_utype,
             r_gl.

    r_ccode[] = s_ccode[].
    r_ptype[] = s_ptype[].
    r_utype[] = s_utype[].
    r_gl[]    = s_gl[].
  ENDIF.

ENDFORM.                    " F_GET_FROM_SETLEAF
*&---------------------------------------------------------------------*
*&      Form  F_REMOVE_FILE_FROM_UNIX
*&---------------------------------------------------------------------*
FORM f_remove_file_from_unix.

  DATA: l_file TYPE string.

  l_file = c_file.
  REPLACE FIRST OCCURRENCE OF c_amp IN l_file WITH sy-sysid.

  DELETE DATASET l_file.

ENDFORM.                    " F_REMOVE_FILE_FROM_UNIX
***End of Insert - XT18910 - D#50 - DV5K991085
