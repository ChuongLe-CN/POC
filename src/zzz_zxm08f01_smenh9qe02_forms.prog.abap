************************************************************************
*                         Canadian National                            *
*                                                                      *
* ABAP Name:  ZZZ_ZXM08F01_SMENH9QE02_FORMS                            *
* Title    :  Create an ERS transaction to process ERS invoice per BOL.*
* Work Unit:  SM-ENH-9QE-002                                           *
* Created by: Rob West                                                 *
* Created on: November 2010                                            *
* Version:    1.0                                                      *
*                                                                      *
* Purpose:    Pick up BOL# and Document date from the PO and populate  *
*             in the Baseline date and Reference fields in the FI      *
*             Vendor invoice document.                                 *
*             User exit MRMH0001 & MRMH0003                            *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************
*----------------------------------------------------------------------*
*        Modification log (From newest to oldest changes)              *
*----------------------------------------------------------------------*
* Changed / Created by       Date                Tracking number       *
* -----------------------   ----------          ---------------------- *
* Rob West                  2010/12/01          DV5K967981             *
*                                                                      *
* Short Description: FLCM defect 656                                   *
*                    Do 9QE logic only for fuel POs - BSART = 'FB'.    *
*----------------------------------------------------------------------*
* Bob Legault               2011/07/15          DV5K965072             *
*                                                                      *
* Short Description: Modified seach fields on the EKBE table to improve*
*                    performance                                       *
*----------------------------------------------------------------------*
* Rob West                  2010/11/29          DV5K961205             *
*                                                                      *
* Short Description: Creation for FLCM project (Fuel Life Cycle        *
*                    Management)                                       *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_SMENH9QE_MRRL
*&---------------------------------------------------------------------*
FORM f_smenh9qe_mrrl USING p_i_rbkpv            TYPE mrm_rbkpv
                           p_t_selwenr          TYPE STANDARD TABLE
                  CHANGING p_e_rbkpv_ers_change TYPE mrm_rbkpv_ers_change
                           p_e_change.

  DATA: l_bsart              TYPE ekko-bsart.              "DV5K967981

  FIELD-SYMBOLS: <selwenr>   TYPE ek08erswe.

*** Initialize the output areas
  CLEAR: p_e_rbkpv_ers_change,
         p_e_change.

  IF p_i_rbkpv IS INITIAL.
    RETURN.      "Leave if nothing to process
  ENDIF.

*** Get the first row in P_T_SELWENR table
  READ TABLE p_t_selwenr ASSIGNING <selwenr> CASTING INDEX 1.
  IF sy-subrc > 0.
    RETURN.
  ENDIF.

*** Check for Fuel related document                        "DV5K967981
  SELECT SINGLE bsart                                      "DV5K967981
    FROM ekko                                              "DV5K967981
    INTO l_bsart                                           "DV5K967981
    WHERE ebeln = <selwenr>-ebeln.                         "DV5K967981
  IF l_bsart <> 'FB'.           "Not a fuel PO             "DV5K967981
    RETURN.                                                "DV5K967981
  ENDIF.                                                   "DV5K967981

*** Move input fields to output fields
  MOVE-CORRESPONDING p_i_rbkpv TO p_e_rbkpv_ers_change.

*** Substitute lookup fields
  PERFORM f_smenh9qe_bldat USING <selwenr>-ebeln
                                 <selwenr>-ebelp
                        CHANGING p_e_rbkpv_ers_change-bldat
                                 p_e_change.

  PERFORM f_smenh9qe_sgtxt USING <selwenr>-ebeln
                                 <selwenr>-ebelp
                        CHANGING p_e_rbkpv_ers_change-sgtxt
                                 p_e_change.

  PERFORM f_smenh9qe_xblnr USING <selwenr>-ebeln
                        CHANGING p_e_rbkpv_ers_change-xblnr
                                 p_e_change.

ENDFORM.                    " F_SMENH9QE_MRRL



*&---------------------------------------------------------------------*
*&      Form  f_smenh9qe_mrnb
*&---------------------------------------------------------------------*
FORM f_smenh9qe_mrnb USING p_i_rbkpv        TYPE mrm_rbkpv
                           p_t_segtab       TYPE mmrap_tsegtab
                  CHANGING p_e_rbkpv_change TYPE mrm_rbkpv
                           p_e_change       TYPE c.

  TYPES: BEGIN OF lt_ekbe,
           ebeln             TYPE ekbe-ebeln,
           ebelp             TYPE ekbe-ebelp,
           gjahr             TYPE ekbe-gjahr,
           belnr             TYPE ekbe-belnr,
           bewtp             TYPE ekbe-bewtp,
         END OF lt_ekbe.

  DATA: li_ekbe              TYPE STANDARD TABLE OF lt_ekbe,
        l_bsart              TYPE ekko-bsart.              "DV5K967981

  FIELD-SYMBOLS: <ekbe>      TYPE lt_ekbe,
                 <segtab>    TYPE mmrap_segtab.

*** Initialize the output areas
  CLEAR: p_e_rbkpv_change,
         p_e_change.

  IF p_i_rbkpv IS INITIAL.
    RETURN.      "Leave if nothing to process
  ENDIF.

*** Get the first row of the P_T_SEGTAB table
  READ TABLE p_t_segtab ASSIGNING <segtab> INDEX 1.
  IF sy-subrc > 0.
    RETURN.
  ENDIF.

*** Get the PO document number
  SELECT ebeln
         ebelp
         gjahr
         belnr
         bewtp
    FROM ekbe
    INTO TABLE li_ekbe
* begin of DV5K965072
*   WHERE gjahr = <segtab>-lfgja AND
*         belnr = <segtab>-lfbnr.
    WHERE lfgja = <segtab>-lfgja AND
          lfbnr = <segtab>-lfbnr.
* end of DV5K965072

  IF sy-subrc = 0.
    READ TABLE li_ekbe ASSIGNING <ekbe>
        WITH KEY bewtp = 'E'.
    IF sy-subrc > 0.
      RETURN.
    ENDIF.
  ENDIF.

*** Check for Fuel related document                        "DV5K967981
  SELECT SINGLE bsart                                      "DV5K967981
    FROM ekko                                              "DV5K967981
    INTO l_bsart                                           "DV5K967981
    WHERE ebeln = <ekbe>-ebeln.                            "DV5K967981
  IF l_bsart <> 'FB'.           "Not a fuel PO             "DV5K967981
    RETURN.                                                "DV5K967981
  ENDIF.                                                   "DV5K967981

*** Move input fields to output fields
  p_e_rbkpv_change = p_i_rbkpv.

*** Substitute lookup fields
  PERFORM f_smenh9qe_bldat USING <ekbe>-ebeln
                                 <ekbe>-ebelp
                        CHANGING p_e_rbkpv_change-bldat
                                 p_e_change.

  PERFORM f_smenh9qe_sgtxt USING <ekbe>-ebeln
                                 <ekbe>-ebelp
                        CHANGING p_e_rbkpv_change-sgtxt
                                 p_e_change.

  PERFORM f_smenh9qe_xblnr USING <ekbe>-ebeln
                        CHANGING p_e_rbkpv_change-xblnr
                                 p_e_change.

ENDFORM.                    "f_smenh9qe_mrnb

*&---------------------------------------------------------------------*
*&      Form  f_smenh9qe_bldat
*&---------------------------------------------------------------------*
FORM f_smenh9qe_bldat      USING p_ebeln
                                 p_ebelp
                        CHANGING p_bldat
                                 p_change.

  DATA: l_bedat              TYPE ekko-bedat.

  SELECT SINGLE eindt      "Get document date from EKET
    FROM eket
    INTO l_bedat
    WHERE ebeln = p_ebeln AND
          ebelp = p_ebelp.
  IF sy-subrc = 0 AND p_bldat <> l_bedat.
    p_bldat = l_bedat.
    p_change = 'X'.      "Set change indicator
  ENDIF.

ENDFORM.                    "f_smenh9qe_bldat

*&---------------------------------------------------------------------*
*&      Form  f_smenh9qe_sgtxt
*&---------------------------------------------------------------------*
FORM f_smenh9qe_sgtxt    USING p_ebeln
                               p_ebelp
                      CHANGING p_sgtxt
                               p_change.

  DATA: l_txz01              TYPE ekpo-txz01.

  SELECT SINGLE txz01
    FROM ekpo
    INTO l_txz01
      WHERE ebeln = p_ebeln AND
            ebelp = p_ebelp.
  IF sy-subrc = 0 AND p_sgtxt <> l_txz01.
    p_sgtxt = l_txz01.
    p_change = 'X'.        "Set change indicator
  ENDIF.

ENDFORM.                    "f_smenh9qe_sgtxt

*&---------------------------------------------------------------------*
*&      Form  f_smenh9qe_xblnr
*&---------------------------------------------------------------------*
FORM f_smenh9qe_xblnr    USING p_ebeln
                      CHANGING p_xblnr
                               p_change.

  DATA: l_ihrez              TYPE ekko-ihrez.

  SELECT SINGLE ihrez
    FROM ekko
    INTO l_ihrez
    WHERE ebeln = p_ebeln.
  IF sy-subrc = 0 AND p_xblnr <> l_ihrez.
    p_xblnr = l_ihrez.
    p_change = 'X'.        "Set change indicator
  ENDIF.

ENDFORM.                    "f_smenh9qe_xblnr
