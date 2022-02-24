"Name: \PR:RMMR1MRB\FO:AUFRUF_REPRUE\SE:BEGIN\EI
ENHANCEMENT 0 ZME_SMENH9QE_MRNB_CHECK.

  AUTHORITY-CHECK OBJECT 'Z_MRNB_UPD'
             ID 'ZRUNMODE' FIELD '02'.    "check for update enabled
  IF sy-subrc = 0.
*    PA_XTEST is left as-is from the selection screen
  ELSE.
    AUTHORITY-CHECK OBJECT 'Z_MRNB_UPD'
             ID 'ZRUNMODE' FIELD '01'.    "check for test mode enabled
    IF sy-subrc = 0.
      pa_xtest = abap_true.     "Override PA_XTEST to Test mode only
    ELSE.
      MESSAGE e034(aut).                  "not authorized.
    ENDIF.
  ENDIF.

ENDENHANCEMENT.
