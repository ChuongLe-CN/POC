"Name: \PR:RMMR1MRB\FO:CREATE_LIST\SE:BEGIN\EI
ENHANCEMENT 0 ZME_SMENH9QE.
*** Start of Implicit Enhancement - Rob West - 2011/03/17 - SM-ENH-9QE                   DV5K961206
  IF zcl_smenh9qe01_revaluation=>store_messages( it_segtab[] ) = 'X'.
    RETURN.      "Exit from form if data saved to ABAP memory
  ENDIF.
*** End of Implicit Enhancement - Rob West - 2011/03/17 - SM-ENH-9QE                     DV5K961206
ENDENHANCEMENT.
