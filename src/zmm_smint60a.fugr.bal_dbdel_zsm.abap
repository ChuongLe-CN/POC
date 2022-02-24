FUNCTION bal_dbdel_zsm.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_T_LOGS_TO_DELETE) TYPE  BALHDR_T
*"     REFERENCE(I_IN_UPDATE_TASK) TYPE  BOOLEAN
*"----------------------------------------------------------------------

  FIELD-SYMBOLS: <hdr>          TYPE balhdr.

  LOOP AT i_t_logs_to_delete ASSIGNING <hdr>.
    DELETE FROM DATABASE bal_indx(zs) ID <hdr>-lognumber.
  ENDLOOP.

ENDFUNCTION.
