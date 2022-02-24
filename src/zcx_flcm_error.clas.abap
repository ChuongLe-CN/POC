class ZCX_FLCM_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

*"* public components of class ZCX_FLCM_ERROR
*"* do not include other source files here!!!
public section.

  data MSGID type SY-MSGID .
  data MSGTY type SY-MSGTY .
  data MSGNO type SY-MSGNO .
  data MSGV1 type SY-MSGV1 .
  data MSGV2 type SY-MSGV2 .
  data MSGV3 type SY-MSGV3 .
  data MSGV4 type SY-MSGV4 .
  data SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS .
  data MSGTAB type BAPIRET2TAB .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !MSGID type SY-MSGID optional
      !MSGTY type SY-MSGTY optional
      !MSGNO type SY-MSGNO optional
      !MSGV1 type SY-MSGV1 optional
      !MSGV2 type SY-MSGV2 optional
      !MSGV3 type SY-MSGV3 optional
      !MSGV4 type SY-MSGV4 optional
      !SUB_STUS type ZMM_FLCM_TDS-ZTRAN_SUB_STUS optional
      !MSGTAB type BAPIRET2TAB optional .
  methods GET_BAPIMSG_TABLE
    returning
      value(RT_RETURN) type BAPIRET2_TAB .
protected section.
*"* protected components of class ZCX_FLCM_ERROR
*"* do not include other source files here!!!
private section.
*"* private components of class ZCX_FLCM_ERROR
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCX_FLCM_ERROR IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
me->MSGID = MSGID .
me->MSGTY = MSGTY .
me->MSGNO = MSGNO .
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
me->SUB_STUS = SUB_STUS .
me->MSGTAB = MSGTAB .
endmethod.


METHOD GET_BAPIMSG_TABLE.

  FIELD-SYMBOLS:
    <msg>            LIKE LINE OF rt_return.

  REFRESH rt_return.

  APPEND INITIAL LINE TO rt_return ASSIGNING <msg>.

  <msg>-type = msgty.
  <msg>-id = msgid.
  <msg>-number = msgno.
  <msg>-message_v1 = msgv1.
  <msg>-message_v2 = msgv2.
  <msg>-message_v3 = msgv3.
  <msg>-message_v4 = msgv4.

  MESSAGE ID <msg>-id TYPE <msg>-type NUMBER <msg>-number
          INTO <msg>-message
          WITH <msg>-message_v1
               <msg>-message_v2
               <msg>-message_v3
               <msg>-message_v4.

  APPEND LINES OF msgtab TO rt_return.

ENDMETHOD.
ENDCLASS.
