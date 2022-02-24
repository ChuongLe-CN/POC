*"* use this source file for any type declarations (class
*"* definitions, interfaces or data types) you need for method
*"* implementation or private method's signature

TYPES: BEGIN OF t_ekko,
         ebeln TYPE ekko-ebeln,
         lifnr TYPE ekko-lifnr,
         ihrez TYPE ekko-ihrez,
         unsez TYPE ekko-unsez,
         END OF t_ekko,
       BEGIN OF t_ekpo,
         ebeln TYPE ekpo-ebeln,
         ebelp TYPE ekpo-ebelp,
         loekz TYPE ekpo-loekz,
         matnr TYPE ekpo-matnr,
         ematn TYPE ekpo-ematn,
         knttp TYPE ekpo-knttp,
         elikz TYPE ekpo-elikz,
         retpo TYPE ekpo-retpo,
         bednr TYPE ekpo-bednr,
         END OF t_ekpo,
       BEGIN OF t_contract,
*        EKKO
         ebeln TYPE ekko-ebeln,
         bsart TYPE ekko-bsart,
         lifnr TYPE ekko-lifnr,
         kdatb TYPE ekko-kdatb,
         kdate TYPE ekko-kdate,
         konnr TYPE ekko-konnr,
*        EKPO
         ebelp TYPE ekpo-ebelp,
         meins TYPE ekpo-meins,
         ktpnr TYPE ekpo-ktpnr,
         END OF t_contract,
       BEGIN OF t_agreement,
         number  TYPE bapimeoutheader-number,
         item_no TYPE bapimeoutaddrdelivery-item_no,
         sort1   TYPE bapimeoutaddrdelivery-sort1,
         END OF t_agreement.
