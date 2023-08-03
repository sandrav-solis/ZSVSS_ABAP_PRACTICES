CLASS zcl_svss_new_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_svss_new_abap IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    TYPES:
      BEGIN OF ty_struct1,
        field1 TYPE i,
        field2 TYPE string,
      END OF ty_struct1,
      BEGIN OF ty_struct2,
        field1 TYPE i,
        field2 TYPE string,
        field3 TYPE i,
      END OF ty_struct2.

*Define table types
    TYPES: gtt_struct1 TYPE STANDARD TABLE OF ty_struct1 WITH DEFAULT KEY,
           gtt_struct2 TYPE STANDARD TABLE OF ty_struct2 WITH DEFAULT KEY,
           gtt_travel  TYPE STANDARD TABLE OF zdlc_travel_001 WITH DEFAULT KEY.

* 3) llenado de tablas.
    DATA(lt_source) = VALUE gtt_struct1(
        ( field1 = 1 field2 = 'A' )
        ( field1 = 2 field2 = 'B' ) ).

* 4) Usar for para llemar tablas internas
* blog: https://blogs.sap.com/2017/11/08/for-expression-in-abap-7.40-best-case-scenarios/
    DATA(lt_target1) = VALUE gtt_struct2(
        FOR lwa_source IN lt_source ( CORRESPONDING #( lwa_source ) ) ).

* 4) Usar for para llemar tablas internas con where
    SELECT * FROM zdlc_travel_001
    INTO TABLE @DATA(it_travel_bd).

    DATA(it_travel) = VALUE gtt_travel(
     FOR wa_travel IN it_travel_bd WHERE ( agency_id = '070001' ) ( CORRESPONDING #( wa_travel ) ) ).

* 8)TYPE REF TO DATA pag 222
    DATA: it_Travel8           TYPE STANDARD TABLE OF zdlc_travel_001,
          it_Travel_aux        TYPE STANDARD TABLE OF zdlc_travel_001,
          wa_travel_reference8 TYPE REF TO data.

    SELECT * FROM zdlc_travel_001
    INTO TABLE @it_Travel8.

    LOOP AT it_Travel8 INTO DATA(wa_travel8).
      "After 7.4
      wa_travel_reference8 = REF #( wa_travel8 ).

      IF sy-subrc = 0.
      ENDIF.
    ENDLOOP.

* 9) switch
    DATA: vl_cond   TYPE i,
          vl_result TYPE char30.

    vl_cond = sy-datum+6(1).

    "ejemplo case.
    CASE vl_cond.
      WHEN 1.
        vl_result = 'read'.
      WHEN 2.
        vl_result = 'modify'.
      WHEN 3.
        vl_result = 'delete'.
    ENDCASE.

    "en su lugar: menos líneas de cóodigo.
    vl_result = SWITCH string( vl_cond
      WHEN 1 THEN 'read'
      WHEN 2 THEN 'modify'
      WHEN 3 THEN 'delete' ).

    " Si no tenemos variable y queremos mostrar mensaje:
    vl_result = SWITCH string( vl_cond
    WHEN 1 THEN 'read'
    WHEN 2 THEN 'modify'
    WHEN 3 THEN 'delete' ).

    "con las consultas:
*    LOOP AT lt_vbpa ASSIGNING FIELD-SYMBOL(<ls_vbpa>).
*      INSERT VALUE #(
*      vbeln = <ls_vbpa>-vbeln
*      parvw = <ls_vbpa>-parvw
*      kunde = SWITCH #( <ls_vbpa>-parvw
*      WHEN 'AG' OR 'WE' OR 'RG' OR 'RE'
*      THEN <ls_vbpa>-kunnr
*      ELSE '00' && <ls_vbpa>-pernr )
*      adrnr = <ls_vbpa>-adrnr )
*      INTO TABLE gt_vakpa.
*    ENDLOOP


* 10) COND reemplaza IF/ELSE y también al CASE
       vl_result = COND string(
       WHEN vl_cond = 1 THEN 'read'
       WHEN vl_cond = 2 THEN 'modify'
       WHEN vl_cond = 3 THEN 'delete' ).

  ENDMETHOD.
ENDCLASS.







