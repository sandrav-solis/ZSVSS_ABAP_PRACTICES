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

* 12) y 13) Lectura de tablas e inline declartions

    SELECT * FROM zdlc_travel_001
    INTO TABLE @DATA(it_travel_bd_12).

    "Lectura por index:
    DATA(wa_travel_12) = it_travel_bd_12[ 1 ].

    "Lectura condición where.
    wa_travel_12 = it_travel_bd_12[ travel_id = '00000022' ].

    "Asignación de un solo campo a una variable
    DATA(vl_travel_id) = VALUE #( it_travel_bd_12[ 1 ]-travel_id DEFAULT '000000' ).

    TRY .
        wa_travel_12 = it_travel_bd_12[ 2 ].
      CATCH cx_sy_itab_line_not_found.
        "Message error:
        "MESSAGE 'Read failed' TYPE 'E'.
    ENDTRY.

*    14) MOVE CORRESPONDING.
    SELECT * FROM zdlc_travel_001
   INTO TABLE @DATA(it_travel_bd_14).

    DATA wa_travel4_aux TYPE zdlc_travel_001.
    DATA it_travel_bd_14_AUX TYPE STANDARD TABLE OF zdlc_travel_001.

    "Normal way,
    "can use mapping to different names of fields,
    "and except to don transport the value.
    READ TABLE it_travel_bd_14 INTO DATA(wa_travel4) INDEX 1.
    wa_travel4_aux = CORRESPONDING #( wa_travel4 EXCEPT description ).

*   ¡¡¡¡¡¡ "CORRESPONDING DINAMIC !!!!!
*   ¡¡¡¡¡¡ "CORRESPONDING DINAMIC !!!!!
*   ¡¡¡¡¡¡ "CORRESPONDING DINAMIC !!!!!

    DATA: mapping_record TYPE cl_abap_corresponding=>mapping_info,
          mapping_table  TYPE cl_abap_corresponding=>mapping_table.

    mapping_record-level = 0.
    mapping_record-kind = cl_abap_corresponding=>mapping_component.

    mapping_record-dstname = 'MYKEY'.
    mapping_record-srcname = 'MYKEY'.
    APPEND mapping_record TO mapping_table.

    mapping_record-dstname = 'TRAVEL_ID'.
    mapping_record-srcname = 'TRAVEL_ID'.
    APPEND mapping_record TO mapping_table.

    mapping_record-dstname = 'DESCRIPTION'.
    mapping_record-srcname = 'DESCRIPTION'.
     mapping_record-KIND = 3. "VALUES: 1 - mapped to each other.
                              "        2 - is excluded from the mapping of identically named components.
                              "        3 - exclude

    APPEND mapping_record TO mapping_table.

    TRY.
        DATA(dynamic_mapper) = cl_abap_corresponding=>create(
        source = it_travel_bd_14
        destination = it_travel_bd_14_AUX
        mapping = mapping_table ).

        dynamic_mapper->execute(
        EXPORTING source = it_travel_bd_14
        CHANGING destination = it_travel_bd_14_AUX ).

      CATCH cx_corr_dyn_error ##NO_HANDLER."In real life you need one
        "Raise a Fatal Error Message
        RETURN.

    ENDTRY.

    LOOP AT it_travel_bd_14_AUX INTO wa_travel4_aux.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.







