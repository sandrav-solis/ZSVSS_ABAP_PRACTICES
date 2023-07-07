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
           gtt_travel TYPE STANDARD TABLE OF zdlc_travel_001 WITH DEFAULT KEY.

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

  ENDMETHOD.
ENDCLASS.







